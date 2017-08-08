;;; amx.el --- M-x interface with Ido-style fuzzy matching. -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2014 Cornelius Mika and contributors
;;
;; Author: Cornelius Mika <cornelius.mika@gmail.com> and contributors
;; URL: http://github.com/DarwinAwardWinner/amx/
;; Package-Requires: ((emacs "24.4"))
;; Version: 4.0
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;; run (amx-initialize)
;;
;; Bind the following commands:
;; amx, amx-major-mode-commands
;;
;; For a detailed introduction see:
;; http://github.com/DarwinAwardWinner/amx/blob/master/README.markdown

;;; Code:

(require 'cl-lib)
(require 'ido)

(defvar amx-initialized nil
  "If non-nil amx is initialized.")

(defvar amx-cache)
(defvar amx-data)
(defvar amx-history)

(defvar amx-command-count 0
  "Number of commands known to amx.")

(defvar amx-custom-action nil
  "If non-nil, amx will call this in place of `execute-extended-command'.")

(defvar amx-minibuffer-depth -1
  "Used to determin if amx \"owns\" the current active minibuffer.")

(defvar amx-command-keybind-hash nil
  "Hash table for translating between commands and key bindings.

See `amx-make-keybind-hash'.")

(defvar amx-last-active-maps nil
  "List of keymaps last used to update `amx-command-keybind-hash'.

When `amx-command-keybind-hash' is updated, this is set to the
value of `(current-active-maps)' at that time. This is used to
figure out whether to invalidate the hash table for the next call
to amx.")

(defvar amx-origin-buffer nil
  "The buffer amx was called from.

This is used to determine which buffer's key bindings to use when
`amx-show-key-bindings' is non-nil.")

(defvar amx-known-backends nil
  "Plist of known amx completion backends.")

(defvar amx-temp-prompt-string nil
  "if non-nil, overrides `amx-prompt-string' once.

Each time `amx-prompt-with-prefix-arg' is called, this is reset
to nil.")

;; This timer will run every time Emacs is idle for 1 second, but most
;; of the time it will do nothing.
(defvar amx-short-idle-update-timer nil)
;; This timer forces a periodic updates to happen if you walk away for
;; a few hours, so that amx won't wait until you come back to do a
;; periodic update
(defvar amx-long-idle-update-timer nil)

(defvar amx-last-update-time nil
  "Time when `amx-update' was last run.

If nil, a `amx-update' is needed ASAP.")

(cl-defstruct amx-backend
  name
  required-feature
  comp-fun
  get-text-fun
  exit-fun)

(defgroup amx nil
  "M-x interface with Ido-style fuzzy matching and ranking heuristics."
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "amx.el"))

;;;###autoload
(define-minor-mode amx-mode
  ;; TODO Update all references to ido
  "Use ido completion for M-x"
  :global t
  :group 'amx
  (if amx-mode
      (progn
        (amx-initialize)
        (global-set-key [remap execute-extended-command] 'amx))
    (when (eq (global-key-binding [remap execute-extended-command]) 'amx)
      (global-unset-key [remap execute-extended-command]))))

(defun amx-set-auto-update-interval (symbol value)
  "Custom setter for `amx-auto-update-interval'.

Arguments are the same as in `set-default'.

In addition to setting the variable, this will also set up an
idle timer to ensure that updates happen when idle."
  (cl-assert (eq symbol 'amx-auto-update-interval))
  (set-default symbol value)
  ;; Cancel any previous timer
  (when amx-long-idle-update-timer
    (cancel-timer amx-long-idle-update-timer)
    (setq amx-long-idle-update-timer nil))
  (when value
    ;; Enable idle updating
    (setq amx-long-idle-update-timer
          (run-with-idle-timer (1+ (* 60 value)) t
                               'amx-idle-update))))

(defcustom amx-auto-update-interval nil
  "Time in minutes between periodic updates of the command list.

Amx already updates the command list after functions like `load'
and `eval-expression' that could possibly define new commands.
Generally this should be enough to catch all newly-loaded
commands, but just in case any slip through, you can enable
periodic updates to catch them. If this variable is nil, no
periodic updates will be performed."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Minutes"))
  :set #'amx-set-auto-update-interval)

(defun amx-set-save-file (symbol value)
  "Custom setter for `amx-backend'.

Arguments are the same as in `set-default'.

This function will refuse to set the backend unless it can load
the associated feature, if any."
  (cl-assert (eq symbol 'amx-save-file))
  (set-default symbol value)
  ;; Reinitialize from the new save file if it exists (but only if amx
  ;; is already initialized, and only after amx is finished loading)
  (when (and (bound-and-true-p amx-initialized)
             (file-exists-p amx-save-file))
    (eval-after-load 'amx
      (amx-initialize t))))

(defcustom amx-save-file (locate-user-emacs-file "amx-items" ".amx-items")
  "File in which the amx state is saved between Emacs sessions.

Variables stored are: `amx-data', `amx-history'."
  :type '(choice (string :tag "File name")
                 (const :tag "Don't save" nil))
  :set #'amx-set-save-file)

(defcustom amx-history-length 7
  "Number of recently executed commands to record."
  ;; TODO allow this to be set any time
  :type 'integer)

(defcustom amx-show-key-bindings t
  "If non-nil, show key binding while completing commands."
  :type 'boolean)

(defcustom amx-prompt-string "M-x "
  "String to display in the Amx prompt."
  :type 'string)

(defcustom amx-ignored-command-matchers
  '("\\`self-insert-command\\'"
    "\\`ad-Orig-"
    "\\`menu-bar"
    amx-command-marked-ignored-p
    amx-command-obsolete-p
    amx-command-mouse-interactive-p)
  "List of regexps and/or functions.

Each element is either a regular expression or a function of one
argument. Commands that match any of the regexps or return
non-nil for any of these functions will be hidden from the amx
completion list."
  :type '(repeat
          (choice
           (regexp :tag "Regular expression")
           (function :tag "Function"))))

;;--------------------------------------------------------------------------------
;; Amx Internals

(defun amx-get-command-name (cmd)
  "Return CMD as a string.

CMD can be a string, symbol, or cons cell whose `car' is a string
or symbol."
  (cond
   ((symbolp cmd)
    (symbol-name cmd))
   ((consp cmd)
    (amx-get-command-name (car cmd)))
   ((stringp cmd)
    cmd)
   (t
    (error "Unrecognized command: %s" cmd))))

(defun amx-get-default (choices)
  "Get the first entry from CHOICES as a string."
  (amx-augment-command-with-keybind
   (amx-get-command-name
    (car
     (if (listp choices)
         choices
       (all-completions "" choices))))
   (when amx-show-key-bindings (amx-update-keybind-hash))))

;;--------------------------------------------------------------------------------
;; Amx Interface

;;;###autoload
(defun amx ()
  (interactive)
  (amx-initialize)
  (if (amx-active)
      (amx-update-and-rerun)
    (amx-update-if-needed)
    (amx-read-and-run amx-cache)))

(defun amx-active ()
  "Return non-nil if amx is currently using the minibuffer"
  (>= amx-minibuffer-depth (minibuffer-depth)))

(defun amx-update-and-rerun ()
  (let ((new-initial-input
         (funcall (amx-backend-get-text-fun (amx-get-backend)))))
    (amx-do-with-selected-item
     (lambda (_) (amx-update) (amx-read-and-run amx-cache new-initial-input)))))

(defun amx-read-and-run (commands &optional initial-input)
  (let* ((amx-origin-buffer
          (or amx-origin-buffer (current-buffer)))
         (commands
          ;; Add key bindings to completions
          (if amx-show-key-bindings
              (amx-augment-commands-with-keybinds commands)
            commands))
         (collection
          ;; Initially complete with only non-ignored commands, but if
          ;; all of those are ruled out, allow completing with ignored
          ;; commands.
          (apply-partially #'completion-table-with-predicate
                           commands
                           (lambda (cmd) (not (amx-command-ignored-p cmd)))
                           nil))
         ;; Symbol
         (chosen-item
          (amx-clean-command-name
           (amx-completing-read commands :initial-input initial-input)))
         ;; String
         (chosen-item-name (symbol-name chosen-item)))
    (cl-assert (commandp chosen-item))
    (if amx-custom-action
        (let ((action amx-custom-action))
          (setq amx-custom-action nil)
          (funcall action chosen-item))
      (unwind-protect
          ;; Don't warn about non-interactive use of
          ;; `execute-extended-command'
          (with-no-warnings
            (execute-extended-command current-prefix-arg chosen-item-name))
        (amx-rank chosen-item)))))

;;;###autoload
(defun amx-major-mode-commands ()
  "Like `amx', but limited to commands that are relevant to the active major mode."
  (interactive)
  (amx-initialize)
  (let ((commands (delete-dups (append (amx-extract-commands-from-keymap (current-local-map))
                                       (amx-extract-commands-from-features major-mode)))))
    (setq commands (amx-sort-according-to-cache commands))
    (setq commands
          (apply-partially #'completion-table-with-predicate
                           commands
                           (lambda (cmd) (not (amx-command-ignored-p cmd)))
                           nil))
    (amx-read-and-run commands)))

(defvar amx-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-h f") 'amx-describe-function)
    (define-key keymap (kbd "C-h w") 'amx-where-is)
    (define-key keymap (kbd "M-.") 'amx-find-function)
    keymap)
  "Additional key bindings for amx completion.")

(defvar amx-ido-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-a") 'move-beginning-of-line)
    (set-keymap-parent keymap amx-map)
    keymap))

(defun amx-prepare-ido-bindings ()
  ;; FIXME: defvar ido vars?
  (setq ido-completion-map
        (make-composed-keymap (list amx-ido-map ido-completion-map))))

(defun amx-default-exit-minibuffer ()
  "Run the key binding for RET.

This should work for most completion backends, without having to
know exactly which functions each one uses to exit the
minibuffer.."
  (execute-kbd-macro (kbd "RET")))

(cl-defun amx-completing-read (choices &key initial-input predicate)
  (let ((amx-minibuffer-depth (1+ (minibuffer-depth)))
        (comp-fun (amx-backend-comp-fun (amx-get-backend))))
    (funcall comp-fun choices :initial-input initial-input
             ;; Work around a bug
             :predicate (or predicate #'identity))))

(defun amx-prompt-with-prefix-arg ()
  (let ((amx-prompt-string
         (or amx-temp-prompt-string amx-prompt-string)))
    (setq amx-temp-prompt-string nil)
    (if (not current-prefix-arg)
        amx-prompt-string
      (concat
       (if (eq current-prefix-arg '-)
           "- "
         (if (integerp current-prefix-arg)
             (format "%d " current-prefix-arg)
           (if (= (car current-prefix-arg) 4)
               "C-u "
             (format "%d " (car current-prefix-arg)))))
       amx-prompt-string))))

;;--------------------------------------------------------------------------------
;; Pluggable Backends

(cl-defun amx-define-backend (&key name comp-fun get-text-fun
                                   (exit-fun 'amx-default-exit-minibuffer)
                                   required-feature)
  (cl-assert
   (and (symbolp name) name
        (functionp comp-fun)
        (functionp get-text-fun)
        (functionp exit-fun)
        (symbolp required-feature))
   nil
   "Invalid amx backend spec: (:name %S :comp-fun %S :get-text-fun %S :exit-fun %S)"
   (list name comp-fun get-text-fun exit-fun))
  (let ((backend
         (make-amx-backend :name name
                           :comp-fun comp-fun
                           :get-text-fun get-text-fun
                           :exit-fun exit-fun
                           :required-feature required-feature)))
    (setq amx-known-backends
          (plist-put amx-known-backends name backend))))

(cl-defun amx-get-backend (&optional (backend amx-backend))
  (cond
   ((amx-backend-p backend)
    backend)
   ((plist-get amx-known-backends backend))
   (t (error "Unknown amx backed %S" backend))))

(cl-defun amx-completing-read-default (choices &key initial-input predicate)
  "Amx backend for default Emacs completion"
  (require 'minibuf-eldef)
  (let ((minibuffer-completion-table choices)
        (prompt (concat (amx-prompt-with-prefix-arg)
                        (format "[%s]: " (amx-get-default choices))))
        (prev-eldef-mode minibuffer-electric-default-mode))
    (unwind-protect
        (progn
          (minibuffer-electric-default-mode 1)
          (minibuffer-with-setup-hook
              (lambda ()
                (use-local-map (make-composed-keymap
                                (list amx-map (current-local-map)))))
            (completing-read-default
             prompt choices predicate t initial-input
             'extended-command-history
             (amx-get-default choices))))
      (minibuffer-electric-default-mode
       (if prev-eldef-mode 1 0)))))

(defun amx-default-get-text ()
  "Default function for getting the user's current text input.

May not work for things like ido and ivy."
  (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))

(amx-define-backend
 :name 'standard
 :comp-fun 'amx-completing-read-default
 :get-text-fun 'amx-default-get-text)

(declare-function ido-completing-read+ "ext:ido-completing-read+")

(cl-defun amx-completing-read-ido (choices &key initial-input predicate)
  "Amx backend for ido completion"
  (require 'ido-completing-read+)
  (let ((ido-completion-map ido-completion-map)
        (ido-setup-hook (cons 'amx-prepare-ido-bindings ido-setup-hook))
        (minibuffer-completion-table choices))
    (ido-completing-read+ (amx-prompt-with-prefix-arg) choices predicate t
                          initial-input 'extended-command-history
                          (amx-get-default choices))))

(defun amx-ido-get-text ()
  ido-text)

(amx-define-backend
 :name 'ido
 :comp-fun 'amx-completing-read-ido
 :get-text-fun 'amx-ido-get-text
 :required-feature 'ido-completing-read+)

(declare-function ivy-read "ext:ivy")

(cl-defun amx-completing-read-ivy (choices &key initial-input predicate)
  "Amx backend for ivy completion"
  (require 'ivy)
  (ivy-read (amx-prompt-with-prefix-arg) choices
            :predicate predicate
            :keymap amx-map
            :history 'extended-command-history
            :initial-input initial-input
            :preselect (amx-get-default choices)))

(defvar ivy-text)

(defun amx-ivy-get-text ()
  ivy-text)

(amx-define-backend
 :name 'ivy
 :comp-fun 'amx-completing-read-ivy
 :get-text-fun 'amx-ivy-get-text
 :required-feature 'ivy)

(cl-defun amx-completing-read-auto (choices &key initial-input predicate)
  "Automatically select between ivy, ido, and standard completion."
  (let ((amx-backend
         (cond
          ((bound-and-true-p ivy-mode) 'ivy)
          ((or (bound-and-true-p ido-mode)
               (bound-and-true-p ido-ubiquitous-mode))
           'ido)
          (t 'standard))))
    (amx-completing-read choices :initial-input initial-input :predicate predicate)))

(amx-define-backend
 :name 'auto
 :comp-fun 'amx-completing-read-auto
 :get-text-fun (lambda () (error "This exit function should never be called."))
 :exit-fun (lambda () (error "This get-text function should never be called.")))

(defun amx-set-backend (symbol value)
  "Custom setter for `amx-backend'.

Arguments are the same as in `set-default'.

This function will refuse to set the backend unless it can load
the associated feature, if any."
  (cl-assert (eq symbol 'amx-backend))
  (let* ((backend (or (plist-get amx-known-backends value)
                      (error "Unknown amx backend: %s" value)))
         (feature (amx-backend-required-feature backend)))
    (when feature
      (unless (require feature nil 'noerror)
        (error "You must install %s to use the %s backend for amx"
               feature value))))
  ;; If we got through that, then actually set the variable
  (set-default symbol value))

(defcustom amx-backend 'auto
  "Completion function to select a candidate from a list of strings.

This function should take the same arguments as
`amx-completing-read': CHOICES and INITIAL-INPUT.

By default, an appropriate method is selected based on whether
`ivy-mode' or `ido-mode' is enabled."
  :type '(choice
          (const :tag "Auto-select" auto)
          (const :tag "Ido" ido)
          (const :tag "Ivy" ivy)
          (const :tag "Standard" standard)
          (symbol :tag "Custom backend"))
  :set #'amx-set-backend)

;;--------------------------------------------------------------------------------
;; Cache and Maintenance

(defun amx-rebuild-cache ()
  (interactive)
  (setq amx-cache nil)

  ;; Build up list 'new-commands' and later put it at the end of 'amx-cache'.
  ;; This speeds up sorting.
  (let (new-commands)
    (mapatoms (lambda (symbol)
                (when (commandp symbol)
                  (let ((known-command (assq symbol amx-data)))
                    (if known-command
                        (setq amx-cache (cons known-command amx-cache))
                      (setq new-commands (cons (list symbol) new-commands)))))))
    (if (eq (length amx-cache) 0)
        (setq amx-cache new-commands)
      (setcdr (last amx-cache) new-commands)))

  (setq amx-cache (sort amx-cache 'amx-sorting-rules))
  (amx-restore-history))

(defun amx-convert-for-ido (command-items)
  (mapcar (lambda (command-item) (symbol-name (car command-item))) command-items))

(defun amx-restore-history ()
  "Rearranges `amx-cache' according to `amx-history'"
  (if (> (length amx-history) amx-history-length)
      (setcdr (nthcdr (- amx-history-length 1) amx-history) nil))
  (mapc (lambda (command)
          (unless (eq command (caar amx-cache))
            (let ((command-cell-position (amx-detect-position
                                          amx-cache
                                          (lambda (cell)
                                            (eq command (caar cell))))))
              (when command-cell-position
                (let ((command-cell (amx-remove-nth-cell
                                     command-cell-position amx-cache)))
                  (setcdr command-cell amx-cache)
                  (setq amx-cache command-cell))))))
        (reverse amx-history)))

(defun amx-sort-according-to-cache (list)
  "Sorts a list of commands by their order in `amx-cache'"
  (let (sorted)
    (dolist (command-item amx-cache)
      (let ((command (car command-item)))
        (when (memq command list)
          (setq sorted (cons command sorted))
          (setq list (delq command list)))))
    (nreverse (append list sorted))))

(defun amx-update ()
  (interactive)
  (amx-save-history)
  (amx-rebuild-cache)
  (setq amx-last-update-time (current-time)))

(defun amx-detect-new-commands ()
  "Return non-nil if the number of defined commands has changed.

The return value is actually the new count of commands."
  (let ((i 0))
    (mapatoms (lambda (symbol) (if (commandp symbol) (setq i (1+ i)))))
    (unless (= i amx-command-count)
      (setq amx-command-count i))))

(defun amx-update-if-needed (&optional count-commands)
  "Run `amx-update' if necessary.

If `amx-last-update-time' is nil, do an update unconditionally.
Otherwise, if optional arg COUNT-COMMANDS is non-nil, count the
total number of defined commands in `obarray' and update if it
has changed."
  (when (or (null amx-last-update-time)
            (and count-commands
                 (amx-detect-new-commands)))
    (amx-update)))

;;;###autoload
(defun amx-initialize (&optional reinit)
  "Ensure that amx is properly initialized.

This function is normally idempotent, only having an effect the
first time it is called, so it is safe to call it at the
beginning of any function that expects amx to be initialized.
However, optional arg REINIT forces the initialization needs to
be re-run. Interactively, reinitialize when a prefix arg is
provided."
  (interactive "P")
  (when (or reinit (not amx-initialized))
    (amx-load-save-file)
    (amx-detect-new-commands)
    (amx-rebuild-cache)
    (add-hook 'kill-emacs-hook 'amx-save-to-file)
    (setq amx-initialized t)))

(defsubst amx-buffer-not-empty-p ()
  "Returns non-nil if current buffer contains a non-space character."
  (string-match-p "\[^[:space:]\]" (buffer-string)))

(defun amx-load-save-file ()
  "Loads `amx-history' and `amx-data' from `amx-save-file'"
  (setq amx-history nil amx-data nil)
  (when amx-save-file
    (let ((save-file (expand-file-name amx-save-file)))
      (when (file-readable-p save-file)
        (with-temp-buffer
          (insert-file-contents save-file)
          (condition-case nil
              (setq amx-history (read (current-buffer))
                    amx-data    (read (current-buffer)))
            (error (if (amx-buffer-not-empty-p)
                       (error "Invalid data in amx-save-file (%s). Can't restore history."
                              amx-save-file)
                     (unless (boundp 'amx-history) (setq amx-history nil))
                     (unless (boundp 'amx-data)    (setq amx-data nil))))))))))

(defun amx-save-history ()
  "Updates `amx-history'"
  (setq amx-history
        (cl-loop
         for i from 1 upto amx-history-length
         for (command-name . count) in amx-cache
         collect command-name)))

;; A copy of `ido-pp' that's compatible with lexical bindings
(defun amx-pp* (list list-name)
  (let ((print-level nil) (eval-expression-print-level nil)
        (print-length nil) (eval-expression-print-length nil))
    (insert "\n;; ----- " list-name " -----\n(\n ")
    (while list
      (let* ((elt (car list))
             (s (if (consp elt) (car elt) elt)))
        (if (and (stringp s) (= (length s) 0))
            (setq s nil))
        (if s
            (prin1 elt (current-buffer)))
        (if (and (setq list (cdr list)) s)
            (insert "\n "))))
    (insert "\n)\n")))

(defmacro amx-pp (list-var)
  `(amx-pp* ,list-var ,(symbol-name list-var)))

(defun amx-save-to-file ()
  (interactive)
  ;; If `init-file-user' is nil, we are running under "emacs -Q", so
  ;; don't save anything to disk
  (if init-file-user
      (when amx-save-file
        (amx-save-history)
        (with-temp-file (expand-file-name amx-save-file)
          (amx-pp amx-history)
          (amx-pp amx-data)))
    (display-warning 'amx "Not saving amx state from \"emacs -Q\".")))

;;--------------------------------------------------------------------------------
;; Ranking

(defun amx-sorting-rules (command-item other-command-item)
  "Returns true if COMMAND-ITEM should sort before OTHER-COMMAND-ITEM."
  (let* ((count        (or (cdr command-item      ) 0))
         (other-count  (or (cdr other-command-item) 0))
         (name         (car command-item))
         (other-name   (car other-command-item))
         (length       (length (symbol-name name)))
         (other-length (length (symbol-name other-name))))
    (or (> count other-count)                         ; 1. Frequency of use
        (and (= count other-count)
             (or (< length other-length)              ; 2. Command length
                 (and (= length other-length)
                      (string< name other-name))))))) ; 3. Alphabetical order

(defun amx-rank (command)
  (let ((command-item (or (assq command amx-cache)
                          ;; Update caches and try again if not found.
                          (progn (amx-update)
                                 (assq command amx-cache)))))
    (when command-item
      (amx-update-counter command-item)

      ;; Don't touch the cache order if the chosen command
      ;; has just been execucted previously.
      (unless (eq command-item (car amx-cache))
        (let (command-cell
              (pos (amx-detect-position amx-cache (lambda (cell)
                                                    (eq command-item (car cell))))))
          ;; Remove the just executed command.
          (setq command-cell (amx-remove-nth-cell pos amx-cache))
          ;; And put it on top of the cache.
          (setcdr command-cell amx-cache)
          (setq amx-cache command-cell)

          ;; Now put the last history item back to its normal place.
          (amx-sort-item-at amx-history-length))))))

(defun amx-update-counter (command-item)
  (let ((count (cdr command-item)))
    (setcdr command-item
            (if count
                (1+ count)
              ;; Else: Command has just been executed for the first time.
              ;; Add it to `amx-data'.
              (if amx-data
                  (setcdr (last amx-data) (list command-item))
                (setq amx-data (list command-item)))
              1))))

(defun amx-sort-item-at (n)
  "Sorts item at position N in `amx-cache'."
  (let* ((command-cell (nthcdr n amx-cache))
         (command-item (car command-cell)))
    (let ((insert-at (amx-detect-position
                      command-cell
                      (lambda (cell)
                        (amx-sorting-rules command-item (car cell))))))
      ;; TODO: Should we handle the case of 'insert-at' being nil?
      ;; This will never happen in practice.
      (when (> insert-at 1)
        (setq command-cell (amx-remove-nth-cell n amx-cache))
        ;; amx-cache just got shorter by one element, so subtract '1' from insert-at.
        (setq insert-at (+ n (- insert-at 1)))
        (amx-insert-cell command-cell insert-at amx-cache)))))

(defun amx-detect-position (cell function)
  "Detects, relatively to CELL, the position of the cell
on which FUNCTION returns true.
Only checks cells after CELL, starting with the cell right after CELL.
Returns nil when reaching the end of the list."
  (let ((pos 1))
    (catch 'break
      (while t
        (setq cell (cdr cell))
        (if (not cell)
            (throw 'break nil)
          (if (funcall function cell) (throw 'break pos))
          (setq pos (1+ pos)))))))

(defun amx-remove-nth-cell (n list)
  "Removes and returns the Nth cell in LIST."
  (let* ((previous-cell (nthcdr (- n 1) list))
         (result (cdr previous-cell)))
    (setcdr previous-cell (cdr result))
    result))

(defun amx-insert-cell (new-cell n list)
  "Inserts cell at position N in LIST."
  (let* ((cell (nthcdr (- n 1) list))
         (next-cell (cdr cell)))
    (setcdr (setcdr cell new-cell) next-cell)))

;;--------------------------------------------------------------------------------
;; Display key bindings in completions

(defun amx-make-keybind-hash (&optional keymap)
  "Return a hash table of all commands that might be bound in KEYMAP.

The KEYMAP argument is interpreted as in `where-is-internal'.

The hash will actually contain two kinds of mappings. Symbol keys
are mappings of command symbols to key bindings, while string
keys are mappings of string representations of the command and
its binding together, e.g. \"forward-char (C-f)\", to the command
symbol by itself."
  (let* ((keymap-list
          (cond
           ((keymapp keymap)
            (list keymap global-map))
           ((null keymap)
            ;; Run `current-active-maps' in `amx-origin-buffer' if
            ;; any
            (with-current-buffer (or amx-origin-buffer (current-buffer))
              (current-active-maps)))
           ((listp keymap)
            keymap)))
         (composed-keymap
          (make-composed-keymap keymap-list)))
    (cl-loop
     with bindhash = (make-hash-table :test 'equal)
     for kseq being the key-seqs of composed-keymap using (key-bindings cmd)
     for curbind = (gethash cmd bindhash)
     ;; Only take the first binding for each command
     if (and (not curbind) (commandp cmd))
     ;; Let's abuse this hash by storing two different
     ;; kinds of key/values pairs in it
     do (progn
          ;; cmd => key
          (puthash cmd (key-description kseq) bindhash)
          ;; "cmd (key)" => cmd, for looking up the original command
          (puthash (format "%s (%s)" cmd (key-description kseq)) cmd bindhash))
     finally return bindhash)))

(defun amx-invalidate-keybind-hash (&rest args)
  "Force a rebuild of `amx-command-keybind-hash'.

This function takes any number of arguments and ignores them so
that it can be used as advice on other functions."
  (setq amx-command-keybind-hash nil
        amx-last-active-maps nil))

(defun amx-maybe-invalidate-keybind-hash ()
  "If `amx-command-keybind-hash' is stale, set it to nil.

Returns non-nil if the hash is still valid and nil if it was
invalidated. This uses `amx-last-active-maps' to figure out if
the set of active keymaps has changed since the last rebuild of
`amx-command-keybind-hash'. Note that this function does not, by
itself, detect when new keys are bound in the current active
keymaps."
  (let ((valid
         (and amx-command-keybind-hash
              amx-last-active-maps
              (equal
               amx-last-active-maps
               (with-current-buffer (or amx-origin-buffer (current-buffer))
                 (current-active-maps))))))
    (unless valid
      (amx-invalidate-keybind-hash))
    valid))

(defun amx-update-keybind-hash ()
  "Update (if needed) and return `amx-command-keybind-hash'.

 If so, it rebuilds it based on the
current set of active keymaps.e"
  (amx-maybe-invalidate-keybind-hash)
  (or amx-command-keybind-hash
      (setq amx-command-keybind-hash (amx-make-keybind-hash))))

;; Since keymaps can contain other keymaps, checking whether these
;; functions are affecting the current active maps (or any maps
;; contained in them) is not much faster than just rebuilding the hash
;; table from scratch.
(cl-loop
 for fun in '(define-key set-keymap-parent)
 do (advice-add fun :before 'amx-invalidate-keybind-hash))

(defsubst amx-augment-command-with-keybind (command &optional bind-hash)
  (let* ((cmdname (amx-get-command-name command))
         (cmdsym (intern cmdname))
         (keybind (and bind-hash (gethash cmdsym bind-hash))))
    (if (and keybind (not (amx-command-ignored-p cmdsym)))
        (format "%s (%s)" cmdname keybind)
      cmdname)))

(defun amx-augment-commands-with-keybinds
    (commands &optional bind-hash)
  "Append key bindings from BIND-HASH to COMMANDS.

Given a list of commands (either as symbols or cons cells in the
form of `amx-cache'), returns an equivalent list, except that
every command is converted to a string, and any command with a
key binding recorded in `BIND-HASH will have that binding
appended. By default, key bindings are looked up in
`amx-command-keybind-hash', which is updated using
`amx-make-keybind-hash' if necessary.

In the returned list, each element will be a string."
  (cl-loop
   ;; Default to `amx-command-keybind-hash', updating it if
   ;; necessary.
   with bind-hash = (or bind-hash (amx-update-keybind-hash))
   for cmd in commands
   collect (amx-augment-command-with-keybind cmd bind-hash)))

(defun amx-clean-command-name (command-name)
  "Inverse of `amx-augment-commands-with-keybinds', approximately.

Given a string starting with a command name and possibly ending
with a key binding, it returns just the command name as a
symbol."
  (or
   ;; First try getting it from the hash table
   (and amx-command-keybind-hash
        (gethash command-name amx-command-keybind-hash))
   ;; Otherwise chop chars off the end until the result is a command
   (cl-loop
    for s = (cl-copy-seq command-name) then (substring s 0 -1)
    for sym = (intern-soft s)
    if (and sym (commandp sym))
    return sym
    if (= 0 (length s))
    do (error "Could not find command: %S" command-name))))

;;--------------------------------------------------------------------------------
;; Ignored commands

(defun amx-command-ignored-p (command)
  "Return non-nil if COMMAND is ignored by amx completion.

See `amx-ignored-command-matchers'."
  ;; Allow passing entries from `amx-cache', whose `car' is the
  ;; command symbol.
  (when (consp command)
    (setq command (car command)))
  ;; Command might be a string like "CMD (KEY)", requiring a lookup of
  ;; the real command name
  (when (stringp command)
    (setq command (gethash command amx-command-keybind-hash (intern command))))
  (cl-loop
   with matched = nil
   for matcher in amx-ignored-command-matchers
   ;; regexp
   if (stringp matcher)
   do (setq matched (string-match-p matcher (symbol-name command)))
   ;; function
   else
   do (setq matched (funcall matcher command))
   if matched return t
   finally return nil))

(defun amx-command-marked-ignored-p (command)
  "Return non-nil if COMMAND's `amx-ignored' property is non-nil.

See `amx-ignore-command'."
  ;; Allow passing entries from `amx-cache', whose `car' is the
  ;; command symbol.
  (when (consp command)
    (setq command (car command)))
  (get command 'amx-ignored))

(defun amx-command-obsolete-p (command)
  "Return non-nil if COMMAND is marked obsolete."
  (get command 'byte-obsolete-info))

(defun amx-command-mouse-interactive-p (command)
  "Return non-nil if COMMAND uses mouse events.

This is not guaranteed to detect all mouse-interacting commands,
but it should find most of them."
  (and (listp (help-function-arglist command))
       (not (eq ?\& (aref (symbol-name (car (help-function-arglist command))) 0)))
       (stringp (cadr (interactive-form command)))
       (string-match-p "\\`[*@^]*e" (cadr (interactive-form command)))))

(cl-defun amx-ignore-command (command &optional (do-ignore t))
  "Tell amx to ignore COMMAND.

Ignored commands are still usable, but are hidden from completion
in amx.

COMMAND can also be a list of commands to ignore.

A hidden second arg defaults to t, but if nil is explicitly
passed for this arg, it tells amx *not* to ignore COMMAND,
reversing the effect of a previous `amx-ignore'. "
  (interactive
   (list
    (let ((amx-temp-prompt-string "Ignore command: "))
      (amx-completing-read
       amx-cache
       :predicate (lambda (cmd) (not (amx-command-ignored-p cmd))))))
   (declare (advertised-calling-convention (command) nil)))
  (unless (listp command)
    (setq command (list command)))
  (cl-loop
   for cmd in command
   if (stringp cmd)
   do (setq cmd (intern cmd))
   do (put cmd 'amx-ignored do-ignore)))

(defun amx-unignore-command (command)
  "Undo a previous `amx-ignore' on COMMAND."
  (interactive
   (list
    (let ((amx-temp-prompt-string "Un-ignore command: "))
      (amx-completing-read
       amx-cache
       :predicate #'amx-command-marked-ignored-p))))
  (amx-ignore-command command nil))


;;--------------------------------------------------------------------------------
;; Help and Reference

(defun amx-exit-minibuffer ()
  "Call the backend-specific minibuffer exit function."
  (interactive)
  (funcall (amx-backend-exit-fun (amx-get-backend))))

(defun amx-do-with-selected-item (fn)
  "Exit minibuffer and call FN on the selected item."
  (setq amx-custom-action fn)
  (amx-exit-minibuffer))

(defun amx-describe-function ()
  (interactive)
  (amx-do-with-selected-item (lambda (chosen)
                               (describe-function chosen)
                               (pop-to-buffer "*Help*"))))

(defun amx-where-is ()
  (interactive)
  (amx-do-with-selected-item 'where-is))

(defun amx-find-function ()
  (interactive)
  (amx-do-with-selected-item 'find-function))

;; TODO: These are redundant with the keymap functions I wrote. DRY it
;; out.
(defun amx-extract-commands-from-keymap (keymap)
  (let (commands)
    (amx-parse-keymap keymap commands)
    commands))

(defun amx-parse-keymap (keymap commands)
  (map-keymap (lambda (_binding element)
                (if (and (listp element) (eq 'keymap (car element)))
                    (amx-parse-keymap element commands)
                  ;; Strings are commands, too. Reject them.
                  (if (and (symbolp element) (commandp element))
                      (push element commands))))
              keymap))

(defun amx-extract-commands-from-features (mode)
  (let ((library-path (symbol-file mode))
        (mode-name (symbol-name mode))
        commands)

    (string-match "\\(.+?\\)\\(-mode\\)?$" mode-name)
    ;; 'lisp-mode' -> 'lisp'
    (setq mode-name (match-string 1 mode-name))
    (if (string= mode-name "c") (setq mode-name "cc"))
    (setq mode-name (regexp-quote mode-name))

    (dolist (feature load-history)
      (let ((feature-path (car feature)))
        (when (and feature-path (or (equal feature-path library-path)
                                    (string-match mode-name (file-name-nondirectory
                                                             feature-path))))
          (dolist (item (cdr feature))
            (if (and (listp item) (eq 'defun (car item)))
                (let ((function (cdr item)))
                  (when (commandp function)
                    (setq commands (append commands (list function))))))))))
    commands))

(defun amx-show-unbound-commands ()
  "Shows unbound commands in a new buffer,
sorted by frequency of use."
  (interactive)
  (setq amx-data (sort amx-data 'amx-sorting-rules))
  (let ((unbound-commands (delq nil
                                (mapcar (lambda (command-item)
                                          (unless (where-is-internal (car command-item))
                                            command-item))
                                        amx-data))))
    (view-buffer-other-window "*Amx: Unbound Commands*")
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (amx-pp unbound-commands))
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

;;--------------------------------------------------------------------------------
;; Auto Update

(defun amx-post-eval-force-update (&rest args)
  ;; Setting this will force an update the next time Emacs is idle
  (setq amx-last-update-time nil))

;; It's pretty much impossible to define a new command without going
;; through one of these 4 functions, so updating after any of them is
;; called should catch all new command definitions.
(cl-loop for fun in '(load eval-last-sexp eval-buffer eval-region eval-expression)
         do (advice-add fun :after #'amx-post-eval-force-update))

(defun amx-idle-update (&optional force)
  "Function meant to be run in idle timers to update amx caches.

Optional argument FORCE tells amx to completely rebuild all of
its cached data, even if it believes that data is already
current."
  (amx-initialize)
  (let ((do-recount
         (or force
             ;; If periodic updates are enabled, force a full search
             ;; for new commands after the auto-update interval has
             ;; elapsed.
             (and amx-auto-update-interval
                  amx-last-update-time
                  (> (float-time (time-since amx-last-update-time))
                     (* 60 amx-auto-update-interval))))))
    (amx-update-if-needed do-recount))
  (when force
    (amx-invalidate-keybind-hash))
  (amx-update-keybind-hash))

;; This does a quick update every time emacs is idle
(setq amx-short-idle-update-timer
      (run-with-idle-timer 1 t 'amx-idle-update))

(provide 'amx)
;;; amx.el ends here
