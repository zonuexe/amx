;;; smex.el --- M-x interface with Ido-style fuzzy matching. -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2014 Cornelius Mika and contributors
;;
;; Author: Cornelius Mika <cornelius.mika@gmail.com> and contributors
;; URL: http://github.com/nonsequitur/smex/
;; Package-Requires: ((emacs "24.4"))
;; Version: 4.0
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;; run (smex-initialize)
;;
;; Bind the following commands:
;; smex, smex-major-mode-commands
;;
;; For a detailed introduction see:
;; http://github.com/nonsequitur/smex/blob/master/README.markdown

;;; Code:

(require 'cl-lib)
(require 'ido)

(defvar smex-initialized nil
  "If non-nil smex is initialized.")
(define-obsolete-variable-alias 'smex-initialized-p 'smex-initialized "4.0")

(defvar smex-cache)
(defvar smex-data)
(defvar smex-history)

(defvar smex-command-count 0
  "Number of commands known to smex.")

(defvar smex-custom-action nil
  "If non-nil, smex will call this in place of `execute-extended-command'.")

(defvar smex-minibuffer-depth -1
  "Used to determin if smex \"owns\" the current active minibuffer.")

(defvar smex-command-keybind-hash nil
  "Hash table for translating between commands and key bindings.

See `smex-make-keybind-hash'.")

(defvar smex-last-active-maps nil
  "List of keymaps last used to update `smex-command-keybind-hash'.

When `smex-command-keybind-hash' is updated, this is set to the
value of `(current-active-maps)' at that time. This is used to
figure out whether to invalidate the hash table for the next call
to smex.")

(defvar smex-origin-buffer nil
  "The buffer smex was called from.

This is used to determine which buffer's key bindings to use when
`smex-show-key-bindings' is non-nil.")

(defvar smex-known-backends nil
  "Plist of known smex completion backends.")

(defvar smex-temp-prompt-string nil
  "if non-nil, overrides `smex-prompt-string' once.

Each time `smex-prompt-with-prefix-arg' is called, this is reset
to nil.")

;; This timer will run every time Emacs is idle for 1 second, but most
;; of the time it will do nothing.
(defvar smex-short-idle-update-timer nil)
;; This timer forces a periodic updates to happen if you walk away for
;; a few hours, so that smex won't wait until you come back to do a
;; periodic update
(defvar smex-long-idle-update-timer nil)

(defvar smex-last-update-time nil
  "Time when `smex-update' was last run.

If nil, a `smex-update' is needed ASAP.")

(cl-defstruct smex-backend
  name
  required-feature
  comp-fun
  get-text-fun
  exit-fun)

(defgroup smex nil
  "M-x interface with Ido-style fuzzy matching and ranking heuristics."
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "smex.el"))

;;;###autoload
(define-minor-mode smex-mode
  ;; TODO Update all references to ido
  "Use ido completion for M-x"
  :global t
  :group 'smex
  (if smex-mode
      (progn
        (unless smex-initialized-p
          (smex-initialize))
        (global-set-key [remap execute-extended-command] 'smex))
    (when (eq (global-key-binding [remap execute-extended-command]) 'smex)
      (global-unset-key [remap execute-extended-command]))))

(defun smex-set-auto-update-interval (symbol value)
  "Custom setter for `smex-auto-update-interval'.

Arguments are the same as in `set-default'.

In addition to setting the variable, this will also set up an
idle timer to ensure that updates happen when idle."
  (cl-assert (eq symbol 'smex-auto-update-interval))
  (set-default symbol value)
  ;; Cancel any previous timer
  (when smex-long-idle-update-timer
    (cancel-timer smex-long-idle-update-timer)
    (setq smex-long-idle-update-timer nil))
  (when value
    ;; Enable idle updating
    (setq smex-long-idle-update-timer
          (run-with-idle-timer (1+ (* 60 value)) t
                               'smex-idle-update))))

(defcustom smex-auto-update-interval nil
  "Time in minutes between periodic updates of the command list.

Smex already updates the command list after functions like `load'
and `eval-expression' that could possibly define new commands.
Generally this should be enough to catch all newly-loaded
commands, but just in case any slip through, you can enable
periodic updates to catch them. If this variable is nil, no
periodic updates will be performed."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Minutes"))
  :set #'smex-set-auto-update-interval)

(make-obsolete-variable 'smex-auto-update "Set `smex-auto-update-interval' instead." "4.0")

(defcustom smex-save-file (locate-user-emacs-file "smex-items" ".smex-items")
  "File in which the smex state is saved between Emacs sessions.
Variables stored are: `smex-data', `smex-history'.
Must be set before initializing Smex."
  ;; TODO allow this to be set any time
  :type '(choice (string :tag "File name")
                 (const :tag "Don't save" nil)))

(defcustom smex-history-length 7
  "Determines on how many recently executed commands
Smex should keep a record.
Must be set before initializing Smex."
  ;; TODO allow this to be set any time
  :type 'integer)

(defcustom smex-show-key-bindings t
  "If non-nil, show key binding while completing commands."
  :type 'boolean)

(defcustom smex-prompt-string "M-x "
  "String to display in the Smex prompt."
  :type 'string)

(defcustom smex-ignored-command-matchers
  '("\\`self-insert-command\\'"
    "\\`ad-Orig-"
    "\\`menu-bar"
    smex-command-marked-ignored-p
    smex-command-obsolete-p
    smex-command-mouse-interactive-p)
  "List of regexps and/or functions.

Each element is either a regular expression or a function of one
argument. Commands that match any of the regexps or return
non-nil for any of these functions will be hidden from the smex
completion list."
  :type '(repeat
          (choice
           (regexp :tag "Regular expression")
           (function :tag "Function"))))

(define-obsolete-variable-alias 'smex-flex-matching 'ido-enable-flex-matching "4.0")

;;--------------------------------------------------------------------------------
;; Smex Internals

(defun smex-get-command-name (cmd)
  "Return CMD as a string.

CMD can be a string, symbol, or cons cell whose `car' is a string
or symbol."
  (cond
   ((symbolp cmd)
    (symbol-name cmd))
   ((consp cmd)
    (smex-get-command-name (car cmd)))
   ((stringp cmd)
    cmd)
   (t
    (error "Unrecognized command: %s" cmd))))

(defun smex-get-default (choices)
  "Get the first entry from CHOICES as a string."
  (smex-augment-command-with-keybind
   (smex-get-command-name
    (car
     (if (listp choices)
         choices
       (all-completions "" choices))))
   (when smex-show-key-bindings (smex-update-keybind-hash))))

;;--------------------------------------------------------------------------------
;; Smex Interface

;;;###autoload
(defun smex ()
  (interactive)
  (unless smex-initialized
    (smex-initialize))
  (if (smex-active)
      (smex-update-and-rerun)
    (smex-update-if-needed)
    (smex-read-and-run smex-cache)))

(defun smex-active ()
  "Return non-nil if smex is currently using the minibuffer"
  (>= smex-minibuffer-depth (minibuffer-depth)))
(define-obsolete-function-alias 'smex-already-running 'smex-active "4.0")

(defun smex-update-and-rerun ()
  (let ((new-initial-input
         (funcall (smex-backend-get-text-fun (smex-get-backend)))))
    (smex-do-with-selected-item
     (lambda (_) (smex-update) (smex-read-and-run smex-cache new-initial-input)))))

(defun smex-read-and-run (commands &optional initial-input)
  (let* ((smex-origin-buffer
          (or smex-origin-buffer (current-buffer)))
         (commands
          ;; Add key bindings to completions
          (if smex-show-key-bindings
              (smex-augment-commands-with-keybinds commands)
            commands))
         (collection
          ;; Initially complete with only non-ignored commands, but if
          ;; all of those are ruled out, allow completing with ignored
          ;; commands.
          (apply-partially #'completion-table-with-predicate
                           commands
                           (lambda (cmd) (not (smex-command-ignored-p cmd)))
                           nil))
         ;; Symbol
         (chosen-item
          (smex-clean-command-name
           (smex-completing-read commands :initial-input initial-input)))
         ;; String
         (chosen-item-name (symbol-name chosen-item)))
    (cl-assert (commandp chosen-item))
    (if smex-custom-action
        (let ((action smex-custom-action))
          (setq smex-custom-action nil)
          (funcall action chosen-item))
      (unwind-protect
          ;; Don't warn about non-interactive use of
          ;; `execute-extended-command'
          (with-no-warnings
            (execute-extended-command current-prefix-arg chosen-item-name))
        (smex-rank chosen-item)))))

;;;###autoload
(defun smex-major-mode-commands ()
  "Like `smex', but limited to commands that are relevant to the active major mode."
  (interactive)
  (unless smex-initialized
    (smex-initialize))
  (let ((commands (delete-dups (append (smex-extract-commands-from-keymap (current-local-map))
                                       (smex-extract-commands-from-features major-mode)))))
    (setq commands (smex-sort-according-to-cache commands))
    (setq commands
          (apply-partially #'completion-table-with-predicate
                           commands
                           (lambda (cmd) (not (smex-command-ignored-p cmd)))
                           nil))
    (smex-read-and-run commands)))

(defvar smex-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-h f") 'smex-describe-function)
    (define-key keymap (kbd "C-h w") 'smex-where-is)
    (define-key keymap (kbd "M-.") 'smex-find-function)
    keymap)
  "Additional key bindings for smex completion.")

(defvar smex-ido-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-a") 'move-beginning-of-line)
    (set-keymap-parent keymap smex-map)
    keymap))

(defun smex-prepare-ido-bindings ()
  (setq ido-completion-map
        (make-composed-keymap (list smex-ido-map ido-completion-map))))

(defun smex-default-exit-minibuffer ()
  "Run the key binding for RET.

This should work for most completion backends, without having to
know exactly which functions each one uses to exit the
minibuffer.."
  (execute-kbd-macro (kbd "RET")))

(cl-defun smex-completing-read (choices &key initial-input predicate)
  (let ((smex-minibuffer-depth (1+ (minibuffer-depth)))
        (comp-fun (smex-backend-comp-fun (smex-get-backend))))
    (funcall comp-fun choices :initial-input initial-input
             ;; Work around a bug
             :predicate (or predicate #'identity))))

(defun smex-prompt-with-prefix-arg ()
  (let ((smex-prompt-string
         (or smex-temp-prompt-string smex-prompt-string)))
    (setq smex-temp-prompt-string nil)
    (if (not current-prefix-arg)
        smex-prompt-string
      (concat
       (if (eq current-prefix-arg '-)
           "- "
         (if (integerp current-prefix-arg)
             (format "%d " current-prefix-arg)
           (if (= (car current-prefix-arg) 4)
               "C-u "
             (format "%d " (car current-prefix-arg)))))
       smex-prompt-string))))

;;--------------------------------------------------------------------------------
;; Pluggable Backends

(cl-defun smex-define-backend (&key name comp-fun get-text-fun
                                    (exit-fun 'smex-default-exit-minibuffer)
                                    required-feature)
  (cl-assert
   (and (symbolp name) name
        (functionp comp-fun)
        (functionp get-text-fun)
        (functionp exit-fun)
        (symbolp required-feature))
   nil
   "Invalid smex backend spec: (:name %S :comp-fun %S :get-text-fun %S :exit-fun %S)"
   (list name comp-fun get-text-fun exit-fun))
  (let ((backend
         (make-smex-backend :name name
                            :comp-fun comp-fun
                            :get-text-fun get-text-fun
                            :exit-fun exit-fun
                            :required-feature required-feature)))
    (setq smex-known-backends
          (plist-put smex-known-backends name backend))))

(cl-defun smex-get-backend (&optional (backend smex-backend))
  (cond
   ((smex-backend-p backend)
    backend)
   ((plist-get smex-known-backends backend))
   (t (error "Unknown smex backed %S" backend))))

(cl-defun smex-completing-read-default (choices &key initial-input predicate)
  "Smex backend for default Emacs completion"
  (require 'minibuf-eldef)
  (let ((minibuffer-completion-table choices)
        (prompt (concat (smex-prompt-with-prefix-arg)
                        (format "[%s]: " (smex-get-default choices))))
        (prev-eldef-mode minibuffer-electric-default-mode))
    (unwind-protect
        (progn
          (minibuffer-electric-default-mode 1)
          (minibuffer-with-setup-hook
              (lambda ()
                (use-local-map (make-composed-keymap
                                (list smex-map (current-local-map)))))
            (completing-read-default
             prompt choices predicate t initial-input
             'extended-command-history
             (smex-get-default choices))))
      (minibuffer-electric-default-mode
       (if prev-eldef-mode 1 0)))))

(defun smex-default-get-text ()
  "Default function for getting the user's current text input.

May not work for things like ido and ivy."
  (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))

(smex-define-backend
 :name 'standard
 :comp-fun 'smex-completing-read-default
 :get-text-fun 'smex-default-get-text)

(declare-function ido-completing-read+ "ext:ido-completing-read+")

(cl-defun smex-completing-read-ido (choices &key initial-input predicate)
  "Smex backend for ido completion"
  (require 'ido-completing-read+)
  (let ((ido-completion-map ido-completion-map)
        (ido-setup-hook (cons 'smex-prepare-ido-bindings ido-setup-hook))
        (minibuffer-completion-table choices))
    (ido-completing-read+ (smex-prompt-with-prefix-arg) choices predicate t
                          initial-input 'extended-command-history
                          (smex-get-default choices))))

(defun smex-ido-get-text ()
  ido-text)

(smex-define-backend
 :name 'ido
 :comp-fun 'smex-completing-read-ido
 :get-text-fun 'smex-ido-get-text
 :required-feature 'ido-completing-read+)

(declare-function ivy-read "ext:ivy")

(cl-defun smex-completing-read-ivy (choices &key initial-input predicate)
  "Smex backend for ivy completion"
  (require 'ivy)
  (ivy-read (smex-prompt-with-prefix-arg) choices
            :predicate predicate
            :keymap smex-map
            :history 'extended-command-history
            :initial-input initial-input
            :preselect (smex-get-default choices)))

(defvar ivy-text)

(defun smex-ivy-get-text ()
  ivy-text)

(smex-define-backend
 :name 'ivy
 :comp-fun 'smex-completing-read-ivy
 :get-text-fun 'smex-ivy-get-text
 :required-feature 'ivy)

(cl-defun smex-completing-read-auto (choices &key initial-input predicate)
  "Automatically select between ivy, ido, and standard completion."
  (let ((smex-backend
         (cond
          ((bound-and-true-p ivy-mode) 'ivy)
          ((or (bound-and-true-p ido-mode)
               (bound-and-true-p ido-ubiquitous-mode))
           'ido)
          (t 'standard))))
    (smex-completing-read choices :initial-input initial-input :predicate predicate)))

(smex-define-backend
 :name 'auto
 :comp-fun 'smex-completing-read-auto
 :get-text-fun (lambda () (error "This exit function should never be called."))
 :exit-fun (lambda () (error "This get-text function should never be called.")))

(defun smex-set-backend (symbol value)
  "Custom setter for `smex-backend'.

Arguments are the same as in `set-default'.

This function will refuse to set the backend unless it can load
the associated feature, if any."
  (let* ((backend (or (plist-get smex-known-backends value)
                      (error "Unknown smex backend: %s" value)))
         (feature (smex-backend-required-feature backend)))
    (when feature
      (unless (require feature nil 'noerror)
        (error "You must install %s to use the %s backend for smex"
               feature value))))
  ;; If we got through that, then actually set the variable
  (set-default symbol value))

(defcustom smex-backend 'auto
  "Completion function to select a candidate from a list of strings.

This function should take the same arguments as
`smex-completing-read': CHOICES and INITIAL-INPUT.

By default, an appropriate method is selected based on whether
`ivy-mode' or `ido-mode' is enabled."
  :type '(choice
          (const :tag "Auto-select" auto)
          (const :tag "Ido" ido)
          (const :tag "Ivy" ivy)
          (const :tag "Standard" standard)
          (symbol :tag "Custom backend"))
  :set #'smex-set-backend)
(define-obsolete-variable-alias 'smex-completion-method 'smex-backend "4.0")

;;--------------------------------------------------------------------------------
;; Cache and Maintenance

(defun smex-rebuild-cache ()
  (interactive)
  (setq smex-cache nil)

  ;; Build up list 'new-commands' and later put it at the end of 'smex-cache'.
  ;; This speeds up sorting.
  (let (new-commands)
    (mapatoms (lambda (symbol)
                (when (commandp symbol)
                  (let ((known-command (assq symbol smex-data)))
                    (if known-command
                        (setq smex-cache (cons known-command smex-cache))
                      (setq new-commands (cons (list symbol) new-commands)))))))
    (if (eq (length smex-cache) 0)
        (setq smex-cache new-commands)
      (setcdr (last smex-cache) new-commands)))

  (setq smex-cache (sort smex-cache 'smex-sorting-rules))
  (smex-restore-history))

(defun smex-convert-for-ido (command-items)
  (mapcar (lambda (command-item) (symbol-name (car command-item))) command-items))

(defun smex-restore-history ()
  "Rearranges `smex-cache' according to `smex-history'"
  (if (> (length smex-history) smex-history-length)
      (setcdr (nthcdr (- smex-history-length 1) smex-history) nil))
  (mapc (lambda (command)
          (unless (eq command (caar smex-cache))
            (let ((command-cell-position (smex-detect-position
                                          smex-cache
                                          (lambda (cell)
                                            (eq command (caar cell))))))
              (when command-cell-position
                (let ((command-cell (smex-remove-nth-cell
                                     command-cell-position smex-cache)))
                  (setcdr command-cell smex-cache)
                  (setq smex-cache command-cell))))))
        (reverse smex-history)))

(defun smex-sort-according-to-cache (list)
  "Sorts a list of commands by their order in `smex-cache'"
  (let (sorted)
    (dolist (command-item smex-cache)
      (let ((command (car command-item)))
        (when (memq command list)
          (setq sorted (cons command sorted))
          (setq list (delq command list)))))
    (nreverse (append list sorted))))

(defun smex-update ()
  (interactive)
  (smex-save-history)
  (smex-rebuild-cache)
  (setq smex-last-update-time (current-time)))

(defun smex-detect-new-commands ()
  (let ((i 0))
    (mapatoms (lambda (symbol) (if (commandp symbol) (setq i (1+ i)))))
    (unless (= i smex-command-count)
      (setq smex-command-count i))))

(defun smex-update-if-needed (&optional count-commands)
  "Run `smex-update' if necessary.

If `smex-last-update-time' is nil, do an update unconditionally.
Otherwise, if optional arg COUNT-COMMANDS is non-nil, count the
total number of defined commands in `obarray' and update if it
has changed."
  (if (or (null smex-last-update-time)
          (smex-detect-new-commands))
      (smex-update)))

;;;###autoload
(defun smex-initialize ()
  (interactive)
  (ido-common-initialization)
  (smex-load-save-file)
  (smex-detect-new-commands)
  (smex-rebuild-cache)
  (add-hook 'kill-emacs-hook 'smex-save-to-file)
  (setq smex-initialized t))

(define-obsolete-function-alias
  'smex-initialize-ido 'ido-common-initialization
  "4.0")

(define-obsolete-function-alias
  'smex-save-file-not-empty-p 'smex-buffer-not-empty-p "4.0")
(defsubst smex-buffer-not-empty-p ()
  (string-match-p "\[^[:space:]\]" (buffer-string)))

(defun smex-load-save-file ()
  "Loads `smex-history' and `smex-data' from `smex-save-file'"
  (setq smex-history nil smex-data nil)
  (when smex-save-file
    (let ((save-file (expand-file-name smex-save-file)))
      (when (file-readable-p save-file)
        (with-temp-buffer
          (insert-file-contents save-file)
          (condition-case nil
              (setq smex-history (read (current-buffer))
                    smex-data    (read (current-buffer)))
            (error (if (smex-buffer-not-empty-p)
                       (error "Invalid data in smex-save-file (%s). Can't restore history."
                              smex-save-file)
                     (unless (boundp 'smex-history) (setq smex-history nil))
                     (unless (boundp 'smex-data)    (setq smex-data nil))))))))))

(defun smex-save-history ()
  "Updates `smex-history'"
  (setq smex-history
        (cl-loop
         for i from 1 upto smex-history-length
         for (command-name . count) in smex-cache
         collect command-name)))

;; A copy of `ido-pp' that's compatible with lexical bindings
(defun smex-pp* (list list-name)
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

(defmacro smex-pp (list-var)
  `(smex-pp* ,list-var ,(symbol-name list-var)))

(defun smex-save-to-file ()
  (interactive)
  (when (and init-file-user smex-save-file)
    (smex-save-history)
    (with-temp-file (expand-file-name smex-save-file)
      (smex-pp smex-history)
      (smex-pp smex-data))))

;;--------------------------------------------------------------------------------
;; Ranking

(defun smex-sorting-rules (command-item other-command-item)
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

(defun smex-rank (command)
  (let ((command-item (or (assq command smex-cache)
                          ;; Update caches and try again if not found.
                          (progn (smex-update)
                                 (assq command smex-cache)))))
    (when command-item
      (smex-update-counter command-item)

      ;; Don't touch the cache order if the chosen command
      ;; has just been execucted previously.
      (unless (eq command-item (car smex-cache))
        (let (command-cell
              (pos (smex-detect-position smex-cache (lambda (cell)
                                                      (eq command-item (car cell))))))
          ;; Remove the just executed command.
          (setq command-cell (smex-remove-nth-cell pos smex-cache))
          ;; And put it on top of the cache.
          (setcdr command-cell smex-cache)
          (setq smex-cache command-cell)

          ;; Now put the last history item back to its normal place.
          (smex-sort-item-at smex-history-length))))))

(defun smex-update-counter (command-item)
  (let ((count (cdr command-item)))
    (setcdr command-item
            (if count
                (1+ count)
              ;; Else: Command has just been executed for the first time.
              ;; Add it to `smex-data'.
              (if smex-data
                  (setcdr (last smex-data) (list command-item))
                (setq smex-data (list command-item)))
              1))))

(defun smex-sort-item-at (n)
  "Sorts item at position N in `smex-cache'."
  (let* ((command-cell (nthcdr n smex-cache))
         (command-item (car command-cell)))
    (let ((insert-at (smex-detect-position
                      command-cell
                      (lambda (cell)
                        (smex-sorting-rules command-item (car cell))))))
      ;; TODO: Should we handle the case of 'insert-at' being nil?
      ;; This will never happen in practice.
      (when (> insert-at 1)
        (setq command-cell (smex-remove-nth-cell n smex-cache))
        ;; smex-cache just got shorter by one element, so subtract '1' from insert-at.
        (setq insert-at (+ n (- insert-at 1)))
        (smex-insert-cell command-cell insert-at smex-cache)))))

(defun smex-detect-position (cell function)
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

(defun smex-remove-nth-cell (n list)
  "Removes and returns the Nth cell in LIST."
  (let* ((previous-cell (nthcdr (- n 1) list))
         (result (cdr previous-cell)))
    (setcdr previous-cell (cdr result))
    result))

(defun smex-insert-cell (new-cell n list)
  "Inserts cell at position N in LIST."
  (let* ((cell (nthcdr (- n 1) list))
         (next-cell (cdr cell)))
    (setcdr (setcdr cell new-cell) next-cell)))

;;--------------------------------------------------------------------------------
;; Display key bindings in completions

(defun smex-make-keybind-hash (&optional keymap)
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
            ;; Run `current-active-maps' in `smex-origin-buffer' if
            ;; any
            (with-current-buffer (or smex-origin-buffer (current-buffer))
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

(defun smex-invalidate-keybind-hash (&rest args)
  "Force a rebuild of `smex-command-keybind-hash'.

This function takes any number of arguments and ignores them so
that it can be used as advice on other functions."
  (setq smex-command-keybind-hash nil
            smex-last-active-maps nil))

(defun smex-maybe-invalidate-keybind-hash ()
  "If `smex-command-keybind-hash' is stale, set it to nil.

Returns non-nil if the hash is still valid and nil if it was
invalidated. This uses `smex-last-active-maps' to figure out if
the set of active keymaps has changed since the last rebuild of
`smex-command-keybind-hash'. Note that this function does not, by
itself, detect when new keys are bound in the current active
keymaps."
  (let ((valid
         (and smex-command-keybind-hash
              smex-last-active-maps
              (equal
               smex-last-active-maps
               (with-current-buffer (or smex-origin-buffer (current-buffer))
                 (current-active-maps))))))
    (unless valid
      (smex-invalidate-keybind-hash))
    valid))

(defun smex-update-keybind-hash ()
  "Update (if needed) and return `smex-command-keybind-hash'.

 If so, it rebuilds it based on the
current set of active keymaps.e"
  (smex-maybe-invalidate-keybind-hash)
  (or smex-command-keybind-hash
      (setq smex-command-keybind-hash (smex-make-keybind-hash))))

;; Since keymaps can contain other keymaps, checking whether these
;; functions are affecting the current active maps (or any maps
;; contained in them) is not much faster than just rebuilding the hash
;; table from scratch.
(cl-loop
 for fun in '(define-key set-keymap-parent)
 do (advice-add fun :before 'smex-invalidate-keybind-hash))

(defsubst smex-augment-command-with-keybind (command &optional bind-hash)
  (let* ((cmdname (smex-get-command-name command))
         (cmdsym (intern cmdname))
         (keybind (and bind-hash (gethash cmdsym bind-hash))))
    (if (and keybind (not (smex-command-ignored-p cmdsym)))
        (format "%s (%s)" cmdname keybind)
      cmdname)))

(defun smex-augment-commands-with-keybinds
    (commands &optional bind-hash)
  "Append key bindings from BIND-HASH to COMMANDS.

Given a list of commands (either as symbols or cons cells in the
form of `smex-cache'), returns an equivalent list, except that
every command is converted to a string, and any command with a
key binding recorded in `BIND-HASH will have that binding
appended. By default, key bindings are looked up in
`smex-command-keybind-hash', which is updated using
`smex-make-keybind-hash' if necessary.

In the returned list, each element will be a string."
  (cl-loop
   ;; Default to `smex-command-keybind-hash', updating it if
   ;; necessary.
   with bind-hash = (or bind-hash (smex-update-keybind-hash))
   for cmd in commands
   collect (smex-augment-command-with-keybind cmd bind-hash)))

(defun smex-clean-command-name (command-name)
  "Inverse of `smex-augment-commands-with-keybinds', approximately.

Given a string starting with a command name and possibly ending
with a key binding, it returns just the command name as a
symbol."
  (or
   ;; First try getting it from the hash table
   (and smex-command-keybind-hash
        (gethash command-name smex-command-keybind-hash))
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

(defun smex-command-ignored-p (command)
  "Return non-nil if COMMAND is ignored by smex completion.

See `smex-ignored-command-matchers'."
  ;; Allow passing entries from `smex-cache', whose `car' is the
  ;; command symbol.
  (when (consp command)
    (setq command (car command)))
  ;; Command might be a string like "CMD (KEY)", requiring a lookup of
  ;; the real command name
  (when (stringp command)
    (setq command (gethash command smex-command-keybind-hash (intern command))))
  (cl-loop
   with matched = nil
   for matcher in smex-ignored-command-matchers
   ;; regexp
   if (stringp matcher)
   do (setq matched (string-match-p matcher (symbol-name command)))
   ;; function
   else
   do (setq matched (funcall matcher command))
   if matched return t
   finally return nil))

(defun smex-command-marked-ignored-p (command)
  "Return non-nil if COMMAND's `smex-ignored' property is non-nil.

See `smex-ignore-command'."
  ;; Allow passing entries from `smex-cache', whose `car' is the
  ;; command symbol.
  (when (consp command)
    (setq command (car command)))
  (get command 'smex-ignored))

(defun smex-command-obsolete-p (command)
  "Return non-nil if COMMAND is marked obsolete."
  (get command 'byte-obsolete-info))

(defun smex-command-mouse-interactive-p (command)
  "Return non-nil if COMMAND uses mouse events.

This is not guaranteed to detect all mouse-interacting commands,
but it should find most of them."
  (and (listp (help-function-arglist command))
       (not (eq ?\& (aref (symbol-name (car (help-function-arglist command))) 0)))
       (stringp (cadr (interactive-form command)))
       (string-match-p "\\`[*@^]*e" (cadr (interactive-form command)))))

(cl-defun smex-ignore-command (command &optional (do-ignore t))
  "Tell smex to ignore COMMAND.

Ignored commands are still usable, but are hidden from completion
in smex.

COMMAND can also be a list of commands to ignore.

A hidden second arg defaults to t, but if nil is explicitly
passed for this arg, it tells smex *not* to ignore COMMAND,
reversing the effect of a previous `smex-ignore'. "
  (interactive
   (list
    (let ((smex-temp-prompt-string "Ignore command: "))
      (smex-completing-read
       smex-cache
       :predicate (lambda (cmd) (not (smex-command-ignored-p cmd))))))
   (declare (advertised-calling-convention (command) nil)))
  (unless (listp command)
    (setq command (list command)))
  (cl-loop
   for cmd in command
   if (stringp cmd)
   do (setq cmd (intern cmd))
   do (put cmd 'smex-ignored do-ignore)))

(defun smex-unignore-command (command)
  "Undo a previous `smex-ignore' on COMMAND."
  (interactive
   (list
    (let ((smex-temp-prompt-string "Un-ignore command: "))
      (smex-completing-read
       smex-cache
       :predicate #'smex-command-marked-ignored-p))))
  (smex-ignore-command command nil))


;;--------------------------------------------------------------------------------
;; Help and Reference

(defun smex-exit-minibuffer ()
  "Call the backend-specific minibuffer exit function."
  (interactive)
  (funcall (smex-backend-exit-fun (smex-get-backend))))

(defun smex-do-with-selected-item (fn)
  "Exit minibuffer and call FN on the selected item."
  (setq smex-custom-action fn)
  (smex-exit-minibuffer))

(defun smex-describe-function ()
  (interactive)
  (smex-do-with-selected-item (lambda (chosen)
                                (describe-function chosen)
                                (pop-to-buffer "*Help*"))))

(defun smex-where-is ()
  (interactive)
  (smex-do-with-selected-item 'where-is))

(defun smex-find-function ()
  (interactive)
  (smex-do-with-selected-item 'find-function))

;; TODO: These are redundant with the keymap functions I wrote. DRY it
;; out.
(defun smex-extract-commands-from-keymap (keymap)
  (let (commands)
    (smex-parse-keymap keymap commands)
    commands))

(defun smex-parse-keymap (keymap commands)
  (map-keymap (lambda (_binding element)
                (if (and (listp element) (eq 'keymap (car element)))
                    (smex-parse-keymap element commands)
                  ;; Strings are commands, too. Reject them.
                  (if (and (symbolp element) (commandp element))
                      (push element commands))))
              keymap))

(defun smex-extract-commands-from-features (mode)
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

(defun smex-show-unbound-commands ()
  "Shows unbound commands in a new buffer,
sorted by frequency of use."
  (interactive)
  (setq smex-data (sort smex-data 'smex-sorting-rules))
  (let ((unbound-commands (delq nil
                                (mapcar (lambda (command-item)
                                          (unless (where-is-internal (car command-item))
                                            command-item))
                                        smex-data))))
    (view-buffer-other-window "*Smex: Unbound Commands*")
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (smex-pp unbound-commands))
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

;;--------------------------------------------------------------------------------
;; Auto Update

(defun smex-post-eval-force-update (&rest args)
  ;; Setting this will force an update the next time Emacs is idle
  (setq smex-last-update-time nil))

;; It's pretty much impossible to define a new command without going
;; through one of these 4 functions, so updating after any of them is
;; called should catch all new command definitions.
(cl-loop for fun in '(load eval-last-sexp eval-buffer eval-region eval-expression)
         do (advice-add fun :after #'smex-post-eval-force-update))

(defun smex-idle-update ()
  (unless smex-initialized
           (smex-initialize))
         (let ((do-recount
                ;; If periodic updates are enabled, force a thorough
                ;; check for new commands after the auto-update
                ;; interval has elapsed.
                (and smex-auto-update-interval
                     smex-last-update-time
                     (> (float-time (time-since smex-last-update-time))
                        (* 60 smex-auto-update-interval)))))
           (smex-update-if-needed do-recount)))

;; This does a quick update check every time emacs is idle
(setq smex-short-idle-update-timer
      (run-with-idle-timer 1 t 'smex-idle-update))

(provide 'smex)
;;; smex.el ends here
