;;; -*- lexical-binding: t -*-

(require 'smex)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)

(unless smex-initialized
  (smex-initialize))

(defun test-save-custom-vars (vars)
  (cl-loop
   for var in vars
   if (not (custom-variable-p var))
   do (error "Variable `%s' is not a customizable variable" var)
   for curval = (symbol-value var)
   for stdval = (eval (car (get var 'standard-value)))
   do
   (progn
     ;; Save the current value
     (put var 'test-saved-value curval)
     ;; Set it to the standard value, using it's custom setter
     ;; function
     (customize-set-variable var stdval))))

(defun test-restore-custom-vars (vars)
  (cl-loop
   for var in vars
   for savedval = (get var 'test-saved-value)
   do
   (progn
     ;; Set it to the saved value, using it's custom setter function
     (customize-set-variable var savedval)
     ;; Delete the saved value from the symbol plist
     (put var 'test-saved-value nil))))

(defun key-sequences-equal (k1 k2)
  "Return non-nil if K1 and K2 represent the same key sequence.

If K1 or K2 are strings, they are converted to internal
representation using `kbd', and the results are compared. Hence
`C-M-a', `M-C-a', and `[134217729]' should all be considered
equal."
  (let ((k1 (if (stringp k1)
                (kbd k1)
              k1))
        (k2 (if (stringp k2)
                (kbd k2)
              k2)))
    (equal k1 k2)))

(defun canonicalize-key-sequence (k)
  (key-description (kbd k)))

(cl-defun smex-completing-read-return-first-choice
    (choices &key initial-input predicate)
  (car (all-completions (or initial-input "") choices predicate)))

;; Create a command with a known name and key binding
(defun my-temp-command ()
  (interactive)
  (message "Ran my-temp-command"))

(describe "The smex package"

  ;; Reset all of these variables to their standard values before each
  ;; test
  (before-each
    (test-save-custom-vars
     '(smex-mode
       smex-auto-update-interval
       smex-save-file
       smex-history-length
       smex-show-key-bindings
       smex-prompt-string
       smex-ignored-command-matchers
       smex-backend))
    ;; Don't save anything to disk during testing
    (setq smex-save-file nil))

  ;; Restore the saved value after each test
  (after-each
    (test-restore-custom-vars
     '(smex-mode
       smex-auto-update-interval
       smex-save-file
       smex-history-length
       smex-show-key-bindings
       smex-prompt-string
       smex-ignored-command-matchers
       smex-backend)))

  (it "should execute the selected command"
    (spy-on 'my-temp-command)
    (with-simulated-input "my-temp-command RET"
      (smex-read-and-run '(my-temp-command)))
    (expect 'my-temp-command :to-have-been-called))

  (it "should not allow setting a backend without loading the required feature"
    ;; Override `require' to return nil to prevent loading of new features
    (spy-on 'require :and-return-value nil)
    (expect
     (lambda ()
       (customize-set-variable 'smex-backend 'ido))
     :to-throw))

  (describe "standard backend"

    (before-each
      (customize-set-variable 'smex-backend 'standard)
      (spy-on 'completing-read-default :and-call-through)
      (spy-on 'completing-read :and-call-through))

    (it "should call `completing-read-default' and not `completing-read'"
      (expect
       (with-simulated-input "ignore RET"
         (smex-completing-read '("ignore")))
       :to-equal "ignore")
      (expect 'completing-read-default :to-have-been-called)
      (expect 'completing-read :not :to-have-been-called)))

  (describe "ido backend"

    (before-each
      (customize-set-variable 'smex-backend 'ido)
      (spy-on 'ido-completing-read+ :and-call-through))

    (it "should load `ido-completing-read+' when selected"
      (customize-set-variable 'smex-backend 'ido)
      (expect (featurep 'ido-completing-read+)))

    (it "should call `ido-completing-read+'"
      (expect
       (with-simulated-input "ignore RET"
         (smex-completing-read '("ignore")))
       :to-equal "ignore")
      (expect 'ido-completing-read+ :to-have-been-called)))

  (describe "ivy backend"

    (before-each
      (customize-set-variable 'smex-backend 'ivy)
      (spy-on 'ivy-read :and-call-through))

    (it "should load `ivy' when selected"
      (customize-set-variable 'smex-backend 'ivy)
      (expect (featurep 'ivy)))

    (it "should call `ivy-read'"
      (expect
       (with-simulated-input "ignore RET"
         (smex-completing-read '("ignore")))
       :to-equal "ignore")
      (expect 'ivy-read :to-have-been-called)))

  (describe "auto backend"

    (before-each
      (customize-set-variable 'smex-backend 'auto)
      ;; Pre-load features so we can spy on their functions
      (require 'ido-completing-read+)
      (require 'ivy)
      ;; Reset all of these modes to their standard values
      ;; before each test
      (test-save-custom-vars '(ido-mode ivy-mode))
      ;; Start with both modes off
      (ido-mode 0)
      (ivy-mode 0)
      (cl-loop
       for fun in
       '(completing-read-default ido-completing-read+ ivy-read)
       do (spy-on fun :and-return-value "ignore")))

    ;; Restore the saved value after each test
    (after-each
      (test-restore-custom-vars '(ido-mode ivy-mode)))

    (it "should normally use standard completion"
      (smex-completing-read '("ignore"))
      (expect 'completing-read-default :to-have-been-called))

    (it "should use ido completion when `ido-mode' or `ido-ubiquitous-mode' are enabled"
      (ido-mode 1)
      (smex-completing-read '("ignore"))
      (expect 'ido-completing-read+ :to-have-been-called))

    (it "should use ivy completion when `ivy-mode' is enabled"
      (ivy-mode 1)
      (smex-completing-read '("ignore"))
      (expect 'ivy-read :to-have-been-called)))

  (describe "with `smex-show-key-bindings'"

    :var (orig-local-map
          orig-smex-completing-read
          my-key-sequence
          temp-map
          last-choice-list)

    (before-each
      ;; Save the information needed to undo everything
      (setq orig-local-map (current-local-map)
            orig-smex-completing-read (symbol-function 'smex-completing-read)
            temp-map (make-sparse-keymap)
            my-key-sequence (canonicalize-key-sequence "C-M-A-H-s-a"))
      ;; Reversibly add a custom binding to the local map
      (define-key temp-map (kbd my-key-sequence) 'my-temp-command)
      (use-local-map (make-composed-keymap temp-map orig-local-map))
      ;; Ido lets us select entries using any substring
      (customize-set-variable 'smex-backend 'ido)
      (customize-set-variable 'smex-show-key-bindings t)
      ;; Start with an up-to-date keybind hash (before setting up
      ;; spies, so none of the spies' call counters are incremented
      ;; yet)
      (smex-update)
      (smex-update-keybind-hash)
      (spy-on 'smex-completing-read :and-call-fake
              ;; Save the choices list and then call original
              (cl-function
               (lambda (choices &key initial-input predicate)
                 (setq last-choice-list (all-completions "" choices predicate))
                 (funcall orig-smex-completing-read choices
                          :initial-input initial-input
                          :predicate predicate))))
      (spy-on 'smex-augment-commands-with-keybinds :and-call-through)
      (spy-on 'smex-update-keybind-hash :and-call-through)
      (spy-on 'smex-make-keybind-hash :and-call-through)
      (spy-on 'smex-invalidate-keybind-hash :and-call-through)
      ;; Don't actually execute selected commands
      (spy-on 'execute-extended-command))

    (after-each
      ;; Undo the overridden local map
      (use-local-map orig-local-map))

    (it "should add key bindings successfully"
      (expect
       (cl-some
        (apply-partially 's-contains? my-key-sequence)
        (smex-augment-commands-with-keybinds '(my-temp-command)))))

    (it "should show key bindings and update the keybind hash when enabled"
      (with-simulated-input "RET"
        (smex-read-and-run smex-cache "my-temp-command"))
      (expect 'execute-extended-command :to-have-been-called-with nil "my-temp-command")
      (expect 'smex-augment-commands-with-keybinds :to-have-been-called)
      (expect (cl-some (apply-partially 's-contains? my-key-sequence)
                       last-choice-list)))

    (it "should allow completion on key bindings"
      ;; Needed to avoid scoping issues with macro
      (eval
       `(with-simulated-input "RET"
          (smex-read-and-run smex-cache ,my-key-sequence)))
      (expect 'execute-extended-command :to-have-been-called-with nil "my-temp-command"))

    (it "should not show key bindings or update the keybind hash when disabled"
      (setq smex-show-key-bindings nil)
      (smex-invalidate-keybind-hash)
      (with-simulated-input "RET"
        (smex-read-and-run smex-cache "my-temp-command"))
      (expect 'execute-extended-command :to-have-been-called-with nil "my-temp-command")
      (expect 'smex-augment-commands-with-keybinds :not :to-have-been-called)
      (expect 'smex-make-keybind-hash :not :to-have-been-called)
      (expect (not (cl-some (apply-partially 's-contains? my-key-sequence)
                            last-choice-list))))

    (it "should update the keybind hash after any keymap is modified"
      (setq smex-show-key-bindings t)
      ;; Call `define-key'
      (define-key temp-map my-key-sequence 'my-temp-command)
      (expect 'smex-invalidate-keybind-hash :to-have-been-called)
      (with-simulated-input "RET"
        (smex-read-and-run smex-cache "my-temp-command"))
      (expect 'execute-extended-command :to-have-been-called-with nil "my-temp-command")
      (expect 'smex-augment-commands-with-keybinds :to-have-been-called)
      (expect 'smex-make-keybind-hash :to-have-been-called))

    (it "should use `smex-origin-buffer' instead of current buffer when looking up key binds"
      (setq smex-show-key-bindings t)
      (with-temp-buffer
        (let ((smex-origin-buffer (current-buffer)))
          ;; Now my-temp-command is not bound in active maps, so its
          ;; key binding should not show up in completions
          (with-simulated-input "RET"
            (smex-read-and-run smex-cache "my-temp-command"))
          (expect 'execute-extended-command :to-have-been-called-with nil "my-temp-command")
          (expect (not (cl-some (apply-partially 's-contains? my-key-sequence)
                                last-choice-list))))))

    (it "should update the keybind hash after switching buffers"
      (setq smex-show-key-bindings t)
      (with-temp-buffer
        (let ((smex-origin-buffer (current-buffer)))
          (with-simulated-input "RET"
            (smex-read-and-run smex-cache "my-temp-command"))
          (expect 'execute-extended-command :to-have-been-called-with nil "my-temp-command")
          (expect 'smex-augment-commands-with-keybinds :to-have-been-called)
          (expect 'smex-update-keybind-hash :to-have-been-called)))))

  (describe "auto-update functionality"

    (it "should force an update when idle for `auto-update-interval'")

    (it "should cancel the long-update timer when `auto-update-interval' is nil")

    (it "should force an update 1 second after clearing `smex-last-update-time'"))

  (describe "with `smex-save-file'"

    (it "should be able to save to a file")

    (it "should be able to load from a file")

    (it "should not fail when loading a nonexistent file")

    (it "should not save when `init-file-user' or `smex-save-file' are nil"))

  (describe "with `smex-prompt-string'"

    (it "should use the specified prompt string"))

  (describe "with `smex-ignored-command-matchers'"

    (it "should ignore commands matching a regexp")

    (it "should ignore commands matching a function")

    (it "should still allow executing ignored commands")

    (it "should not offer ignored commands for completion"))

  (describe "smex keybinds"

    (it "should activate `smex-map' while running smex")

    (it "should have functioning key binds"))



  (describe "smex-mode"

    (before-each
      (smex-mode 1))

    (it "should replace M-x when enabled")

    (it "should not replace M-x when disabled")))

;;; test-smex.el ends here
