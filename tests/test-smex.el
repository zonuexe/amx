;;; -*- lexical-binding: t -*-

(require 'smex)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)

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

(describe "Using smex"

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

  (describe "In the smex package"

    (it "should not allow setting a backend without loading the required feature")

    (describe "the standard backend"
      (it "should call `completing-read-default'")
      (it "should not call `completing-read'"))
    (describe "the ido backend"
      (it "should load `ido-completing-read+' when selected")
      (it "should call `ido-completing-read+'"))
    (describe "the ivy backend"
      (it "should load `ivy' when selected")
      (it "should call `ivy-read'"))
    (describe "the auto backend"
      (it "should normally use standard completion")
      (it "should use ido completion when `ido-mode' or `ido-ubiquitous-mode' are enabled")
      (it "should use ivy completion when `ivy-mode' is enabled"))

    (describe "with `smex-show-key-bindings'"
      (it "should show key bindings when enabled")
      (it "should show key bindings when enabled")
      (it "should update the keybind hash after binding a command in an active map")
      (it "should not update the keybind hash after binding a command in an inactive map")
      (it "should use `smex-origin-buffer' instead of current buffer when looking up key binds")
      (it "should not show key bindings when disabled")
      (it "should not update the key binding hash table when disabled"))
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
    (it "should not replace M-x when disabled"))))

;;; test-smex.el ends here
