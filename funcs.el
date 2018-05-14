;;; funcs.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3


;; emmet-mode

(defun spacemacs//emmet-mode-enable ()
  "Enable emmet-mode in current buffer."
  (emmet-mode 1))


;; flycheck

(defun spacemacs//flycheck-eslint-set-executable ()
  "Set the executable path of ESLint."
  (interactive)
  (when-let ((found (executable-find "eslint_d")))
    (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))

(defun spacemacs//javascript-flycheck-setup ()
  "Disable the checkers of javascript-hint, javascript-standard."
  (push 'javascript-jshint flycheck-disabled-checkers)
  (push 'javascript-standard flycheck-disabled-checkers))


;; import-js

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))

(defun spacemacs//import-js-set-key-bindings (mode)
  "Set the key bindings for import-js in the given MODE"
  (spacemacs/declare-prefix-for-mode mode "mi" "import")
  (spacemacs/set-leader-keys-for-major-mode mode
    "if" #'import-js-fix
    "ii" #'import-js-import
    "ig" #'import-js-goto))


;; prettier-eslint

(defun spacemacs//prettier-eslint-detect ()
  "Detect prettier-eslint binary and warn if not found."
  (let ((found (executable-find "prettier-eslint")))
    (unless found
      (spacemacs-buffer/warning "prettier-eslint binary not found!"))
    found))


;; rjsx-mode

(defun spacemacs//js2-mode-disable-builtin-checker ()
  "Disable the built-in checker in js2-mode."
  (set (make-local-variable 'js2-mode-show-parse-errors) nil)
  (set (make-local-variable 'js2-mode-assume-strict) nil)
  (set (make-local-variable 'js2-mode-show-strict-warnings) nil)
  (set (make-local-variable 'js2-strict-trailing-comma-warning) nil)
  (set (make-local-variable 'js2-strict-missing-semi-warning) nil)
  (set (make-local-variable 'js2-strict-inconsistent-return-warning) nil)
  (set (make-local-variable 'js2-strict-cond-assign-warning) nil)
  (set (make-local-variable 'js2-strict-var-redeclaration-warning) nil)
  (set (make-local-variable 'js2-strict-var-hides-function-arg-warning) nil))

(defun spacemacs//setup-rjsx-mode ()
  "Setup rjsx-mode locally."
  (set (make-local-variable 'company-minimum-prefix-length) 2)
  (set (make-local-variable 'emmet-expand-jsx-className) t)
  (yas-activate-extra-mode 'js-mode))
