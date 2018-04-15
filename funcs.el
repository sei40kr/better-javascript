;;; funcs.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3


;; flycheck

(defun spacemacs//setup-flycheck-eslint-executable ()
  (interactive)
  (when-let (found (executable-find "eslint_d"))
    (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
    (custom-set-variables (flycheck-javascript-eslint-executable found))))


;; import-js

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))


;; prettier-eslint

(defun spacemacs//prettier-eslint-detect ()
  "Detect prettier-eslint binary and warn if not found."
  (let ((found (executable-find "prettier-eslint")))
    (unless found
      (spacemacs-buffer/warning "prettier-eslint binary not found!"))
    found))


;; rjsx-mode

(defun spacemacs//setup-rjsx-mode ()
  ;; See https://github.com/syl20bnr/spacemacs/issues/8222
  (setq-local company-minimum-prefix-length 2)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode))
