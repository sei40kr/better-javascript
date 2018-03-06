;;; funcs.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

(defun spacemacs//javascript-typescript-setup-lsp ())

(defun spacemacs//javascript-flow-setup-lsp ())

(defun spacemacs//javascript-setup-lsp())

(defun spacemacs//javascript-setup-lsp-company())

;; import-js

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))

;; rjsx-mode

(defun spacemacs//setup-rjsx-mode ()
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode)
  ;; See https://github.com/syl20bnr/spacemacs/issues/8222
  (set (make-local-variable 'company-minimum-prefix-length) 2))
