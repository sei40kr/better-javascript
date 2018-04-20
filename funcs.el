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

(defun spacemacs//setup-javascript-flycheck-eslint ()
  (interactive)
  (when-let* ((found (executable-find "eslint_d")))
    (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
    (custom-set-variables '(flycheck-javascript-eslint-executable found))))


;; import-js

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))


;; js2-jsx-mode

(defun spacemacs//setup-javascript-lsp ()
  (if (and
        (configuration-layer/package-used-p 'flow-minor-mode)
        (flow-minor-tag-present-p)
        (flow-minor-configured-p))
    (lsp-javascript-flow-enable)
    (lsp-javascript-typescript-enable)))

(defun spacemacs//setup-js2-jsx-mode ()
  (setq-local company-minimum-prefix-length 2)
  (setq-local emmet-expand-jsx-className? t)
  (when (configuration-layer/layer-used-p 'lsp)
    (spacemacs//setup-javascript-lsp))
  (yas-activate-extra-mode 'js-mode))


;; lsp-javascript-flow

(defun spacemacs//flow-language-server-detect ()
  "Detect flow-language-server binary and warn if not found."
  (let ((found (executable-find "flow-language-server")))
    (unless found
      (spacemacs-buffer/warning "flow-language-server not found!"))
    found))


;; lsp-javascript-typescript

(defun spacemacs//javascript-typescript-stdio-detect ()
  "Detect javascript-typescript-stdio binary and warn if not found."
  (let ((found (executable-find "javascript-typescript-stdio")))
    (unless found
      (spacemacs-buffer/warning "javascript-typescript-stdio not found!"))
    found))


;; prettier-eslint

(defun spacemacs//prettier-eslint-detect ()
  "Detect prettier-eslint binary and warn if not found."
  (let ((found (executable-find "prettier-eslint")))
    (unless found
      (spacemacs-buffer/warning "prettier-eslint binary not found!"))
    found))
