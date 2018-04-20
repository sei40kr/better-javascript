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
  (when-let* (('found (executable-find "eslint_d")))
    (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
    (custom-set-variables (flycheck-javascript-eslint-executable found))))


;; import-js

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))


;; lsp-javascript-flow

(defun spacemacs//flow-language-server-detect ()
  "Detect flow-language-server binary and warn if not found."
  (let ((found (executable-find "flow-language-server")))
    (unless found
      (spacemacs-buffer/warning "flow-language-server not found!"))
    found))

(defun spacemacs//flow-tag-present-p ()
  "Return true if the '// @flow' tag is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (stop found)
      (while (not stop)
        (when (not (re-search-forward "[^\n[:space:]]" nil t))
          (setq stop t))
        (if (equal (point) (point-min))
          (setq stop t)
          (backward-char))
        (cond ((or (looking-at "//+[ ]*@flow")
                 (looking-at "/\\**[ ]*@flow"))
                (setq found t)
                (setq stop t))
          ((looking-at "//")
            (forward-line))
          ((looking-at "/\\*")
            (when (not (re-search-forward "*/" nil t))
              (setq stop t)))
          (t (setq stop t))))
      found)))

(defun spacemacs//flow-configured-p ()
  "Predicate to check configuration."
  (locate-dominating-file
    (or (buffer-file-name) default-directory)
    ".flowconfig"))


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


;; rjsx-mode

(defun spacemacs//setup-javascript-lsp ()
  (if (and (spacemacs//flow-tag-present-p) (spacemacs//flow-configured-p))
    (lsp-javascript-flow-enable)
    (lsp-javascript-typescript-enable)))

(defun spacemacs//setup-rjsx-mode ()
  (setq-local company-minimum-prefix-length 2)
  (setq-local emmet-expand-jsx-className? t)
  (when (configuration-layer/layer-used-p 'lsp)
    (spacemacs//setup-javascript-lsp))
  (yas-activate-extra-mode 'js-mode))
