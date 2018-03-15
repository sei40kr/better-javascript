;;; funcs.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

;; Based on https://github.com/an-sh/flow-minor-mode/blob/master/flow-minor-mode.el
(defun spacemacs//flow-annotation-present-p ()
  "Detect the '@flow' annotation is present in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (stop found)
      (while (not stop)
        (when (not (re-search-forward "[^\n[:space:]]" nil t))
          (setq stop t))
        (backward-char)
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

(defun spacemacs//javascript-setup-backend ()
  "Conditionally setup javascript backend."
  (spacemacs//javascript-setup-lsp))

(defun spacemacs//javascript-setup-company ()
  "Conditionally setup company based on backend."
  (spacemacs//javascript-setup-lsp-company))


;; import-js

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))


;; lsp

(defun spacemacs//javascript-setup-lsp ()
  "Conditionally setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
    (if (spacemacs//flow-annotation-present-p)
      (spacemacs//javascript-setup-lsp-javascript-flow)
      (spacemacs//javascript-setup-lsp-javascript-typescript))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dofile.")))

(defun spacemacs//javascript-setup-lsp-javascript-flow ()
  "Setup lsp backend with lsp-javascript-flow."
  (if (configuration-layer/package-used-p 'lsp-javascript-flow)
    (progn
      (require 'lsp-javascript-flow)
      (lsp-javascript-flow-enable))))

(defun spacemacs//javascript-setup-lsp-javascript-typescript ()
  "Setup lsp backend with lsp-javascript-typescript."
  (if (configuration-layer/package-used-p 'lsp-javascript-typescript)
    (progn
      (require 'lsp-javascript-typescript)
      (lsp-javascript-typescript-enable))))

(defun spacemacs//javascript-setup-lsp-typescript ()
  "Setup lsp backend with lsp-typescript."
  (if (configuration-layer/package-used-p 'lsp-typescript)
    (progn
      (require 'lsp-typescript)
      (lsp-typescript-enable))))

(defun spacemacs//javascript-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
    (progn
      (spacemacs|add-company-backends
        :backends company-lsp
        :modes rjsx-mode)
      (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dofile.")))


;; rjsx-mode

(defun spacemacs//setup-rjsx-mode ()
  ;; See https://github.com/syl20bnr/spacemacs/issues/8222
  (setq-local company-minimum-prefix-length 2)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; lsp show parse errors on its own
  (setq-local js2-mode-show-parse-errors nil)
  (setq-local js2-mode-assume-strict nil)
  (setq-local js2-mode-show-strict-warnings nil)
  (setq-local js2-strict-trailing-comma-warning nil)
  (setq-local js2-strict-missing-semi-warning nil)
  (setq-local js2-missing-semi-one-line-override t)
  (setq-local js2-strict-inconsistent-return-warning nil)
  (setq-local js2-strict-cond-assign-warning nil)
  (setq-local js2-strict-var-redeclaration-warning nil)
  (setq-local js2-strict-var-hides-function-arg-warning nil)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode))
