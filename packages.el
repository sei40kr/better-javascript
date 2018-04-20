;;; packages.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

(setq better-javascript-packages
  '(
     add-node-modules-path
     company
     emmet-mode
     evil-matchit
     (flow-js2-mode :location local)
     flow-minor-mode
     flycheck
     (import-js :toggle (spacemacs//import-js-detect))
     js-doc
     livid-mode
     (lsp-javascript-flow
       :location local
       :toggle (spacemacs//flow-language-server-detect))
     (lsp-javascript-typescript
       :location local
       :toggle (spacemacs//javascript-typescript-stdio-detect))
     (prettier-eslint
       :location local
       :toggle (spacemacs//prettier-eslint-detect))
     rjsx-mode
     skewer-mode
     smartparens))

(setq better-javascript-excluded-packages
  '(
     company-tern
     counsel-gtags
     ggtags
     helm-gtags
     tern))

(defun better-javascript/post-init-add-node-modules-path ()
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(defun better-javascript/post-init-company ()
  (spacemacs|add-company-backends :backends company-files))

(defun better-javascript/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook #'spacemacs//enable-emmet-mode))

(defun better-javascript/post-init-evil-matchit ()
  (add-hook 'rjsx-mode-hook #'turn-on-evil-matchit-mode)
  (eval-after-load 'evil-matchit
    #'spacemacs//setup-evil-matchit-for-rjsx-mode))

(defun better-javascript/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :commands activate-flow-js2-mode
    :config (spacemacs|hide-lighter flow-js2-mode)
    :hook (rjsx-mode . activate-flow-js2-mode)))

(defun better-javascript/init-flow-minor-mode ()
  (use-package flow-minor-mode
    :commands (flow-minor-tag-present-p flow-minor-configured-p)))

(defun better-javascript/post-init-flycheck ()
  (add-hook 'rjsx-mode-hook #'spacemacs//setup-flycheck-javascript-eslint)
  (when (configuration-layer/layer-used-p 'lsp)
    (add-hook 'rjsx-mode-hook
      #'spacemacs//setup-flycheck-lsp-ui-for-javascript))
  (eval-after-load 'flycheck #'spacemacs//setup-javascript-flycheck))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :commands run-import-js
    :init (spacemacs//set-key-bindings-for-import-js 'rjsx-mode)
    :hook (rjsx-mode . run-import-js)))

(defun better-javascript/post-init-js-doc ()
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode)
  (add-hook 'rjsx-mode-hook #'spacemacs/js-doc-require))

(defun better-javascript/post-init-livid-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
    "Tl" #'spacemacs/toggle-livid-mode))

(defun better-javascript/init-lsp-javascript-flow ()
  (use-package lsp-javascript-flow))

(defun better-javascript/init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript))

(defun better-javascript/init-prettier-eslint ()
  (use-package prettier-eslint
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
      "=" #'prettier-eslint)))

(defun better-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :config
    (progn
      (when (configuration-layer/package-used-p 'flycheck)
        (spacemacs/enable-flycheck 'rjsx-mode))
      ;; Disable the key bindings by rjsx-mode.
      (define-key rjsx-mode-map "<" nil)
      (define-key rjsx-mode-map (kbd "C-d") nil)
      (define-key rjsx-mode-map ">" nil)
      ;; Add the key bindings for syntax-aware code folding.
      (spacemacs//set-js2-mode-key-bindings 'rjsx-mode))
    :mode "\\.jsx?\\'"
    :interpreter "node"
    :hook
    (rjsx-mode . spacemacs//disable-js2-mode-checker)
    (rjsx-mode . spacemacs//setup-rjsx-mode)))

(defun better-javascript/post-init-skewer-mode ()
  (spacemacs//set-skewer-mode-key-bindings 'rjsx-mode)
  (add-hook 'rjsx-mode-hook #'skewer-mode))

(defun better-javascript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))
