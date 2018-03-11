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
     flycheck
     (import-js :toggle (spacemacs//import-js-detect))
     js-doc
     livid-mode
     lsp-mode
     (lsp-javascript-flow :location local)
     (lsp-typescript :location local)
     rjsx-mode
     skewer-mode
     smartparens))

(setq better-javascript-excluded-packages
  '(
     company-tern
     tern
     web-beautify))

(defun better-javascript/post-init-add-node-modules-path ()
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(defun better-javascript/post-init-company ()
  (add-hook 'rjsx-mode-hook #'spacemacs//javascript-setup-lsp-company))

(defun better-javascript/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook #'emmet-mode))

(defun better-javascript/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'rjsx-mode
      '((evilmi-simple-get-tag evilmi-simple-jump)
         (evilmi-javascript-get-tag evilmi-javascript-jump)
         (evilmi-html-get-tag evilmi-html-jump))))
  (add-hook 'rjsx-mode-hook #'turn-on-evil-matchit-mode))

(defun better-javascript/post-init-flycheck ()
  (dolist (mode '(rjsx-mode json-mode))
    (spacemacs/enable-flycheck mode)))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
        "if" 'import-js-fix
        "ii" 'import-js-import
        "ig" 'import-js-goto))))

(defun better-javascript/post-init-js-doc ()
  (add-hook 'rjsx-mode-hook #'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode))

(defun better-javascript/pre-init-livid-mode ()
  (spacemacs|add-toggle javascript-repl-live-evaluation
    :mode livid-mode
    :documentation "Live evaluation of JS buffer change."
    :evil-leader-for-mode (rjsx-mode . "Tl")))

(defun better-javascript/pre-init-lsp-mode ()
  (add-hook 'rjsx-mode-hook #'spacemacs//javascript-setup-lsp))

(defun better-javascript/init-lsp-javascript-flow ()
  (use-package lsp-javascript-flow
    :commands lsp-javascript-flow-enable))

(defun better-javascript/init-lsp-typescript ()
  (use-package lsp-typescript
    :commands lsp-typescript-enable))

(defun better-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js[x]?$" . rjsx-mode))
      (add-hook 'rjsx-mode-hook #'spacemacs//setup-rjsx-mode))
    :config
    (progn
      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mz" "folding")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))))

(defun better-javascript/pre-init-skewer-mode ()
  (add-hook 'rjsx-mode-hook #'skewer-mode))

(defun better-javascript/post-init-skewer-mode ()
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "ms" "skewer")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
    "'" 'spacemacs/skewer-start-repl
    "ee" 'skewer-eval-last-expression
    "eE" 'skewer-eval-print-last-expression
    "sb" 'skewer-load-buffer
    "sB" 'spacemacs/skewer-load-buffer-and-focus
    "si" 'spacemacs/skewer-start-repl
    "sf" 'skewer-eval-defun
    "sF" 'spacemacs/skewer-eval-defun-and-focus
    "sr" 'spacemacs/skewer-eval-region
    "sR" 'spacemacs/skewer-eval-region-and-focus
    "ss" 'skewer-repl))

(defun better-javascript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))
