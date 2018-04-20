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
     emmet-mode
     eslintd-fix
     evil-matchit
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

(defun better-javascript/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook #'emmet-mode))

(defun better-javascript/init-eslintd-fix ()
  (use-package eslintd-fix
    :commands eslintd-fix
    :init
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
      "=" #'eslintd-fix)
    :custom
    (eslintd-fix-preprocess-command "prettier")))

(defun better-javascript/post-init-evil-matchit ()
  (add-hook 'rjsx-mode-hook #'turn-on-evil-matchit-mode)
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'rjsx-mode
      '((evilmi-simple-get-tag evilmi-simple-jump)
         (evilmi-javascript-get-tag evilmi-javascript-jump)
         (evilmi-html-get-tag evilmi-html-jump)))))

(defun better-javascript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'rjsx-mode)
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'javascript-standard flycheck-disabled-checkers))
  (add-hook 'rjsx-mode-hook #'spacemacs//setup-javascript-flycheck-eslint))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
        "if" #'import-js-fix
        "ii" #'import-js-import
        "ig" #'import-js-goto))
    :hook (rjsx-mode . run-import-js)))

(defun better-javascript/post-init-js-doc ()
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode)
  (add-hook 'rjsx-mode-hook #'spacemacs/js-doc-require))

(defun better-javascript/pre-init-livid-mode ()
  (spacemacs|add-toggle javascript-repl-live-evaluation
    :mode livid-mode
    :documentation "Live evaluation of JS buffer change."
    :evil-leader-for-mode (rjsx-mode . "Tl")))

(defun better-javascript/init-lsp-javascript-flow ()
  (use-package lsp-javascript-flow
    :commands lsp-javascript-flow-enable))

(defun better-javascript/init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript
    :commands lsp-javascript-typescript-enable))

(defun better-javascript/init-prettier-eslint ()
  (use-package prettier-eslint
    :commands prettier-eslint
    :init
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
      "=" #'prettier-eslint)))

(defun better-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mz" "folding")
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
        "zc" #'js2-mode-hide-element
        "zo" #'js2-mode-show-element
        "zr" #'js2-mode-show-all
        "ze" #'js2-mode-toggle-element
        "zF" #'js2-mode-toggle-hide-functions
        "zC" #'js2-mode-toggle-hide-comments))
    :mode ("\\.js[x]?$" . rjsx-mode)
    :hook (rjsx-mode . spacemacs//setup-rjsx-mode)
    :custom
    (js2-mode-show-parse-errors nil)
    (js2-mode-assume-strict nil)
    (js2-mode-show-strict-warnings nil)
    (js2-strict-trailing-comma-warning nil)
    (js2-strict-missing-semi-warning nil)
    (js2-missing-semi-one-line-override t)
    (js2-strict-inconsistent-return-warning nil)
    (js2-strict-cond-assign-warning nil)
    (js2-strict-var-redeclaration-warning nil)
    (js2-strict-var-hides-function-arg-warning nil)))

(defun better-javascript/pre-init-skewer-mode ()
  (add-hook 'rjsx-mode-hook #'skewer-mode))

(defun better-javascript/post-init-skewer-mode ()
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "ms" "skewer")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
    "'" #'spacemacs/skewer-start-repl
    "ee" #'skewer-eval-last-expression
    "eE" #'skewer-eval-print-last-expression
    "sb" #'skewer-load-buffer
    "sB" #'spacemacs/skewer-load-buffer-and-focus
    "si" #'spacemacs/skewer-start-repl
    "sf" #'skewer-eval-defun
    "sF" #'spacemacs/skewer-eval-defun-and-focus
    "sr" #'spacemacs/skewer-eval-region
    "sR" #'spacemacs/skewer-eval-region-and-focus
    "ss" #'skewer-repl))

(defun better-javascript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))
