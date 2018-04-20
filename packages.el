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
     (flow-js2-mode
       :location local
       :requires flow-minor-mode)
     flow-minor-mode
     flycheck
     (import-js :toggle (spacemacs//import-js-detect))
     js-doc
     js2-mode
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
  (add-hook 'js2-jsx-mode-hook #'add-node-modules-path))

(defun better-javascript/post-init-company ()
  (spacemacs|add-company-backends :backends company-files))

(defun better-javascript/post-init-emmet-mode ()
  (add-hook 'js2-jsx-mode-hook #'emmet-mode))

(defun better-javascript/post-init-evil-matchit ()
  (add-hook 'js2-jsx-mode-hook #'turn-on-evil-matchit-mode)
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'js2-jsx-mode
      '((evilmi-simple-get-tag evilmi-simple-jump)
         (evilmi-javascript-get-tag evilmi-javascript-jump)
         (evilmi-html-get-tag evilmi-html-jump)))))

(defun better-javascript/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :commands flow-js2-mode
    :init
    (add-hook 'js2-jsx-mode-hook #'(lambda () (flow-js2-mode 1)))
    :config
    (spacemacs|hide-lighter flow-js2-mode)))

(defun better-javascript/init-flow-minor-mode ()
  (use-package flow-minor-mode
    :commands (flow-minor-tag-present-p flow-minor-configured-p)))

(defun better-javascript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'js2-jsx-mode)
  (add-hook 'js2-jsx-mode-hook #'spacemacs//setup-javascript-flycheck-eslint)
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'javascript-standard flycheck-disabled-checkers)))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-jsx-mode "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode 'js2-jsx-mode
        "if" #'import-js-fix
        "ii" #'import-js-import
        "ig" #'import-js-goto))
    :hook (js2-jsx-mode . run-import-js)))

(defun better-javascript/post-init-js-doc ()
  (spacemacs/js-doc-set-key-bindings 'js2-jsx-mode)
  (add-hook 'js2-jsx-mode-hook #'spacemacs/js-doc-require))

(defun better-javascript/post-init-js2-mode ()
  (custom-set-variables
    '(js2-mode-show-parse-errors nil)
    '(js2-mode-assume-strict nil)
    '(js2-mode-show-strict-warnings nil)
    '(js2-strict-trailing-comma-warning nil)
    '(js2-strict-missing-semi-warning nil)
    '(js2-missing-semi-one-line-override t)
    '(js2-strict-inconsistent-return-warning nil)
    '(js2-strict-cond-assign-warning nil)
    '(js2-strict-var-redeclaration-warning nil)
    '(js2-strict-var-hides-function-arg-warning nil))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
  (add-hook 'js2-jsx-mode-hook #'spacemacs//setup-js2-jsx-mode)
  (spacemacs|use-package-add-hook
    :post-config
    (spacemacs/declare-prefix-for-mode 'js2-jsx-mode "mz" "folding")
    (spacemacs/set-leader-keys-for-major-mode 'js2-jsx-mode
      "zc" #'js2-mode-hide-element
      "zo" #'js2-mode-show-element
      "zr" #'js2-mode-show-all
      "ze" #'js2-mode-toggle-element
      "zF" #'js2-mode-toggle-hide-functions
      "zC" #'js2-mode-toggle-hide-comments)))

(defun better-javascript/pre-init-livid-mode ()
  (spacemacs|add-toggle javascript-repl-live-evaluation
    :mode livid-mode
    :documentation "Live evaluation of JS buffer change."
    :evil-leader-for-mode (js2-jsx-mode . "Tl")))

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
    (spacemacs/set-leader-keys-for-major-mode 'js2-jsx-mode
      "=" #'prettier-eslint)))

(defun better-javascript/pre-init-skewer-mode ()
  (add-hook 'js2-jsx-mode-hook #'skewer-mode))

(defun better-javascript/post-init-skewer-mode ()
  (spacemacs/declare-prefix-for-mode 'js2-jsx-mode "ms" "skewer")
  (spacemacs/declare-prefix-for-mode 'js2-jsx-mode "me" "eval")
  (spacemacs/set-leader-keys-for-major-mode 'js2-jsx-mode
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
    (add-hook 'js2-jsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'js2-jsx-mode-hook #'smartparens-mode)))
