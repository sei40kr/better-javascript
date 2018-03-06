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
        evil-matchit
        flycheck
        (import-js :toggle (spacemacs//import-js-detect))
        js-doc
        js2-refactor
        livid-mode
        rjsx-mode
        skewer-mode
        smartparens))

(setq better-javascript-excluded-packages '(company-tern tern web-beautify))

(defun better-javascript/post-init-add-node-modules-path ()
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(defun better-javascript/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook 'emmet-mode))

(defun better-javascript/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'rjsx-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump))))
  (add-hook `rjsx-mode `turn-on-evil-matchit-mode))

(defun better-javascript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'rjsx-mode))

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
  (add-hook 'rjsx-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode))

(defun better-javascript/pre-init-js2-refactor ()
  (add-hook 'rjsx-mode-hook 'spacemacs/js2-refactor-require)
  ;; prefixes
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr3" "ternary")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mra" "add/args")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrb" "barf")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrc" "contract")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mre" "expand/extract")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mri" "inline/inject/introduct")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrl" "localize/log")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrr" "rename")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrs" "split/slurp")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrt" "toggle")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mru" "unwrap")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrv" "var")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrw" "wrap")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mx" "text")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "mxm" "move")
  ;; key bindings
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
    "r3i" 'js2r-ternary-to-if
    "rag" 'js2r-add-to-globals-annotation
    "rao" 'js2r-arguments-to-object
    "rba" 'js2r-forward-barf
    "rca" 'js2r-contract-array
    "rco" 'js2r-contract-object
    "rcu" 'js2r-contract-function
    "rea" 'js2r-expand-array
    "ref" 'js2r-extract-function
    "rem" 'js2r-extract-method
    "reo" 'js2r-expand-object
    "reu" 'js2r-expand-function
    "rev" 'js2r-extract-var
    "rig" 'js2r-inject-global-in-iife
    "rip" 'js2r-introduce-parameter
    "riv" 'js2r-inline-var
    "rlp" 'js2r-localize-parameter
    "rlt" 'js2r-log-this
    "rrv" 'js2r-rename-var
    "rsl" 'js2r-forward-slurp
    "rss" 'js2r-split-string
    "rsv" 'js2r-split-var-declaration
    "rtf" 'js2r-toggle-function-expression-and-declaration
    "ruw" 'js2r-unwrap
    "rvt" 'js2r-var-to-this
    "rwi" 'js2r-wrap-buffer-in-iife
    "rwl" 'js2r-wrap-in-for-loop
    "k" 'js2r-kill
    "xmj" 'js2r-move-line-down
    "xmk" 'js2r-move-line-up))

(defun better-javascript/pre-init-livid-mode ()
  (spacemacs|add-toggle javascript-repl-live-evaluation
    :mode livid-mode
    :documentation "Live evaluation of JS buffer change."
    :evil-leader-for-mode (rjsx-mode . "Tl")))

(defun better-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
      (add-hook 'rjsx-mode-hook 'spacemacs//setup-rjsx-mode))
    :config
    (progn
      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mz" "folding")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
        "w" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))))

(defun better-javascript/pre-init-skewer-mode ()
  (add-hook 'rjsx-mode-hook 'skewer-mode))

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
