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
       :location
       (recipe
         :fetcher github
         :repo "Fuco1/flow-js2-mode"))
     flow-minor-mode
     flycheck
     (import-js :toggle (spacemacs//import-js-detect))
     js-doc
     livid-mode
     (prettier-eslint
       :location
       (recipe
         :fetcher github
         :repo "evindor/prettier-eslint-emacs")
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
  (add-hook 'rjsx-mode-hook #'spacemacs//emmet-mode-enable))

(defun better-javascript/post-init-evil-matchit ()
  (add-hook 'rjsx-mode-hook #'turn-on-evil-matchit-mode)
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'rjsx-mode
      '(
         (evilmi-simple-get-tag evilmi-simple-jump)
         (evilmi-javascript-get-tag evilmi-javascript-jump)
         (evilmi-html-get-tag evilmi-html-jump)))))

(defun better-javascript/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :commands activate-flow-js2-mode
    :config (spacemacs|hide-lighter flow-js2-mode)
    :hook (rjsx-mode . activate-flow-js2-mode)))

(defun better-javascript/init-flow-minor-mode ()
  (use-package flow-minor-mode
    :commands (flow-minor-tag-present-p flow-minor-configured-p)))

(defun better-javascript/post-init-flycheck ()
  (add-hook 'rjsx-mode-hook #'spacemacs//flycheck-eslint-set-executable)
  (eval-after-load 'flycheck #'spacemacs//javascript-flycheck-setup))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :commands run-import-js
    :init (spacemacs//import-js-set-key-bindings 'rjsx-mode)
    :hook (rjsx-mode . run-import-js)))

(defun better-javascript/post-init-js-doc ()
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode)
  (add-hook 'rjsx-mode-hook #'spacemacs/js-doc-require))

(defun better-javascript/post-init-livid-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
    "Tl" #'spacemacs/toggle-livid-mode))

(defun better-javascript/init-prettier-eslint ()
  (use-package prettier-eslint
    :commands prettier-eslint
    :init
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
      "=" #'prettier-eslint)))

(defun better-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :config
    (progn
      (when (configuration-layer/package-used-p 'flycheck)
        (spacemacs/enable-flycheck 'rjsx-mode))
      (define-key rjsx-mode-map "<" nil)
      (define-key rjsx-mode-map (kbd "C-d") nil)
      (define-key rjsx-mode-map ">" nil)
      (spacemacs/declare-prefix-for-mode 'rjsx-mode "mz" "folding")
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
        "zc" #'js2-mode-hide-element
        "zo" #'js2-mode-show-element
        "zr" #'js2-mode-show-all
        "ze" #'js2-mode-toggle-element
        "zF" #'js2-mode-toggle-hide-functions
        "zC" #'js2-mode-toggle-hide-comments))
    :mode "\\.jsx?\\'"
    :interpreter "node"
    :hook
    (rjsx-mode . spacemacs//js2-mode-disable-builtin-checker)
    (rjsx-mode . spacemacs//setup-rjsx-mode)))

(defun better-javascript/post-init-skewer-mode ()
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "me" "eval")
  (spacemacs/declare-prefix-for-mode 'rjsx-mode "ms" "skewer")
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
    "ss" #'skewer-repl)
  (add-hook 'rjsx-mode-hook #'skewer-mode))

(defun better-javascript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))
