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
     (flow-js2-mode
       :location (recipe :fetcher github
                         :repo "Fuco1/flow-js2-mode"))
     flycheck
     import-js
     rjsx-mode
     web-beautify))

(defun better-javascript/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :commands activate-flow-js2-mode
    :init
    (progn
      (add-hook 'js2-mode-hook #'activate-flow-js2-mode)
      (add-hook 'rjsx-mode-hook #'activate-flow-js2-mode))
    :config (spacemacs|hide-lighter flow-js2-mode)))

(defun better-javascript/post-init-flycheck ()
  (push 'javascript-jshint flycheck-disabled-checkers)
  (push 'javascript-standard flycheck-disabled-checkers)
  (add-hook 'js2-mode-hook #'spacemacs//flycheck-javascript-eslint-set-executable)
  (add-hook 'rjsx-mode-hook #'spacemacs//flycheck-javascript-eslint-set-executable))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :init
    (progn
      (spacemacs//import-js-set-key-bindings 'js2-mode)
      (spacemacs//import-js-set-key-bindings 'rjsx-mode)
      (add-hook 'js2-mode-hook #'spacemacs/turn-on-import-js)
      (add-hook 'rjsx-mode-hook #'spacemacs/turn-on-import-js))))

(defun better-javascript/post-init-rjsx-mode ()
  (setq
    js2-mode-show-parse-errors nil
    js2-mode-assume-strict nil
    js2-mode-show-strict-warnings nil
    js2-strict-trailing-comma-warning nil
    js2-strict-missing-semi-warning nil
    js2-strict-inconsistent-return-warning nil
    js2-strict-cond-assign-warning nil
    js2-strict-var-redeclaration-warning nil
    js2-strict-var-hides-function-arg-warning nil))

(defun better-javascript/post-init-web-beautify ()
  (setq
    web-beautify-css-program "prettier"
    web-beautify-js-program "prettier-eslint"
    web-beautify-args '("--write")))
