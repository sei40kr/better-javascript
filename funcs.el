;;; funcs.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

;; flow-js2-mode

(defun spacemacs//activate-flow-js2-mode ()
  (require 'flow-minor-mode)
  (require 'flow-js2-mode)
  (activate-flow-js2-mode))


;; flycheck

(defun spacemacs//flycheck-javascript-eslint-set-executable ()
  "Set the executable path of ESLint."
  (when-let ((found (executable-find "eslint_d")))
    (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))


;; import-js

(defun spacemacs/turn-on-import-js ()
  (interactive)
  (require 'import-js)
  (run-import-js))

(defun spacemacs//import-js-set-key-bindings (mode)
  "Set the key bindings for import-js in the given MODE"
  (spacemacs/declare-prefix-for-mode mode "mi" "import")
  (spacemacs/set-leader-keys-for-major-mode mode
    "if" #'import-js-fix
    "ii" #'import-js-import
    "ig" #'import-js-goto))


;; prettier-js

(defun spacemacs//set-key-bindings-for-prettier-js (mode)
  "Set the key bindings for prettier-js in the given MODE"
  (spacemacs/set-leader-keys-for-major-mode mode
    "=" #'spacemacs/prettier-js))

(defun spacemacs/prettier-js ()
  "Format the current buffer according to the prettier tool."
  (interactive)
  (require 'prettier-js)
  (prettier-js))
