;;; prettier-eslint.el --- Autoformat javascript files with prettier-eslint

;; Copyright (C) 2017 Arseny Zarechnev
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Arseny Zarechnev <me@evindor.com> (twitter.com/evindor)
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: javascript, prettier, prettier-eslint, eslint, lint, formatting, style
;; URL: https://github.com/evindor/prettier-eslint-emacs

;;; Commentary:
;;
;; This file provides `prettier-eslint', which fixes the current file using prettier-eslint
;; https://github.com/kentcdodds/prettier-eslint-cli
;;
;; Usage:
;;     M-x prettier-eslint
;;
;;     To automatically format after saving:
;;
;;     (add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-eslint nil t)))
;;     (add-hook 'react-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-eslint nil t)))
;;
;;     In Spacemacs add the above to your dotspacemacs/user-config
;;
;;     Otherwise:
;;
;;     (eval-after-load 'js2-mode
;;       '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-eslint nil t))))
;;

;;; Code:
;;;###autoload

(defun prettier-eslint/binary ()
  (or
   ;; Try to find bin in node_modules (via 'npm install prettier-eslint-cli')
    (let ((root (locate-dominating-file buffer-file-name "node_modules")))
      (if root
          (let ((prettier-binary (concat root "node_modules/.bin/prettier-eslint")))
            (if (file-executable-p prettier-binary) prettier-binary))))
    ;; Fall back to a globally installed binary
    (executable-find "prettier-eslint")
    ;; give up
    (error "Couldn't find a prettier-eslint executable")))

(defun prettier-eslint ()
  "Format the current file with ESLint."
  (interactive)
  (progn
    (call-process
      (prettier-eslint/binary)
      nil "*Prettier-ESLint Errors*" nil
      "--write" buffer-file-name)
    (revert-buffer t t t)))


(provide 'prettier-eslint)

;;; prettier-eslint.el ends here
