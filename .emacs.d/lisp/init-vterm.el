;; init-vterm.el --- Initialize vteem configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Vterm configurations.
;;

;;; Code:

;; Vterm
(use-package eshell
  :bind (("C-x /" . vterm-toggle)
         ("C-x ." . vterm-toggle-cd)))

(provide 'init-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vterm.el ends here
