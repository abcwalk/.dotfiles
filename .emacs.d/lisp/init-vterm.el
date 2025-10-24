;; init-vterm.el --- Initialize vteem configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Vterm configurations.
;;

;;; Code:

;; Vterm
(use-package vterm
  :config
  (defun my/vterm-toggle-copy-mode ()
    "Toggle vterm-copy-mode on/off."
    (interactive)
    (if (bound-and-true-p vterm-copy-mode)
        (progn
          (message "vterm-copy-mode: disabled")
          (vterm-copy-mode -1))
      (progn
        (message "vterm-copy-mode: enabled")
        (vterm-copy-mode 1))))
  (global-set-key (kbd "C-x ,") 'my/vterm-toggle-copy-mode))

(use-package vterm-toggle)

(defun my/vterm-toggle-project-root ()
  "Open vterm in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (vterm-toggle)))

(defun my/vterm-toggle-here ()
  "Open vterm in current buffer dir."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (vterm-toggle)))

(global-set-key (kbd "C-x /") 'my/vterm-toggle-project-root)
(global-set-key (kbd "C-x .") 'my/vterm-toggle-here)

(provide 'init-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vterm.el ends here
