;; init-player.el --- Initialize player configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-p") 'projectile-command-map)
  (projectile-mode +1))

(defun my-set-buffer-directory-to-project-root ()
  "Set 'default-directory' to 'projectile-project-root'."
  (when-let ((root (projectile-project-root)))
    (setq-local default-directory root)))

(add-hook 'prog-mode-hook 'my-set-buffer-directory-to-project-root)

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
