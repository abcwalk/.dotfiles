;; init-player.el --- Initialize player configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
