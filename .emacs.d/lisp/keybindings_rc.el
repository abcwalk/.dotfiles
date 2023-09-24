;;; mappings_rc.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is my personal mappings config
;;; Code:

(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-x v") 'ivy-push-view)
(global-set-key (kbd "C-x V") 'ivy-switch-view)

(define-key global-map (kbd "<f5>") 'lsp-execute-code-actions)

(define-key global-map (kbd "C-d") 'scroll-up-command)
(define-key global-map (kbd "C-u") 'scroll-down-command)

(global-set-key (kbd "\C-xf") 'recentf-open-files)

(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;; Move line up/down
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

(provide 'mappings_rc)
;;; mappings_rc.el ends here
