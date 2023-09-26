;;; keybindings_rc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; This is keybindings configuration
;;; Code:

(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-x v") 'ivy-push-view)
(global-set-key (kbd "C-x V") 'ivy-switch-view)

(define-key global-map (kbd "<f5>") 'lsp-execute-code-actions)

(define-key global-map (kbd "C-d") 'scroll-up-command)
(define-key global-map (kbd "C-u") 'scroll-down-command)
(define-key global-map (kbd "M-0") 'treemacs-select-window)

(global-set-key (kbd "C-x f") 'counsel-recentf)

(define-key global-map (kbd "C-x l") #'pulsar-pulse-line-blue)

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
;;; keybindings_rc.el ends here
