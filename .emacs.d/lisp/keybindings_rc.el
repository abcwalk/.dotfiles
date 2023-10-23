;;; keybindings_rc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; This is keybindings configuration
;;; Code:

(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-x v") 'ivy-push-view)
(global-set-key (kbd "C-x V") 'ivy-switch-view)

;; (define-key global-map (kbd "<f5>") 'lsp-execute-code-actions)

(define-key global-map (kbd "C-d") 'scroll-up-command)
(define-key global-map (kbd "C-u") 'scroll-down-command)
(define-key global-map (kbd "M-0") 'treemacs-select-window)

(define-key global-map (kbd "M-g") 'git-gutter-mode)

(global-set-key (kbd "C-x f") 'counsel-recentf)

(define-key global-map (kbd "C-x l") 'pulsar-pulse-line-blue)

(global-unset-key (kbd "C-x b"))
(global-set-key (kbd "C-x b") 'ibuffer-other-window)

;; Emms
(global-set-key (kbd "C-c r") 'emms-toggle-repeat-track)
(global-set-key (kbd "C-c p") 'emms-pause)
(global-set-key (kbd "C-c n") 'emms-previous)
(global-set-key (kbd "C-c b") 'emms-next)

;; (global-set-key (kbd "M-%") 'phi-replace-query)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

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

(global-set-key (kbd "C-c y") 'company-yasnippet)

(provide 'mappings_rc)
;;; keybindings_rc.el ends here
