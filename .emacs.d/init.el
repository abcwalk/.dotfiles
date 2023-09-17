;; Load cutom-variables
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Set up package and use-package

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; Options

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(save-place-mode 1)
(menu-bar-mode -1)
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
       '(
         (?\" . ?\")
 	(?{ . ?\})))
(global-display-line-numbers-mode 1)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "\C-xf") 'recentf-open-files)

;; ox-md
(require 'ox-md)

;; mood-line
(use-package mood-line

  ;; Enable mood-line
  :config
  (mood-line-mode)):
(setq mood-line-glyph-alist mood-line-glyphs-unicode)

;; vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; org
(require 'org)
(setq org-clock-sound "~/Documents/emacs/ding.wav")

;; Evil-Nerd-commenter
(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys nil t))
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; Aggressive-indent
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; undo fu & undo fu session

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; helm
(use-package helm
  :init
  (helm-mode 1))

;; Theme
(load-theme 'gruber-darker t)

;; Keybindings

;; C-M-x eval-defun

;; Move start/end of document
(global-set-key (kbd "S-<up>") #'beginning-of-buffer)
(global-set-key (kbd "S-<down>") #'end-of-buffer)

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

;; Copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key "\C-c\C-c" 'copy-line)

