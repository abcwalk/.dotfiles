;;; init.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max (* 10 1024 1024)
      bidi-inhibit-bpa t)


(setq package-list '
      '(dap-mode vimrc-mode yaml-mode xclip use-package undo-fu-session undo-fu org-bullets orderless minions magit lua-mode lsp-ui lsp-pyright lsp-java json-mode ivy-prescient hl-todo gruber-darker-theme gcmh format-all flycheck evil-nerd-commenter dashboard counsel company-prescient))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; {{{
;; Bootstrap 'straight'
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; automatically ensure every package exists (like :ensure or :straight)
;; (setq straight-use-package-by-default t)

;; Configure use-package to use straight.el by default
;; (use-package straight
;;   :custom
;;   (straight-use-package-by-default t))

;;}}}

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Control buffer placement
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;; Open files externally
(use-package openwith
  ;; :if (not dw/is-termux)
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "feh"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))


;; Block templates
;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; Pomodoro
;; (use-package org-pomodoro
;;   :after org
;;   :config
;;   (setq org-pomodoro-start-sound "~/.dotfiles/.emacs.d/sounds/focus_bell.wav")
;;   (setq org-pomodoro-short-break-sound "~/.dotfiles/.emacs.d/sounds/three_beeps.wav")
;;   (setq org-pomodoro-long-break-sound "~/.dotfiles/.emacs.d/sounds/three_beeps.wav")
;;   (setq org-pomodoro-finished-sound "~/.dotfiles/.emacs.d/sounds/meditation_bell.wav")

;;   (dw/leader-key-def
;;     "op"  '(org-pomodoro :which-key "pomodoro")))

;; auto close bracket insertion. New in emacs 24
;; (electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
;; (setq electric-pair-pairs
;;       '(
;; 	(?\" . ?\")
;;  	(?{ . ?\})))
;;(global-display-line-numbers-mode 1)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(save-place-mode 1)
(setq window-combination-resize t
      split-width-threshold 300)
(require 'recentf)
(recentf-mode 1)
(set-fringe-mode 0)
(global-set-key (kbd "\C-xf") 'recentf-open-files)

;; gcmh
(use-package gcmh
  :hook (emacs-startup-hook . gcmh-mode)
  :demand t
  :config
  (setq gcmh-low-cons-threshold (* 16 1024 1024))
  (gcmh-mode +1))

;; mood-line
;; (use-package mood-line

;; Enable mood-line
;; :config
;; (mood-line-mode)):
;; (setq mood-line-glyph-alist mood-line-glyphs-unicode)

;; vertico
;; (use-package vertico
;;   :init
;;   (vertico-mode)
;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   (setq vertico-cycle t)
;;   )

;; lambda-line
;; (use-package lambda-line
;;   :straight (:type git :host github :repo "lambda-emacs/lambda-line")
;;   :custom
;;   (lambda-line-position 'bottom) ;; Set position of status-line
;;   (lambda-line-abbrev t) ;; abbreviate major modes
;;   (lambda-line-hspace "  ")  ;; add some cushion
;;   (lambda-line-prefix t) ;; use a prefix symbol
;;   (lambda-line-prefix-padding nil) ;; no extra space for prefix
;;   (lambda-line-status-invert nil)  ;; no invert colors
;;   (lambda-line-gui-ro-symbol  " ") ;; symbols
;;   (lambda-line-gui-mod-symbol " ⬤")
;;   (lambda-line-gui-rw-symbol  " ◯")
;;   (lambda-line-vc-symbol " ")
;;   (lambda-line-space-top +.50)  ;; padding on top and bottom of line
;;   (lambda-line-space-bottom -.50)
;;   (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
;;   :config
;;   ;; activate lambda-line
;;   (lambda-line-mode)
;;   ;; set divider line in footer
;;   (when (eq lambda-line-position 'top)
;;     (setq-default mode-line-format (list "%_"))
;;     (setq mode-line-format (list "%_"))))

;; (use-package lambda-themes
;;   :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
;;   :custom
;;   (lambda-themes-set-italic-comments t)
;;   (lambda-themes-set-italic-keywords t)
;;   (lambda-themes-set-variable-pitch t)
;;   :config
;;   ;; load preferred theme
;;   (load-theme 'lambda-light))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;(setq org-clock-sound "~/Documents/emacs/ding.wav")

;; (use-package display-line-numbers
;;   :ensure nil
;;   :hook (prog-mode . display-line-numbers-mode)
;;   :config
;;   (setq-default display-line-numbers-width 1))

;; DAP
(use-package dap-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode
	  ;; java-mode
	  js-mode
	  js-jsx-mode
	  typescript-mode
	  web-mode
	  ) . lsp-deferred)
  :commands lsp
  :config
  (add-hook 'java-mode-hook #'(lambda () (when (eq major-mode 'java-mode) (lsp-deferred))))
  (global-unset-key (kbd "<f4>"))
  (define-key global-map (kbd "<f4>") 'lsp-rename)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-links nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-lens-enable 1)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-completion-show-detail nil)
  ;; (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-completion-show-kind nil)
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1MB
  ;; (setq lsp-idle-delay 0.25)
  (setq lsp-auto-execute-action nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight  ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enhanced-markdown nil)
  (setq lsp-ui-doc-delay 0.01)
  (setq lsp-prefer-capf t)
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-text-scale-level -1.0)
    (setq lsp-ui-doc-max-width 80)
    (setq lsp-ui-doc-max-height 25)
    (setq lsp-ui-doc-position 'at-point))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (setq lsp-ui-sideline-diagnostic-max-line-length 80)
  (setq lsp-ui-sideline-diagnostic-max-lines 2)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

;; Python
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))
;; Java
(use-package lsp-java
  :after lsp)

;; (use-package java-mode
;;   :ensure nil
;;   :after lsp-java)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-format-margin-function nil)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-limit 7)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match nil)
  ;; (setq company-format-margin-function #'company-vscode-light-icons-margin)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase t)
  ;; (setq company-text-icons-add-background t)
  ;; (setq company-text-icons-mapping t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (unless (display-graphic-p)
    (define-key company-active-map (kbd "C-h") #'backward-kill-word)
    (define-key company-active-map (kbd "C-w") #'backward-kill-word))
  (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (if (display-graphic-p)
      (define-key company-active-map (kbd "<tab>") 'company-select-next)
    (define-key company-active-map (kbd "TAB") 'company-select-next))
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

;; (use-package company-box
;;   :if (display-graphic-p)
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-doc-enable nil)
;;   (setq company-box-scrollbar nil)
;;   (setq company-box-frame-behavior 'default))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode))
  :custom-face
  (flycheck-error   ((t (:inherit error :underline t))))
  (flycheck-warning ((t (:inherit warning :underline t))))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.1)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil)
  (define-key flycheck-mode-map (kbd "<f8>") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "<S-f8>") #'flycheck-previous-error)
  (flycheck-define-checker proselint
    "A linter for prose. Install the executable with `pip3 install proselint'."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message) line-end))
    :modes (markdown-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package lua-mode)

(use-package json-mode)

(use-package vimrc-mode)

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Compile
(use-package compile
  :straight nil
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;; Snippets
;; M-x package-install RET yasnippet-snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))
;; icons
(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 0.8))

;; TODO WHY DOING
(use-package hl-todo
  :custom-face
  (hl-todo                        ((t (:inverse-video nil :italic t :bold nil))))
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb"))
  (global-hl-todo-mode +1))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode +1))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "( E M A C S )")
  (setq dashboard-init-info "")
  (setq dashboard-items nil)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-icon "")
  ;; (setq dashboard-footer-messages '("😈 Happy hacking!   "))
  (define-key dashboard-mode-map (kbd "<f5>") #'(lambda ()
                                                  (interactive)
                                                  (dashboard-refresh-buffer)
                                                  (message "Refreshing Dashboard...done"))))
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

;; Evil-Nerd2-commenter
(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys nil t))
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(use-package minions
  :config
  (setq minions-mode-line-lighter "")
  (setq minions-mode-line-delimiters '("" . ""))
  (minions-mode +1))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . auto-fill-mode)
	 (org-mode . org-indent-mode)
	 (org-mode . (lambda ()
		       (setq-local olivetti-body-width (+ fill-column 5)))))
  :config
  (require 'org-tempo)
  (setq org-link-descriptive nil)
  (setq org-startup-folded nil)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-html-checkbox-type 'html))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Aggressive-indent
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; undo fu & undo fu session

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git;-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Git
;; (use-package git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :ensure t
;;   :init
;;   (global-git-gutter-mode nil))

;; Git integration

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (local-unset-key (kbd "f"))
  (define-key magit-mode-map (kbd "<f5>") #'(lambda ()
                                              (interactive)
                                              (magit-refresh)
                                              (message "Refreshing Magit...done"))))

;; Markdown
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked"))
;; (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))
;; (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

;; format
(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer))
    (save-buffer))
  :config
  (global-set-key (kbd "C-f") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))

;; Compile
(add-hook 'java-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
                 (concat "java " buffer-file-name))))

;; HTML
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)


;; ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

;; swiper
(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

;; ivy-prescient
(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

;; Counsel
(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; (use-package general
;;   :config
;;   (general-evil-setup t)

;;   (general-create-definer dw/leader-key-def
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (general-create-definer dw/ctrl-c-keys
;;     :prefix "C-c"))

;; (dw/leader-key-def
;;   "r"   '(ivy-resume :which-key "ivy resume")
;;   "f"   '(:ignore t :which-key "files")
;;   "ff"  '(counsel-find-file :which-key "open file")
;;   "C-f" 'counsel-find-file
;;   "fr"  '(counsel-recentf :which-key "recent files")
;;   "fR"  '(revert-buffer :which-key "revert file")
;;   "fj"  '(counsel-file-jump :which-key "jump to file"))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(global-set-key (kbd "C-.") 'avy-goto-char-2)

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

;; Keybindings

;; C-M-x eval-defun

(define-key global-map (kbd "<f2>") 'save-buffer)
(define-key global-map (kbd "<f5>") 'lsp-ececute-code-actions)

(define-key global-map (kbd "C-d") 'scroll-up-command)
(define-key global-map (kbd "C-u") 'scroll-down-command)

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
(global-set-key (kbd "<f12>") 'save-buffers-kill-emacs)

;; Copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key "\C-c\C-c" 'copy-line)

;; OS

;; Windows
(if (eq system-type 'windows-nt)
    (progn
      (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" ':weight 'regular :height 160)
      (setq visible-bell t)
      (setq custom-file "c:/Users/cculpc/AppData/Roaming/.emacs.d/custom.el")
      (load custom-file)))

;; Linux
(if (eq system-type 'gnu/linux)
    (progn
      (set-face-attribute 'default nil :font "IosevkaTerm Nerd Font Mono" :weight 'regular :height 160)
      (setq custom-file "~/.emacs.d/custom.el")
      (load custom-file)))

;; Terminal
(if (not window-system)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (menu-bar-mode -1)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;;init.el ends here
