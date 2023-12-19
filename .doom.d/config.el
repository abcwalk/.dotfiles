;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Font

(setq doom-font (font-spec :family "Iosevka Comfy" :size 20))

(setq user-full-name "Maxim Rozhkov"
      user-mail-address "foldersjarer@gmail.com")

;;; Screen settings

;; `window-divider-mode' gives us finer control over the border between windows.
;; The native border "consumes" a pixel of the left fringe on righter-most splits
;; (in Yamamoto's emacs-mac at least), window-divider does not.
;; NOTE Only available on Emacs 25.1+
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 0
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

;;; Trash bin

(setq delete-by-moving-to-trash t)

;;; Line numbers
(setq display-line-numbers-type t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq org-directory "~/org/")

(fset 'rainbow-delimiters-mode #'ignore)

;;; Nerd icons

(use-package! nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;; Dashboard

(use-package! dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (projects )))
  (setq dashboard-display-icons-p nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-icon-type 'nerd-icons)
  (dashboard-modify-heading-icons '((recents . "nf-oct-file_text")
                                    (projects . "nf-oct-book")))
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(
          ((,(nerd-icons-sucicon "nf-seti-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/behemothbucket/.dotfiles")))
           ("󱌢" "Settings" "Show settings" (lambda (&rest _) (counsel-find-file "~/.doom.d/"))))))
  (setq dashboard-item-names '(("Recent Files:" . " Recent Files:")
                               ("Projects:" . "󰹕 Projects"))
        ))

;;; Git

;; TEST Magit credentials again

;; disable conflicts window
(setq magit-ediff-dwim-show-on-hunks t)

;;; org

;; Option 1: Per buffer
;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Option 2: Globally
;; (with-eval-after-load 'org (global-org-modern-mode) #'set-frame-borders-and-window-dividers)

(setq
 org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
 )

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tab-bar

;; (when (< 26 emacs-major-version)
;;   (tab-bar-mode 1)                           ;; enable tab bar
;;   (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
;;   (setq tab-bar-new-tab-choice "*doom*"))    ;; buffer to show in new tabs

;; (map! :desc "tab-bar-prevoius-tab"
;;       "s-," 'tab-bar-switch-to-prev-tab)

;; (map! :desc "tab-bar-next-tab"
;;       "s-." 'tab-bar-switch-to-next-tab)

;; (map! :desc "tab-bar-new-tab"
;;       "s-w" 'tab-bar-new-tab)

;; (map! :desc "tab-bar-close-tab"
;;       "s-c" 'tab-bar-close-tab)

;; (defface ct/tab-bar-numbers
;;   '((t
;;      :inherit tab-bar
;;      :family "Iosevka Comfy"
;;      :weight light))
;;   "Face for tab numbers in both active and inactive tabs.")
;; (defvar ct/circle-numbers-alist
;;   '((0 . "⓪")
;;     (1 . "①")
;;     (2 . "②")
;;     (3 . "③")
;;     (4 . "④")
;;     (5 . "⑤")
;;     (6 . "⑥")
;;     (7 . "⑦")
;;     (8 . "⑧")
;;     (9 . "⑨"))
;;   "Alist of integers to strings of circled unicode numbers.")

;; (defun ct/tab-bar-tab-name-format-default (tab i)
;;   (let ((current-p (eq (car tab) 'current-tab)))
;;     (concat
;;      (propertize
;;       (when (and tab-bar-tab-hints (< i 10)) (alist-get i ct/circle-numbers-alist))
;;       'face 'ct/tab-bar-numbers)
;;      " "
;;      (propertize
;;       (concat (alist-get 'name tab)
;; 	      (or (and tab-bar-close-button-show
;; 		       (not (eq tab-bar-close-button-show
;; 				(if current-p 'non-selected 'selected)))
;; 		       tab-bar-close-button)
;; 		  ""))
;;       'face (funcall tab-bar-tab-face-function tab))
;;      " ")))
;; (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default
;;       tab-bar-tab-hints t)

;; (setq tab-bar-close-button-show nil
;;       tab-bar-close-button " \x00d7 ") ;; Cross multiplication character
;; (setq tab-bar-format nil
;;       tab-bar-new-button " + ")  ;; Thicker + than the flimsy default
;; (setq tab-bar-separator nil)
;; (setq tab-bar-format
;;       '(;;tab-bar-format-history ;; forward/back buttons
;; 	tab-bar-format-tabs-groups
;; 	tab-bar-separator
;;         ;; tab-bar-format-add-tab ;; new tab button
;; 	tab-bar-format-align-right
;; 	tab-bar-format-global))

;; (defun ct/modus-themes-tab-bar-colors ()
;;   "Override `modus-themes-tab-*' to have even less variety"
;;   (let* ((bg-color (modus-themes-color 'bg-main))
;;          ;; Additional padding between tabs
;;          (box `(:line-width
;;                 (2 . -1)  ;; -1 for no vertical space
;;                 :color ,bg-color :style flat-button))
;;          (active-accent-color (modus-themes-color 'blue-active)))
;;     (set-face-attribute 'tab-bar nil
;;                         :height 0.8)
;;     (set-face-attribute 'modus-themes-tab-backdrop nil
;;                         :background bg-color
;;                         :box nil)
;;     (set-face-attribute 'modus-themes-tab-inactive nil
;;                         :background bg-color
;;                         :box box)
;;     (set-face-attribute 'modus-themes-tab-active nil
;;                         :background bg-color
;;                         :underline `(:color ,active-accent-color :style line)
;;                         :box box)))

;; (add-hook! 'modus-themes-after-load-theme-hook #'ct/modus-themes-tab-bar-colors)

;;; mode-line

(load! "lisp/custom-mode-line")

;;; Emms

(use-package emms
  :init
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-source-file-default-directory "~/Music")
  :config
  (emms-mode-line-disable))

(defun track-title-from-file-name (file)
  "For using with EMMS description functions.
      Extracts the track
      title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
  (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
      (search-forward-regexp (rx "." (+ alnum) eol))
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun my-emms-track-description (track)
  "Return a description of TRACK, for EMMS.
But try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
           (concat (format "%s" artist) " - " (format "%s" title)))
          (title title)
          ((eq (emms-track-type track) 'file)
           (track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))

(setq emms-track-description-function 'my-emms-track-description)

(use-package! emms-state
  :config
  (eval-after-load 'emms '(emms-state-mode)))

(map! :desc "emms-play-directory"
      "<f5>" 'emms-play-directory)
(map! :desc "emms-toggle-repeat-track"
      "C-c r" 'emms-toggle-repeat-track)
(map! :desc "emms-pause"
      "C-c <f1>" 'emms-pause)
(map! :desc "emms-previous"
      "C-c ," 'emms-previous)
(map! :desc "emms-next"
      "C-c ." 'emms-next)
(map! :desc "emms-minus"
      "C-c <f2>" 'emms-volume-mode-minus)
(map! :desc "emms-plus"
      "C-c <f3>" 'emms-volume-mode-plus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Kill Unsaved Emacs Buffers UX: Replacing Yes/No/Save with Meaningful Options

(defun ct/kill-buffer--possibly-save--advice (original-function buffer &rest args)
  "Ask user in the minibuffer whether to save before killing.

Replaces `kill-buffer--possibly-save' as advice, so
ORIGINAL-FUNCTION is unused and never delegated to. Its first
parameter is the buffer, which is the `car' or ARGS."
  (let ((response
         (car
          (read-multiple-choice
           (format "Buffer %s modified."
                   (buffer-name))
           '((?s "Save and kill buffer" "save the buffer and then kill it")
             (?d "Discard and kill buffer without saving" "kill buffer without saving")
             (?c "Cancel" "Exit without doing anything"))
           nil nil (and (not use-short-answers)
                        (not (use-dialog-box-p)))))))
    (cond ((= response ?s)
           (with-current-buffer buffer (save-buffer))
           t)
          ((= response ?d)
           t)
          ((= response ?c)
           nil)
          )))

(advice-add 'kill-buffer--possibly-save :around #'ct/kill-buffer--possibly-save--advice)

;;; Use System File Open Dialog for File Actions in Emacs (Just Once!)

;; (let ((last-nonmenu-event nil)
;;       (use-dialog-box t)
;;       (use-file-dialog t))
;;   (call-interactively #'find-file))

(use-package! git-gutter-fringe
  :config
  ;; (fringe-helper-define 'git-gutter-fr:added nil " ")
  ;; (fringe-helper-define 'git-gutter-fr:deleted nil " ")
  ;; (fringe-helper-define 'git-gutter-fr:modified nil " ")
  ;; (define-fringe-bitmap 'git-gutter-fr:added
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [0]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [0]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [0]
    nil nil 'bottom)
  (defun my-modus-themes-custom-faces ()
    (modus-themes-with-colors
     (custom-set-faces
      ;; Replace green with blue if you use `modus-themes-deuteranopia'.
      `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
      `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
      `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg))))))
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces))

(defun pingvi/toggle-theme()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
	(disable-theme 'modus-operandi)
	(load-theme 'modus-vivendi))
    (progn
      (disable-theme 'modus-vivendi)
      (load-theme 'modus-operandi))))

(map! :desc "toggle-themes"
      "<f12>" #'pingvi/toggle-theme)

;; (setq doom-theme 'modus-vivendi)

(use-package! vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))

;;; Olivetti

(use-package! olivetti
  :config
  (setq-default olivetti-body-width 120)
  (add-hook 'mixed-pitch-mode-hook  (lambda () (setq-local olivetti-body-width 80))))

(map! :desc "toggle-olivetti-mode"
      "C-x z" 'olivetti-mode)

;; (use-package! auto-olivetti
;;   :custom
;;   (load-file "~/.doom.d/lisp/auto-olivetti.el")
;;   (auto-olivetti-enabled-modes '(text-mode prog-mode helpful-mode ibuffer-mode image-mode))
;;   :config
;;   (auto-olivetti-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Go

(setq exec-path (append exec-path '("~/go/bin")))
(setq lsp-gopls-server-path (expand-file-name "~/go/bin/gopls"))
(setq lsp-gopls-staticcheck t)
(setq gofmt-command "goimports")

(add-hook 'before-save-hook 'gofmt-before-save)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Icons

(use-package! nerd-icons
  :custom
  ;; (nerd-icons-font-family  "Iosevka Nerd Font Mono")
  ;; (nerd-icons-scale-factor 2)
  ;; (nerd-icons-default-adjust -.075)
  (doom-modeline-major-mode-icon t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Doom-dashboard

;; (add-to-list '+doom-dashboard-menu-sections
;;              '("Open diary"
;;                :icon (nerd-icons-mdicon "nf-md-notebook_heart" :face 'doom-dashboard-menu-title)
;;                :action diary))

;; (setq
;;       +doom-dashboard-ascii-banner-fn ""
;;       +doom-dashboard-banner-padding '(0 . 2))

;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Completion

;; File pats

(setq completion-at-point-functions '(elisp-completion-at-point comint-dynamic-complete-filename t))

;;; Treemacs

;; (use-package! treemacs-nerd-icons
;;   :config
;;   (treemacs-load-theme "nerd-icons"))

;; (use-package! treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (map! :desc "treemacs"
;;       "<f2>" 'treemacs)

;;; Ditaa

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

(setq org-ditaa-jar-path "~/.doom.d/misc/ditaa.jar")

;;; Company

(use-package! company
  :config
  (setq company-format-margin-function  'company-text-icons-margin))

;;; org export html

(setq org-src-fontify-natively t)

(defun my-change-style (backend)
  (when (org-export-derived-backend-p backend 'html)
    (goto-char (point-min))
    (insert "#+OPTIONS: html-postamble:nil")
    (newline)
    (insert "#+OPTIONS: org-html-head-include-default-style:nil")
    (newline)
    (insert "#+HTML_HEAD_EXTRA: <style>*{font-family: Iosevka Comfy !important; font-size: 24px;}</s\
tyle>")
    (newline)
    ))
(add-hook 'org-export-before-parsing-functions #'my-change-style)

;;; Info colors
;;;

(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Improve completions
(after! lsp-mode
  (setq +lsp-company-backends
        '(:separate company-capf company-yasnippet company-dabbrev)))

(after! company
  (setq company-prefix-length 1))

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; Scrolling

(pixel-scroll-precision-mode)

(defun filter-mwheel-always-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (apply orig args)
    (setq mwheel-coalesce-scroll-events t)))

(defun filter-mwheel-never-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
non-coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (setq mwheel-coalesce-scroll-events nil)
    (apply orig args)))

;; Don't coalesce for high precision scrolling
(advice-add 'pixel-scroll-precision :around #'filter-mwheel-never-coalesce)

;; Coalesce for default scrolling (which is still used for horizontal scrolling)
;; and text scaling (bound to ctrl + mouse wheel by default).
(advice-add 'mwheel-scroll          :around #'filter-mwheel-always-coalesce)
(advice-add 'mouse-wheel-text-scale :around #'filter-mwheel-always-coalesce)

;; Horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
;; Reversed/Natural scrolling
(setq mouse-wheel-flip-direction t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use smaller step for text scaling
(setq text-scale-mode-step 1.05)

(map! :desc "save-buffer"
      "C-s" 'save-buffer)
(map! :desc "swiper"
      "C-f" 'swiper-isearch)
(map! :desc "avy-goto-char-2"
      :map 'override  "C-\\" 'avy-goto-char-2)
(map! :desc "treemacs-select-window"
      "M-o" 'treemacs-select-window)
(map! :desc "counsel-recentf"
      "C-x f" 'counsel-recentf)
(map! :desc "dired"
      "C-x j" 'dired-jump)
(map! :desc "exit-emacs"
      "s-x" 'save-buffers-kill-emacs)
(map! :desc "dashboard"
      "s-d" 'dashboard-open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dired

(with-eval-after-load 'dired (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Selection

(use-package! selection-highlight-mode
  :config (selection-highlight-mode))

;;; Editorconfig

(use-package! editorconfig
  :config
  (editorconfig-mode 1))

;;; Python

(use-package! python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

(setq lsp-enable-file-watchers nil)

;;; Spell check

(flyspell-mode -1)
;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)
;; (add-hook 'java-mode-hook 'flyspell-prog-mode)
(remove-hook 'markdown-mode-hook 'flyspell-mode)
(remove-hook 'org-mode-hook 'flyspell-mode)
(setq ispell-program-name "hunspell")


(map! :desc "Flyspell mode"
      "<f8>" 'flyspell-mode)

(setq company-ispell-available nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org-superstar

;; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(map! :desc "move-line-up"
      "<M-up>" #'move-line-up)
(map! :desc "move-line-down"
      "<M-down>" #'move-line-down)

(use-package! recentf
  :custom
  (setq recentf-max-saved-items 40)
  (setq recentf-max-menu-items 10)
  (setq recentf-show-file-shortcuts-flag nil)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package! evil-nerd-commenter
  :init
  (evilnc-default-hotkeys nil t))
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(use-package! undo-fu
  :custom
  (global-unset-key (kbd "C-z")))

(map! :desc "undo-fu-only-undo"
      :map 'override "C-z" 'undo-fu-only-undo)

(map! :desc "undo-fu-only-redo"
      :map 'override "C-S-z" 'undo-fu-only-redo)

(use-package! pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  (add-hook 'next-error-hook 'pulsar-pulse-line-red)
  (add-hook 'flycheck-next-error 'pulsar-pulse-line-yellow)
  (add-hook 'flycheck-previous-error 'pulsar-pulse-line-)
  (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line-red)
  (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line)
  (add-hook 'imenu-after-jump-hook 'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook 'pulsar-reveal-entry))

(map! :desc "current-line-pulse"
      "C-c l" 'pulsar-pulse-line)

(set-popup-rule! "*doom:vterm-popup:*" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

(use-package! multi-compile
  :config
  (setq multi-compile-alist '(
			      (go-mode . (
					  ("go-build" "go build -v"
					   (locate-dominating-file buffer-file-name ".git"))
					  ("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
					   (multi-compile-locate-file-dir ".git"))
					  ("go-build-test-and-run" "go build -v && go test -v && go vet && eval ./${PWD##*/}"
					   (multi-compile-locate-file-dir ".git")))))))
