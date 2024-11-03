;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(add-to-list 'load-path "~/.dotfiles/doom/lisp")
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Maxim Rozhkov"
      user-mail-address "w79014580859@gmail.com")

;; NOTE refresh modeline git branch
;; revert-buffer (SPC b r)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; Must be used *after* the theme is loaded

;; Theme
(use-package! doom-themes
  :config
  (load-theme 'doom-zenburn t))

;; Font
(require 'battery)

(defvar font-size
  (if (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B"
                                                (funcall battery-status-function)))))
      20
    18)
  "Default font size depending on battery mode availability.")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size font-size :weight 'normal)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size (- font-size 3)))

;; doom-zenburn theme config
(custom-theme-set-faces! 'doom-zenburn
  (if (display-graphic-p)
      (progn
        (set-frame-parameter (selected-frame) 'alpha '(85 85))
        (add-to-list 'default-frame-alist '(alpha 85 85))
        (custom-theme-set-faces! 'doom-zenburn
          '(default :background "black")
          '(whitespace-tab :background "#333333")
          `(font-lock-keyword-face :foreground ,(doom-color 'yellow))
          `(font-lock-builtin-face :foreground ,(doom-color 'fg))
          `(highlight-numbers-number :foreground ,(doom-color 'fg))
          `(dired-directory :foreground ,(doom-color 'green))))
        (progn
          (custom-theme-set-faces! 'doom-zenburn
            '(default :background nil)
            '(magit-diff-context :background nil)
            '(lsp-face-highlight-read :background nil)
            '(magit-diff-context-highlight :background nil)
            '(diff-refine-added :background nil :foreground "#7F9F7F")
            '(diff-refine-removed :background nil :foreground "#CC9393")
            '(magit-diff-added :background nil :foreground "#7F9F7F")
            '(magit-diff-added-highlight :background nil :foreground "#7F9F7F" :weight bold)
            '(magit-diff-removed-highlight :background nil :foreground "#CC9393" :weight bold)
            '(magit-diff-removed :background nil :foreground "#CC9393")))))


;; Diff-hl
(use-package! diff-hl
  :init
  ;; Modus and ef fix
  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "#7ccd7c"))))
   '(diff-hl-change((t (:foreground "#3a81c3"))))
   '(diff-hl-delete ((t (:foreground "#ee6363")))))
  (unless (display-graphic-p)
    (let* ((width 2)
           (bitmap (vector (1- (expt 2 width)))))
      (define-fringe-bitmap 'my:diff-hl-bitmap bitmap 1 width '(top t)))
    (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my:diff-hl-bitmap))
    (xterm-mouse-mode 1))
  ;; On-the-fly diff updates
  (diff-hl-flydiff-mode)
  ;; Makes fringe and margin react to mouse clicks to show the corresponding hunk
  (global-diff-hl-show-hunk-mouse-mode)
  ;; Enable diff-hl globally
  (global-diff-hl-mode))

(use-package! evil-goggles
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse t)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (setq evil-goggles-duration 0.300)
  (evil-goggles-use-diff-faces))

;; a b c
;; d e f
;; g h i

;; prot themes fringe fix
;; (use-package! git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :config
;;   (setq git-gutter:update-interval 0.02)
;;   (custom-set-variables
;;  '(git-gutter:modified-sign " ")
;;  '(git-gutter:added-sign " ")
;;  '(git-gutter:deleted-sign "_")))

;; Tabline
;; (global-tab-line-mode t)
;; (setq tab-line-new-button-show nil)  ;; do not show add-new button
;; (setq tab-line-close-button-show nil)  ;; do not show close button

;; (use-package! intuitive-tab-line
;;   :load-path "git/intuitive-tab-line-mode"
;;   :custom
;;   (tab-line-tabs-function 'intuitive-tab-line-buffers-list)
;;   (tab-line-switch-cycling t)
;;   :config
;;   (global-tab-line-mode 1)
;;   ;; (recentf-mode 1)
;;   (setq
;;    tab-line-new-button-show nil  ;; do not show add-new button
;;    tab-line-close-button-show nil  ;; do not show close button
;;    tab-line-separator " "  ;; delimitation between tabs
;;    ))

  ;; (global-set-key (kbd "M-[") 'tab-line-switch-to-prev-tab)
  ;; (global-set-key (kbd "M-]") 'tab-line-switch-to-next-tab)

  ;; (global-set-key (kbd "C-<iso-lefttab>") 'tab-line-switch-to-prev-tab)
  ;; (global-set-key (kbd "C-<tab>") 'tab-line-switch-to-next-tab)
  ;; (global-set-key (kbd "C-S-<prior>") 'intuitive-tab-line-shift-tab-left)
  ;; (global-set-key (kbd "C-S-<next>") 'intuitive-tab-line-shift-tab-right)
  ;; (global-set-key (kbd "C-S-t") 'recentf-open-most-recent-file)

;; Modeline
(use-package! modeline)

;;; Keycast mode
(use-package! keycast
  :after modeline
  :init
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil)))
  (keycast-mode-line-mode 1))

;; Solaire
(after! solaire-mode
  (solaire-global-mode -1))

(setq
 delete-by-moving-to-trash t
 confirm-kill-emacs nil
 ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
 ;; They're generally unhelpful and only add confusing visual clutter.
 mode-line-default-help-echo nil
 show-help-function nil
 ;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
 ;; elisp or note something down (that isn't worth an entry in my notes). I can
 ;; do both in `lisp-interaction-mode'.
 doom-scratch-initial-major-mode 'lisp-interaction-mode
 ;; Line numbers are pretty slow all around. The performance boost of disabling
 ;; them outweighs the utility of always keeping them on.
 display-line-numbers-type nil)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Fullscreen
;; (toggle-frame-maximized)
;; (add-to-list 'initial-frame-alist '(maximized . fullscreen))
(add-to-list 'initial-frame-alist '(maximized))
(add-hook 'window-setup-hook #'toggle-frame-maximized)
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Dired
(require 'dired)
(with-eval-after-load 'dired (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))

;; Keymaps
(after! evil-escape
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.5))

(global-set-key (kbd "C-j") (kbd "<down>"))
(global-set-key (kbd "C-k") (kbd "<up>"))

(map! :n "s" 'evil-avy-goto-char-2)
(map! :desc "save-buffer"
      "C-s" 'save-buffer)
;; (map! :desc "swiper"
;;       "C-f" 'swiper-isearch)
;; (map! :desc "avy-goto-char-2"
;;       :map 'override  "C-\\" 'avy-goto-char-2)
;; (map! :desc "treemacs-select-window"
;;       "M-o" 'treemacs-select-window)
;; (map! :desc "counsel-recentf"
;;       "C-x r" 'counsel-recentf)
(map! :desc "dired"
      "C-x j" 'dired-jump)
;; (map! :desc "exit-emacs"
;;       "s-x" 'save-buffers-kill-emacs)
;; (map! :desc "dashboard"
;;       "s-d" 'dashboard-open)

;; (setq doom-modeline-enable-word-count t)

;; (when (< 26 emacs-major-version)
;;   (tab-bar-mode 1)                           ;; enable tab bar
;;   (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
;;   (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;;   (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
;;   (setq tab-bar-tab-hints nil)                 ;; show tab numbers
;;   (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; (global-set-key (kbd "s-[") 'tab-bar-switch-to-prev-tab)
;; (global-set-key (kbd "s-]") 'tab-bar-switch-to-next-tab)
;; (global-set-key (kbd "s-w") 'tab-bar-close-tab)

(setq inhibit-startup-screen nil)

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

;; Recentf
;; (use-package! recentf
;;   :custom
;;   (setq recentf-max-saved-items 40)
;;   (setq recentf-max-menu-items 10)      ;
;;   (setq recentf-show-file-shortcuts-flag nil)
;;   (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100)
  :hook (after-init . recentf-mode))

(use-package init-open-recentf
      :after recentf
      :config (init-open-recentf))

;; Python
;; (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))

  (use-package! flymake-ruff
    :hook (python-mode . flymake-ruff-load))

;; (use-package! nerd-icons
;;   :custom
;;   ;; The Nerd Font you want to use in GUI
;;   ;; "Symbols Nerd Font Mono" is the default and is recommended
;;   ;; but you can use any other Nerd Font if you want
;;   (nerd-icons-font-family "JetBrainsMono Nerd Font")
;;   (doom-modeline-major-mode-icon t))

;; (use-package! olivetti
;;   :config
;;   (setq-default olivetti-body-width 120)
;;   (add-hook 'mixed-pitch-mode-hook  (lambda () (setq-local olivetti-body-width 80))))

;; (map! :desc "toggle-olivetti-mode"
;;       "C-x z" 'olivetti-mode)

;;Completion File pats (annoying))
;; (setq completion-at-point-functions '(elisp-completion-at-point comint-dynamic-complete-filename t))

;; Company
;; (use-package! company
;;   :config
;;   (setq company-format-margin-function  'company-text-icons-margin)
;;   (setq company-prefix-length 1))

;; (use-package! company
;;   :config
;;   ;; disable auto popup after x seconds
;;   (setq company-idle-delay nil
;;         ;; allow code completion inside comments and string
;;         company-dabbrev-code-everywhere t
;;         ;; press M-<digit> to select a given number
;;         company-show-numbers t
;;         ;; Go back to first item
;;         company-selection-wrap-around t
;;         ;; allow code completion matching all buffer
;;         company-dabbrev-code-other-buffers 'all
;;         company-dabbrev-other-buffers 'all)
;;   )

;; Improve completions
;; (after! lsp-mode
;;   (setq +lsp-company-backends
;;         '(:separate company-capf company-yasnippet company-dabbrev)))
;;

;;; :completion company
;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
;; (after! company
;;    (company-mode -1))

;;; :completion corfu
;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(after! corfu
  (setq corfu-auto nil))

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
;; (setq doom-modeline-modal nil)

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Implicit /g flag on evil ex substitution, because I use the default behavior
;; less often.
(setq evil-ex-substitute-global t)

;;; :tools lsp
;; Disable invasive lsp-mode features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some machines I don't care to have
        ;; a whole development environment for some ecosystems.
        lsp-enable-suggest-server-download nil
        lsp-completion-provider :none))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

;; Scrolling

(pixel-scroll-precision-mode 1)

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

(setq
 ;; Horizontal scrolling
 mouse-wheel-tilt-scroll t
 ;; Reversed/Natural scrolling
 mouse-wheel-flip-direction t
 ;; Use smaller step for text scaling
 text-scale-mode-step 1.05)

;;; Selection
;; (use-package! selection-highlight-mode
;;   :config (selection-highlight-mode))
;; (use-package! undo-fu
;;   :custom
;;   (global-unset-key (kbd "C-z")))
;; (map! :desc "undo-fu-only-undo"
;;       :map 'override "C-z" 'undo-fu-only-undo)
;; (map! :desc "undo-fu-only-redo"
;;       :map 'override "C-S-z" 'undo-fu-only-redo)

(when (display-graphic-p)
  (use-package! pulsar
    :config
    (setq pulsar-pulse t)
    (setq pulsar-delay 0.055)
    (setq pulsar-iterations 10)
    (setq pulsar-face 'pulsar-yellow)
    (setq pulsar-highlight-face 'pulsar-yellow)

    (pulsar-global-mode 1)

    (add-hook 'next-error-hook 'pulsar-pulse-line-red)
    (add-hook 'flycheck-next-error 'pulsar-pulse-line-red)
    (add-hook 'flycheck-previous-error 'pulsar-pulse-line-red)
    (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line-red)
    (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line)
    (add-hook 'imenu-after-jump-hook 'pulsar-recenter-top)
    (add-hook 'imenu-after-jump-hook 'pulsar-reveal-entry))

  (map! :desc "current-line-pulse"
        "C-c l" 'pulsar-pulse-line)
  (map! :desc "current-line-pulse"
        "C-c h" 'pulsar-highlight-line)

  (defun meain/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around 'meain/evil-yank-advice)

  ;; Spacious padding
  (setq spacious-padding-widths
        '( :internal-border-width 30
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  (spacious-padding-mode 1)
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

;;Vterm
(set-popup-rule! "*doom:vterm-popup:*" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

;; Makes *scratch* empty.
;; (setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
