;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

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
(setq doom-font (font-spec :family "Iosevka Comfy" :size 18))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; emms
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

(use-package! emms-state
  :config
  (eval-after-load 'emms '(emms-state-mode)))

(map! :desc "emms-play-directory"
      "<f2>" 'emms-play-directory)
(map! :desc "emms-toggle-repeat-track"
      "C-c r" 'emms-toggle-repeat-track)
(map! :desc "emms-pause"
      "C-c <f1>" 'emms-pause)
(map! :desc "emms-previous"
      "C-c ," 'emms-prevoius)
(map! :desc "emms-next"
      "C-c ." 'emms-next)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(map! :desc "swiper"
      "C-s" 'swiper-isearch)
(map! :desc "avy-goto-char-2"
      "C-'" 'avy-goto-char-2)
(map! :desc "scroll-up"
      "C-d" 'scroll-up-command)
(map! :desc "scroll-down"
      "C-u" 'scroll-up-command)
(map! :desc "treemacs-select-window"
      "M-0" 'treemacs-select-window)
(map! :desc "counsel-recentf"
      "C-x f" 'counsel-recentf)

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

(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys nil t))
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(use-package! undo-fu
  :custom
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(map! :desc "undo-fu-only-undo"
      :map 'override "C-z" 'undo-fu-only-undo)

(use-package! pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.

  (add-hook 'next-error-hook 'pulsar-pulse-line-red)
  (add-hook 'flycheck-next-error 'pulsar-pulse-line-yellow)
  (add-hook 'flycheck-previous-error 'pulsar-pulse-line-)
  (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line-red)
  (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line)
  (add-hook 'imenu-after-jump-hook 'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook 'pulsar-reveal-entry))

(map! :desc "current-line-pulse"
      "C-c l" 'pulsar-pulse-line)

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
