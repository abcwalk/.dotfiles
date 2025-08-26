;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:
;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:7897")          ; HTTP/HTTPS proxy
;; (setq centaur-socks-proxy "127.0.0.1:7897")    ; SOCKS proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
;;(setq centaur-icon t)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'iscas)         ; Package repo: melpa, bfsu, iscas, netease, sjtu, tencent, tuna or ustc
(setq centaur-theme 'doom-tomorrow-night)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
;; (setq centaur-completion-style 'minibuffer)      Completion display style: minibuffer or childframe
(setq centaur-frame-maximized-on-startup t)    ; Maximize frame on startup or not: t or nil
;; (setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
(setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
(setq centaur-tree-sitter t)                 ; Enable tree-sitter or not: t or nil Only available in 29+.
;; (setq centaur-chinese-calendar t)              ; Support Chinese calendar or not: t or nil
;; (setq centaur-player t)                        ; Enable players or not: t or nil
(setq centaur-prettify-symbols-alist '())      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("JetBrainsMono Nerd Font Mono")
             when (font-available-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 160)
                                                      (sys/win32p 110)
                                                      (t 160))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("SF Mono" "Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-available-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-available-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    ;; (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
    ;;                        "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                   (setq face-font-rescale-alist `((,font . 1.3)))
    ;;                   (set-fontset-font t 'han (font-spec :family font))))
    ))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)
;; (setq package-check-signature nil)

;; Enable proxy
;; (enable-http-proxy)
;; (enable-socks-proxy)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-pinyin add-node-modules-path aggressive-indent anzu atomic-chrome avy-zap
                beginend browse-at-remote browse-kill-ring cape cask-mode ccls
                cmake-mode coffee-mode colorful-mode consult-flyspell
                consult-lsp consult-yasnippet corfu csv-mode cue-sheet-mode dape
                dart-mode dashboard default-text-scale devdocs diff-hl diminish
                dired-git-info dired-quick-sort dired-rsync diredfl disk-usage
                docker dockerfile-mode doom-modeline doom-themes drag-stuff
                easy-kill eat eldoc-box elfeed elixir-mode embark-consult
                esh-help eshell-prompt-extras eshell-z exec-path-from-shell
                fanyi fish-mode flymake-popon gcmh git-messenger git-modes
                git-timemachine gnu-elpa-keyring-update go-dlv go-fill-struct
                go-gen-test go-impl go-tag goggles gotest goto-chg gptel-magit
                grip-mode gt haml-mode helpful hide-mode-line highlight-defined
                hungry-delete ialign ibuffer-project iedit inf-ruby link-hint
                list-environment lsp-java lsp-julia lsp-pyright lsp-sourcekit
                lsp-ui lua-mode macrostep magit-todos marginalia markdown-toc
                memory-usage mermaid-mode minions mixed-pitch mwim
                nerd-icons-completion nerd-icons-corfu nerd-icons-dired
                nerd-icons-ibuffer nov ob-go ob-mermaid ob-powershell ob-rust
                olivetti orderless org-fragtog org-mime org-modern org-pomodoro
                org-preview-html org-rich-yank org-roam-ui org-tree-slide ox-gfm
                page-break-lines pdf-tools persistent-scratch php-mode pomidor
                popper powershell pretty-hydra protobuf-mode quickrun
                rainbow-delimiters region-occurrences-highlighter restclient
                reveal-in-folder rg ron-mode rspec-mode ruby-refactor rust-mode
                scala-mode scss-mode smart-region solaire-mode sudo-edit
                swift-mode symbol-overlay tabspaces toc-org transient-posframe
                transwin treemacs-magit treemacs-nerd-icons treemacs-tab-bar
                treesit-auto ultra-scroll v-mode vertico-posframe vimrc-mode
                vterm-toggle vundo web-mode which-key-posframe xclip xterm-color
                yaml-mode yard-mode yari yasnippet-capf yasnippet-snippets ztree))
 '(warning-suppress-types '((lsp-mode))))

(custom-set-faces
 '(markdown-code-face ((t (:background nil)))))

;;; custom.el ends here
