;; Thank you Prot

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :height 130    ; Размер: 13pt (130 = 13.0)
                    :weight 'normal)

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***"
                            "|||" "||>" "<||" "<==" "<=~" "<~>" "<~~" "<~"
                            "<=" "<:" "<|" "<" "==" "=>"
                            "=/=" ">=>" ">-" ">=" ">>" "-}" "-->" "---" "-~"
                            "#{" "#[" "#:" "#=" "##" "::" ":#" ":>" ":<"
                            "$>" "+++" "->" "-<" "-<<" "-<+" "-->" "---" "-~~"
                            "!!" "!=" "!==" "!#" "!<" "!>" "!~" "!!!"
                            "&&" "&&&" "&&>" "&&<" "&>" "&<" "&&"
                            "*>" "*>=" "*<" "*<=" "*=" "***" "*/"
                            "\\\\" "||" "||>" "|||" "|||>" "||<" "|||<"
                            "{}" "{|" "|}" "|]" "|>" "|-" "|=" "||-" "|=="
                            "=>" "===" "!!!" "$$$" "+++" "..." ".*" "::"))
  ;; Включить глобально
  (global-ligature-mode t))

;;; Set up the package manager
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Tweak the looks of Emacs
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(save-place-mode 1)

(electric-pair-mode 1)
;; (setq electric-pair-pairs
;;       '(
;;         (?\" . ?\")
;;         (?\( . ?\))
;;         (?\[ . ?\])
;;         (?\{ . ?\})
;;         (?' . ')))   ; ← включить одинарные кавычки

(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Recentf
(recentf-mode 1)
(setq recentf-max-saved-items 15)

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; Configure the minibuffer and completions
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package consult
  :ensure t
  :bind (("M-e" . consult-recent-file)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic orderless))
     (buffer (styles basic orderless))
     (command (styles basic orderless))
     (variable (styles basic orderless)))))

(use-package embark
  :ensure t
  :bind (("C-," . embark-act)   ; C-; для действий
         ("C-c C-;" . embark-dwim)) ; "сделай то, что я имею в виду"
  :init
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; The file manager (Dired)
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   (dired-mode . (lambda ()
                   (define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
                   (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
	("<tab>" . dired-subtree-toggle)
	("TAB" . dired-subtree-toggle)
	("<backtab>" . dired-subtree-remove)
	("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-eagle :no-confirm))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
   '(:internal-border-width 15
			    :header-line-width 4
			    :mode-line-width 6
			    :tab-width 4
			    :right-divider-width 30
			    :scroll-bar-width 8
			    :fringe-width 8))
  (spacious-padding-subtle-frame-lines nil)
  :config
  (spacious-padding-mode 1))

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :hook (after-init . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory "~/.emacs.d/undo-fu-session/")
  (undo-fu-session-max-saved 200)  ; максимум 200 шагов на файл
  (undo-fu-session-incompatible-modes '(pdf-view-mode doc-view-mode))
  :config
  (make-directory undo-fu-session-directory t))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

(use-package pulsar
  :ensure t
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055) 
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-green)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

(add-hook 'next-error-hook #'pulsar-pulse-line)
(add-hook 'kill-line #'pulsar-pulse-line-red)

;; (setq pulsar-pulse-region-functions
;;       '(
;;         ;; Базовые команды
;; 	   kill-region
;;         copy-region-as-kill
;;         yank
;;         yank-pop

;;         ;; Отмена/повтор
;;         undo
;;         undo-only  ; если используется
;;         undo-redo
	
;;         ;; Замена и трансформации
;;         replace-string
;;         query-replace
;;         query-replace-regexp

;;         ;; Операции с текстом
;;         transpose-regions
;;         rotate-yank-pointer

;; 	;; Другие возможные
;;         kill-ring-save          ; синоним copy-region-as-kill
;;         kill-line
;;         kill-word
;;         backward-kill-word
;;         ))

(let ((map global-map)) 
  (define-key map (kbd "C-x l") #'pulsar-pulse-line)
  (define-key map (kbd "C-x L") #'pulsar-highlight-line))

;; Autocompletion
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)                    ; автодополнение при вводе
  (corfu-auto-delay 0.0)            ; без задержки
  (corfu-preview-current t)         ; подсветка текущего элемента
  (corfu-min-width 80)              ; ширина меню
  (corfu-echo-documentation t)      ; показывать документацию в echo area
  :config
  ;; Включить corfu в minibuffer
  (setq read-extended-command-completion-mode t))

;; (use-package corfu-doc
;;   :ensure t
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :custom
;;   (corfu-doc-delay 0.3)
;;   (corfu-doc-max-width 60)
;;   (corfu-doc-max-height 15))

(use-package corfu-terminal
  :ensure t)

;; LSP
;; === Tree-sitter для Python ===
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; === LSP Mode (база) ===
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-inhibit-message t)
  (setq lsp-echo-disabled t)
  (setq lsp-format-on-save nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-completion-provider :capf)
  :config
  (lsp-enable-which-key-integration t))

;; === lsp-pyright — для Python ===
(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred)))
  :custom
  (lsp-pyright-python-executable-cmd "python")
  (lsp-pyright-linting "ruff")           ; использовать ruff как линтер
  (lsp-pyright-typechecking-mode "basic") ; или "strict"
  (lsp-pyright-formatting-provider "ruff") ; форматировать через ruff
  ;; (lsp-pyright-organize-imports-provider "ruff") ;
  )

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-lens-enable t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t))

(defun my-python-before-save ()
  "Форматировать и организовать импорты перед сохранением."
  (when (derived-mode-p 'python-ts-mode)
    ;; 1. Организовать импорты (ruff + isort)
    (lsp-organize-imports)
    ;; 2. Форматировать (ruff format)
    (lsp-format-buffer)))

(remove-hook 'before-save-hook 'my-python-format-on-save)
(add-hook 'before-save-hook 'my-python-before-save)

