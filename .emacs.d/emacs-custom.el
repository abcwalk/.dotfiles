;;; init.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gandalf))
 '(custom-safe-themes
   '("e613c2ffe0d6c9463d67f37275566ab3c47bdd70114fc3387738a4eb292ea156" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "53b9f04f5b80c9d503078c2df432c694709669d6e3c4b0986591a61162063923" "66dc416fae79e0be8d5ec3763995db3a0a956d04403e14836941a2658a423d1f" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577" "85071f5fbcdece09bc805d67c01e84988876bac2777f88480431473a68f1ef3d" "26a8f9baf6a7887110c25e0bc5aff033e54555251bd80233ad99e33b32e5f3f6" "15b9d72e21010989679a934f525d3cf2e36844610e8df3a19f91e936f2ecafe4" "49dd5cee4d3ce3373fbf2420bcdded7e41b416573be9d1297686251708c3bc68" "0e1ad26b1bd1a2b26132c3b0a50979e5b0f03333a93b587d4e551aa71f301156" default))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(git-gutter:visual-line t)
 '(git-gutter:window-width 1)
 '(package-selected-packages
   '(dap-mode git-gutter vimrc-mode yaml-mode xclip use-package undo-fu-session undo-fu org-bullets orderless minions magit lua-mode lsp-ui lsp-pyright lsp-java json-mode ivy-prescient hl-todo gruber-darker-theme git-gutter-fringe gcmh format-all flycheck evil-nerd-commenter dashboard counsel company-prescient company-box all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(mode-line ((t (:background "#d7d7d7" :foreground "#0a0a0a" :box "#505050"))))
 `(mode-line-active ((t (:background "#d7d7d7" :foreground "#0a0a0a" :box "#505050"))))
 `(mode-line-inactive ((t (:background "#efefef" :foreground "#404148" :box "#505050"))))
 `(mode-line-emphasis ((t (:background "#d7d7d7" :foreground "#5c2092" :weight bold))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-highlight ((t (:inherit highlight))))
 `(cursor ((t(:background "black"))))
 `(company-tooltip ((t (:background "#505050" :foreground "#f0f0f0"))))
 `(company-tooltip-deprecated ((t (:strike-through t))))
 `(company-preview ((t (:background "#f8f8f8" :foreground "#282828"))))
 `(company-preview-common ((t (:foreground "#2544bb"))))
 `(company-preview-search ((t (:background "#f8ddea" :foreground "#61284f"))))
 `(company-echo-common ((t (:foreground "#5317ac"))))
 `(company-box-scrollbar ((t (:foreground "#282828" :background "#c0efff" :weight bold))))
 `(company-tooltip-scrollbar-thumb ((t(:background "#0a0a0a"))))
 `(company-tooltip-scrollbar-track ((t(:background "#d7d7d7"))))
 `(company-tooltip-selection ((t (:background "#c0efff" :foreground "#282828" :weight bold))))
 `(company-tooltip-common ((t(:foreground "#2544bb" :weight bold))))
 `(company-tooltip-common-selection ((t(:foreground "white" :background "blue"))))
 `(company-tooltip-mouse ((t (:foreground "#000000" :background "#77baff"))))
 `(company-template-field ((t (:foreground "#000000" :background "#d5baff"))))
 `(company-tooltip-annotation ((t (:foreground "#093060" :slant italic))))
 `(company-tooltip-search ((t (:foreground "#004850" :background "#8eecf4" :weight bold))))
 `(company-tooltip-search-selection ((t (:foreground "#000000" :background "#5ada88" :weight bold))))
 `(company-deprecated ((t (:strike-through t))))
 `(company-tooltip-quick-access ((t (:foreground "#093060" :slant italic))))
 `(company-tooltip-quick-access-selection ((t (:foreground "#000000" :weight bold))))
 `(company-tooltip-annotation-selection((t (:foreground "#000000" :weight bold)))))

(provide 'emacs-custom)
;;; emacs-custom.el ends here
