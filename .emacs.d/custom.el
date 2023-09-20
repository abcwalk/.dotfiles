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
   '("1353963c20b4fc37e84385a6ba1c637bd26ed8752d9d5b57b569fce81bda7854" default)))

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
 '(default ((t (:background "#f2f2f2" :foreground "black"))))
 '(company-tooltip ((t (:background "gainsboro" :foreground "#505050"))))
 '(cursor ((t (:background "black")))))

(provide 'custom)
;;;custom.el ends here
