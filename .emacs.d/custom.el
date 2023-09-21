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
<<<<<<< HEAD
 '(custom-enabled-themes '(gandalf))
 '(custom-safe-themes
   '("1353963c20b4fc37e84385a6ba1c637bd26ed8752d9d5b57b569fce81bda7854" default)))
=======
 '(company-tooltip-limit 7)
 '(company-tooltip-maximum-width 60)
 '(company-tooltip-minimum-width 60)
 '(custom-enabled-themes '(gandalf))
 '(custom-file "/home/nonh/.emacs.d/custom.el")
 '(custom-safe-themes
   '("ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "e613c2ffe0d6c9463d67f37275566ab3c47bdd70114fc3387738a4eb292ea156" default)))
>>>>>>> a1a4477 (updated)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
<<<<<<< HEAD
 `(mode-line ((t (:background "#d7d7d7" :foreground "#0a0a0a" :box "#505050"))))
 `(mode-line-active ((t (:background "#d7d7d7" :foreground "#0a0a0a" :box "#505050"))))
 `(mode-line-inactive ((t (:background "#efefef" :foreground "#404148" :box "#505050"))))
 `(mode-line-emphasis ((t (:background "#d7d7d7" :foreground "#5c2092" :weight bold))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-highlight ((t (:inherit highlight))))
 '(default ((t (:background "#f2f2f2" :foreground "black"))))
 '(company-tooltip ((t (:background "gainsboro" :foreground "#505050"))))
 '(cursor ((t (:background "black")))))
=======
;; '(default ((t (:background "#f2f2f2" :foreground "black"))))
'(cursor ((t (:background "black")))))
>>>>>>> a1a4477 (updated)

(provide 'custom)
;;;custom.el ends here
