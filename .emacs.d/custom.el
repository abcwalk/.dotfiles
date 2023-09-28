;;; custom.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-file "~/.emacs.d/custom.el"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t :background unspecified :box unspecified :overline "#595959")))
 '(mode-line-active ((t :inherit mode-line :box unspecified)))
 '(mode-line-inactive ((t :background unspecified :foreground "#595959" :box unspecified :overline "#595959"))))

(provide 'custom)
;;; custom.el ends here
