;;; init.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max (* 10 1024 1024)
      bidi-inhibit-bpa t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
