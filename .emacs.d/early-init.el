;;; early-init.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is early configuration
;;; Code:

;; Disable package.el in favor of straight.el
(defvar default-file-name-handler-alist file-name-handler-alist)

;; The default is 800 kilobytes.  Measured in bytes.
(defvar extended-gc-cons-threshold most-positive-fixnum)
(defvar default-gc-cons-threshold (* 100 1024 1024))

;; Native Compilation Vars
(setq-default native-comp-speed 2
              native-comp-deferred-compilation t)

;; Prevents libgccjit error
;; Solution found at: https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;; (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/11:/opt/homebrew/opt/libgccjit/lib/gcc/11:/opt/homebrew/opt/gcc/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0:/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.2.0")

(setq-default auto-window-vscroll nil
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              frame-inhibit-implied-resize t
              inhibit-default-init t
              site-run-file nil
              load-prefer-newer t
              read-process-output-max (* 1024 1024 3))

(setq file-name-handler-alist nil
      package-enable-at-startup nil
      gc-cons-threshold extended-gc-cons-threshold)

(defun arco/return-gc-to-default ()
  (setq-default gc-cons-threshold default-gc-cons-threshold
                load-prefer-newer nil))

(defun arco/reset-file-handler-alist-h ()
  (dolist (handler file-name-handler-alist)
    (add-to-list 'default-file-name-handler-alist handler))
  (setq file-name-handler-alist default-file-name-handler-alist))

(add-hook 'after-init-hook #'arco/reset-file-handler-alist-h)
(add-hook 'after-init-hook #'arco/return-gc-to-default)
(advice-add #'package--ensure-init-file :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
