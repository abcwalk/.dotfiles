;;; early-init.el --- -*- lexical-binding: t -*-
;;  Author: Maksim Rozhkov
;;; Commentary:
;;  This is early configuration
;;; Code:

;; Disable package.el in favor of straight.el
(defvar default-file-name-handler-alist file-name-handler-alist)

;; The default is 800 kilobytes.  Measured in bytes.
;; (defvar extended-gc-cons-threshold most-positive-fixnum)
;; (defvar default-gc-cons-threshold (* 100 1024 1024))

;; Native Compilation Vars
(setq-default native-comp-speed 2
              native-comp-deferred-compilation t)

;; Prevents libgccjit error
;; Solution found at: https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;; (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/11:/opt/homebrew/opt/libgccjit/lib/gcc/11:/opt/homebrew/opt/gcc/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0:/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.2.0")

;; (setq-default auto-window-vscroll nil
;;               bidi-display-reordering 'left-to-right
;;               bidi-paragraph-direction 'left-to-right
;;               frame-inhibit-implied-resize t
;;               inhibit-default-init t
;;               site-run-file nil
;;               load-prefer-newer t
;;               read-process-output-max (* 1024 1024 3))

;; (setq file-name-handler-alist nil
;;       package-enable-at-startup nil
;;       gc-cons-threshold extended-gc-cons-threshold)

;; (defun arco/return-gc-to-default ()
;;   (setq-default gc-cons-threshold default-gc-cons-threshold
;;                 load-prefer-newer nil))

;; (defun arco/reset-file-handler-alist-h ()
;;   (dolist (handler file-name-handler-alist)
;;     (add-to-list 'default-file-name-handler-alist handler))
;;   (setq file-name-handler-alist default-file-name-handler-alist))

;; (add-hook 'after-init-hook #'arco/reset-file-handler-alist-h)
;; (add-hook 'after-init-hook #'arco/return-gc-to-default)
;; (advice-add 'package--ensure-init-file :override #'ignore)

;; cl deprecated
(setq byte-compile-warnings '(cl-functions))

;; Emacs 29, check the definition right below

(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	(and (minibuffer-window-active-p (minibuffer-window))
	     (with-selected-window (minibuffer-window)
	       (eq window (minibuffer-selected-window)))))))

(mode-line-window-selected-p)

(defun string-pixel-width (string)
  "Return the width of STRING in pixels."
  (with-temp-buffer
    (insert string)
    (save-window-excursion
      (let ((dedicated (window-dedicated-p)))
        ;; Avoid errors if the selected window is a dedicated one,
        ;; and they just want to insert a document into it.
        (unwind-protect
            (progn
              (when dedicated
                (set-window-dedicated-p nil nil))
              (set-window-buffer nil (current-buffer))
              (car (window-text-pixel-size
                    nil (line-beginning-position) (point))))
          (when dedicated
            (set-window-dedicated-p nil dedicated)))))))

(provide 'early-init)
;;; early-init.el ends here
