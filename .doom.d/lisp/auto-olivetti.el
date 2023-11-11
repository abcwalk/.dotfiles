;;; auto-olivetti.el --- Automatically enable olivetti-mode in wide windows -*- lexical-binding: t -*-

;; Copyright (C) 2023 Ashton Wiersdorf

;; Author: Ashton Wiersdorf <mail@wiersdorf.dev>
;; Created: 2023
;; Version: 1.0.0-rc
;; Package-Requires: ((emacs "24.3") (olivetti "2.0.0"))
;; SPDX-License-Identifier: MIT
;; Homepage: https://sr.ht/~ashton314/auto-olivetti
;; Keywords: frames, wp

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the MIT license.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; Olivetti is a minor mode that adjusts the window margins to center the text
;; for a more pleasant writing experience. This package makes `olivetti-mode'
;; turn on automatically when the window goes beyond a particular width.

;;; Code:

(require 'olivetti)

(defgroup auto-olivetti nil
  "Automatically enable `olivetti-mode' in wide windows."
  :link '(url-link :tag "Homepage" "https://sr.ht/~ashton314/auto-olivetti")
  :group 'text
  :prefix "auto-olivetti-")

(defcustom auto-olivetti-enabled-modes '(text-mode)
  "Modes for which `olivetti-mode' should automatically be enabled for."
  :type '(repeat symbol))

(defcustom auto-olivetti-threshold-fraction 1.3
  "Fraction of `olivetti-body-width' at which to enable `olivetti-mode'."
  :type 'float)

(defcustom auto-olivetti-threshold-absolute 150
  "Number of columns at which to enable `olivetti-mode'."
  :type 'natnum)

(defcustom auto-olivetti-threshold-method 'fraction
  "How to determine if the activation threshold has been met.
- fraction: use `auto-olivetti-threshold-fraction' * `olivetti-body-width'
- absolute: use `auto-olivetti-threshold-absolute'"
  :type '(choice (const fraction) (const absolute)))

(defvar-local auto-olivetti--vlm-active nil
  "Old value of `visual-line-mode' in current buffer.")

(defun auto-olivetti--do-change ()
  "Turn on or off `olivetti-mode' depending on the current window configuration."
  (setq-local auto-olivetti--vlm-active (or olivetti--visual-line-mode
                                            (and (not olivetti-mode) visual-line-mode)))
  (if (and (bound-and-true-p auto-olivetti-mode)                  ; mode enabled?
           (apply #'derived-mode-p auto-olivetti-enabled-modes)   ; in correct major-mode
           (> (window-total-width)                                ; window big enough?
              (if (eq auto-olivetti-threshold-method 'fraction)
                  (* (or olivetti-body-width 80) auto-olivetti-threshold-fraction)
                auto-olivetti-threshold-absolute)))
      (olivetti-mode +1)
    (when olivetti-mode
      (olivetti-mode -1)
      (when (bound-and-true-p auto-olivetti--vlm-active)
        (visual-line-mode)))))

;;;###autoload
(define-minor-mode auto-olivetti-mode
  "Automatically enable `olivetti-mode' in wide windows."
  :global t :group 'auto-olivetti
  (if auto-olivetti-mode
      (add-hook 'window-configuration-change-hook 'auto-olivetti--do-change)
    (prog2
        (remove-hook 'window-configuration-change-hook 'auto-olivetti--do-change)
        (olivetti-mode -1)
      (when (bound-and-true-p auto-olivetti--vlm-active)
        (visual-line-mode)))))

(provide 'auto-olivetti)
;;; auto-olivetti.el ends here
