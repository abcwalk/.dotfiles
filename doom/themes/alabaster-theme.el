;;; alabaster-theme.el -- Alabaster theme for Emacs.

;; Author: Chris Etheridge (theme originally by Nikita Tonsky)
;; URL: https://github.com/chris-etheridge/alabaster-emacs
;; Package-Version: 20160525.0001
;; Version: 1.0

;;; Commentary:
;;
;; Alabaster is a theme originally created by Nikita Tonsky for Light Table.
;; Source: <https://github.com/tonsky/alabaster-lighttable-skin>

;;; Code:

(deftheme alabaster
  "Темная версия темы Alabaster.")

(let ((selection-color (if (featurep 'ns) "ns_selection_color" "#4B4B4B"))
      (highlight-color "#3E3E3E")
      (secondary-color "#5A5A5A")
      (active-color "#2E2E2E")
      (passive-color "#AAAAAA")
      (subtle-color "#3A3A3A")
      (error-color "#FF4C4C")
      (border-color "#555555"))
  (custom-theme-set-faces
   'alabaster
   ;; Basics
   '(default ((t (:background "#1C1C1C" :foreground "#FFFFFF"))))
   '(blue ((t (:foreground "#5A9BD5"))))
   '(bold ((t (:bold t))))
   '(bold-italic ((t (:italic t :bold t))))
   '(border-glyph ((t (nil))))
   '(green ((t (:foreground "#A6D96A"))))
   '(info-node ((t (:italic t :bold t))))
   '(info-xref ((t (:bold t))))
   '(italic ((t (:italic t))))
   '(left-margin ((t (nil))))
   '(pointer ((t (nil))))
   '(red ((t (:foreground "#FF4C4C"))))
   '(right-margin ((t (nil))))
   '(underline ((t (:underline t))))
   '(yellow ((t (:foreground "#FFD700"))))

   ;; Frame
   '(fringe ((t (:background "#2A2A2A"))))
   `(mode-line ((t (:background "#3A3A3A" :foreground "white"
                                :box (:line-width -1 :color ,border-color)))))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "#9599B0")))))
   `(mode-line-inactive
     ((t (:inherit mode-line :background "#2A2A2A" :foreground "grey80"
                   :box (:line-width -1 :color ,border-color) :weight light))))

   ;; Parens
   `(show-paren-match ((t (:background ,passive-color))))
   `(show-paren-mismatch ((t (:foreground "#F9F2CE" :background ,error-color))))

   ;; Highlighting
   `(hl-line ((t (:background ,active-color))))
   `(highline-face ((t (:background ,active-color))))
   `(highlight ((t (:background ,highlight-color))))
   `(highlight-symbol-face ((t (:background ,secondary-color))))
   `(isearch ((t (:background ,highlight-color))))
   `(lazy-highlight ((t (:background ,secondary-color))))
   `(primary-selection ((t (:background ,selection-color))))
   `(region ((t (:background ,selection-color))))
   `(secondary-selection ((t (:background ,secondary-color))))
   `(shadow ((t (:foreground "grey50" :background ,subtle-color))))
   `(text-cursor ((t (:background "white" :foreground ,passive-color))))
   `(zmacs-region ((t (:background ,selection-color))))

   ;; More
   '(mumamo-background-chunk-submode ((t (:background "#2A2A2A"))))

   ;; Font-lock
   '(font-lock-builtin-face ((t (:foreground "#9B59B6"))))
   '(font-lock-comment-face ((t (:foreground "#7F7F7F"))))
   '(font-lock-constant-face ((t (:foreground "#DDA0DD" :background "#3A3A3A"))))
   '(font-lock-doc-string-face ((t (:foreground "#1A93AE" :background "#2A2A2A"))))
   '(font-lock-function-name-face ((t (:foreground "#D19A66"))))
   '(font-lock-keyword-face ((t (:foreground "#C678DD"))))
   '(font-lock-preprocessor-face ((t (:foreground "#BEBEBE"))))
   '(font-lock-reference-face ((t (:foreground "#D19A66" :background "#3A3A3A"))))
   '(font-lock-string-face ((t (:foreground "#E06C75" :background "#2A2A2A"))))
   '(font-lock-type-face ((t (:foreground "#98C379"))))
   '(font-lock-variable-name-face ((t (:foreground "#E5C07B"))))
   '(font-lock-warning-face ((t (:foreground "#FF4C4C"))))

   ;; Diff Mode
   '(diff-file-header ((t (:bold t :inherit diff-header))))
   '(diff-header ((t (:background "#3A3A3A" :foreground "grey80"))))
   '(diff-added ((t (:background "#2A2A2A"))))
   '(diff-removed ((t (:background "#3A3A3A"))))
   '(diff-changed ((t (:background "#3A3A3A"))))
   '(diff-refine-change ((t (:background "#3A3A3A"))))

   ;; Magit
   '(magit-diff-file-header ((t (:bold t :inherit diff-header))))
   '(magit-diff-hunk-header ((t (:inherit diff-header))))
   '(magit-diff-add ((t (:inherit diff-added :foreground "grey80"))))
   '(magit-diff-del ((t (:inherit diff-removed :foreground "grey80"))))
   '(magit-diff-none ((t (:inherit diff-context :foreground "grey80"))))
   '(magit-item-highlight ((t (:background nil :foreground "white"))))

   ;; Done
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster)

;;; alabaster-theme.el ends here
