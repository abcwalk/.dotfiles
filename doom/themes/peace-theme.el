;; package --- peace -*- lexical-binding: t; -*-

;;; Commentary:

;; A dark Emacs theme with warm and peaceful colors.

;;; Code:

(deftheme peace
  "Created 2022-08-21.")

(let
    ((peace-white   "cornsilk2")
     (peace-red     "IndianRed3")
     (peace-green   "light green")
     (peace-yellow  "khaki1")
     (peace-blue    "DodgerBlue2")
     (peace-magenta "plum")
     (peace-purple  "MediumPurple1")
     (peace-cyan    "DarkSlateGray1")
     (peace-steel   "cornsilk4")
     (peace-gray    "gray18")
     (peace-black   "gray9")
     (peace-orange  "DarkOrange2"))

  (custom-theme-set-faces
   'peace
   `(cursor
     ((t (:background ,peace-white))))
   '(fixed-pitch
     ((t (:family "JetBrainsMono Nerd Font"
          :height 170))))
   `(variable-pitch
     ((t (:family "JetBrainsMono Nerd Font"
          :height 180))))
   `(escape-glyph
     ((t (:foreground ,peace-blue))))
   `(homoglyph
     ((t (:foreground ,peace-blue))))
   `(minibuffer-prompt
     ((t (:foreground ,peace-blue
          :weight bold))))
   `(highlight
     ((t (:background ,peace-gray
          :foreground ,peace-white))))
   `(region
     ((t (:extend t
          :background ,peace-gray))))
   `(shadow
     ((t (:foreground ,peace-steel))))
   `(line-number
     ((t (:foreground ,peace-steel))))
   `(line-number-current-line
     ((t (:foreground ,peace-magenta
          :weight bold))))
   `(secondary-selection ((t nil)))
   `(font-lock-builtin-face
     ((t (:foreground ,peace-purple))))
   `(font-lock-comment-face
     ((t (:foreground ,peace-steel))))
   `(font-lock-doc-face
     ((t (:foreground ,peace-steel))))
   `(font-lock-doc-markup-face
     ((t (:inherit (font-lock-constant-face)))))
   `(font-lock-constant-face
     ((t (:foreground ,peace-cyan))))
   `(font-lock-function-name-face
     ((t (:foreground ,peace-red
                      :weight bold))))
   `(font-lock-keyword-face
     ((t (:foreground ,peace-magenta
                      :weight bold))))
   `(font-lock-negation-char-face
     ((t (:foreground ,peace-yellow))))
   `(font-lock-preprocessor-face
     ((t (:foreground ,peace-orange))))
   `(font-lock-regexp-grouping-backslash
     ((t (:inherit bold
                   :foreground ,peace-yellow))))
   `(font-lock-regexp-grouping-construct
     ((t (:inherit bold
                   :foreground ,peace-yellow))))
   `(font-lock-string-face
     ((t (:foreground ,peace-yellow))))
   `(font-lock-type-face
     ((t (:foreground ,peace-green))))
   `(font-lock-variable-name-face
     ((t (:foreground ,peace-white))))
   `(font-lock-warning-face
     ((t (:foreground ,peace-purple
                      :weight bold))))
   `(mode-line
     ((t (:background ,peace-gray
                      :foreground ,peace-white))))
   `(mode-line-inactive
     ((t (:background ,peace-gray
                      :foreground ,peace-white))))
   `(mode-line-buffer-id
     ((t (:inherit mode-line
                   :foreground ,peace-white))))
   `(mode-line-highlight
     ((t (:background ,peace-steel))))
   `(fringe
     ((t (:background ,peace-black))))
   `(default
      ((t (:font "JetBrainsMono Nerd Font"
           :height 140
           :extend nil
           :stipple nil
           :background ,peace-black
           :foreground ,peace-white
           :inverse-video nil
           :box nil
           :strike-through nil
           :overline nil
           :underline nil
           :slant normal
           :weight normal
           :width normal))))
   `(link
     ((t (:foreground ,peace-blue
          :underline (:color foreground-color :style line)))))
   `(match
     ((t (:background ,peace-blue))))
   `(show-paren-match
     ((t (:background ,peace-blue))))
   `(meow-position-highlight-number
     ((t (:foreground ,peace-blue))))
   `(meow-normal-indicator
     ((t (:foreground ,peace-steel
                      :weight bold))))
   `(meow-insert-indicator
     ((t (:foreground ,peace-white
                      :weight bold))))
   `(meow-motion-indicator
     ((t (:foreground ,peace-red
                      :weight bold))))
   `(meow-keypad-indicator
     ((t (:foreground ,peace-cyan
                      :weight bold))))
   `(meow-beacon-indicator
     ((t (:foreground ,peace-magenta
                      :weight bold))))
   `(ivy-current-match
     ((t (:background ,peace-blue
                      :foreground ,peace-white))))
   `(isearch
     ((t (:background ,peace-blue))))
   `(isearch-fail
     ((t (:background ,peace-red))))
   `(success
     ((t (:foreground ,peace-green
                      :weight bold))))
   `(warning
     ((t (:foreground ,peace-orange
                      :weight bold))))
   `(error
     ((t (:foreground ,peace-red
                      :weight bold))))))

(provide-theme 'peace)

;;; peace-theme.el ends here
