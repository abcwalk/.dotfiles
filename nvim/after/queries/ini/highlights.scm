;; vim: ft=query
;; extends

(section_name
  (text) @AlabasterConstant) ; consistency with toml

(comment) @comment @spell

[
  "["
  "]"
] @punctuation.bracket

"=" @operator

(setting
  (setting_name) @AlabasterDefinition)

; (setting_value) @none ; grammar does not support subtypes
