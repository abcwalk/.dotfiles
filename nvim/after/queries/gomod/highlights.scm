;; vim: ft=query
;; extends

[
  "require"
  "replace"
  "go"
  "toolchain"
  "exclude"
  "retract"
  "module"
] @AlabasterConstant

"=>" @operator

(comment) @comment @spell

(module_path) @string.special.url

[
  (version)
  (go_version)
] @string
