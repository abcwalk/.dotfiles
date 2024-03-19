;; vim: ft=query
;; extends

(function_definition
  name: (identifier) @AlabasterDefinition)
(class_definition
  name: (identifier) @AlabasterDefinition)

[
  "if"
  "elif"
  "else"
  "match"
  "case"
] @AlabasterDefinition

[
  "for"
  "while"
  "break"
  "continue"
] @AlabasterDefinition

[
  "try"
  "except"
  "except*"
  "raise"
  "finally"
] @character.special

(raise_statement
  "from" @AlabasterHashbang)
(try_statement
  (else_clause
    "else" @AlabasterHashbang))

[
  "async"
  "await"
] @Number

[
  "lambda"
] @Number

[
  "return"
  "yield"
  "assert"
  "exec"
  "global"
  "nonlocal"
  "pass"
  "with"
  "as"
  "type"
] @Number

(yield
  "from" @Number)

(future_import_statement
  "from" @Number
  "__future__" @Number)

(import_from_statement
  "from" @Number)

"import" @Number

(aliased_import
  "as" @Number)

((module . (comment) @AlabasterHashbang)
 (#match? @AlabasterHashbang "^#!/"))

; Regex from the `re` module
; (call
;   function:
;     (attribute
;       object: (identifier) @_re)
;   arguments:
;     (argument_list
;       .
;       (string
;         (string_content) @string.escape))
;   (#eq? @_re "re"))
