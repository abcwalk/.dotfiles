;; vim: ft=query
;; extends

(assignment_statement
  (variable_list
    name: (dot_index_expression
            field: (identifier) @AlabasterDefinition))
  (expression_list
    value: (function_definition)))

(assignment_statement
  (variable_list
    name: (identifier) @AlabasterDefinition)
  (expression_list
    value: (function_definition)))

(function_declaration
  name:
    [
      (identifier) @function
      (dot_index_expression
        field: (identifier) @function)
    ])

(function_declaration
  name:
    (method_index_expression
      method: (identifier) @function.method))

(field
  name: (identifier) @keyword)

(hash_bang_line) @AlabasterHashbang

"return" @AlabasterConstant

[
  "goto"
  "in"
  "local"
] @AlabasterDefinition

(break_statement) @AlabasterConstant

(do_statement
  [
    "do"
    "end"
  ] @AlabasterConstant)

(while_statement
  [
    "while"
    "do"
    "end"
  ] @AlabasterConstant)

(repeat_statement
  [
    "repeat"
    "until"
  ] @AlabasterConstant)

(if_statement
  [
    "if"
    "elseif"
    "else"
    "then"
    "end"
  ] @AlabasterConstant)

(elseif_statement
  [
    "elseif"
    "then"
    "end"
  ] @AlabasterConstant)

(else_statement
  [
    "else"
    "end"
  ] @AlabasterConstant)

(for_statement
  [
    "for"
    "do"
    "end"
  ] @AlabasterConstant)

(function_declaration
  [
    "function"
    "end"
  ] @AlabasterDefinition)

(function_definition
  [
    "function"
    "end"
  ] @AlabasterDefinition)

; Operators
[
  "and"
  "not"
  "or"
] @AlabasterConstant

