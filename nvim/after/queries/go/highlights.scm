;; vim: ft=query
;; extends

(package_clause
  (package_identifier) @keyword)

(const_declaration
  (const_spec
    name: (identifier) @AlabasterDefinition))

(function_declaration
  name: (identifier) @AlabasterDefinition)

(method_declaration
  name: (field_identifier) @AlabasterDefinition)

(type_identifier) @AlabasterDefinition

(type_spec
  name: (type_identifier) @AlabasterDefinition)

(escape_sequence) @string.escape

"return" @AlabasterConstant

"go" @AlabasterConstant

"for" @AlabasterConstant

[
  (nil)
  (iota)
] @constant.builtin

[
  "import"
  "package"
] @AlabasterConstant

[
  "else"
  "case"
  "switch"
  "if"
] @AlabasterConstant

; Builtin types
[
  "chan"
  "map"
] @AlabasterDefinition

; ((type_identifier) @type.builtin
;   (#any-of? @type.builtin
;     "any" "bool" "byte" "comparable" "complex128" "complex64" "error" "float32" "float64" "int"
;     "int16" "int32" "int64" "int8" "rune" "string" "uint" "uint16" "uint32" "uint64" "uint8"
;     "uintptr"))
;
; ; Builtin functions
; ((identifier) @function.builtin
;   (#any-of? @function.builtin
;     "append" "cap" "clear" "close" "complex" "copy" "delete" "imag" "len" "make" "max" "min" "new"
;     "panic" "print" "println" "real" "recover"))
