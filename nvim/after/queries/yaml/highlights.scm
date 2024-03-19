;; vim: ft=query
;; extends

(block_mapping_pair
  key:
    (flow_node
      [
        (double_quote_scalar)
        (single_quote_scalar)
      ] @AlabasterDefinition))

(block_mapping_pair
  key:
    (flow_node
      (plain_scalar
        (string_scalar) @AlabasterDefinition)))

(flow_mapping
  (_
    key:
      (flow_node
        [
          (double_quote_scalar)
          (single_quote_scalar)
        ] @AlabasterDefinition)))

(flow_mapping
  (_
    key:
      (flow_node
        (plain_scalar
          (string_scalar) @AlabasterDefinition))))
