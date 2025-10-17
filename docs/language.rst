############
Typed Remora
############
=======
Grammar
=======

----------------------
Declarations
----------------------
.. productionlist::
   decl : "(" "def" `id` ("(" type_param* ")")? ("[" index_param* "]")? "(" ("(" `id` ":" `type` ")")* ")" `exp` ")"

-------------------------------
Indices, Shapes, and Dimensions
-------------------------------

.. productionlist::
   dim : `id`
     : | `integer_lit`
     : | "(" "+" `dim`+ ")"
   shape : `id`
       : | "[" `dim`* "]"
       : | "(" "++" `shape`+ ")"
   shape_lit : "[" `integer_lit`* "]"
   index_param : `id` | "$" `id`
   index_app : "@" "[" `shape`* "]"
..   index_app : "$" `shape`*

-----
Types
-----

.. productionlist::
   type : `id`
      : | `base_type`
      : | "(" "A" `type` `shape` ")"
      : | "(" "->" `type` `type` ")"
      : | "(" "->" "(" `type`+ ")" `type` ")"
      : | "(" "forall" "(" type_param* ")" `type`)
      : | "(" "prod" "[" index_param* "]" `type`)
      : | "(" "exists" "[" `index_param`* "]" `type` ")"
   base_type : `Int` | `Bool` | `Float`
   type_param : `id` | "@" `id`
   type_app  : "@" "(" `type`* ")"
..   type_app  : "@" `type`

--------------------------------
Patterns, Atoms, and Expressions
--------------------------------

.. productionlist::
   pat : `id` | "_"
   atom : `base`
      : | `op`
      : | "(" "fn" ("(" type_param* ")")? ("[" index_param* "]")? "(" ("(" `id` ":" `type` ")")* ")" `exp` ")"
      : | "(" "box" `shape`* `exp` `type` ")"
   exp : `id`
     : | "(" "array" `shape_lit` `atom`+ ")"
     : | "(" "array" `shape_lit` `type` ")"
     : | "(" "frame" `shape_lit` `exp`+ ")"
     : | "(" "frame" `shape_lit` `type` ")"
     : | "[" `atom`+ "]"
     : | "[" `exp`+ "]"
     : | "(" `exp` `type_app`? `index_app`? `exp`* ")"
     : | "(" "unbox" "(" `pat`+ `exp` ")" `exp` ")"
     : | "(" "let" "(" `pat`+ ")" `exp` ")"
   op : "+" | "-" | "iota" | ...

..      : | "(" "t-fn" "(" type_param* ")" `exp` ")"
..      : | "(" "i-fn" "[" index_param* "]" `exp` ")"


-----------
Identifiers
-----------
.. productionlist::
   id :
