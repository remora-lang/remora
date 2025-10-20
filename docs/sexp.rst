####################
Remora S-expressions
####################

The ``remora parse`` command can transform an input program into an
s-expression-based representation with all syntactic sugar removed by passing
``-s``. The output adheres to the following grammar:

--------------------------------------
Indices, Shapes, Dimensions, and Sorts
--------------------------------------

.. productionlist::
   idx_var : "(" "dim-ivar" `id` ")" | "(" "shape-ivar" `id` ")"
   dim : "(" "dim-ivar" `id` ")"
     : | "(" "dim" `integer_lit` ")"
     : | "(" "dim-+" `dim`* ")"
   shape : "(" "shape-ivar" `id` ")"
       : | "(" "shape-dim"  `dim` ")"
       : | "(" "shape-++" `shape`* ")"
   idx : `dim` | `shape`
   shape_lit : "(" "shape-lit" `integer_lit`* ")"

---------------
Types and Kinds
---------------

.. productionlist::
   type_var : "(" "atom-tvar" `id` ")" |  "(" "array-tvar" `id` ")"
   type : `type_var`
      : | `base_type`
      : | "(" "A" `type` `shape` ")"
      : | "(" "->" "(" `type`* ")" `type` ")"
      : | "(" "forall" "(" `type_var`* ")" `type` ")"
      : | "(" "prod" "(" `idx_var`* ")" `type` ")"
      : | "(" "exists" "(" `idx_var`* ")" `type` ")"
   base_type : "Int" | "Bool" | "Float"

-------------------------------------------------
Source position, Atoms, and Expressions
-------------------------------------------------

.. productionlist::
   file : "(" "file" `string_lit` ")"
   line : "(" "line" `integer_lit` ")"
   col : "(" "col" `integer_lit` ")"
   pos : "(" "source-pos" `file` `line` `col` ")"

.. productionlist::
   val_param : "(" `id` `type` ")"
   base : "(" "int" `integer_lit` ")"
      : | "(" "float" `float_lit` ")"
      : | "#t"
      : | "#f"
   atom : "(" "base" `base` `pos` ")"
      : | "(" "fn" "(" `val_param`* ")" `exp` `pos` ")"
      : | "(" "t-fn" "(" `type_var`* ")" `exp` `pos` ")"
      : | "(" "i-fn" "(" `idx_var`* ")" `exp` `pos` ")"
      : | "(" "box" `idx`* `exp` `type` ")"
   exp : "(" "var" `id` `pos` ")"
     : | "(" "array" `shape_lit` "(" `atom`+ ")" `pos` ")"
     : | "(" "array" `shape_lit` `type` `pos` ")"
     : | "(" "frame" `shape_lit` "(" `exp`+ ")" `pos` ")"
     : | "(" "frame" `shape_lit` `type` `pos` ")"
     : | "(" "app" `exp` "(" `exp`* ")" `pos` ")"
     : | "(" "t-app" `exp` "(" `type`* ")" `pos` ")"
     : | "(" "i-app" `exp` "(" `idx`* ")" `pos` ")"
     : | "(" "unbox" "(" `idx_var`* ")" `id` `exp` ")" `exp` `pos` ")"

-----------
Identifiers
-----------
.. productionlist::
   id :
