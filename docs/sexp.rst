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
     : | "(" "dim-n" `integer_lit` ")"
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
   annot_type : "no-info" | "(" "info" `type` ")"
   annot_type_pframe  :  "no-info" | "(" "info" "(" `type` `shape`")" ")"

.. productionlist::
   val_param : "(" `id` `type` ")"
   base : "(" "int" `integer_lit` ")"
      : | "(" "float" `float_lit` ")"
      : | "#t"
      : | "#f"
   atom : "(" "base" `base` `annot_type` `pos` ")"
      : | "(" "fn" "(" `val_param`* ")" `exp` `annot_type` `pos` ")"
      : | "(" "t-fn" "(" `type_var`* ")" `exp` `annot_type` `pos` ")"
      : | "(" "i-fn" "(" `idx_var`* ")" `exp` `annot_type` `pos` ")"
      : | "(" "box" `idx`* `exp` `type` `pos` ")"
   exp : "(" "var" `id` `annot_type` `pos` ")"
     : | "(" "array" `shape_lit` "(" `atom`+ ")" `annot_type` `pos` ")"
     : | "(" "empty-array" `shape_lit` `type` `annot_type` `pos` ")"
     : | "(" "frame" `shape_lit` "(" `exp`+ ")" `annot_type` `pos` ")"
     : | "(" "empty-frame" `shape_lit` `type` `annot_type` `pos` ")"
     : | "(" "app" `exp` "(" `exp`* ")" `annot_type_pframe` `pos` ")"
     : | "(" "t-app" `exp` "(" `type`* ")" `annot_type` `pos` ")"
     : | "(" "i-app" `exp` "(" `idx`* ")" `annot_type` `pos` ")"
     : | "(" "unbox" "(" `idx_var`* ")" `id` `exp` ")" `exp` `annot_type` `pos` ")"

-----------
Identifiers
-----------
.. productionlist::
   id :
