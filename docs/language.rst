############
Typed Remora
############

==============
Source Grammar
==============

--------------------------------------------------------------------------------

This is the grammar that the parser currently supports. It'the sugar-free
grammar below with some syntactic sugar tacked on, as used/defined in Justin's
thesis and the Remora tutorial. The exact sugar is:

- ``atom``: Atom literals (scalars) appearing in expression-positions are
  converted into zero-dimensional arrays: ``(array () atom)``.

- ``[ e_1 ... e_n ]``: Bracketed sequences of expressions are converted into a
  frame: ``(frame [n] e_1 ... e_n)``. If ``e_1 ... e_n`` themselves are frames,
  then the nested frames will be collapsed into a single frame.

- Frames whose constituent expressions are all array literals flattened into an
  array literal (whose leading dimensions are equal to the shape of the frame).

These changes add a couple of productions to the `exp` non-terminal:

.. productionlist::
   exp : ... | `atom` | "[" `exp`+ "]"


=========================
Sugar-free Source Grammar
=========================

--------------------------------------------------------------------------------

The source language free of any sugar.

--------------------------------------
Indices, Shapes, Dimensions, and Sorts
--------------------------------------

.. productionlist::
   sort : "Shape" | "Dim"
   dim : `id`
     : | `integer_lit`
     : | "(" "+" `dim`* ")"
   shape : `id`
       : | "(" "shape"  `dim`* ")"
       : | "(" "++" `shape`* ")"
   idx : `dim` | `shape`
   shape_lit : "(" `integer_lit`* ")"
   idx_param : "(" `id` `sort` ")"

---------------
Types and Kinds
---------------

.. productionlist::
   kind : "Array" | "Atom"
   type : `id`
      : | `base_type`
      : | "(" "A" `type` `shape` ")"
      : | "(" "->" "(" `type`* ")" `type` ")"
      : | "(" "forall" "(" `type_param`* ")" `type` ")"
      : | "(" "prod" "(" `idx_param`* ")" `type` ")"
      : | "(" "exists" "(" `idx_param`* ")" `type` ")"
   base_type : `Int` | `Bool` | `Float`
   type_param : "(" `id` `kind` ")"

--------------------------------
Patterns, Atoms, and Expressions
--------------------------------

.. productionlist::
   pat : `id`
   val_param : "(" `id` `type` ")"
   atom : `base`
      : | `op`
      : | "(" "fn" "(" val_param* ")" `exp` ")"
      : | "(" "t-fn" "(" type_param* ")" `exp` ")"
      : | "(" "i-fn" "(" idx_param* ")" `exp` ")"
      : | "(" "box" `idx`* `exp` `type` ")"
   exp : `id`
     : | "(" "array" `shape_lit` `atom`+ ")"
     : | "(" "array" `shape_lit` `type` ")"
     : | "(" "frame" `shape_lit` `exp`+ ")"
     : | "(" "frame" `shape_lit` `type` ")"
     : | "(" `exp` `exp`* ")"
     : | "(" "t-app" `exp` `type`* ")"
     : | "(" "i-app" `exp` `idx`* ")"
     : | "(" "unbox" "(" `pat`* `pat` `exp` ")" `exp` ")"
   op : "+" | "-" | "iota" | ...

-----------
Identifiers
-----------
.. productionlist::
   id :


======================
Future Grammar Musings
======================

--------------------------------------------------------------------------------

Musings about what kind of syntactic sugar we actually want to support.

----------------------
Declarations
----------------------
.. productionlist::
   decl : "(" "def" `id` ("(" type_param* ")")? ("[" idx_param* "]")? "(" ("(" `id` ":" `type` ")")* ")" `exp` ")"

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
   idx_param : `id` | "$" `id`
   idx_app : "@" "[" `shape`* "]"
..   idx_app : "$" `shape`*

-----
Types
-----

.. productionlist::
   type : `id`
      : | `base_type`
      : | "(" "A" `type` `shape` ")"
      : | "(" "->" `type` `type` ")"
      : | "(" "->" "(" `type`+ ")" `type` ")"
      : | "(" "forall" "(" type_param* ")" `type` ")"
      : | "(" "prod" "[" idx_param* "]" `type` ")"
      : | "(" "exists" "[" `idx_param`* "]" `type` ")"
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
      : | "(" "fn" ("(" type_param* ")")? ("[" idx_param* "]")? "(" ("(" `id` ":" `type` ")")* ")" `exp` ")"
      : | "(" "box" `shape`* `exp` `type` ")"
   exp : `id`
     : | "(" "array" `shape_lit` `atom`+ ")"
     : | "(" "array" `shape_lit` `type` ")"
     : | "(" "frame" `shape_lit` `exp`+ ")"
     : | "(" "frame" `shape_lit` `type` ")"
     : | "[" `atom`+ "]"
     : | "[" `exp`+ "]"
     : | "(" `exp` `type_app`? `idx_app`? `exp`* ")"
     : | "(" "unbox" "(" `pat`+ `exp` ")" `exp` ")"

   op : "+" | "-" | "iota" | ...

..      : | "(" "t-fn" "(" type_param* ")" `exp` ")"
..      : | "(" "i-fn" "[" idx_param* "]" `exp` ")"


-----------
Identifiers
-----------
.. productionlist::
   id :
