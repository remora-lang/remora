############
Typed Remora
############

==============
Source Grammar
==============

--------------------------------------------------------------------------------

This is the grammar that the parser currently supports. It's the sugar-free
grammar below with some syntactic sugar tacked on, as used/defined in Justin's
thesis and the Remora tutorial. The exact sugar is:

- ``atom``: Atom literals (scalars) appearing in expression-positions are
  converted into zero-dimensional arrays: ``(array () atom)``.

- ``[ e_1 ... e_n ]``: Bracketed sequences of expressions are converted into a
  frame: ``(frame [n] e_1 ... e_n)``. If ``e_1 ... e_n`` themselves are frames,
  then the nested frames will be collapsed into a single frame.

- Frames whose constituent expressions are all array literals flattened into an
  array literal (whose leading dimensions are equal to the shape of the frame).

- Function application sugar: ``(@f (t_1 ... t_m) (i_1 ... i_n) e_1 ... e_o)``:
  sugar for ``((i-app (t-app f t_1 ... t_m) i_1 ... i_n) e_1 ... e_o)``.

These changes add a couple of productions to the `exp` non-terminal:

.. productionlist::
   exp : ...
     : | `atom`
     : | "[" `exp` `exp` ... "]"
     : | "(" "@" `exp` "("`type` ...")" "("`idx` ...")" `exp` ...")"

We also have sugar for writing shapes:

.. productionlist::
   shape : ... | "[" `idx` ... "]"

which permits you to splice dimensions and shapes together.

The language is also extended with a ``let``-expression:

.. productionlist::
   val_bind : "(" `val_param` `exp` ")"
   fun_bind : "(" `id` "(" val_param ... ")" `type` `exp` ")"
   type_bind : "(" "type" `type_var` `type` ")"
   idx_bind : "(" "idx" `idx_var` `idx` ")"
   bind : val_bind | fun_bind | type_bind | idx_bind
   exp : ... | "(" "let" "(" bind ... ")" `exp` ")"

which is syntactic sugar for a nest of function applications applied to the
let-bound values and functions to introduce them into the scope of the `let`'s
body.


=========================
Sugar-free Source Grammar
=========================

--------------------------------------------------------------------------------

The language is almost exactly as defined in Chapter 4 of Justin's thesis except
that the variable-distinguishing prefixes from Section 6.1 are incorporated into
the grammar.

--------------------------------------
Indices, Shapes, Dimensions, and Sorts
--------------------------------------

.. productionlist::
   idx_var : "$" `id` | "@" `id`
   dim : "$" `id`
     : | `natural`
     : | "(" + `dim` ... ")"
   shape : "@" `id`
       : | "(" shape `dim` ... ")"
       : | "(" ++ `shape` ... ")"
   idx : `dim` | `shape`
   shape_lit : "(" `natural` ... ")"

---------------
Types and Kinds
---------------

.. productionlist::
   type_var : "*" `id` | "&" `id`
   type : `type_var`
      : | `base_type`
      : | "(" "A" `type` `shape` ")"
      : | "(" ("->" | "→") "(" `type` ... ")" `type` ")"
      : | "(" ("Forall" | "∀") "(" `type_var` ... ")" `type` ")"
      : | "(" ("Pi" | "П")  "(" `idx_var` ... ")" `type` ")"
      : | "(" ("Sigma" | "Σ") "(" `idx_var` ... ")" `type` ")"
   base_type : "Int" | "Bool" | "Float"

--------------------------------
Patterns, Atoms, and Expressions
--------------------------------

.. productionlist::
   pat : `id`
   val_param : "(" `id` `type` ")"
   atom : `base`
      : | "(" ("fn"  | "λ") "(" val_param ... ")" `exp` ")"
      : | "(" ("t-fn" | "tλ") "(" type_var ... ")" `exp` ")"
      : | "(" ("i-fn" | "iλ") "(" idx_var ... ")" `exp` ")"
      : | "(" "box" `idx` ... `exp` `type` ")"
   exp : `id`
     : | "(" "array" `shape_lit` `atom` `atom` ... ")"
     : | "(" "array" `shape_lit` `type` ")"
     : | "(" "frame" `shape_lit` `exp` `exp` ... ")"
     : | "(" "frame" `shape_lit` `type` ")"
     : | "(" `exp` `exp` ... ")"
     : | "(" "t-app" `exp` `type` ... ")"
     : | "(" "i-app" `exp` `idx` ... ")"
     : | "(" "unbox" "(" `pat` ... `pat` `exp` ")" `exp` ")"

-----------
Identifiers
-----------
.. productionlist::
   id :
