/**
 * @file Tree-sitter grammar for Remora programming language
 * @author Greg Sullivan <gsullivan@draper.com>
 *
 * Based on condensed grammar in Appendix B of https://github.com/remora-lang/remora/tree/main/tutorial
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
    name: "remora",

    extras: ($) => [
	/\s/,
	$.comment
    ],

    word: $ => $.id,
    reserved: {
	global: $ => ['fn', 't-fn', 'tλ', 'i-fn', 'iλ', 'box'],
    },

    rules: {
	program: $ => seq(
	    repeat($.import),
	    repeat($._decl)
	),

	import: $ => seq(
	    'import',
	    '"',
	    field('filename', $.filename),
	    '"'
	),

	_decl: $ => choice(
	    $.entrydecl,
	    $.defdecl),
	entrydecl: $ => seq(
	    '(',
	    'entry',
	    '(', field('id', $.id),
	    field('args', $.args),
	    optional(
		seq(
		    ':',
		    field('return_type', $._type)
		)
	    ),
	    ')',
	    field('body', $._exp),
	    ')'
	),
	defdecl: $ => seq('(', 'def',
			  field('bind', $._bind),
			  ')'),

	args: $ => choice(	// allows for ()
	    seq('(', ')'),
	    repeat1(
		seq(
		    '(',
		    field('arg', $.id),
		    field('type', $._type),
		    ')'
		)
	    )
	),

	_exp: $ => choice(
	    $.atomexp,
	    $.bracketsexp,
	    $.idexp,
	    $.stringexp,
	    $.arrayexpatom,
	    $.arrayexptype,
	    $.frameexpexp,
	    $.frameexptype,
	    $.applicationexp,
	    $.tappexp,
	    $.iappexp,
	    $.atappexp,
	    $.unboxexp,
	    $.letexp),
	atomexp: $ => field('atom', $._atom),
	bracketsexp: $ => seq('[', repeat(field('exp', $._exp)), ']'),
	idexp: $ => field('id', $.id),
	stringexp: $ => seq('"', field('string', $.string), '"'),
	arrayexpatom: $ => seq('(', 'array', field('shape', $.shape_lit), repeat1(field('atom', $._atom)), ')'),
	arrayexptype: $ => seq('(', 'array', field('shape', $.shape_lit), field('type', $._type), ')'),
	frameexpexp: $ =>  seq('(', 'frame', field('shape', $.shape_lit), repeat1(field('exp', $._exp)), ')'),
	frameexptype: $ => seq('(', 'frame', field('shape', $.shape_lit), field('type', $._type), ')'),
	applicationexp: $ => seq('(', field('fnexp', $._exp), repeat(field('param', $._exp)), ')'),
	tappexp: $ => seq('(', 't-app', field('fnexp', $._exp), repeat(field('type', $._type)), ')'),
	iappexp: $ => seq('(', 'i-app', field('fnexp', $._exp), repeat(field('ispace', $._ispace)), ')'),
	atappexp: $ => seq('(', '@', field('fnexp', $._exp),
			   choice(seq('(', repeat1(field('type', $._type)), ')'),
				  '_'),
			   choice(seq('(', repeat1(field('ispace', $._ispace)), ')'),
				  '_'),
			   repeat1(field('param', $._exp)), ')'),
	unboxexp: $ => seq('(', 'unbox', '(', repeat1(field('ispace_var', $._ispace_var)),
			   field('id', $.id),
			   field('exp1', $._exp), ')',
			   field('exp2', $._exp), ')'),
	letexp: $ => seq('(', 'let', '(', repeat1(field('bind', $._bind)), ')', field('exp', $._exp), ')'),

	_atom: $ => choice(
	    $.litatom,
	    $.typeatom,
	    $.fnatom,
	    $.tfnatom,
	    $.ifnatom,
	    $.boxatom,
	),
	litatom: $ => field('lit', $.boollit),
	typeatom: $ => field('type', $.typelit),
	fnatom: $ => seq('(', choice('fn', 'λ' ), '(',
			 repeat(seq('(', field('arg', $.id), field('type', $._type), ')')), ')',
			 field('body', $._exp), ')'),
	tfnatom: $ => seq('(', choice('t-fn', 'tλ'), '(',
			  repeat(field('tvar', $._tvar)), ')', field('body', $._exp), ')'),
	ifnatom: $ => seq('(', choice('i-fn', 'iλ'), '(',
			  repeat(field('ivar', $._ispace_var)), ')', field('body', $._exp), ')'),
	boxatom: $ => seq('(', 'box', '(', repeat(field('ispace', $._ispace)), ')',
			  field('exp', $._exp), field('type', $._type), ')'),

	_bind: $ => choice(
	    $.valbind,
	    $.funbind,
	    $.tfunbind,
	    $.ifunbind,
	    $.typebind,
	    $.ispacebind),

	valbind: $ => choice(
	    seq('(', 'val',
		field('id', $.id),
		field('exp', $._exp),
		')'),
	    seq('(', 'val', '(',
		field('id', $.id),
		':', field('type', $._type),
		')',
		field('exp', $._exp),
		')')),

	funbind: $ => choice(
	    seq('(', 'fun', '(',
		field('id', $.id),
		$.args,
		optional(
		    seq(':', field('ret_type', $._type))),
		')',
		field('body', $._exp),
		')'),
	    seq('(', 'fun', '(', '@',
		field('id', $.id),
		choice(
		    seq('(',
			field('tvars', repeat1($._tvar)),
			')'),
		    '_'),
		choice(
		    seq('(',
			field('ivars', repeat1($._ispace_var)),
			')'),
		    '_'),
		$.args,
		':',
		field('ret_type', $._type),
		')',
		field('body', $._exp),
		')')),

	tfunbind: $ => seq(
	    '(', 't-fun','(',
	    field('id', $.id),
	    '(', repeat1( field('tvars', $._tvar)), ')',
	    optional(
		seq(':', field('ret_type', $._type))),
	    ')',
	    field('body', $._exp),
	    ')'),

	ifunbind: $ => seq(
	    '(', 'i-fun', '(',
	    field('id', $.id),
	    '(', repeat1(field('ivars', $._ispace_var)), ')',
	    optional(
		seq(':', field('ret_type', $._type))),
	    ')',
	    field('body', $._exp),
	    ')'),

	typebind: $ => seq('(', 'type', field('id', $._tvar), field('type', $._type), ')'),

	ispacebind: $ => seq('(', 'ispace', field('ispacevar', $._ispace_var), field('ispace', $._ispace),')'),

	_type: $ => choice(
	    $._tvar,
	    $.immedtype,
	    $._arraytype, 
	    $._functiontype,
	    $.foralltype,
	    $.pitype,
	    $.sigmatype
	),

	immedtype: $ => choice(
	    'Int',
	    'Bool',
	    'Float'),

	_arraytype: $ => choice(
	    $.bracketarraytype,
	    $.aarraytype),
	bracketarraytype: $ => seq('[', field('type', $._type), repeat1(field('ispace', $._ispace)), ']'),
	aarraytype: $ => seq('(', 'A', field('type', $._type), field('ispace', $._ispace), ')'),

	_functiontype: $ => choice(
	    $.generalfntype,
	    $.simplefntype),
	generalfntype: $ => seq('(', $._rightarrow, '(', repeat(field('argtype', $._type)), ')', field('returntype', $._type), ')'),
	simplefntype: $ => seq('(', $._rightarrow, field('argtype', $._type), field('returntype', $._type), ')'),

	foralltype: $ => seq('(', $._forall, '(', repeat1(field('tvar', $._tvar)), ')', field('returntype', $._type), ')'),

	pitype: $ => seq('(', $._pi, '(', repeat1(field('ivar', $._ispace_var)), ')', field('returntype', $._type), ')'),

	sigmatype: $ => seq('(', $._sigma, '(', repeat(field('ivar', $._ispace_var)), ')', field('returntype', $._type), ')'),

	_dim: $ => choice(
	    $.dollardim,
	    $.natdim,
	    $.plusdim,
	    $.timesdim,
	    $.minusdim),
	dollardim: $ => seq('$', $.id),
	natdim: $ => $.nat,
	plusdim: $ => seq('(', '+', repeat1(field('dim', $._dim)), ')'),
	timesdim: $ => seq('(', '*', repeat1(field('dim', $._dim)), ')'),
	minusdim: $ => seq('(', '-', repeat1(field('dim', $._dim)), ')'),

	_shape: $ => choice(
	    $.atshape,
	    $.dimsshape,
	    $.plusplusshape,
	    $.ispaceshape),
	atshape: $ => seq('@', $.id),
	dimsshape: $ => seq('(', 'dims', repeat1(field('dim', $._dim)), ')'),
	plusplusshape: $ => prec.left(seq('(', '++', repeat1(field('shape', $._shape)), ')')),
	ispaceshape: $ => seq('[', repeat(field('ispace', $._ispace)), ']'),
	
	_ispace: $ => choice(
	    $._dim,
	    $._shape),
	
	_tvar: $ => choice(
	    $.attvar,
	    $.startvar),
	attvar: $ => seq('&', $.id),
	startvar: $ => seq('*', $.id),
	
	_ispace_var: $ => choice(
	    $.dollarispacevar,
	    $.atispacevar),
	dollarispacevar: $ => seq('$', $.id),
	atispacevar: $ => seq('@', $.id),

	shape_lit : $ => seq('[', repeat(field('num', $.nat)), ']'),

	comment: ($) => token(
	    seq(';', /.*/)),

	_rightarrow: $ => choice(
	    '->',
	    '→'),
	_pi: $ => choice(
	    'Pi',
	    'Π'),
	_forall: $ => choice(
	    'Forall',
	    '∀'),
	_sigma: $ => choice(
	    'Sigma',
	    'Σ'),

	id: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,
	filename: $ => /[a-z_.]+/,
	nat: $ => /\d+/,
	boollit: $ => choice('#t', '#f'),
        typelit: $ => choice('int', 'float'),
	string: $ => /[^"]*/,
    }
});
