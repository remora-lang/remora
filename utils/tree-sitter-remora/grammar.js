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

    // word: $ => $.id,
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
	    repeat(seq('(', field('arg', $.id), field('type', $._type), ')')),
	    optional(seq(':', field('return_type', $._type))),
	    ')',
	    field('body', $._exp),
	    ')'
	),
	defdecl: $ => seq('(', 'def',
			  field('bind', $._bind),
			  ')'),

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
	atomexp: $ => $._atom,
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
			   repeat(field('param', $._exp)), ')'),
	unboxexp: $ => seq('(', 'unbox', '(', repeat1(field('ispace_var', $._ispace_var)),
			   field('id', $.id),
			   field('exp1', $._exp), ')',
			   field('exp2', $._exp), ')'),
	letexp: $ => seq('(', 'let', '(', repeat1(field('bind', $._bind)), ')', field('exp', $._exp), ')'),

	_atom: $ => choice(
	    $.litatom,
	    $.intatom,
	    $.floatatom,
	    $.fnatom,
	    $.tfnatom,
	    $.ifnatom,
	    $.boxatom,
	),
	litatom: $ => field('lit', $.boollit),
	intatom: $ => field('int', $.int),
	floatatom: $ => field('float', $.float),
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
	    $._valbind,
	    $._funbind,
	    $.tfunbind,
	    $.ifunbind,
	    $.typebind,
	    $.ispacebind),

	_valbind: $ => choice($.valnotypebind, $.valtypebind),
	valnotypebind: $ => seq('(', 'val', field('id', $.id), field('exp', $._exp), ')'),
	valtypebind: $ => seq('(', 'val', '(', field('id', $.id), ':', field('type', $._type), ')',
			      field('exp', $._exp),
			      ')'),

	_funbind: $ => choice($.regfunbind, $.atfunbind),
	regfunbind: $ => seq('(', 'fun', '(',
			     field('id', $.id),
			     repeat(seq('(', field('arg', $.id), field('type', $._type), ')')),
			     optional(seq(':', field('ret_type', $._type))),
			     ')',
			     field('body', $._exp),
			     ')'),
	atfunbind: $ => seq('(', 'fun', '(',
		field('id', $.atid),
		choice(
		    seq('(', repeat(field('tvar', $._tvar)), ')'),
		    '_'),
		choice(
		    seq('(', repeat1(field('ivar', $._ispace_var)), ')'),
		    '_'),
		repeat(seq('(', field('arg', $.id), field('type', $._type), ')')),
		':',
		field('ret_type', $._type),
		')',
		field('body', $._exp),
		')'),

	tfunbind: $ => seq(
	    '(', 't-fun','(',
	    field('id', $.id),
	    '(', repeat(field('tvar', $._tvar)), ')',
	    optional(seq(':', field('ret_type', $._type))),
	    ')',
	    field('body', $._exp),
	    ')'),

	ifunbind: $ => seq(
	    '(', 'i-fun', '(',
	    field('id', $.id),
	    '(', repeat(field('ivar', $._ispace_var)), ')',
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
	bracketarraytype: $ => seq('[', field('type', $._type), repeat(field('ispace', $._ispace)), ']'),
	aarraytype: $ => seq('(', 'A', field('type', $._type), field('ispace', $._ispace), ')'),

	_functiontype: $ => choice(
	    $.generalfntype,
	    $.simplefntype),
	generalfntype: $ => seq('(', $._rightarrow, '(', repeat(field('argtype', $._type)), ')', field('returntype', $._type), ')'),
	simplefntype: $ => seq('(', $._rightarrow, field('argtype', $._type), field('returntype', $._type), ')'),

	foralltype: $ => seq('(', $._forall, '(', repeat1(field('tvar', $._tvar)), ')', field('returntype', $._type), ')'),

	pitype: $ => seq('(', $._pi, '(', repeat1(field('ivar', $._ispace_var)), ')', field('returntype', $._type), ')'),

	sigmatype: $ => seq('(', $._sigma, '(', repeat1(field('ivar', $._ispace_var)), ')', field('returntype', $._type), ')'),

	_dim: $ => choice(
	    $.dollardim,
	    $.natdim,
	    $.plusdim,
	    $.timesdim,
	    $.minusdim),
	dollardim: $ => $.dollid,
	natdim: $ => $.nat,
	plusdim: $ => seq('(', '+', repeat(field('dim', $._dim)), ')'),
	timesdim: $ => seq('(', '*', repeat(field('dim', $._dim)), ')'),
	minusdim: $ => seq('(', '-', repeat(field('dim', $._dim)), ')'),

	_shape: $ => choice(
	    $.atshape,
	    $.dimsshape,
	    $.plusplusshape,
	    $.ispaceshape),
	atshape: $ => $.atid,
	dimsshape: $ => seq('(', 'dims', repeat1(field('dim', $._dim)), ')'),
	plusplusshape: $ => prec.left(seq('(', '++', repeat1(field('shape', $._shape)), ')')),
	ispaceshape: $ => seq('[', repeat(field('ispace', $._ispace)), ']'),
	
	_ispace: $ => choice(
	    $._dim,
	    $._shape),
	
	_tvar: $ => choice(
	    $.amptvar,
	    $.startvar),
	amptvar: $ => $.ampid,
	startvar: $ => $.starid,
	
	_ispace_var: $ => choice(
	    $.dollarispacevar,
	    $.atispacevar),
	dollarispacevar: $ => $.dollid,
	atispacevar: $ => $.atid,

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
	id: $ => token(seq(choice(new RustRegex('[A-Za-z]'), '-', '+', '*', '/'),
			   repeat(choice(new RustRegex('[A-Za-z0-9_]'), '/', '-', '+', '*', '.')))),
	atid: $ => token(seq('@', seq(choice(new RustRegex('[A-Za-z]'), '-', '+', '*', '/'),
			   repeat(choice(new RustRegex('[A-Za-z0-9_]'), '/', '-', '+', '*', '.'))))),
	ampid: $ => token(seq('&', seq(choice(new RustRegex('[A-Za-z]'), '-', '+', '*', '/'),
			   repeat(choice(new RustRegex('[A-Za-z0-9_]'), '/', '-', '+', '*', '.'))))),
	dollid: $ => token(seq('$', seq(choice(new RustRegex('[A-Za-z]'), '-', '+', '*', '/'),
			   repeat(choice(new RustRegex('[A-Za-z0-9_]'), '/', '-', '+', '*', '.'))))),
	starid: $ => token(seq('*', seq(choice(new RustRegex('[A-Za-z]'), '-', '+', '*', '/'),
			   repeat(choice(new RustRegex('[A-Za-z0-9_]'), '/', '-', '+', '*', '.'))))),
	// _id_start: $ => /[!$%&*+.:<=>?A-Z^_a-z]/,
	// _id_continue: $ => /[!$%&*+.:<=>?A-Z^_a-z\-0-9]/,
	// id: $ => /[^0-9\(\)\[\]{}",'`;#\|\@][^\(\)\[\]{}",'`;#\|\@]*/,
	filename: $ => /[a-z_.]+/,
	nat: $ => /\d+/,
	int: $ => /-?\d+/,
	float: $ => /-?\d+\.?[\de]+/,
	boollit: $ => choice('#t', '#f'),
	string: $ => /[^"]*/,
    }
});
