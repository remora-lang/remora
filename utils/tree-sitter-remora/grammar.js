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

    supertypes: $ => [ $.bind, $.decl, $.exp, $.atom, $.type],

    rules: {
	program: $ => seq(
	    repeat($.import),
	    repeat($.decl)
	),

	import: $ => seq(
	    'import',
	    '"',
	    field('filename', $.filename),
	    '"'
	),

	decl: $ => choice(
	    $.entrydecl,
	    $.defdecl),
	entrydecl: $ => seq(
	    '(',
	    'entry',
	    '(', field('id', $.id),
	    repeat(seq('(', field('arg', $.id), field('type', $.type), ')')),
	    optional(seq(':', field('return_type', $.type))),
	    ')',
	    field('body', $.exp),
	    ')'
	),
	defdecl: $ => seq('(', 'def',
			  field('bind', $.bind),
			  ')'),

	exp: $ => choice(
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
	atomexp: $ => $.atom,
	bracketsexp: $ => seq('[', repeat(field('exp', $.exp)), ']'),
	idexp: $ => field('id', $.id),
	stringexp: $ => seq('"', field('string', $.string), '"'),
	arrayexpatom: $ => seq('(', 'array', field('shape', $.shape_lit), repeat1(field('atom', $.atom)), ')'),
	arrayexptype: $ => seq('(', 'array', field('shape', $.shape_lit), field('type', $.type), ')'),
	frameexpexp: $ =>  seq('(', 'frame', field('shape', $.shape_lit), repeat1(field('exp', $.exp)), ')'),
	frameexptype: $ => seq('(', 'frame', field('shape', $.shape_lit), field('type', $.type), ')'),
	applicationexp: $ => seq('(', field('fnexp', $.exp), repeat(field('param', $.exp)), ')'),
	tappexp: $ => seq('(', 't-app', field('fnexp', $.exp), repeat(field('type', $.type)), ')'),
	iappexp: $ => seq('(', 'i-app', field('fnexp', $.exp), repeat(field('ispace', $._ispace)), ')'),
	atappexp: $ => seq('(', '@', field('fnexp', $.exp),
			   choice(seq('(', repeat1(field('type', $.type)), ')'),
				  '_'),
			   choice(seq('(', repeat1(field('ispace', $._ispace)), ')'),
				  '_'),
			   repeat(field('param', $.exp)), ')'),
	unboxexp: $ => seq('(', 'unbox', '(', repeat1(field('ispace_var', $._ispace_var)),
			   field('id', $.id),
			   field('exp1', $.exp), ')',
			   field('exp2', $.exp), ')'),
	letexp: $ => seq('(', 'let', '(', repeat1(field('bind', $.bind)), ')', field('exp', $.exp), ')'),

	atom: $ => choice(
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
			 repeat(seq('(', field('arg', $.id), field('type', $.type), ')')), ')',
			 field('body', $.exp), ')'),
	tfnatom: $ => seq('(', choice('t-fn', 'tλ'), '(',
			  repeat(field('tvar', $._tvar)), ')', field('body', $.exp), ')'),
	ifnatom: $ => seq('(', choice('i-fn', 'iλ'), '(',
			  repeat(field('ivar', $._ispace_var)), ')', field('body', $.exp), ')'),
	boxatom: $ => seq('(', 'box', '(', repeat(field('ispace', $._ispace)), ')',
			  field('exp', $.exp), field('type', $.type), ')'),

	bind: $ => choice(
	    $._valbind,
	    $._funbind,
	    $.tfunbind,
	    $.ifunbind,
	    $.typebind,
	    $.ispacebind),

	_valbind: $ => choice($.valnotypebind, $.valtypebind),
	valnotypebind: $ => seq('(', 'val', field('id', $.id), field('exp', $.exp), ')'),
	valtypebind: $ => seq('(', 'val', '(', field('id', $.id), ':', field('type', $.type), ')',
			      field('exp', $.exp),
			      ')'),

	_funbind: $ => choice($.regfunbind, $.atfunbind),
	regfunbind: $ => seq('(', 'fun', '(',
			     field('id', $.id),
			     repeat(seq('(', field('arg', $.id), field('type', $.type), ')')),
			     optional(seq(':', field('ret_type', $.type))),
			     ')',
			     field('body', $.exp),
			     ')'),
	atfunbind: $ => seq('(', 'fun', '(',
		field('id', $.atid),
		choice(
		    seq('(', repeat(field('tvar', $._tvar)), ')'),
		    '_'),
		choice(
		    seq('(', repeat1(field('ivar', $._ispace_var)), ')'),
		    '_'),
		repeat(seq('(', field('arg', $.id), field('type', $.type), ')')),
		':',
		field('ret_type', $.type),
		')',
		field('body', $.exp),
		')'),

	tfunbind: $ => seq(
	    '(', 't-fun','(',
	    field('id', $.id),
	    '(', repeat(field('tvar', $._tvar)), ')',
	    optional(seq(':', field('ret_type', $.type))),
	    ')',
	    field('body', $.exp),
	    ')'),

	ifunbind: $ => seq(
	    '(', 'i-fun', '(',
	    field('id', $.id),
	    '(', repeat(field('ivar', $._ispace_var)), ')',
	    optional(
		seq(':', field('ret_type', $.type))),
	    ')',
	    field('body', $.exp),
	    ')'),

	typebind: $ => seq('(', 'type', field('id', $._tvar), field('type', $.type), ')'),

	ispacebind: $ => seq('(', 'ispace', field('ispacevar', $._ispace_var), field('ispace', $._ispace),')'),

	type: $ => choice(
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
	bracketarraytype: $ => seq('[', field('type', $.type), repeat(field('ispace', $._ispace)), ']'),
	aarraytype: $ => seq('(', 'A', field('type', $.type), field('ispace', $._ispace), ')'),

	_functiontype: $ => choice(
	    $.generalfntype,
	    $.simplefntype),
	generalfntype: $ => seq('(', $._rightarrow, '(', repeat(field('argtype', $.type)), ')', field('returntype', $.type), ')'),
	simplefntype: $ => seq('(', $._rightarrow, field('argtype', $.type), field('returntype', $.type), ')'),

	foralltype: $ => seq('(', $._forall, '(', repeat1(field('tvar', $._tvar)), ')', field('returntype', $.type), ')'),

	pitype: $ => seq('(', $._pi, '(', repeat1(field('ivar', $._ispace_var)), ')', field('returntype', $.type), ')'),

	sigmatype: $ => seq('(', $._sigma, '(', repeat1(field('ivar', $._ispace_var)), ')', field('returntype', $.type), ')'),

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
