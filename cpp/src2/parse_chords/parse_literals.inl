#define REDUCE_LITERALS(ty) \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(semicol))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(comma))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(dot))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(cparen))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(cast))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(plus))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(minus))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(asterisk))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(slash))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(eq))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(eqeq))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(neq))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\
 \
CHORD_BEGIN\
	STATE(NODE(ty), TOKEN(cbrace))\
	syntax::node::ty intlit = GETNODE(ty);\
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::ty, intlit);\
	return {.t = result::type::reduce_success};\
CHORD_END\

REDUCE_LITERALS(integer_literal)
REDUCE_LITERALS(decimal_literal)
REDUCE_LITERALS(null_literal)
REDUCE_LITERALS(char_literal)
REDUCE_LITERALS(bool_literal)
REDUCE_LITERALS(string_literal)
REDUCE_LITERALS(identifier)
REDUCE_LITERALS(function_call)
REDUCE_LITERALS(namespace_access)