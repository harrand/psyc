#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// if nothing is preceding an expression, send it to the output.
CHORD_BEGIN
	STATE(NODE(expression))
	return {.t = reduce.no_prefix() ? result::type::send_to_output : result::type::silent_reject};
CHORD_END

#ifndef INFUNC
}}
#endif