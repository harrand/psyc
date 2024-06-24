#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// no more reductions can take place, send this to the output.
CHORD_BEGIN
	STATE(NODE(primary_expression))
	return {.t = result::type::send_to_output};
CHORD_END

#ifndef INFUNC
}}
#endif