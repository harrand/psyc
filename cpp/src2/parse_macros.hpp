#ifndef PSYC_PARSE_MACROS_HPP
#define PSYC_PARSE_MACROS_HPP

#define my_xstr(a) my_str(a)
#define my_str(a) #a
#define VARARGS_COUNT(...) std::tuple_size<decltype(std::make_tuple(__VA_ARGS__))>::value

#define TOKEN(x) subtree_index{.idx = syntax::make_node(lex::token{lex::type::x, "0"}).hash(), .name_hint = my_xstr(x)}
#define NODE(x) subtree_index{.idx = syntax::node{.payload = syntax::x{}}.hash(), .name_hint = my_xstr(x)}
// set the state for a given chord. you're expected to pass a collection of NODEs or TOKENs. e.g NODE(integer_literal) or TOKEN(semicol)
// this creates a reduction function that will be called if the set of subtrees (with the lookahead token appended to the end if there is one) within the parser matches the state you specified.
// the reduction function must return a result. possible results are:
/*
	- reduce success (you are expected to do a reduction in this case). parser will assume you've done a reduction and move on.
	- shift (tell the parser it needs to shift a new token in)
	- shift-but-clear-lookahead (same as shift, but the lookahead symbol is cleared out. this is useful if you've found a perfect match but the last element is the lookahead symbol, meaning you're one shift away from being able to do the reduction)
	- error (syntax error). potentially useful if you want to give helpful error messages in the case of a certain known-erroneous pattern
*/
#define STATE(...) [](){return std::array{__VA_ARGS__};}(), [](reducer reduce)->result{std::size_t index = 0; constexpr std::size_t count = VARARGS_COUNT(__VA_ARGS__);diag::assert_that(reduce.subtrees.size() - reduce.idx + 1 >= count, error_code::ice, "current parser state ({} subtrees excluding +1 lookahead token) is too small for this chord, which demands at least {} subtrees + lookaheads total", reduce.subtrees.size(), count);

#define CHORD_BEGIN add_new_reduction(
#define CHORD_END });
// note: if you dont call SETINDEX, the default is 0 for obvious reasons.
// set the current index to a certain position within the state (e.g SETINDEX(2) means the token/node at index 2 in the state will be retrieved next)
#define SETINDEX(i) index = i
// get the node at the current index, and increment the index. if the node type you specify does not match what you said it would be in the state, then the behaviour is undefined.
#define GETNODE(x) std::get<syntax::x>(reduce.subtrees[reduce.idx + index++].payload)
// get the token at the current index, and increment the index. if the thing at the current index is not a token as per your state definition, then the behaviour is undefined. as you match against a specific token type already this may or may not be very useful.
#define GETTOKEN() std::get<syntax::unparsed_token>(reduce.subtrees[reduce.idx + index++].payload).tok
// figure out whether the subtrees currently matching the provided state contains the lookahead token at the end (meaning the subtrees dont actually match, but will once you shift one more time). it may be useful to return shift-but-clear-lookahead in this case. otherwise, the next lookahead may not be a token you care about but will prevent the state from matching as it could be anything. this is unaffected by the current index as per SETINDEX
// reduce everything emcompassed by the state to a single new subtree.
// currently we assume a reduction function will only perform one single reduction, and will always reduce the entire state into that single result. this is unaffected by the current index as per SETINDEX
#define REDUCE_TO_ADVANCED(prefix, suffix, type, ...) PROFZONE_BEGIN(do_reduce); auto meta = reduce.subtrees[reduce.idx + prefix].loc(); reduce.subtrees.erase(reduce.subtrees.begin() + reduce.idx + prefix, reduce.subtrees.begin() + reduce.idx + count - suffix); auto payload = syntax::type(__VA_ARGS__); payload.loc = meta; reduce.subtrees.insert(reduce.subtrees.begin() + reduce.idx, syntax::node{.payload = payload}); PROFZONE_END(do_reduce);
#define REDUCE_TO(type, ...) REDUCE_TO_ADVANCED(0, 0, type, __VA_ARGS__)

#endif // PSYC_PARSE_MACROS_HPP