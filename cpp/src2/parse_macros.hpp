#ifndef PSYC_PARSE_MACROS_HPP
#define PSYC_PARSE_MACROS_HPP

#define my_xstr(a) my_str(a)
#define my_str(a) #a
#define VARARGS_COUNT(...) std::tuple_size<decltype(std::make_tuple(__VA_ARGS__))>::value

#define TOKEN(x) subtree_index{.idx =syntax::node::unparsed_token{lex::token{lex::type::x}}.hash(), .name_hint = my_xstr(x)}
#define NODE(x) subtree_index{.idx = syntax::node::x{}.hash(), .name_hint = my_xstr(x)}
#define STATE(...) [](){return std::array{__VA_ARGS__};}(), [](reducer reduce)->result{std::size_t index = 0; constexpr std::size_t count = VARARGS_COUNT(__VA_ARGS__);diag::assert_that(reduce.subtrees.size() >= count, error_code::ice, "current parser state ({} subtrees) is too small for this chord, which demands at least {} subtrees", reduce.subtrees.size(), count);

#define CHORD_BEGIN add_new_reduction(
#define CHORD_END });
#define SETINDEX(i) reduce.idx = i
#define GETNODE(x) *static_cast<const syntax::node::x*>(reduce.subtrees[reduce.idx++].get())
#define GETTOKEN() (diag::assert_that(reduce.idx < count, error_code::ice, "internal parse error. subtrees out of bounds (limit {})", count), static_cast<const syntax::node::unparsed_token*>(reduce.subtrees[reduce.idx++].get())->tok)
// reduce everything emcompassed by the state to a single new subtree.
#define REDUCE_TO(x) reduce.subtrees.erase(reduce.subtrees.begin() + 0, reduce.subtrees.begin() + 0 + count); reduce.subtrees.insert(reduce.subtrees.begin() + 0, x)

#endif // PSYC_PARSE_MACROS_HPP