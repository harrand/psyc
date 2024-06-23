#ifndef PSYC_PARSE_MACROS_HPP
#define PSYC_PARSE_MACROS_HPP

#define VARARGS_COUNT(...) std::tuple_size<decltype(std::make_tuple(__VA_ARGS__))>::value

#define TOKEN(x) subtree_index{.idx =syntax::node::unparsed_token{lex::token{lex::type::x}}.hash()}
#define NODE(x) subtree_index{.idx = syntax::node::x{}.hash()}
#define STATE(...) []()->std::span<const subtree_index>{return std::array{__VA_ARGS__};}(), [](subtree_view subtrees)->bool{std::size_t index = 0; constexpr std::size_t count = VARARGS_COUNT(__VA_ARGS__);

#define CHORD_BEGIN add_new_reduction(
#define CHORD_END });
#define SETINDEX(i) index = i
#define GETNODE(x) *static_cast<const syntax::node::x*>(subtrees[index++].get())
#define GETTOKEN() (diag::assert_that(index < count, error_code::ice, "internal parse error. subtrees out of bounds (limit {})", count), static_cast<const syntax::node::unparsed_token*>(subtrees[index++].get())->tok)

#endif // PSYC_PARSE_MACROS_HPP