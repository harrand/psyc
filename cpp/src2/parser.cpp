#include "parser.hpp"
#include "parse.hpp"
#include "diag.hpp"

namespace parse
{
	parser::parser(lex::const_token_view tokens):
	tokens(tokens),
	unscanned_tokens(tokens)
	{
		this->output = std::make_unique<syntax::node::root>(tokens.front().meta_srcloc.file);
	}

	void parser::parse()
	{
		while(this->step()){}
	}

	bool parser::step()
	{
		constexpr std::size_t max_offset = 8;
		for(std::size_t i = 0; std::cmp_less(i, std::min(max_offset, this->subtrees.size())); i++)
		{
			auto state = this->get_parsed_state(i);
			if(!state.empty())
			{
				reduction reduc = find_reduction(state);
				if(!reduc.is_null())
				{
					result res = reduc.reduce_fn(this->make_reducer(i));
					switch(res.t)
					{
						case result::type::reduce_success:
							return true;
						break;
						case result::type::shift:
							return shift();
						break;
						case result::type::shift_but_clear_lookahead:
						{
							bool ret = shift();
							this->lookahead = std::nullopt;
							return ret;
						}
						break;
						case result::type::send_to_output:
						{
							auto ptr = std::move(this->subtrees[i]);
							this->subtrees.erase(this->subtrees.begin() + i);
							this->output->children.push_back(std::move(ptr));
							return true;
						}
						break;
						case result::type::error:
							diag::error(error_code::parse, "{}", res.errmsg);
							return false;
						break;
					}
				}
				else
				{
					if(!this->lookahead.has_value() && this->unscanned_tokens.empty())
					{
						// parsing is complete. no more reductions go to and no more things to scan.
						return false;
					}
					std::string subtree_str;
					for(std::size_t j = i; j < this->subtrees.size(); j++)
					{
						subtree_str += this->subtrees[j]->to_string();
						if(j < (this->subtrees.size() - 1))
						{
							subtree_str += "\n";
						}
					}
					diag::error(error_code::parse, "invalid syntax. state: \n{{\n{}\n}}", subtree_str);
				}
			}
		}
		return shift();
	}

	bool parser::shift()
	{
		if(this->unscanned_tokens.empty())
		{
			return false;
		}
		lex::token t = this->unscanned_tokens.front();
		this->unscanned_tokens = this->unscanned_tokens.subspan(1);
		if(lex::get_trait(t.t).parse_skip)
		{
			return shift();
		}

		this->subtrees.push_back(syntax::make_node(t));
		if(this->unscanned_tokens.size())
		{
			this->lookahead = this->unscanned_tokens.front();
		}
		else
		{
			this->lookahead = std::nullopt;
		}
		return true;
	}

	subtree_state parser::get_parsed_state(std::size_t offset) const
	{
		subtree_state ret;
		ret.reserve(this->subtrees.size() - offset);
		for(auto iter = this->subtrees.begin() + offset; iter != this->subtrees.end(); iter++)
		{
			ret.push_back(subtree_index{.idx = iter->get()->hash(), .name_hint = iter->get()->name()});
		}
		if(this->lookahead.has_value())
		{
			ret.push_back(subtree_index{.idx = syntax::node::unparsed_token{this->lookahead.value(), true}.hash(), .name_hint = "lookahead token"});
		}
		return ret;
	}

	reducer parser::make_reducer(std::size_t offset)
	{
		return {.subtrees = this->subtrees, .idx = offset};
	}

	syntax::node_ptr parser::get_output()
	{
		diag::assert_that(this->subtrees.empty(), error_code::nyi, "{} remaining subtrees that were never sent to the output AST", this->subtrees.size());
		return std::move(this->output);
	}

	syntax::node_ptr tokens(lex::const_token_view toks)
	{
		parser state{toks};
		state.parse();
		return state.get_output();
	}
}