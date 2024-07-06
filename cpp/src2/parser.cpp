#include "parser.hpp"
#include "parse.hpp"
#include "diag.hpp"

namespace parse
{
	parser::parser(lex::output tokens):
	tokens(tokens.tokens),
	unscanned_tokens(this->tokens),
	source(tokens.psy_source)
	{
		this->output = std::make_unique<syntax::node::root>(this->tokens.front().meta_srcloc.file);
	}

	void parser::parse()
	{
		while(this->step()){}
	}

	bool parser::step()
	{
		for(std::size_t i = 0; i < this->subtrees.size(); i++)
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
						case result::type::send_to_output:
						{
							auto ptr = std::move(this->subtrees[i + res.offset]);
							this->subtrees.erase(this->subtrees.begin() + i + res.offset, this->subtrees.begin() + i + state.size());
							this->output->children.push_back(std::move(ptr));
							return true;
						}
						break;
						case result::type::error:
						{
							const syntax::node_ptr& cur = this->subtrees[i];
							std::string_view relevant_src = this->source;
							const auto& loc = cur->loc;
							for(std::size_t i = 0; i < loc.line; i++)
							{
								relevant_src = relevant_src.substr(relevant_src.find_first_of('\n'));
							}
							relevant_src = relevant_src.substr(loc.column, relevant_src.substr(loc.column).find_first_of('\n'));
							diag::error(error_code::parse, "within {}\n{}\n┌──[{}]\n│\n├── {}", cur->name(), res.errmsg, cur->loc.to_string(), relevant_src.substr(0, cur->to_string().size()));
							return false;
						}
						break;
						case result::type::silent_reject: continue; break;
					}
				}
				else
				{
					// continue
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
		return ret;
	}

	reducer parser::make_reducer(std::size_t offset)
	{
		return {.subtrees = this->subtrees, .idx = offset};
	}

	syntax::node_ptr parser::get_output()
	{
		if(!(this->subtrees.size() == 1 && this->subtrees.front()->hash() == syntax::node::unparsed_token{{.t = lex::type::source_begin}}.hash()))
		{
			diag::error_nonblocking(error_code::parse, "{} remaining subtrees that were never sent to the output AST. remaining subtree ASTs:\n{{\n", this->subtrees.size());
			for(const auto& tree : this->subtrees)
			{
				tree->pretty_print();
			}
			std::cout << "\n}" << std::endl;
			diag::detail::on_error();
		}
		return std::move(this->output);
	}

	syntax::node_ptr tokens(lex::output tokens)
	{
		parser state{tokens};
		state.parse();
		return state.get_output();
	}
}