#include "parser.hpp"
#include "parse.hpp"
#include "diag.hpp"
#include "profile.hpp"

namespace parse
{
	parser::parser(lex::output tokens):
	tokens(tokens.tokens),
	unscanned_tokens(this->tokens),
	source(tokens.psy_source)
	{
		this->output.payload = syntax::root{.source_file = this->tokens.front().meta_srcloc.file};
	}

	void parser::parse()
	{
		while(this->step()){}
	}

	bool parser::step()
	{
		PROFZONE("parser step");
		PROFTEXTF("%zu", this->total_reduction_count);
		for(std::size_t i = 0; i < this->subtrees.size(); i++)
		{
			auto state = this->get_parsed_state(i);
			if(!state.empty())
			{
				reduction reduc = find_reduction(state);
				if(!reduc.is_null())
				{
					PROFZONE("reduce");
					result res = reduc.reduce_fn(this->make_reducer(i));
					this->total_reduction_count++;
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
							PROFZONE("send to output");
							auto ptr = std::move(this->subtrees[i + res.offset]);
							this->subtrees.erase(this->subtrees.begin() + i + res.offset, this->subtrees.begin() + i + state.size());
							this->output.children().push_back(ptr);
							return true;
						}
						break;
						case result::type::error:
						{
							const syntax::node& cur = this->subtrees[i + res.offset];
							std::string_view relevant_src = this->source;
							const auto& loc = cur.loc();
							for(std::size_t i = 1; i < loc.line; i++)
							{
								relevant_src = relevant_src.substr(relevant_src.find_first_of('\n'));
							}
							relevant_src = relevant_src.substr(loc.column - 1, relevant_src.substr(loc.column - 1).find_first_of('\n'));
							std::string bottom_text = "└─";
							std::string mid_text = "│ ";
							for(std::size_t i = 1; i < loc.column; i++)
							{
								bottom_text += "─";
								mid_text += "═";
							}
							mid_text += "│";
							if(relevant_src.size() > loc.column)
							{
								for(std::size_t i = 0; i < relevant_src.size() - loc.column; i++)
								{
									mid_text += "═";
								}
							}
							bottom_text += "┘";
							diag::error(error_code::parse, "within {}\n{}\n┌──[{}]\n│\n│ {}\n{}\n{}", cur.name(), res.errmsg, cur.loc().to_string(), relevant_src.substr(0, cur.to_string().size()), mid_text, bottom_text);
							return false;
						}
						break;
						case result::type::silent_reject: this->silent_rejection_count++; continue; break;
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
		PROFZONE("shift");
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

	subtree_view parser::get_parsed_state(std::size_t offset) const
	{
		return subtree_view{this->subtrees}.subspan(offset);
	}

	reducer parser::make_reducer(std::size_t offset)
	{
		return {.subtrees = this->subtrees, .idx = offset};
	}

	syntax::node parser::get_output()
	{
		if(!(this->subtrees.size() == 1 && this->subtrees.front().hash() == syntax::node{.payload = syntax::unparsed_token{.tok = {.t = lex::type::source_begin}}}.hash()))
		{
			diag::error_nonblocking(error_code::parse, "{} remaining subtrees that were never sent to the output AST. remaining subtree ASTs:\n{{\n", this->subtrees.size());
			for(const auto& tree : this->subtrees)
			{
				tree.pretty_print();
			}
			std::cout << "\n}" << std::endl;
			diag::detail::on_error();
		}
		return std::move(this->output);
	}

	syntax::node tokens(lex::output tokens)
	{
		parser state{tokens};
		state.parse();
		return state.get_output();
	}
}