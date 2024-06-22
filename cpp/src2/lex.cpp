#include "lex.hpp"
#include "diag.hpp"
#include <cstddef>
#include <limits>
#include <filesystem>
#include <string_view>
#include <fstream>

namespace lex
{
	constexpr std::size_t npos = std::numeric_limits<std::size_t>::max();

	struct internal_state
	{
		std::filesystem::path filename;
		std::string_view source;

		unsigned int line = 1;
		unsigned int col = 1;
		std::size_t cursor = 0;

		void advance(std::size_t count = 1)
		{
			this->col += count;
			this->cursor += count;
		}

		template<typename... Ts>
		void error(std::format_string<Ts...> fmt, Ts&&... ts)
		{
			srcloc curloc{.file = this->filename, .line = this->line, .column = this->col};
			std::string msg = std::format(fmt, std::forward<Ts>(ts)...);

			diag::error(error_code::lex, "at {}, {}", curloc.to_string(), msg);
		}
	};

	token tokenise_once(internal_state& state, std::string_view data);

	output tokenise(std::filesystem::path psy_file)
	{
		tokens_list tokens = {};
		internal_state state;

		std::ifstream fstr(psy_file);
		diag::assert_that(fstr.good(), error_code::badargs, "could not open input file \"{}\"", psy_file.string());
		// slurp the whole file into a string.
		std::stringstream buffer;
		buffer << fstr.rdbuf();
		std::string str = buffer.str();
		if(str.empty())
		{
			diag::warning("empty file: {}", psy_file.filename().string());
		}
		state.filename = psy_file;
		state.source = str;

		while(state.cursor < str.size())
		{
			std::string_view data = str.data() + state.cursor;
			const srcloc curloc
			{
				.file = psy_file,
				.line = state.line,
				.column = state.col
			};

			token tok = tokenise_once(state, data);
			if(tok.t == type::_undefined)
			{
				state.error("ill-defined token(s) detected");
			}
			tok.meta_srcloc = curloc;
		}
		return
		{
			.tokens = tokens,
			.psy_source = str
		};
	}

	token tokenise_once(internal_state& state, std::string_view data)
	{
		while(data.starts_with("\n"))
		{
			state.col = 0;
			state.line++;
			state.cursor++;
		}
		for(int i = 0; i < static_cast<int>(type::_count); i++)
		{
			auto t = static_cast<type>(i);
			std::string_view name = type_name[i];
			if(!name.empty())
			{
				// just check for "name" at the start
				if(data.starts_with(name))
				{
					// advance the state. for nearly everything, advance by the lexeme size. for some things (e.g line comment) we have a different behaviour.
					switch(t)
					{
						case type::comment:
						[[fallthrough]];
						case type::doc_comment:
							// advance till end of line
							diag::nyi("single-line comments");
						break;
						case type::mlcomment:
						[[fallthrough]];
						case type::mldoc_comment:
							// advance till "*/" is found
							diag::nyi("multi-line comments");
						break;
						default:
							state.advance(name.size());
						break;
					}
					return token{.t = t, .lexeme = std::string{name}};
				}
			}
			if(name.empty())
			{
				// its not a trivial check. need to check specifics.
				switch(t)
				{
					case type::identifier:
						diag::nyi("identifiers");
					break;
					case type::integer_literal:
						diag::nyi("integer-literals");
					break;
					case type::decimal_literal:
						diag::nyi("decimal-literals");
					break;
					case type::string_literal:
						diag::nyi("string-literals");
					break;
					case type::char_literal:
						diag::nyi("char-literals");
					break;
					case type::bool_literal:
						diag::nyi("bool-literals");
					break;
					case type::null_literal:
						diag::nyi("null-literals");
					break;
					default:
						state.error("unknown token(s) {}", name.substr(0, std::min(name.size(), std::size_t{8})));
					break;
				}
			}
		}
		return token{.t = type::_undefined};
	}
}