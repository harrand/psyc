#ifndef PSYC_SESSION_HPP
#define PSYC_SESSION_HPP
#include "settings.hpp"
#include "lex.hpp"
#include "ast.hpp"

#include <string>
#include <vector>
#include <filesystem>

struct session
{
	std::vector<std::string> input_files = {};
	std::vector<lexer::tokens> lexed_files = {};
	std::vector<std::filesystem::path> object_files = {};
	std::vector<ast> parsed_files = {};
	std::string output_dir = "";
	std::string target = "build";
	link_output link = link_output::none;
	std::string linker = "";
	flag_t flags = {};
};

#endif // PSYC_SESSION_HPP