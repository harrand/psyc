#ifndef PSYC_SESSION_HPP
#define PSYC_SESSION_HPP
#include "settings.hpp"
#include "lex.hpp"
#include "ast.hpp"
#include "semantic.hpp"

#include <string>
#include <vector>
#include <filesystem>

constexpr const char* default_target = "build";

struct session
{
	std::vector<std::string> input_files = {};
	std::vector<lexer::tokens> lexed_files = {};
	std::vector<semantic::state> analysed_files = {};
	std::vector<std::filesystem::path> object_files = {};
	std::vector<ast> parsed_files = {};
	std::string output_dir = "";
	std::string target = default_target;
	link_output link = link_output::none;
	std::string linker = "";
	flag_t flags = {};
	std::string target_triple = "";
};

#endif // PSYC_SESSION_HPP