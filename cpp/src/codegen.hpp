#ifndef PSYC_CODEGEN_HPP
#define PSYC_CODEGEN_HPP
#include "semal.hpp"

namespace code
{
	struct output
	{
		void* codegen_handle = nullptr;
		std::string module_name;
		std::string dump_ir() const;
		std::string get_output_filename() const;
		void write_to_object_file(std::filesystem::path output_dir);
	};

	output generate(const semal::output& input, std::string module_name = "<unnamed_module>");

	struct state
	{
		std::unordered_map<std::filesystem::path, output> codegend_input_files = {};
	};
}

#endif // PSYC_CODEGEN_HPP