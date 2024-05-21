#ifndef PSYC_CODEGEN_HPP
#define PSYC_CODEGEN_HPP
#include "semal.hpp"
#include "build.hpp"

namespace code
{
	struct output
	{
		void* codegen_handle = nullptr;
		std::string module_name;
		std::string dump_ir() const;
		std::string get_output_filename() const;
		void write_to_object_file(const build::info& binfo) const;
	};

	// initialise and terminate *once*, at the beginning and end of the compiler program respectively.
	void static_initialise();
	void static_terminate();

	void* unsafe_release();
	void cleanup();
	// perform code generation. `codegen_handle` member of the returned output remains valid until you invoke `code::cleanup()`. ideally, you generate, then dump-ir/write-object, cleanup and then codegen another file.
	output generate(const ast& tree, const semal::output& input, const build::info& binfo, std::string module_name = "<unnamed_module>");

	struct state
	{
		std::unordered_map<std::filesystem::path, output> codegend_input_files = {};
	};
}

#endif // PSYC_CODEGEN_HPP