#include <span>
#include <string_view>
#include <vector>
#include <format>
#include <utility>
#include <iostream>
#include <filesystem>

enum class output
{
	unknown,
	llvm_ir,
	x86_64_asm,
};

struct session
{
	std::vector<std::string> input_files = {};
	output type = output::unknown;
};

template<typename... Ts>
void print(Ts&&... ts)
{
    ((std::cout<<std::forward<Ts>(ts)<<','),...);
    std::cout<<'\n';
}

template<typename... Ts>
void message(Ts&&... ts)
{
	std::cout << "message: ";
	print(std::forward<Ts>(ts)...);
}

template<typename... Ts>
void warning(Ts&&... ts)
{
	std::cout << "warning: ";
	print(std::forward<Ts>(ts)...);
}

template<typename... Ts>
void error(Ts&&... ts)
{
	std::cout << "error: ";
	print(std::forward<Ts>(ts)...);
	std::exit(-1);
}


template<typename... Ts>
void assert_that(bool expr, std::string msg)
{
	if(!expr)
	{
		error(msg);
	}
}

void parse_args(std::span<const std::string_view> args, session& ses)
{
	ses.type = output::unknown;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const std::string_view& arg = args[i];
		const std::string_view* argnext = nullptr;
		if(i < (args.size() - 1))
		{
			argnext = &args[i + 1];
		}
		if(arg.starts_with("-ot") || arg.starts_with("--output_type"))
		{
			if(*argnext == "llvm")
			{
				ses.type = output::llvm_ir;
			}
			else if(*argnext == "x86_64_asm")
			{
				ses.type = output::x86_64_asm;
			}
			else
			{
				error(std::format("unknown output type `{} (from \"{}\")", *argnext, std::string(arg) + " " + std::string(*argnext)));
			}
			i++;
			continue;
		}
		else
		{
			assert_that(arg.ends_with(".psy"), std::format("input file \"{}\" does not end with `.psy`", arg));
			ses.input_files.push_back(std::string{arg});
		}
	}
}

int main(int argc, char** argv)
{
	const std::vector<std::string_view> args(argv + 1, argv + argc);
	session ses;
	parse_args(args, ses);
	assert_that(ses.type != output::unknown, "no output type specified (`-ot / --output_type`)");
	assert_that(ses.input_files.size(), "no input files specified");
	for(const auto& input_file : ses.input_files)
	{
		assert_that(std::filesystem::exists(input_file), std::format("cannot find input file `{}`", input_file));
		// tokens = lex(input_file)
	}
}
