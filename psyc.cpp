#include <print>
#include <cstdlib>
#include <source_location>
#include <stacktrace>
#include <span>
#include <filesystem>
#include <format>
#include <fstream>
#include <sstream>
#include <cstdint>
#include <cstddef>
#include <cstdlib>
#include <array>
#include <charconv>
#include <variant>
#include <unordered_map>
#include <ranges>
#include <functional>
#include <unordered_set>
#include <chrono>
#include <deque>
#include <memory>
#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"

#include "llvm/IR/Verifier.h"

#include <llvm/IR/LegacyPassManager.h>

#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/Scalar/LoopIdiomRecognize.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"

#define STRINGIFY(...) #__VA_ARGS__

std::string get_preload_source();

std::vector<llvm::DIScope*> lexical_blocks = {};

template <typename T>
class box
{
	// Wrapper over unique_ptr.
	std::unique_ptr<T> _impl;

public:
	// Automatic construction from a `T`, not a `T*`.
	box(T &&obj) : _impl(new T(std::move(obj))) {}
	box(const T &obj) : _impl(new T(obj)) {}

	// Copy constructor copies `T`.
	box(const box &other) : box(*other._impl) {}
	box &operator=(const box &other)
	{
		*_impl = *other._impl;
		return *this;
	}

	// unique_ptr destroys `T` for us.
	~box() = default;

	explicit operator T() const
	{
		return *this->_impl;
	}

	// Access propagates constness.
	T &operator*() { return *_impl; }
	const T &operator*() const { return *_impl; }

	T *operator->() { return _impl.get(); }
	const T *operator->() const { return _impl.get(); }

	bool operator==(const box<T>& rhs) const
	{
		if(this->_impl != nullptr && rhs._impl != nullptr)
		{
			return *this->_impl == *rhs._impl;
		}
		return this->_impl.get() == rhs._impl.get();
	}
};

template<typename T>
box(T) -> box<T>;

struct string_hash
{
    using is_transparent = void;
    [[nodiscard]] size_t operator()(const char *txt) const
	{
        return std::hash<std::string_view>{}(txt);
    }
    [[nodiscard]] size_t operator()(std::string_view txt) const
	{
        return std::hash<std::string_view>{}(txt);
    }
    [[nodiscard]] size_t operator()(const std::string &txt) const
	{
        return std::hash<std::string>{}(txt);
    }
    [[nodiscard]] size_t operator()(const std::filesystem::path &txt) const
	{
        return std::hash<std::filesystem::path>{}(txt);
    }
};

template<typename T>
using string_map = std::unordered_map<std::string, T, string_hash, std::equal_to<>>;

template<typename T>
using path_map = std::unordered_map<std::filesystem::path, T, string_hash, std::equal_to<>>;

path_map<llvm::DIFile*> debug_files;

template<typename T, typename V, std::size_t index_leave_blank = 0>
consteval int payload_index()
{
	static_assert(index_leave_blank < std::variant_size_v<V>, "unknown payload index type");
	if constexpr(std::is_same_v<std::decay_t<T>, std::decay_t<std::variant_alternative_t<index_leave_blank, V>>>)
	{
		return index_leave_blank;
	}
	else
	{
		return payload_index<T, V, index_leave_blank + 1>();
	}
}

#define IS_A(variant, type) ((variant).index() == payload_index<type, std::decay_t<decltype((variant))>>())
#define AS_A(variant, type) std::get<type>(variant)


std::chrono::time_point<std::chrono::system_clock> now;
void timer_restart()
{
	now = std::chrono::system_clock::now();
}

std::uint64_t elapsed_time()
{
	auto right_now = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
	return right_now - std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count();
}

void crash()
{
	std::printf("\033[1;31mcompilation aborted due to error\033[0m");
	asm volatile("int3");
	std::exit(-1);
}

void generic_panic(const char* msg, std::source_location loc, std::stacktrace trace, std::format_args&& args)
{
	std::print("\033[1;31minternal compiler error: {}\n\tat {}({}:{}) `{}`\nstacktrace:\n==========\n{}\n=========\033[0m", std::vformat(msg, args), loc.file_name(), loc.line(), loc.column(), loc.function_name(), std::to_string(trace));
	crash();
}

struct slice	
{
	std::size_t offset = 0;
	std::size_t length = 0;
};

struct srcloc
{
	std::filesystem::path file;
	unsigned int line;
	unsigned int column;
	std::size_t cursor;
};

#define panic(msg, ...) generic_panic(msg, std::source_location::current(), std::stacktrace::current(), std::make_format_args(__VA_ARGS__))
#define panic_ifnt(cond, msg, ...) if(!(cond)){panic(msg, __VA_ARGS__);}

template <>
struct std::formatter<srcloc> : std::formatter<std::string>
{
	auto format(srcloc loc, format_context& ctx) const
	{
		if(loc.file == std::filesystem::path{})
		{
			return formatter<string>::format("", ctx);
		}
		return formatter<string>::format(std::format("at {}({}:{})", loc.file.string(), loc.line, loc.column), ctx);
	}
};

// todo: remove this code when c++26 is used.
template <>
struct std::formatter<std::filesystem::path, char> : std::formatter<std::string, char> {
    auto format(const std::filesystem::path& path, std::format_context& ctx) const
	{
        return std::formatter<std::string, char>::format(path.string(), ctx);
    }
};

std::string_view quote_source(std::string_view source_code, srcloc begin_loc, srcloc end_loc)
{
	panic_ifnt(begin_loc.file == end_loc.file, "attempted to quote source where begin and end locations are in different files (begin {}, end {})", begin_loc, end_loc);
	return {source_code.data() + begin_loc.cursor, end_loc.cursor - begin_loc.cursor};
}

std::string format_source(std::string_view source_code, srcloc begin_loc, srcloc end_loc)
{
	const char* begin_point = source_code.data() + begin_loc.cursor;
	std::size_t len = end_loc.cursor - begin_loc.cursor;

	std::string_view first_part{source_code.data(), begin_point};
	auto closest_previous_newline = std::ranges::find(first_part | std::views::reverse, '\n');
	std::size_t dst_from_prev_newline = 0;

	// the begin location will start on some line. let's make sure we have that whole line.
	if(closest_previous_newline != first_part.rend())
	{
		// there is a newline beforehand. lets add the beginning part of the line.
		begin_point = source_code.data() + std::distance(closest_previous_newline, first_part.rend());
		dst_from_prev_newline = (source_code.data() + begin_loc.cursor) - *&begin_point;
		len += dst_from_prev_newline;
	}

	// same with the end location.
	std::string_view next_part{source_code.data() + end_loc.cursor - 1, source_code.data() + source_code.size() - 1};
	auto next_newline = std::ranges::find(next_part, '\n');
	if(next_newline != next_part.end())
	{
		std::size_t dst_to_next_newline = std::distance(next_part.begin(), next_newline);
		len += dst_to_next_newline;
	}

	std::string_view whole_line{begin_point, len};
	// split the source snippet into lines, return them in a formatted manner.
	// also underline the target line and point to the exact start of the source.
	std::size_t line = begin_loc.line;
	std::stringstream s(std::string{whole_line});
	std::string ret;
	std::size_t i = 0;
	for(std::string cur_line; std::getline(s, cur_line);)
	{
		std::size_t curlinenum = line + i++;
		ret += std::format("\t{} | {}\n", curlinenum, cur_line);
		if(i == 1)
		{
			// add an underline
			std::size_t line_char_count = std::to_string(curlinenum).size();
			std::string underline = "\t";
			for(std::size_t j = 0; j < line_char_count + dst_from_prev_newline; j++)
			{
				underline += ' ';
			}
			underline += "   ^\n";
			ret += underline;
		}
	}
	return ret;
}

	std::string escape(std::string_view literal)
	{
		std::string ret;
		static const std::unordered_map<std::string_view, char> escape_map = {
			{"\\0", '\0'}, // Null terminator
			{"\\a", '\a'}, // Bell (alert)
			{"\\b", '\b'}, // Backspace
			{"\\f", '\f'}, // Formfeed
			{"\\n", '\n'}, // Newline (line feed)
			{"\\r", '\r'}, // Carriage return
			{"\\t", '\t'}, // Horizontal tab
			{"\\v", '\v'}, // Vertical tab
			{"\\\\", '\\'}, // Backslash
			{"\\'", '\''}, // Single quote
			{"\\\"", '\"'}, // Double quote
			{"\\?", '\?'}  // Question mark
		};
		if(literal.size() == 1)
		{
			return std::string{literal};
		}
		for(std::size_t i = 0; i < literal.size(); i++)
		{
			std::string_view substr{literal.data() + i, 2};
			auto iter = escape_map.find(substr);
			if(iter != escape_map.end())
			{
				ret += iter->second;
				i++;
			}
			else
			{
				ret += literal[i];
			}
		}
		return ret;
	}

// user-facing errors

enum class err
{
	argparse,
	lex,
	parse,
	semal,
	type,
	meta,
	build_system,
	codegen,
	assemble,
	link,
	_unknown
};
const char* err_names[] =
{
	"argument",
	"lex",
	"parse",
	"semantic analysis",
	"type",
	"meta program",
	"build system",
	"code generation",
	"assembly",
	"linker",
	"unknown",
};

void generic_error(err ty, const char* msg, srcloc where, bool should_crash, std::format_args&& args)
{
	std::println("\033[1;31m{} error {}\033[0m: {}", err_names[static_cast<int>(ty)], where, std::vformat(msg, args));
	
	if(should_crash)
	{
		crash();
	}
}

void generic_warning(err ty, const char* msg, srcloc where, std::format_args&& args)
{
	std::println("\033[1;33m{} warning {}\033[0m: {}", err_names[static_cast<int>(ty)], where, std::vformat(msg, args));
}

void generic_msg(err ty, const char* msg, srcloc where, std::format_args&& args)
{
	std::println("\033[1;34m{} message {}\033[0m: {}", err_names[static_cast<int>(ty)], where, std::vformat(msg, args));
}

#define COMPILER_STAGE
#define error(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, true, std::make_format_args(__VA_ARGS__))
#define warning(loc, msg, ...) generic_warning(err::COMPILER_STAGE, msg, loc, std::make_format_args(__VA_ARGS__))
#define msg(loc, msg, ...) generic_msg(err::COMPILER_STAGE, msg, loc, std::make_format_args(__VA_ARGS__))
#define error_nonblocking(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, false, std::make_format_args(__VA_ARGS__))
#define error_ifnt(cond, loc, msg, ...) if(!(cond)){error(loc, msg, __VA_ARGS__);}
std::uint64_t time_setup = 0, time_lex = 0, time_parse = 0, time_semal = 0, time_codegen = 0, time_assemble = 0, time_link = 0;

struct type_t;
struct sval;
struct codegen_t
{
	std::unique_ptr<llvm::LLVMContext> ctx = nullptr;
	std::unique_ptr<llvm::Module> mod = nullptr;
	std::unique_ptr<llvm::IRBuilder<>> ir = nullptr;
	std::vector<std::unique_ptr<llvm::GlobalVariable>> global_variable_storage;

	std::unique_ptr<llvm::DIBuilder> debug;
	llvm::DICompileUnit* dbg = nullptr;

	llvm::GlobalVariable* declare_global_variable(std::string_view name, type_t ty, sval val, bool external_linkage = false, bool is_thread_local = false);
} codegen;


//////////////////////////// ARGPARSE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE argparse

enum class target
{
	executable,
	library,
	object
};

struct compile_args
{
	bool should_print_help = false;
	bool verbose_lex = false;
	bool verbose_ast = false;
	bool verbose_parse = false;
	bool verbose_codegen = false;
	bool verbose_link = false;
	std::filesystem::path build_file = {};
	std::filesystem::path output_dir = {};
	std::vector<std::filesystem::path> link_libraries = {};
	std::string output_name = "out";
	target output_type = target::object;
	unsigned int optimisation_level = 0;
	std::string target_triple = "";
	bool debug_symbols = true;
	std::string build_config = "debug";
};

compile_args parse_args(std::span<const std::string_view> args)
{
	compile_args ret;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const auto& arg = args[i];
		auto argnext = [allowed = i < args.size() - 1, &i, &args](){if(!allowed){error({}, "argument missing value");} return args[++i];};

		if(arg == "-h" || arg == "--help")
		{
			ret.should_print_help = true;
		}
		else if(arg == "--verbose-lex")
		{
			ret.verbose_lex = true;
		}
		else if(arg == "--verbose-ast")
		{
			ret.verbose_ast = true;
		}
		else if(arg == "--verbose-parse")
		{
			ret.verbose_parse = true;
		}
		else if(arg == "--verbose-codegen")
		{
			ret.verbose_codegen = true;
		}
		else if(arg == "--verbose-link")
		{
			ret.verbose_link = true;
		}
		else if(arg == "--verbose-all")
		{
			ret.verbose_lex = true;
			ret.verbose_ast = true;
			ret.verbose_parse = true;
			ret.verbose_codegen = true;
			ret.verbose_link = true;
		}
		else if(arg == "-o")
		{
			ret.output_dir = argnext();
		}
		else if(arg == "-t")
		{
			ret.target_triple = argnext();
		}
		else if(arg == "-c")
		{
			ret.build_config = argnext();
		}
		else
		{
			if(arg.starts_with("-"))
			{
				error({}, "unknown option {}", arg);
			}
			ret.build_file = arg;
			error_ifnt(std::filesystem::exists(ret.build_file), {}, "could not find build file {}", arg);
		}
	}

	if(!ret.should_print_help)
	{
	error_ifnt(!ret.build_file.empty(), {}, "no file specified");
	}
	if(ret.output_dir.empty())
	{
		ret.output_dir = ".";
	}
	if(ret.target_triple.empty())
	{
		const char* triple = std::getenv("PSYC_TARGET_TRIPLE");
		if(triple != nullptr)
		{
			ret.target_triple = triple;
		}
	}
	return ret;
}

void print_help()
{
	constexpr auto help_string = R"(OVERVIEW: Psy Compiler
USAGE: psyc OPTION... FILE

OPTION:
	<empty>
	--help
	-h				display version info and help

	--verbose-all	output *all* compiler meta information (for debugging purposes)
	--verbose-lex	output lexer meta information (print all tokens)

FILE:
	- Unlike other compilers, you specify only one file, instead of many.
		- That one source file should act as a build system for your whole project.
	- Can be an absolute path, or a path relative to the current working directory.
	- File must contain valid .psy source code.
	- Recommended to end in .psy, but does not have to.
	)";
	std::print(help_string);
}

//////////////////////////// TYPE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE type

enum typequal : int
{
	typequal_none = 0b0000,
	typequal_mut = 0b0001,
	typequal_weak = 0b0010,
	typequal_static = 0b0100
};

constexpr typequal operator|(typequal lhs, typequal rhs)
{
	return static_cast<typequal>(static_cast<int>(lhs) | static_cast<int>(rhs));
}

constexpr bool operator&(typequal lhs, typequal& rhs)
{
	return static_cast<int>(lhs) & static_cast<int>(rhs);
}

constexpr typequal operator~(typequal rhs)
{
	return static_cast<typequal>(~static_cast<int>(rhs));
}

constexpr const char* typequal_names[] =
{
	"",
	" mut",
	" weak",
	" static"
};
const char* get_typequal_name(typequal t)
{
	std::size_t i = 1;
	auto tval = static_cast<int>(t);
	while(tval >>= 1)
	{
		i++;
	}
	return typequal_names[i];
}

struct prim_ty
{
	enum class type
	{
		s64,
		s32,
		s16,
		s8,
		u64,
		u32,
		u16,
		u8,

		boolean,
		
		f64,
		f32,

		v0,
		_count
	};
	static constexpr std::array<const char*, static_cast<int>(type::_count)> type_names =
	{
		"s64",
		"s32",
		"s16",
		"s8",
		"u64",
		"u32",
		"u16",
		"u8",

		"bool",

		"f64",
		"f32",

		"v0"
	};
	type p;

	bool is_numeric() const
	{
		return
			this->p == type::s64 ||
			this->p == type::s32 ||
			this->p == type::s16 ||
			this->p == type::s8  ||
			this->p == type::u64 ||
			this->p == type::u32 ||
			this->p == type::u16 ||
			this->p == type::u8  ||
			
			this->p == type::f64 ||
			this->p == type::f32;
	}

	bool is_integral() const
	{
		return
			this->p == type::s64 ||
			this->p == type::s32 ||
			this->p == type::s16 ||
			this->p == type::s8  ||
			this->p == type::u64 ||
			this->p == type::u32 ||
			this->p == type::u16 ||
			this->p == type::u8;
	}

	bool is_signed_integral() const
	{
		return this->is_integral() && 
			this->p == type::s64 ||
			this->p == type::s32 ||
			this->p == type::s16 ||
			this->p == type::s8;
	}

	bool is_unsigned_integral() const
	{
		return this->is_integral() && 
			this->p == type::u64 ||
			this->p == type::u32 ||
			this->p == type::u16 ||
			this->p == type::u8;
	}

	bool is_floating_point() const
	{
		return this->is_numeric() && !this->is_integral();
	}

	std::size_t floating_point_size() const
	{
		switch(this->p)
		{
			case type::f64:
				return 64;
			break;
			case type::f32:
				return 32;
			break;
			default:
				return 0;
			break;
		}
	}

	std::size_t integral_size() const
	{
		switch(this->p)
		{
			case type::u64:
			[[fallthrough]];
			case type::s64:
				return 64;
			break;
			case type::u32:
			[[fallthrough]];
			case type::s32:
				return 32;
			break;
			case type::u16:
			[[fallthrough]];
			case type::s16:
				return 16;
			break;
			case type::u8:
			[[fallthrough]];
			case type::s8:
				return 8;
			break;
			default:
				return 0;
			break;
		}
	}

	std::string name() const
	{
		return type_names[static_cast<int>(p)];
	}
	bool operator==(const prim_ty& rhs) const = default;
};

struct type_t;

struct struct_ty
{
	std::string label;
	string_map<box<type_t>> members = {};
	std::vector<std::string> member_order = {};
	std::string name() const
	{
		return label;
	}
	bool operator==(const struct_ty& rhs) const = default;
};

namespace std
{
    template <> struct hash<struct_ty>
    {
        size_t operator()(const struct_ty& ty) const
        {
			std::size_t ret = 30457934875;
			for(const auto& [name, ty] : ty.members)
			{
				ret ^= std::hash<std::string>{}(name);
			}
			return ret;
        }
    };
} // namespace std


struct enum_ty
{
	box<type_t> underlying_ty;
	string_map<std::int64_t> entries = {};
	std::string name() const
	{
		return "enum";
	}
	bool operator==(const enum_ty& rhs) const = default;
};

struct ptr_ty
{
	box<type_t> underlying_ty;
	std::string name() const;
	static ptr_ty ref(const type_t& t);
	bool operator==(const ptr_ty& rhs) const = default;
};

struct arr_ty
{
	box<type_t> underlying_ty;
	std::size_t array_length;

	std::string name() const;
	static arr_ty of(const type_t&, std::size_t length);
	bool operator==(const arr_ty& rhs) const = default;
};

struct fn_ty
{
	std::vector<type_t> params;
	box<type_t> return_ty;
	std::string name() const;
	bool operator==(const fn_ty& rhs) const = default;
};

struct meta_ty
{
	std::string underlying_typename;
	bool operator==(const meta_ty& rhs) const = default;

	std::string name() const;
};

struct type_t
{
	using payload_t = std::variant
	<
		std::monostate,
		prim_ty,
		struct_ty,
		enum_ty,
		ptr_ty,
		arr_ty,
		fn_ty,
		meta_ty
	>;
	payload_t payload;
	typequal qual = typequal_none;

	static type_t create_primitive_type(prim_ty::type p)
	{
		return type_t{.payload = prim_ty{.p = p}};
	}

	static type_t create_void_type()
	{
		return create_primitive_type(prim_ty::type::v0);
	}

	static type_t create_pointer_type(const type_t& pointee)
	{
		return type_t{.payload = ptr_ty{.underlying_ty = pointee}};
	}

	static type_t create_array_type(const type_t& element_ty, std::size_t len)
	{
		return type_t{.payload = arr_ty{.underlying_ty = element_ty, .array_length = len}};
	}

	type_t add_weak()
	{
		type_t cpy = *this;
		cpy.qual = cpy.qual | typequal_weak;
		return cpy;
	}

	bool is_convertible_to(const type_t& rhs, bool second_attempt = false) const
	{
		std::string lhs_name = this->name();
		std::string rhs_name = rhs.name();
		const bool either_is_weak = (this->qual & typequal_weak) || (rhs.qual & typequal_weak);
		#define lhs_is(x) this->payload.index() == payload_index<x, payload_t>()
		#define rhs_is(x) rhs.payload.index() == payload_index<x, payload_t>()
		if(this->payload == rhs.payload)
		{
			return true;
		}
		if(lhs_is(prim_ty))
		{
			if(rhs_is(prim_ty))
			{
				const auto& lhs_prim = std::get<prim_ty>(this->payload);
				const auto& rhs_prim = std::get<prim_ty>(rhs.payload);

				// if the prims are exactly the same, yes
				if(lhs_prim.p == rhs_prim.p)
				{
					return true;
				}

				if(either_is_weak)
				{
					// different prims might convert if at least one is weak..
					// all numeric prims can be converted
					if(lhs_prim.is_numeric() && rhs_prim.is_numeric())
					{
						return true;
					}
					// bool can be converted to any number.
					else if(lhs_prim.p == prim_ty::type::boolean && rhs_prim.is_numeric())
					{
						return true;
					}
					// v0 cannot convert to any other prim.
					else if(lhs_prim.p == prim_ty::type::v0)
					{
						return false;
					}
				}
				else
				{
					return false;
				}
			}
		}
		else if(lhs_is(struct_ty))
		{
			if(rhs_is(struct_ty))
			{
				const auto& lhs_struct = std::get<struct_ty>(this->payload);
				const auto& rhs_struct = std::get<struct_ty>(rhs.payload);
				// structs cannot convert to other structs.
				return lhs_struct == rhs_struct;
			}
			else
			{
				// structs cannot convert to any form of non structs.
				return false;
			}
		}
		else if(lhs_is(enum_ty))
		{
			if(rhs_is(enum_ty))
			{
				// enums are never convertible unless either is weak. if either is weak, then its convertible if the underlying types are convertible.
				if(this->qual & typequal_weak || rhs.qual & typequal_weak)
				{
					return std::get<enum_ty>(this->payload).underlying_ty->is_convertible_to(*std::get<enum_ty>(rhs.payload).underlying_ty);
				}
				else
				{
					const auto& lhs_enum = std::get<enum_ty>(this->payload);
					const auto& rhs_enum = std::get<enum_ty>(rhs.payload);
					// enums cannot convert to other enums.
					return lhs_enum == rhs_enum;
				}
			}
			else
			{
				// enums dont convert to any non-enums
				// except its underlying type (if weak)
				if(either_is_weak)
				{
					return std::get<enum_ty>(this->payload).underlying_ty->is_convertible_to(rhs);
				}
				else
				{
					return false;
				}
			}
		}
		else if(lhs_is(meta_ty))
		{
			if(rhs_is(meta_ty))
			{
				const auto& lhs_ty = std::get<meta_ty>(this->payload);
				const auto& rhs_ty = std::get<meta_ty>(rhs.payload);

				// if either of them have badtype as concrete, then it is a template type and the conversion is okay.
				if(lhs_ty.underlying_typename == type_t::badtype().name() || rhs_ty.underlying_typename == type_t::badtype().name())
				{
					return true;
				}
				// otherwise, types dont convert unless they are the same.
				return lhs_ty == rhs_ty;
			}
			else
			{
				// meta types dont convert to non-meta-types
				return false;
			}
		}
		else if(lhs_is(ptr_ty))
		{
			if(rhs_is(ptr_ty))
			{
				const auto& lhs_ptr = std::get<ptr_ty>(this->payload);
				const auto& rhs_ptr = std::get<ptr_ty>(rhs.payload);

				// should T& be convertible to T mut&
				// yes, but not the other way around
				auto lhs_underlying = *lhs_ptr.underlying_ty;
				auto rhs_underlying = *rhs_ptr.underlying_ty;
				bool lhs_mut = lhs_underlying.qual & typequal_mut;
				bool rhs_mut = rhs_underlying.qual & typequal_mut;
				if(!lhs_mut && rhs_mut && !either_is_weak)
				{
					// cannot convert from T mut& to X&, even if X == T
					return false;
				}

				// pointers always convert to one-another if they are the same or one of them is weak.
				// many of these conversions will be unsafe unless care is taken, like C.
				return lhs_underlying.payload == rhs_underlying.payload || either_is_weak;
			}
			else
			{
				// the only non-ptr type that can convert to a ptr is a u64 (if either are weak).
				if(either_is_weak)
				{
					if(rhs_is(prim_ty))
					{
						return std::get<prim_ty>(rhs.payload).p == prim_ty::type::u64;
					}
					else
					{
						return false;
					}
				}
				else
				{
					return false;
				}
			}
		}
		else if(lhs_is(fn_ty))
		{
			if(rhs_is(fn_ty))
			{
				// can functions convert to other functions?
				// only if the signatures exactly match.
				const auto& lhs_fn = std::get<fn_ty>(this->payload);
				const auto& rhs_fn = std::get<fn_ty>(rhs.payload);
				return lhs_fn == rhs_fn;
			}
			else
			{
				// function types cannot convert to non-function types.
				// with one exception: v0& this is how you get the address of a function.
				if(rhs_is(ptr_ty))
				{
					const auto& rhs_ptr_ty = std::get<ptr_ty>(rhs.payload);
					const auto& rhs_pointee = *rhs_ptr_ty.underlying_ty;
					if(rhs_pointee.payload.index() == payload_index<prim_ty, payload_t>())
					{
						return std::get<prim_ty>(rhs_pointee.payload).p == prim_ty::type::v0;
					}
				}
				return false;
			}
		}
		else
		{
			panic("unhandled check if two similar types can be converted (\"{}\" and \"{}\"). did you add a new type payload?", lhs_name, rhs_name);
		}
		#undef lhs_is
		#undef rhs_is
		// different type of types.
		// if we're not on the second attempt, try the other way around.
		if(!second_attempt)
		{
			return rhs.is_convertible_to(*this, true);
		}
		panic("dont know if {} can be converted to {}", lhs_name, rhs_name);
		return false;
	}

	llvm::Type* llvm() const;
	llvm::DIType* debug_llvm() const;

	std::string name() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
		{
			if constexpr(std::is_same_v<std::decay_t<decltype(arg)>, std::monostate>)
			{
				ret = "<bad type>";
			}
			else
			{
				ret = arg.name();	
			}
			
		}, this->payload);
		if(this->qual & typequal_mut)
		{
			ret += get_typequal_name(typequal_mut);
		}
		if(this->qual & typequal_weak)
		{
			ret += get_typequal_name(typequal_weak);
		}
		if(this->qual & typequal_static)
		{
			ret += get_typequal_name(typequal_static);
		}
		return ret;
	}

	static type_t badtype()
	{
		return {.payload = std::monostate{}};
	}

	bool is_badtype() const
	{
		return this->payload.index() == payload_index<std::monostate, decltype(this->payload)>();
	}

	bool is_void() const
	{
		return this->is_prim() && std::get<prim_ty>(this->payload).p == prim_ty::type::v0;
	}

	bool is_type() const
	{
		return this->payload.index() == payload_index<meta_ty, decltype(this->payload)>();
	}

	bool is_prim() const
	{
		return this->payload.index() == payload_index<prim_ty, decltype(this->payload)>();
	}

	bool is_struct() const
	{
		return this->payload.index() == payload_index<struct_ty, decltype(this->payload)>();
	}

	bool is_enum() const
	{
		return this->payload.index() == payload_index<enum_ty, decltype(this->payload)>();
	}

	bool is_ptr() const
	{
		return this->payload.index() == payload_index<ptr_ty, decltype(this->payload)>();
	}

	bool is_arr() const
	{
		return this->payload.index() == payload_index<arr_ty, decltype(this->payload)>();
	}

	bool is_fn() const
	{
		return this->payload.index() == payload_index<fn_ty, decltype(this->payload)>();
	}

	bool operator==(const type_t& rhs) const = default;
};

std::string ptr_ty::name() const
{
	if(this->underlying_ty->is_fn())
	{
		return this->underlying_ty->name();
	}
	return std::format("{}&", this->underlying_ty->name());
}

/*static*/ptr_ty ptr_ty::ref(const type_t& t)
{
	return {.underlying_ty = {t}};
}

std::string arr_ty::name() const
{
	return std::format("{}#{}", this->underlying_ty->name(), this->array_length);
}

/*static*/ arr_ty arr_ty::of(const type_t& t, std::size_t length)
{
	return{.underlying_ty = {t}, .array_length = length};
}

std::string fn_ty::name() const
{
	std::string params_str = "";
	for(std::size_t i = 0; i < this->params.size(); i++)
	{
		const auto& param_ty = this->params[i];
		params_str += param_ty.name();
		if(i < (this->params.size() - 1))
		{
			params_str += ", ";
		}
	}
	return std::format("func({}) -> {}", params_str, this->return_ty->name());
}

std::string meta_ty::name() const
{
	return std::format("meta type aka {}", this->underlying_typename);
}

struct ast_funcdef_expr;
using literal_val = std::variant<std::int64_t, double, char, std::string, bool>;
struct sval_zero_tag
{
	bool operator==(const sval_zero_tag& t)const = default;
	bool operator!=(const sval_zero_tag& t)const = default;
};

struct sval_null_tag
{
	bool operator==(const sval_null_tag& t)const = default;
	bool operator!=(const sval_null_tag& t)const = default;
};

struct sval
{
	using struct_val = string_map<sval>;
	using array_val = std::vector<sval>;
	std::variant<std::monostate, literal_val, struct_val, sval_zero_tag, sval_null_tag, array_val> val = std::monostate{};
	type_t ty;
	llvm::Value* ll = nullptr;
	const void* usrdata = nullptr;
	void* usrdata2 = nullptr;
	bool is_type = false;

	bool operator==(const sval& rhs) const = default;

	bool has_val() const
	{
		return this->val.index() != payload_index<std::monostate, decltype(val)>();
	}

	std::string value_tostring() const
	{
		std::string ret;
		if(this->val.index() == payload_index<literal_val, decltype(val)>())
		{
			std::visit([&ret](auto&& arg)
			{
				using T = std::decay_t<decltype(arg)>;
				if constexpr(std::is_same_v<T, std::string>)
				{
					ret = arg;
				}
				else
				{
					ret = std::to_string(arg);
				}
			}, std::get<literal_val>(this->val));
		}
		else
		{
			auto name = this->ty.name();
			panic("value_tostring for type {} is NYI", name);
		}
		return ret;
	}

	llvm::Value* convert_to(const type_t& rhs)
	{
		panic_ifnt(this->ll != nullptr, "codegen: cant convert from null");
		panic_ifnt(this->ty.is_convertible_to(rhs), "codegen: cant convert types coz you provided non-convertible types, idiot.");
		#define lhs_is(x) this->ty.payload.index() == payload_index<x, type_t::payload_t>()
		#define rhs_is(x) rhs.payload.index() == payload_index<x, type_t::payload_t>()
		if(this->ty.payload == rhs.payload)
		{
			return this->ll;
		}
		if(rhs_is(enum_ty))
		{
			auto rhs_enum_ty = AS_A(rhs.payload, enum_ty);
			return this->convert_to(rhs_enum_ty.underlying_ty->add_weak());
		}
		if(lhs_is(prim_ty))
		{
			if(rhs_is(prim_ty))
			{
				const auto& lhs_prim = std::get<prim_ty>(this->ty.payload);
				const auto& rhs_prim = std::get<prim_ty>(rhs.payload);

				if(lhs_prim == rhs_prim)
				{
					// same bloody thing.
					return this->ll;
				}

				using enum prim_ty::type;
				// bool can be converted to any number. remember its guaranteed to be smaller than any integral type so always sext (trunc other way around)
				if(lhs_prim.p == boolean && rhs_prim.is_numeric())
				{
					return codegen.ir->CreateZExtOrBitCast(this->ll, rhs.llvm());
				}
				else if(lhs_prim.is_numeric() && rhs_prim.p == boolean)
				{
					// don't trunc.
					// number converts to true if its not zero.
					llvm::Constant* zero =  llvm::ConstantInt::get(this->ty.llvm(), 0, true);
					return codegen.ir->CreateICmpNE(this->ll, zero);
					//return codegen.ir->CreateTrunc(this->ll, rhs.llvm());
				}

				// if lhs < rhs, then sext if rhs is signed, otherwise zext
				// if lhs == rhs, if signedness differ then bitcase, otherwise nothing
				// if lhs > rhs, then always trunc
				if(lhs_prim.is_integral() && rhs_prim.is_integral())
				{
					auto lhs_sz = lhs_prim.integral_size();
					auto rhs_sz = rhs_prim.integral_size();
					if(lhs_sz < rhs_sz)
					{
						if(rhs_prim.is_unsigned_integral())
						{
							return codegen.ir->CreateZExt(this->ll, rhs.llvm());
						}
						else
						{
							return codegen.ir->CreateSExt(this->ll, rhs.llvm());
						}
					}
					else if(lhs_sz == rhs_sz)
					{
						if(lhs_prim.is_signed_integral() == rhs_prim.is_signed_integral())
						{
							return this->ll;
						}
						else
						{
							return codegen.ir->CreateBitCast(this->ll, rhs.llvm());
						}
					}
					else
					{
						return codegen.ir->CreateTrunc(this->ll, rhs.llvm());
					}
				}
				else if (lhs_prim.is_floating_point() && rhs_prim.is_floating_point())
				{
					auto lhs_sz = lhs_prim.floating_point_size();
					auto rhs_sz = rhs_prim.floating_point_size();
					if (lhs_sz < rhs_sz)
					{
						return codegen.ir->CreateFPExt(this->ll, rhs.llvm());
					}
					else if (lhs_sz == rhs_sz)
					{
						return this->ll;
					}
					else
					{
						return codegen.ir->CreateFPTrunc(this->ll, rhs.llvm());
					}
				}
				else if(lhs_prim.is_floating_point() && rhs_prim.is_integral())
				{
					if(rhs_prim.is_signed_integral())
					{
						return codegen.ir->CreateFPToSI(this->ll, rhs.llvm());
					}
					else
					{
						return codegen.ir->CreateFPToUI(this->ll, rhs.llvm());
					}
				}
				else if(lhs_prim.is_integral() && rhs_prim.is_floating_point())
				{
					if(lhs_prim.is_signed_integral())
					{
						return codegen.ir->CreateSIToFP(this->ll, rhs.llvm());
					}
					else
					{
						return codegen.ir->CreateUIToFP(this->ll, rhs.llvm());
					}
				}
				else
				{
					panic("ahh conversion logic is hard what prims am i missing");
				}
			}
			else if(rhs_is(ptr_ty))
			{
				// convert integer to pointer.
				return codegen.ir->CreateIntToPtr(this->ll, rhs.llvm());
			}
			else
			{
				panic("lhs prim converts to wot???");
			}
		}
		else if(lhs_is(ptr_ty))
		{
			if(rhs_is(ptr_ty))
			{
				// pointers are trivially convertible
				return this->ll;
			}
			else if(rhs_is(prim_ty))
			{
				// pointer -> u64
				return codegen.ir->CreatePtrToInt(this->ll, rhs.llvm());
			}
		}
		else if(lhs_is(struct_ty))
		{
			if(rhs_is(struct_ty))
			{
				// they must be the same struct
				// or the is_convertible_to check would've failed.
				return this->ll;
			}
		}
		else if(lhs_is(enum_ty))
		{
			sval cpy = *this;
			cpy.ty = *AS_A(this->ty.payload, enum_ty).underlying_ty;
			cpy.ty = cpy.ty.add_weak();
			return cpy.convert_to(rhs);
		}
		auto lhs_name = this->ty.name();
		auto rhs_name = rhs.name();
		panic("cant convert types codegen-wise (\"{}\" -> \"{}\")", lhs_name, rhs_name);
		return nullptr;
	}

	llvm::Value* load() const
	{
		return codegen.ir->CreateLoad(this->ty.llvm(), this->ll);
	}

	llvm::Constant* llvm() const;
};

enum class scope_type
{
	block,
	metaregion,
	translation_unit,
	_undefined
};

constexpr const char* scope_type_names[] =
{
	"block",
	"metaregion",
	"<UNDEFINED SCOPE TYPE>"
};

llvm::GlobalVariable* codegen_t::declare_global_variable(std::string_view name, type_t ty, sval val, bool external_linkage, bool is_thread_local)
{
	auto linkage = external_linkage ? llvm::GlobalValue::LinkageTypes::ExternalLinkage : llvm::GlobalValue::LinkageTypes::PrivateLinkage;
	auto tls = is_thread_local ? llvm::GlobalVariable::LocalExecTLSModel : llvm::GlobalVariable::NotThreadLocal;
	bool is_const = !(ty.qual | typequal_mut);
	if(val.ll != nullptr)
	{
		val.convert_to(ty);
	}
	llvm::Constant* initialiser = static_cast<llvm::Constant*>(val.ll);
	if(initialiser == nullptr)
	{
		if(val.has_val())
		{
			initialiser = val.llvm();
		}
		else
		{
			initialiser = llvm::Constant::getNullValue(val.ty.llvm());
		}
	}
	auto gvar = std::make_unique<llvm::GlobalVariable>(*this->mod, ty.llvm(), is_const, linkage, initialiser, name, nullptr, tls);
	llvm::GlobalVariable* ret = gvar.get();
	this->global_variable_storage.push_back(std::move(gvar));
	return ret;

	/*
	llvm::Value* ptr = val.ll;
	if(ptr == nullptr)
	{
		ptr = llvm::Constant::getNullValue(val.ty.llvm());
	}
	auto gvar = std::make_unique<llvm::GlobalVariable>(*this->mod, val.ty.llvm(), is_const, linkage, static_cast<llvm::Constant*>(ptr), name);
	llvm::GlobalVariable* ret = gvar.get();
	this->global_variable_storage.push_back(std::move(gvar));
	*/
	return ret;
}

struct semal_state2
{
	struct function_data
	{
		fn_ty ty;
		llvm::Function* location = nullptr;
		srcloc loc = {};
		bool globally_visible = true;
	};

	struct variable_data
	{
		sval var;
		srcloc loc = {};
		bool globally_visible = true;
		bool ignore_already_defined = false;
	};

	string_map<struct_ty> structs = {};
	using struct_value = decltype(structs)::mapped_type;
	string_map<enum_ty> enums = {};
	using enum_value = decltype(enums)::mapped_type;
	string_map<function_data> functions = {};
	using function_value = decltype(functions)::mapped_type;
	string_map<variable_data> variables = {};
	using variable_value = decltype(variables)::mapped_type;
	string_map<type_t> alias_types = {};
	using alias_value = decltype(alias_types)::mapped_type;

	struct macro_data_t
	{
		const void* macrodef_expr;
		void* node;
	};

	string_map<macro_data_t> macros = {};
	using macro_value = decltype(macros)::mapped_type;

	type_t parse_type(std::string_view type_name) const;
};

enum class semal_type
{
	unknown,
	variable_decl,
	alias_decl,
	macro_decl,
	macro_impl,
	function_decl,
	struct_decl,
	enum_decl,
	blkinit,
	if_stmt,
	while_stmt,
	for_stmt,
	err,
	misc,
	variable_use,
	variable_ref,
	zero_expr,
} t;

struct semal_result
{
	static semal_result null()
	{
		return {.t = semal_type::unknown};
	}

	static semal_result zero()
	{
		return {.t = semal_type::zero_expr, .val = sval{.val = sval_zero_tag{}}};
	}

	template<typename... T>
	static semal_result err(std::format_string<T...> fmt, T&&... args)
	{
		return
		{
			.t = semal_type::err,
			.label = std::format(fmt, std::forward<T>(args)...)
		};
	}
	
	bool is_err() const
	{
		return this->t == semal_type::err;
	}

	bool is_null() const
	{
		return this->t == semal_type::unknown;
	}

	bool is_zero() const
	{
		return this->t == semal_type::zero_expr && IS_A(this->val.val, sval_zero_tag);
	}

	void load_if_variable()
	{
		if(this->t == semal_type::variable_use)
		{
			this->val.ll = this->val.load();
			this->t = semal_type::misc;
		}
	}

	void convert_to(const type_t& ty)
	{
		llvm::Value* before = this->val.ll;
		this->val.ll = this->val.convert_to(ty);
		bool is_static = this->val.ty.qual & typequal_static;
		this->val.ty = ty;
		if(is_static)
		{
			this->val.ty.qual = this->val.ty.qual | typequal_static;
		}
		if(this->val.ll == before)
		{
			// the conversion didnt do anything.
		}
		else
		{
			// yes, we are no longer the variable use.
			if(this->t == semal_type::variable_use)
			{
				this->t = semal_type::misc;
			}
		}
	}

	semal_type t = semal_type::unknown;
	std::string label;
	sval val = sval{};
};

template<typename T>
using pair_of = std::pair<T, T>;

struct semal_local_state
{
	scope_type scope = scope_type::_undefined;
	std::deque<semal_result> unfinished_types = {};
	string_map<sval> pending_variables = {};
	string_map<fn_ty> pending_functions = {};
	std::vector<void*> pending_macro_calls = {};
	semal_state2 state;
	semal_local_state* parent = nullptr;
	std::vector<std::pair<const void*, const void*>> current_macros = {};

	type_t parse_type_no_global(std::string_view type_name) const
	{
		type_t local_parse = state.parse_type(type_name);
		if(local_parse.is_badtype())
		{
			// couldnt do it. maybe it's defined in our parent?
			if(this->parent != nullptr)
			{
				local_parse = this->parent->parse_type(type_name);
			}
		}
		return local_parse;
	}

	type_t parse_type(std::string_view type_name) const
	{
		return std::get<0>(this->parse_type_global_fallback(type_name));
	}

	const semal_result* try_find_parent_function() const
	{
		for(auto iter = this->unfinished_types.rbegin(); iter != this->unfinished_types.rend(); iter++)
		{
			if(iter->t == semal_type::function_decl)
			{
				return &*iter;
			}
		}
		if(this->parent != nullptr)
		{
			return this->parent->try_find_parent_function();
		}
		return nullptr;
	}
	const semal_result* try_find_parent_macro() const
	{
		for(auto iter = this->unfinished_types.rbegin(); iter != this->unfinished_types.rend(); iter++)
		{
			if(iter->t == semal_type::macro_decl)
			{
				return &*iter;
			}
		}
		if(this->parent != nullptr)
		{
			return this->parent->try_find_parent_macro();
		}
		return nullptr;
	}

	std::pair<type_t, bool> parse_type_global_fallback(std::string_view type_name) const;

	void declare_macro(std::string macro_name, semal_state2::macro_data_t macro);
	void declare_function(std::string function_name, fn_ty ty, llvm::Function* location = nullptr, srcloc loc = {}, bool maybe_globally_visible = true);
	void declare_variable(std::string variable_name, sval val, srcloc loc = {}, bool globally_visible = true, bool ignore_already_decl = false);
	void declare_enum(std::string enum_name, enum_ty ty, srcloc loc = {});
	void declare_struct(std::string struct_name, struct_ty ty, srcloc loc = {});
	void declare_alias(std::string alias_name, type_t alias_ty, srcloc loc = {});

	pair_of<semal_state2::macro_value*> find_macro(std::string_view macro_name);
	pair_of<semal_state2::function_value*> find_function(std::string_view function_name);
	pair_of<semal_state2::variable_value*> find_variable(std::string_view variable_name);
	pair_of<semal_state2::enum_value*> find_enum(std::string_view enum_name);
	pair_of<semal_state2::struct_value*> find_struct(std::string_view struct_name);
	pair_of<semal_state2::alias_value*> find_alias(std::string_view alias_name);

	bool enum_add_entry(std::string enum_name, std::string entry_name, sval value);
	bool struct_add_member(std::string struct_name, std::string member_name, type_t member_ty);
};

struct semal_global_state
{
	semal_state2 state;
	std::vector<std::pair<std::filesystem::path, srcloc>> added_source_files = {};
	path_map<srcloc> added_link_libraries = {};
	std::deque<semal_local_state> locals = {};
	std::unordered_set<std::filesystem::path> compiled_source_files = {};
	std::unordered_set<std::filesystem::path> registered_link_libraries = {};
	compile_args* args = nullptr;

	string_map<llvm::Function*> llvm_functions = {};
	std::unordered_map<struct_ty, llvm::StructType*> llvm_structs = {};
	std::unordered_map<struct_ty, llvm::DIType*> llvm_debug_structs = {};

	type_t parse_type(std::string_view type_name) const
	{
		return state.parse_type(type_name);
	}
};

semal_global_state global;

type_t semal_state2::parse_type(std::string_view type_name) const
{
	// typenames can get very complicated so this isnt trivial at all.

	// Initialize the type to be parsed
	type_t current_type = type_t::badtype();
	
	std::string_view tyname = type_name;
	bool type_is_fn = false;
	while(!tyname.empty())
	{
		// skip whitespace
		if(std::isspace(tyname.front()))
		{
			tyname.remove_prefix(1);
			continue;
		}
		if(tyname.front() == '&' && !type_is_fn)
		{
			error_ifnt(!current_type.is_badtype(), {}, "type {} is malformed? saw pointer symbol before i found the base type", type_name);
			auto safecpy = ptr_ty::ref(current_type);
			current_type = type_t{safecpy};
			tyname.remove_prefix(1);
			continue;
		}
		if(tyname.front() == '#' && !type_is_fn)
		{
			tyname.remove_prefix(1);
			const char* first_digit = tyname.data();
			const char* last_digit = first_digit;
			while(tyname.size() && std::isdigit(tyname.front()))
			{
				last_digit++;
				tyname.remove_prefix(1);
			}
			std::size_t array_length = std::stoi(std::string{first_digit, last_digit});
			error_ifnt(!current_type.is_badtype(), {}, "type {} is malformed? saw array symbol before i found the base type", type_name);
			current_type = type_t{arr_ty::of(current_type, array_length)};
			continue;
		}
		std::size_t till_next_thing = 0;
		for(std::size_t i = 0; i < tyname.size(); i++)
		{
			if(!(std::isalnum(tyname[i]) || tyname[i] == '_'))
			{
				break;
			}
			till_next_thing++;
		}
		std::string_view word = (till_next_thing == tyname.size()) ? tyname : tyname.substr(0, till_next_thing);
		if(word == "mut" && !type_is_fn)
		{
			current_type.qual = current_type.qual | typequal_mut;
		}
		else if(word == "static" && !type_is_fn)
		{
			current_type.qual = current_type.qual | typequal_static;
		}
		else if(word == "weak" && !type_is_fn)
		{
			current_type.qual = current_type.qual | typequal_weak;
		}
		else
		{
			// im gonna assume this is the base type now then.
			bool is_func = word.starts_with("func");
			if(word.size() > 4)
			{
				if (std::isalnum(word[4]))
				{
					is_func = false;
				}
			}
			if(is_func)
			{
				type_is_fn = true;
				fn_ty retty{.return_ty = type_t::badtype()};
				std::string_view fntyname = tyname;
				fntyname.remove_prefix(4);
				std::size_t offset = 0;
				if(fntyname.starts_with("("))
				{
					// parse params.
					fntyname.remove_prefix(1);
					std::size_t close_pos = fntyname.find_first_of(')');
					if(close_pos == std::string_view::npos)
					{
						error({}, "invalid function typename \"{}\"", type_name);
					}
					std::deque<std::size_t> comma_positions = {};
					for(std::size_t i = 0; i < fntyname.size(); i++)
					{
						if(fntyname[i] == ',')
						{
							comma_positions.push_back(i);
						}
					}
					bool end = false;
					do
					{
						std::size_t first_end_pos;
						if(comma_positions.size())
						{
							first_end_pos = comma_positions.front() - offset;
							comma_positions.pop_front();
						}
						else
						{
							first_end_pos = close_pos - offset;
							end = true;
						}
						if(first_end_pos == 0)
						{
							break;
						}
						retty.params.push_back(this->parse_type(fntyname.substr(0, first_end_pos)));
						fntyname.remove_prefix(first_end_pos + 1);
						//close_pos -= (first_end_pos + 1);
						offset += (first_end_pos) + 1;
					}while(!end);
				}
				// we're beyond the cparen and the arrow is next. could be whitespace though.
				// let's just get the first character that could be the start of a symbol.
				std::size_t i;
				for(i = 0; i < fntyname.size(); i++)
				{
					if(std::isalpha(fntyname[i]) || fntyname[i] == '_')
					{
						break;
					}
				}
				std::size_t j;
				for(j = i; j < fntyname.size(); j++)
				{
					if(!(std::isspace(fntyname[j]) || std::isalnum(fntyname[j]) || fntyname[j] == '_' || fntyname[j] == '&'))
					{
						break;
					}
				}
				retty.return_ty = this->parse_type(fntyname.substr(i));
				// its a function pointer, to make a pointer to the function.
				current_type.payload = retty;
				current_type = {.payload = ptr_ty::ref(current_type)};
				fntyname = fntyname.substr(j);
				tyname = fntyname;
				continue;
			}


			for(int i = 0; i < static_cast<int>(prim_ty::type::_count); i++)
			{
				auto primty = static_cast<prim_ty::type>(i);
				const char* name = prim_ty::type_names[i];
				if(name == word)
				{
					current_type.payload = prim_ty{.p = primty};
				}
			}

			for(const auto& [name, aliasval] : global.state.alias_types)
			{
				if(name == word)
				{
					current_type.payload = aliasval.payload;
					current_type.qual = current_type.qual | aliasval.qual;
					break;
				}
			}

			for(const auto& [name, aliasval] : this->alias_types)
			{
				if(name == word)
				{
					current_type.payload = aliasval.payload;
					current_type.qual = current_type.qual | aliasval.qual;
					break;
				}
			}

			// Or with structs
			for (const auto& [name, structval] : global.state.structs)
			{
				if (name == word)
				{
					current_type.payload = structval;
					break;
				}
			}
			for (const auto& [name, structval] : this->structs)
			{
				if (name == word)
				{
					current_type.payload = structval;
					break;
				}
			}

			// Or with enums
			for (const auto& [name, enumval] : global.state.enums)
			{
				if (name == word)
				{
					current_type.payload = enumval;
					break;
				}
			}
			for (const auto& [name, enumval] : this->enums)
			{
				if (name == word)
				{
					current_type.payload = enumval;
					break;
				}
			}

			if(current_type.payload.index() == payload_index<std::monostate, type_t::payload_t>())
			{
				// bad base type. couldn't figure out what it is.
				return type_t::badtype();
			}
		}

		// Truncate the parsed word
		tyname = (till_next_thing == tyname.size()) ? std::string_view() : tyname.substr(till_next_thing);
	}
	return current_type;
}

void semal_local_state::declare_function(std::string function_name, fn_ty ty, llvm::Function* location, srcloc loc, bool maybe_globally_visible)
{
	auto [local, glob] = this->find_function(function_name);
	if(local != nullptr || glob != nullptr)
	{
		error(loc, "duplicate definition of function \"{}\"", function_name);
	}
	auto data = semal_state2::function_data
	{
		.ty = ty,
		.location = location,
		.loc = loc,
		.globally_visible = maybe_globally_visible
	};
	this->state.functions.emplace(function_name, data);
	if(this->scope == scope_type::translation_unit)
	{
		// declare it globally too.
		global.state.functions.emplace(function_name, data);
	}
}

void semal_local_state::declare_macro(std::string macro_name, semal_state2::macro_data_t macro)
{
	auto [local, glob] = this->find_macro(macro_name);
	if(local != nullptr || glob != nullptr)
	{
		error({}, "duplicate definition of macro \"{}\"", macro_name);
	}
	this->state.macros.emplace(macro_name, macro);
	if(this->scope == scope_type::translation_unit)
	{
		// declare it globally too.
		global.state.macros.emplace(macro_name, macro);
	}
}

void semal_local_state::declare_variable(std::string variable_name, sval val, srcloc loc, bool globally_visible, bool ignore_already_decl)
{
	auto [local, glob] = this->find_variable(variable_name);
	if((local != nullptr || glob != nullptr))
	{
		auto* ptr = local;
		if(ptr == nullptr)
		{
			ptr = glob;
		}
		if(ptr->ignore_already_defined)
		{
			// just overwrite it.
			this->state.variables.erase(variable_name);
			if(this->scope == scope_type::translation_unit)
			{
				global.state.variables.erase(variable_name);
			}
		}
		else if(!ignore_already_decl)
		{
			error(loc, "duplicate definition of variable \"{}\"", variable_name);
		}
	}
	auto data = semal_state2::variable_data
	{
		.var = val,
		.loc = loc,
		.globally_visible = globally_visible,
		.ignore_already_defined = ignore_already_decl
	};
	this->state.variables.emplace(variable_name, data);
	if(this->scope == scope_type::translation_unit)
	{
		// declare it globally too.
		global.state.variables.emplace(variable_name, data);
	}
}

void semal_local_state::declare_enum(std::string enum_name, enum_ty ty, srcloc loc)
{
	auto [local, glob] = this->find_enum(enum_name);
	if(local != nullptr || glob != nullptr)
	{
		error(loc, "duplicate definition of enum \"{}\"", enum_name);
	}
	this->state.enums.emplace(enum_name, ty);
	if(this->scope == scope_type::translation_unit)
	{
		// declare it globally too.
		global.state.enums.emplace(enum_name, ty);
	}
}

void semal_local_state::declare_struct(std::string struct_name, struct_ty ty, srcloc loc)
{
	auto [local, glob] = this->find_struct(struct_name);
	if(local != nullptr || glob != nullptr)
	{
		error(loc, "duplicate definition of struct \"{}\"", struct_name);
	}
	ty.label = struct_name;
	this->state.structs.emplace(struct_name, ty);
	if(this->scope == scope_type::translation_unit)
	{
		// declare it globally too.
		global.state.structs.emplace(struct_name, ty);
	}
}

void semal_local_state::declare_alias(std::string alias_name, type_t alias_ty, srcloc loc)
{
	auto [local, glob] = this->find_alias(alias_name);
	if(local != nullptr || glob != nullptr)
	{
		error(loc, "duplicate alias \"{}\"", alias_name);
	}
	if(!this->state.parse_type(alias_name).is_badtype())
	{
		error(loc, "alias \"{}\" is invalid because typename was already defined", alias_name);
	}
	this->state.alias_types.emplace(alias_name, alias_ty);
	if(this->scope == scope_type::translation_unit)
	{
		global.state.alias_types.emplace(alias_name, alias_ty);
	}
}

pair_of<semal_state2::macro_value*> semal_local_state::find_macro(std::string_view macro_name)
{
	semal_local_state* loc = this;
	semal_state2::macro_value* local_ret = nullptr;
	auto iter = loc->state.macros.find(macro_name);
	while(iter == loc->state.macros.end() && loc->parent != nullptr)
	{
		loc = loc->parent;
		iter = loc->state.macros.find(macro_name);
	}
	if(iter != loc->state.macros.end())
	{
		local_ret = &iter->second;
	}
	auto global_iter = global.state.macros.find(macro_name);

	semal_state2::macro_value* global_ret = nullptr;
	if(global_iter != global.state.macros.end())
	{
		global_ret = &global_iter->second;
	}
	return {local_ret, global_ret};
}

pair_of<semal_state2::function_value*> semal_local_state::find_function(std::string_view function_name)
{
	semal_local_state* loc = this;
	semal_state2::function_value* local_ret = nullptr;
	auto iter = loc->state.functions.find(function_name);
	while(iter == loc->state.functions.end() && loc->parent != nullptr)
	{
		loc = loc->parent;
		iter = loc->state.functions.find(function_name);
	}
	if(iter != loc->state.functions.end())
	{
		local_ret = &iter->second;
	}
	auto global_iter = global.state.functions.find(function_name);

	semal_state2::function_value* global_ret = nullptr;
	if(global_iter != global.state.functions.end())
	{
		global_ret = &global_iter->second;
	}
	return {local_ret, global_ret};
}

pair_of<semal_state2::variable_value*> semal_local_state::find_variable(std::string_view variable_name)
{
	semal_local_state* loc = this;
	semal_state2::variable_value* local_ret = nullptr;
	auto iter = loc->state.variables.find(variable_name);
	while(iter == loc->state.variables.end() && loc->parent != nullptr)
	{
		loc = loc->parent;
		iter = loc->state.variables.find(variable_name);
	}
	if(iter != loc->state.variables.end())
	{
		local_ret = &iter->second;
	}
	auto global_iter = global.state.variables.find(variable_name);

	semal_state2::variable_value* global_ret = nullptr;
	if(global_iter != global.state.variables.end())
	{
		global_ret = &global_iter->second;
	}
	return {local_ret, global_ret};
}

pair_of<semal_state2::enum_value*> semal_local_state::find_enum(std::string_view enum_name)
{
	semal_local_state* loc = this;
	semal_state2::enum_value* local_ret = nullptr;
	auto iter = loc->state.enums.find(enum_name);
	while(iter == loc->state.enums.end() && loc->parent != nullptr)
	{
		loc = loc->parent;
		iter = loc->state.enums.find(enum_name);
	}
	if(iter != loc->state.enums.end())
	{
		local_ret = &iter->second;
	}
	auto global_iter = global.state.enums.find(enum_name);

	semal_state2::enum_value* global_ret = nullptr;
	if(global_iter != global.state.enums.end())
	{
		global_ret = &global_iter->second;
	}
	return {local_ret, global_ret};
}

pair_of<semal_state2::struct_value*> semal_local_state::find_struct(std::string_view struct_name)
{
	semal_local_state* loc = this;
	semal_state2::struct_value* local_ret = nullptr;
	auto iter = loc->state.structs.find(struct_name);
	while(iter == loc->state.structs.end() && loc->parent != nullptr)
	{
		loc = loc->parent;
		iter = loc->state.structs.find(struct_name);
	}
	if(iter != loc->state.structs.end())
	{
		local_ret = &iter->second;
	}
	auto global_iter = global.state.structs.find(struct_name);

	semal_state2::struct_value* global_ret = nullptr;
	if(global_iter != global.state.structs.end())
	{
		global_ret = &global_iter->second;
	}
	return {local_ret, global_ret};
}

pair_of<semal_state2::alias_value*> semal_local_state::find_alias(std::string_view alias_name)
{
	semal_local_state* loc = this;
	semal_state2::alias_value* local_ret = nullptr;
	auto iter = loc->state.alias_types.find(alias_name);
	while(iter == loc->state.alias_types.end() && loc->parent != nullptr)
	{
		loc = loc->parent;
		iter = loc->state.alias_types.find(alias_name);
	}
	if(iter != loc->state.alias_types.end())
	{
		local_ret = &iter->second;
	}
	auto global_iter = global.state.alias_types.find(alias_name);

	semal_state2::alias_value* global_ret = nullptr;
	if(global_iter != global.state.alias_types.end())
	{
		global_ret = &global_iter->second;
	}
	return {local_ret, global_ret};
}

bool semal_local_state::enum_add_entry(std::string enum_name, std::string entry_name, sval value)
{
	bool did_a_write = false;
	auto [local_iter, global_iter] = this->find_enum(enum_name);
	auto v = AS_A(AS_A(value.val, literal_val), std::int64_t);
	if(local_iter != nullptr)
	{
		did_a_write = true;
		local_iter->entries.emplace(entry_name, v);
	}
	if(global_iter != nullptr)
	{
		did_a_write = true;
		global_iter->entries.emplace(entry_name, v);
	}
	return did_a_write;
}

bool semal_local_state::struct_add_member(std::string struct_name, std::string member_name, type_t member_ty)
{
	bool did_a_write = false;
	auto [local_iter, global_iter] = this->find_struct(struct_name);
	if(local_iter != nullptr)
	{
		did_a_write = true;
		local_iter->members.emplace(member_name, member_ty);
		local_iter->member_order.push_back(member_name);
	}
	if(global_iter != nullptr)
	{
		did_a_write = true;
		global_iter->members.emplace(member_name, member_ty);
		global_iter->member_order.push_back(member_name);
	}
	return did_a_write;
}

std::pair<type_t, bool> semal_local_state::parse_type_global_fallback(std::string_view type_name) const
{
	bool need_global = false;
	type_t parse = this->parse_type_no_global(type_name);
	if(parse.is_badtype())
	{
		need_global = true;
		parse = global.parse_type(type_name);
	}
	return {parse, need_global};
}


llvm::Type* type_t::llvm() const
{
	if(this->is_badtype())
	{
		return nullptr;
	}
	if(this->is_ptr())
	{
		return llvm::PointerType::get(*codegen.ctx, 0);
	}
	if(this->is_arr())
	{
		const auto& arr = AS_A(this->payload, arr_ty);
		return llvm::ArrayType::get(arr.underlying_ty->llvm(), arr.array_length);
	}
	if(this->is_struct())
	{
		return global.llvm_structs.at(AS_A(this->payload, struct_ty));
	}

	if(this->is_enum())
	{
		return AS_A(this->payload, enum_ty).underlying_ty->llvm();
	}

	if(this->is_prim())
	{
		auto prim = AS_A(this->payload, prim_ty);
		using enum prim_ty::type;
		switch(prim.p)
		{
			case s64:
				[[fallthrough]];
			case u64:
				return llvm::Type::getInt64Ty(*codegen.ctx);	
			break;
			case s32:
				[[fallthrough]];
			case u32:
				return llvm::Type::getInt32Ty(*codegen.ctx);
			break;

			case s16:
				[[fallthrough]];
			case u16:
				return llvm::Type::getInt16Ty(*codegen.ctx);
			break;

			case s8:
				[[fallthrough]];
			case u8:
				return llvm::Type::getInt8Ty(*codegen.ctx);
			break;

			case v0:
				return llvm::Type::getVoidTy(*codegen.ctx);
			break;

			case boolean:
				return llvm::Type::getInt1Ty(*codegen.ctx);
			break;

			case f64:
				return llvm::Type::getDoubleTy(*codegen.ctx);
			break;
			case f32:
				return llvm::Type::getFloatTy(*codegen.ctx);
			break;
			default:
			{
				auto name = prim.name();
				panic("dont know how to convert primitive type \"{}\" to llvm type", name);
			}
			break;
		}
	}
	auto name = this->name();
	panic("dont know how to convert type \"{}\" to llvm type", name);
	std::unreachable();
}

llvm::DIType* type_t::debug_llvm() const
{
	type_t cpy = *this;
	cpy.qual = typequal_none;
	if(this->is_badtype())
	{
		return nullptr;
	}
	if(this->is_ptr())
	{
		auto ptr = AS_A(this->payload, ptr_ty);
		if(ptr.underlying_ty->is_void())
		{
			return codegen.debug->createBasicType("v0&", 64, llvm::dwarf::DW_ATE_address);
		}
		llvm::DIType* pointee = ptr.underlying_ty->debug_llvm();
		return codegen.debug->createPointerType(pointee, sizeof(void*));
	}
	if(this->is_arr())
	{
		auto arr = AS_A(this->payload, arr_ty);
		llvm::DIType* elem = arr.underlying_ty->debug_llvm();
		// technically should use createArrayType, but its scary.
		return codegen.debug->createPointerType(elem, sizeof(void*));
	}
	if(this->is_fn())
	{
		return codegen.debug->createBasicType("u64", 64, llvm::dwarf::DW_ATE_unsigned);
	}
	if(this->is_struct())
	{
		return global.llvm_debug_structs.at(AS_A(this->payload, struct_ty));
	}

	if(this->is_enum())
	{
		return AS_A(this->payload, enum_ty).underlying_ty->debug_llvm();
	}

	if(this->is_prim())
	{
		auto prim = AS_A(this->payload, prim_ty);
		using enum prim_ty::type;
		switch(prim.p)
		{
			case s64:
				return codegen.debug->createBasicType(prim.name(), 64, llvm::dwarf::DW_ATE_signed);
			case u64:
				return codegen.debug->createBasicType(prim.name(), 64, llvm::dwarf::DW_ATE_unsigned);
			break;
			case s32:
				return codegen.debug->createBasicType(prim.name(), 32, llvm::dwarf::DW_ATE_signed);
			case u32:
				return codegen.debug->createBasicType(prim.name(), 32, llvm::dwarf::DW_ATE_unsigned);
			break;

			case s16:
				return codegen.debug->createBasicType(prim.name(), 16, llvm::dwarf::DW_ATE_signed);
			case u16:
				return codegen.debug->createBasicType(prim.name(), 16, llvm::dwarf::DW_ATE_unsigned);
			break;

			case s8:
				return codegen.debug->createBasicType(prim.name(), 8, llvm::dwarf::DW_ATE_signed);
			case u8:
				return codegen.debug->createBasicType(prim.name(), 8, llvm::dwarf::DW_ATE_unsigned);
			break;

			case v0:
				return codegen.debug->createUnspecifiedType(prim.name());
			break;

			case boolean:
				return codegen.debug->createBasicType(prim.name(), 1, llvm::dwarf::DW_ATE_boolean);
			break;

			case f64:
				return codegen.debug->createBasicType(prim.name(), 64, llvm::dwarf::DW_ATE_float);
			break;
			case f32:
				return codegen.debug->createBasicType(prim.name(), 32, llvm::dwarf::DW_ATE_float);
			break;
			default:
			{
				auto name = prim.name();
				panic("dont know how to convert primitive type \"{}\" to llvm debug type", name);
			}
			break;
		}
	}
	auto name = this->name();
	panic("dont know how to convert type \"{}\" to llvm debug type", name);
	std::unreachable();
}

llvm::Constant* sval::llvm() const
{
	if(IS_A(this->val, std::monostate))
	{
		panic("detected call to sval::llvm() where the payload is monostate.");
		return nullptr;
	}
	if(IS_A(this->val, literal_val))
	{
		auto lit = AS_A(this->val, literal_val);
		if(IS_A(lit, std::int64_t))
		{
			prim_ty primty;
			if(this->ty.is_enum())
			{
				primty = AS_A(AS_A(this->ty.payload, enum_ty).underlying_ty->payload, prim_ty);
			}
			else
			{
				primty = AS_A(this->ty.payload, prim_ty);
			}
			return llvm::ConstantInt::get(*codegen.ctx, llvm::APInt{static_cast<unsigned int>(primty.integral_size()), static_cast<std::uint64_t>(AS_A(lit, std::int64_t)), true});
		}
		else if(IS_A(lit, double))
		{
			return llvm::ConstantFP::get(this->ty.llvm(), llvm::APFloat{AS_A(lit, double)});
		}
		else if(IS_A(lit, char))
		{
			return llvm::ConstantInt::get(*codegen.ctx, llvm::APInt{8, static_cast<std::uint64_t>(AS_A(lit, char))});
		}
		else if(IS_A(lit, bool))
		{
			return llvm::ConstantInt::get(*codegen.ctx, llvm::APInt{1, AS_A(lit, bool) ? std::uint64_t{1} : std::uint64_t{0}});
		}
		else if(IS_A(lit, std::string))
		{
			std::string_view str = AS_A(lit, std::string);
			return codegen.ir->CreateGlobalString(str, "strlit", 0, codegen.mod.get());
		}
		else
		{
			panic("unknown sval literal_val");
		}
	}
	if(IS_A(this->val, sval_null_tag))
	{
		auto name = this->ty.name();
		panic_ifnt(this->ty.is_ptr(), "sval with value sval_null_tag was not a pointer type. instead it was \"{}\"", name);
		return llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(this->ty.llvm()));
	}
	if(IS_A(this->val, sval_zero_tag))
	{
		return llvm::UndefValue::get(this->ty.llvm());
	}
	if(IS_A(this->val, array_val))
	{
		auto arrval = AS_A(this->val, array_val);
		auto arrty = AS_A(this->ty.payload, arr_ty);
		std::vector<llvm::Constant*> elem_inits{};
		for(const sval& elem : arrval)
		{
			elem_inits.push_back(elem.llvm());
		}
		return llvm::ConstantArray::get(llvm::ArrayType::get(arrty.underlying_ty->llvm(), arrty.array_length), elem_inits);
	}
	if(IS_A(this->val, struct_val))
	{
		auto structval = AS_A(this->val, struct_val);
		auto structty = AS_A(this->ty.payload, struct_ty);
		std::vector<llvm::Constant*> member_inits{};
		for(const auto& memname : structty.member_order)
		{
			if(structval.contains(memname))
			{
				member_inits.push_back(structval.at(memname).llvm());
			}
			else
			{
				member_inits.push_back(llvm::UndefValue::get(structty.members.at(memname)->llvm()));
			}
		}
		return llvm::ConstantStruct::get(global.llvm_structs.at(structty), member_inits);
	}
	return nullptr;
}

//////////////////////////// LEXER -> TOKENS ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE lex

#define deduced_type "auto"

// uh read a file and slurp me all its bytes.
std::string read_file(std::filesystem::path file)
{
	std::ifstream fstr(file);
	error_ifnt(fstr.good(), {}, "failed to read file {}", file);

	std::stringstream buffer;
	buffer << fstr.rdbuf();
	auto ret = buffer.str();
	if(ret.empty())
	{
		warning({}, "source file {} is empty", file);
	}
	return ret;
}

// HASH IMPL
constexpr std::uint32_t fnv1a_32(std::string_view str) noexcept {
    constexpr std::uint32_t prime = 0x01000193; // 16777619
    constexpr std::uint32_t offset_basis = 0x811C9DC5; // 2166136261

    std::uint32_t hash = offset_basis;
    for (char c : str) {
        hash ^= static_cast<std::uint8_t>(c);
        hash *= prime;
    }
    return hash;
}
constexpr std::uint32_t string_hash(std::string_view str)
{
	return fnv1a_32(str);
}

enum class token : std::uint32_t;
// when im done lexing i will give the following info to the rest of the compiler
struct lex_output
{
	// what am i lexing
	const std::filesystem::path source_file;

	// the one-true-copy of its source.
	std::string source;
	// list of tokens.
	std::vector<token> tokens = {};
	// lexemes corresponding to each token. slice represents a view into the source (raw bytes) of the file.
	std::vector<slice> lexemes = {};
	// begin location of each token within the source file.
	std::vector<srcloc> begin_locations = {};
	// end location of each token within the source file.
	std::vector<srcloc> end_locations = {};

	void verbose_print();
	void strip_tokens_that_dont_affect_code();
};

// internal lexer state, not exposed to rest of compiler.
struct lex_state
{
	std::string_view src;
	std::size_t cursor = 0;
	unsigned int line = 1;
	unsigned int column = 1;

	// move forward 'count' chars, make sure to keep line count sane. panic if we run out of space.
	void advance(std::size_t count = 1)
	{
		panic_ifnt(this->cursor + count <= src.size(), "advance ran over string capacity");
		for(std::size_t i = 0; i < count; i++)
		{
			char next = src[this->cursor++];
			if(next == '\n')
			{
				this->line++;
				this->column = 0;
			}
			else
			{
				this->column++;
			}
		}
	}

	// simple internal helper. move forward n times where n is the length of the string.
	void advance(const char* str)
	{
		this->advance(std::strlen(str));
	}

	// slower but useful. keeping moving forward until a predicate is satisfied for the first time.
	// return the number of chars advanced. panic if predicate never returns true.
	std::size_t advance_until(bool(*pred)(std::string_view))
	{
		std::string_view current = this->src;
		current.remove_prefix(this->cursor);
		std::size_t dst = 0;
		// find how long we have to go until predicate first returns true
		while(!pred(current))
		{
			dst++;
			std::size_t capacity = src.size();
			panic_ifnt(this->cursor + dst <= capacity, "advance_until(pred) ran out of source file before predicate returned true");
			current.remove_prefix(1);
		}

		// advance that amount, and return that amount.
		this->advance(dst);
		return dst;
	}
};

// below is core tokenisation logic. dont skip over this text.
// the enum class token represents all token types. for each token type, the token_traits array has a member that contains the logic for how to parse it and its side effects.
//
// so based upon some source code, how do we identify which token type it is?
// most of the time, you can compare the front of the source code with some special set of characters
//
// e.g comments always start with "//"
// so token_traits[token::comment].front_identifier = "//" and the lexer api will automatically match it
// if front_identifier == nullptr for a certain token type then it is assumed that the match logic is not trivial.
//
// there is where the tokenise_fn (fn) comes in. the tokenise function will take in the current point in the source code and return true if it matches.
// note: even if you do set a front_identifier, the tokenise function's return value is ignored BUT the side-effect of it should properly deal with adding the token data.
// however, there is a special "trivial" flag you can raise for a common case. if your token type is trivial (i.e no lexeme value, just match against the front identifier and dont do anything special) then set trivial to true.
// if trivial is true, then the tokenise_fn is ignored completely and can be nullptr. it will just match and add the token with an empty lexeme (known in the codebase as trivial lexing)
enum class token : std::uint32_t
{
	comment,
	multicomment,
	integer_literal,
	decimal_literal,
	char_literal,
	string_literal,
	semicol,
	initialiser,
	colon,
	comma,
	dot,
	compare,
	comparen,
	assign,
	arrow,
	oparen,
	cparen,
	obrace,
	cbrace,
	obrack,
	cbrack,
	plus,
	dash,
	asterisk,
	fslash,
	cast,
	arr,
	bitwise_and,
	bitwise_or,
	bitwise_exor,
	modulo,
	invert,
	loreq,
	goreq,
	oanglebrack,
	canglebrack,
	keyword_static_if,
	keyword_if,
	keyword_else,
	keyword_while,
	keyword_for,
	keyword_return,
	keyword_yield,
	keyword_func,
	keyword_macro,
	keyword_extern,
	keyword_struct,
	keyword_enum,
	keyword_ref,
	keyword_deref,
	keyword_atomic_deref,
	keyword_defer,
	keyword_alias,
	keyword_at,
	keyword_true,
	keyword_false,
	keyword_zero,
	keyword_null,
	symbol,
	end_of_file,
	_count
};

using tokenise_fn = bool(*)(std::string_view, lex_state&, lex_output&);
struct tokeniser
{
	const char* name = "<untitled token>";
	const char* front_identifier = nullptr;
	tokenise_fn fn = nullptr;
	bool trivial = false;
	bool affects_code = true;
	bool allow_run_on = true;
};
std::array<tokeniser, static_cast<int>(token::_count)> token_traits
{
	tokeniser
	{
		.name = "comment",
		.front_identifier = "//",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			// rest of the line is ignored.
			std::size_t comment_begin = state.cursor + std::strlen("//");
			std::size_t comment_length = state.advance_until([](std::string_view next){return next.starts_with("\n");});
			out.tokens.push_back(token::comment);
			out.lexemes.push_back({.offset = comment_begin, .length = comment_length - 2});
			return true;
		},
		.affects_code = false
	},

	tokeniser
	{
		.name = "multicomment",
		.front_identifier = "/*",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			std::size_t comment_begin = state.cursor + std::strlen("/*");
			std::size_t comment_length = state.advance_until([](std::string_view next){return next.starts_with("*/");});
			state.advance("*/");
			out.tokens.push_back(token::multicomment);
			out.lexemes.push_back({.offset = comment_begin, .length = comment_length - 2});
			return false;
		},
		.affects_code = false
	},

	tokeniser
	{
		.name = "integer literal",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			std::int64_t val;
			std::from_chars_result result;
			if(front.starts_with("0b"))
			{
				// binary (base 2) literal
				result = std::from_chars(front.data() + 2, front.data() + front.size(), val, 2);
			}
			else if(front.starts_with("0x"))
			{
				// hex (base 16) literal
				result = std::from_chars(front.data() + 2, front.data() + front.size(), val, 16);
			}
			else
			{
				// base 10 integer literal
				result = std::from_chars(front.data(), front.data() + front.size(), val, 10);
			}
			if(result.ec == std::errc() && result.ptr != front.data() && *result.ptr != '.')
			{
				// yes we successfully parsed some numbers.
				std::size_t parse_count = result.ptr - front.data();
				std::size_t cursor_before = state.cursor;

				state.advance(parse_count);
				out.tokens.push_back(token::integer_literal);
				out.lexemes.push_back({.offset = cursor_before, .length = parse_count});
				return true;
			}
			return false;
		},
	},

	tokeniser
	{
		.name = "decimal literal",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			double val;
			auto result = std::from_chars(front.data(), front.data() + front.size(), val);
			// note: "inf" parses as a decimal literal which is wrong.
			// only consider a possible decimal literal if the first char is - or numeric
			char fr = front.front();
			if(!(fr == '-' || std::isdigit(fr)))
			{
				return false;
			}
			if(result.ec == std::errc() && result.ptr != front.data())
			{
				std::size_t parse_count = result.ptr - front.data();
				std::size_t cursor_before = state.cursor;
				state.advance(parse_count);
				out.tokens.push_back(token::decimal_literal);
				out.lexemes.push_back({.offset = cursor_before, .length = parse_count});
				return true;
			}
			return false;
		},
	},

	tokeniser
	{
		.name = "char literal",
		.front_identifier = "\'",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			srcloc cur{.file = out.source_file, .line = static_cast<unsigned int>(state.line), .column = static_cast<unsigned int>(state.column)};
			std::size_t char_begin = state.cursor + 1;
			// careful - advance_until could easily get the same quote as the front, so we nudge the cursor forward once
			state.advance(1);
			std::size_t char_length = state.advance_until([](std::string_view next){return next.starts_with("\'");});
			if(state.cursor < state.src.size())
			{
				state.advance(1);
			}
			out.tokens.push_back(token::char_literal);
			out.lexemes.push_back({.offset = char_begin, .length = char_length});
			return false;
		},
	},

	tokeniser
	{
		.name = "string literal",
		.front_identifier = "\"",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			std::size_t string_begin = state.cursor + 1;
			// careful - advance_until could easily get the same quote as the front, so we nudge the cursor forward once
			state.advance(1);
			std::size_t string_length = state.advance_until([](std::string_view next){return next.starts_with("\"") && *(next.data() - 1) != '\\';});
			if(state.cursor < state.src.size())
			{
				state.advance(1);
			}
			out.tokens.push_back(token::string_literal);
			out.lexemes.push_back({.offset = string_begin, .length = string_length});
			return false;
		},
	},

	tokeniser
	{
		.name = "semicol",
		.front_identifier = ";",
		.trivial = true
	},

	tokeniser
	{
		.name = "initialiser",
		.front_identifier = ":=",
		.trivial = true
	},

	tokeniser
	{
		.name = "colon",
		.front_identifier = ":",
		.trivial = true
	},

	tokeniser
	{
		.name = "comma",
		.front_identifier = ",",
		.trivial = true
	},

	tokeniser
	{
		.name = "dot",
		.front_identifier = ".",
		.trivial = true
	},

	tokeniser
	{
		.name = "compare",
		.front_identifier = "==",
		.trivial = true
	},

	tokeniser
	{
		.name = "compare",
		.front_identifier = "!=",
		.trivial = true
	},

	tokeniser
	{
		.name = "assign",
		.front_identifier = "=",
		.trivial = true
	},

	tokeniser
	{
		.name = "arrow",
		.front_identifier = "->",
		.trivial = true
	},

	tokeniser
	{
		.name = "oparen",
		.front_identifier = "(",
		.trivial = true
	},

	tokeniser
	{
		.name = "cparen",
		.front_identifier = ")",
		.trivial = true
	},

	tokeniser
	{
		.name = "obrace",
		.front_identifier = "{",
		.trivial = true
	},

	tokeniser
	{
		.name = "cbrace",
		.front_identifier = "}",
		.trivial = true
	},

	tokeniser
	{
		.name = "obrack",
		.front_identifier = "[",
		.trivial = true
	},

	tokeniser
	{
		.name = "cbrack",
		.front_identifier = "]",
		.trivial = true
	},

	tokeniser
	{
		.name = "plus",
		.front_identifier = "+",
		.trivial = true
	},

	tokeniser
	{
		.name = "dash",
		.front_identifier = "-",
		.trivial = true
	},

	tokeniser
	{
		.name = "asterisk",
		.front_identifier = "*",
		.trivial = true
	},

	tokeniser
	{
		.name = "fslash",
		.front_identifier = "/",
		.trivial = true
	},

	tokeniser
	{
		.name = "cast",
		.front_identifier = "@",
		.trivial = true
	},

	tokeniser
	{
		.name = "array",
		.front_identifier = "#",
		.trivial = true
	},

	tokeniser
	{
		.name = "bitwise_and",
		.front_identifier = "&",
		.trivial = true
	},

	tokeniser
	{
		.name = "bitwise_or",
		.front_identifier = "|",
		.trivial = true
	},

	tokeniser
	{
		.name = "bitwise_exor",
		.front_identifier = "^",
		.trivial = true
	},

	tokeniser
	{
		.name = "modulo",
		.front_identifier = "%",
		.trivial = true
	},

	tokeniser
	{
		.name = "invert",
		.front_identifier = "!",
		.trivial = true
	},

	tokeniser
	{
		.name = "loreq",
		.front_identifier = "<=",
		.trivial = true
	},

	tokeniser
	{
		.name = "goreq",
		.front_identifier = ">=",
		.trivial = true
	},

	tokeniser
	{
		.name = "oanglebrack",
		.front_identifier = "<",
		.trivial = true
	},

	tokeniser
	{
		.name = "canglebrack",
		.front_identifier = ">",
		.trivial = true
	},

	tokeniser
	{
		.name = "static if keyword",
		.front_identifier = "if static",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "if keyword",
		.front_identifier = "if",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "else keyword",
		.front_identifier = "else",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "while keyword",
		.front_identifier = "while",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "for keyword",
		.front_identifier = "for",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "return keyword",
		.front_identifier = "return",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "yield keyword",
		.front_identifier = "yield",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "func keyword",
		.front_identifier = "func",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "macro keyword",
		.front_identifier = "macro",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "extern keyword",
		.front_identifier = "extern",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "struct keyword",
		.front_identifier = "struct",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "enum keyword",
		.front_identifier = "enum",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "ref keyword",
		.front_identifier = "ref",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "deref keyword",
		.front_identifier = "deref",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "atomic_deref keyword",
		.front_identifier = "atomic deref",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "defer keyword",
		.front_identifier = "defer",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "alias keyword",
		.front_identifier = "alias",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "at keyword",
		.front_identifier = "at",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "true keyword",
		.front_identifier = "true",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "false keyword",
		.front_identifier = "false",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "zero keyword",
		.front_identifier = "zero",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "null keyword",
		.front_identifier = "null",
		.trivial = true,
		.allow_run_on = false
	},

	tokeniser
	{
		.name = "symbol",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			// symbol can start with a letter or _, but not a number
			if(std::isalpha(front.front()) || front.front() == '_')
			{
				// however after the first char a symbol can contain a number
				std::size_t symbol_begin = state.cursor;
				std::size_t symbol_length = state.advance_until([](std::string_view next)
				{
					return !(std::isalnum(next.front()) || next.front() == '_' || next.front() == '&' || next.front() == '#'
								|| next.starts_with(get_typequal_name(typequal_mut))
								|| next.starts_with(get_typequal_name(typequal_weak))
								|| next.starts_with(get_typequal_name(typequal_static))
								);
				});
				out.tokens.push_back(token::symbol);
				out.lexemes.push_back({.offset = symbol_begin, .length = symbol_length});
				return true;
			}
			return false;
		},
	},

	tokeniser
	{
		.name = "eof",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool{return false;}
	}
};

// given a given token trait, try to tokenise the current bit of the source code
// return true if the token matched and the output was updated
// return false (and doesnt touch the output) if the current part of the source code is definitely not this token.
bool try_tokenise(std::string_view front, token tok, lex_output& out, lex_state& state)
{
	const tokeniser& trait = token_traits[static_cast<int>(tok)];
	if(trait.trivial)
	{
		// do trivial lexing
		if(front.starts_with(trait.front_identifier))
		{
			// if the front identifier is "at", we shouldnt match "attention"
			std::string_view next = front.substr(std::strlen(trait.front_identifier));
			if(trait.allow_run_on || next.empty() || !(next.front() == '_' || std::isalnum(next.front())))
			{
				std::size_t cursor_before = state.cursor;
				state.advance(trait.front_identifier);
				out.tokens.push_back(tok);
				out.lexemes.push_back({.offset = cursor_before, .length = std::strlen(trait.front_identifier)});
				return true;
			}
		}
	}
	else
	{
		// ok not going to be that simple
		// firstly we better have a trait function
		if(trait.fn == nullptr)
		{
			const char* description = trait.front_identifier;
			if(description == nullptr)
			{
				description = "<unknown token trait>";
			}
			panic("non-trivial token trait \"{}\" had a nullptr tokenise function, which is a lexer bug.", description);
		}
		// is there still a front identifier?
		if(trait.front_identifier != nullptr)
		{
			if(front.starts_with(trait.front_identifier))
			{
				// ok, run the tokeniser function but ignore the result.
				// assume the side-effects of the function have successfully written to the output. we're done.
				trait.fn(front, state, out);
				return true;
			}
			// otherwise move on and try the next token type.
			return false;
		}
		else
		{
			// no front identifier, so we purely rely on the trait function.
			return trait.fn(front, state, out);
		}
	}
	return false;
}

std::size_t skip_over_whitespace(std::string_view front)
{
	if(std::isspace(front.front()))
	{
		return 1;
	}
	return false;
}

std::int64_t token_hash(token t)
{
	return std::hash<int>{}(static_cast<int>(t));
}

// lex api. "heres a file i know nothing about, give me all the tokens". panic if anything goes wrong.
lex_output lex_from_data(std::filesystem::path file, std::string source)
{
	lex_output ret{.source_file = file};
	ret.source = source;
	lex_state state{.src = ret.source};

	while(state.cursor < state.src.size())
	{
		// note: dont pass just cstr to ctor as it will search for \0 every single time.
		std::string_view front = {ret.source.data() + state.cursor, ret.source.size() - state.cursor};
		// is this whitespace we just skip over?
		auto whitespace = skip_over_whitespace(front);
		if(whitespace > 0)
		{
			// go again.
			state.advance(whitespace);
			continue;
		}

		srcloc loc{.file = file, .line = state.line, .column = state.column + 1, .cursor = state.cursor};
		// todo: try to match this to every known token, and store it in lex output and advance.
		bool found_a_token = false;
		for(int i = 0; i < static_cast<int>(token::_count); i++)
		{
			if(try_tokenise(front, static_cast<token>(i), ret, state))
			{
				// ok we succesfully found a token. save the location of the token and then break out of the loop.
				ret.begin_locations.push_back(loc);
				ret.end_locations.push_back({.file = file, .line = state.line, .column = state.column + 1, .cursor = state.cursor});
				found_a_token = true;
				break;
			}
		}
		// none of the tokens worked.
		// need to error out
		// let's not use the whole source as the snippet, just a hardcoded limit
		if(!found_a_token)
		{
			int cutoff_length = 32;
			int first_semicol_pos = front.find(';');
			if(first_semicol_pos != std::string_view::npos)
			{
				cutoff_length = std::min(cutoff_length, first_semicol_pos - 1);
			}
			if(front.size() > cutoff_length)
			{
				front.remove_suffix(front.size() - cutoff_length);
			}
			error(loc, "invalid tokens {}: \"{}\"", loc, front);
		}
	}

	ret.strip_tokens_that_dont_affect_code();
	ret.tokens.push_back(token::end_of_file);
	ret.lexemes.push_back({});
	ret.begin_locations.push_back(ret.begin_locations.size() ? ret.begin_locations.front() : srcloc{});
	ret.end_locations.push_back(ret.end_locations.size() ? ret.end_locations.back() : srcloc{});
	return ret;
}

lex_output lex(std::filesystem::path file)
{
	return lex_from_data(file, read_file(file));
}

void lex_output::verbose_print()
{
	// print all tokens.
	for(std::size_t i = 0; i < tokens.size(); i++)
	{
		token t = this->tokens[i];

		slice lex_slice = this->lexemes[i];
		std::string_view lexeme {this->source.data() + lex_slice.offset, lex_slice.length};
		srcloc begin_loc = this->begin_locations[i];
		srcloc end_loc = this->end_locations[i];

		std::println("{} ({}) at {}({}:{} -> {}:{})", token_traits[static_cast<int>(t)].name, lexeme, begin_loc.file, begin_loc.line, begin_loc.column, end_loc.line, end_loc.column);
	}
}

void lex_output::strip_tokens_that_dont_affect_code()
{
	for(std::size_t i = 0; i < tokens.size();)
	{
		tokeniser trait = token_traits[static_cast<int>(tokens[i])];
		if(!trait.affects_code)
		{
			this->tokens.erase(this->tokens.begin() + i);
			this->lexemes.erase(this->lexemes.begin() + i);
			this->begin_locations.erase(this->begin_locations.begin() + i);
			this->end_locations.erase(this->end_locations.begin() + i);
		}
		else
		{
			i++;
		}
	}
}

//////////////////////////// PARSE -> AST ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE parse

struct ast_token
{
	token tok;
	std::string_view lexeme;

	std::string value_tostring() const
	{
		return std::string{lexeme};
	}
};

struct ast_translation_unit { std::filesystem::path path; std::string value_tostring() { return ""; } };

struct ast_literal_expr
{
	literal_val value;
	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(value)>>
		{
			"integer literal",
			"floating-point literal",
			"char literal",
			"string literal",
			"boolean literal"
		}[this->value.index()];
	}

	type_t get_type() const
	{
		using enum prim_ty::type;
		type_t ret = std::array<type_t, std::variant_size_v<decltype(value)>>
		{
			type_t::create_primitive_type(s64),
			type_t::create_primitive_type(f64),
			type_t::create_primitive_type(u8),
			type_t::create_pointer_type(type_t::create_primitive_type(u8)),
			type_t::create_primitive_type(boolean)
		}[value.index()];
		ret.qual = ret.qual | typequal_static;
		if(!IS_A(this->value, bool))
		{
			ret.qual = ret.qual | typequal_weak;
		}
		return ret;
	}

	std::string value_tostring() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
				{
					ret = std::format("{}", arg);
				}, this->value);
		return ret;
	}
};

struct ast_zero_expr
{
	std::string value_tostring() const
	{
		return "zero";
	}
};

struct ast_null_expr
{
	std::string value_tostring() const
	{
		return "null";
	}
};

struct ast_decl;

struct ast_macrodef_expr
{
	std::vector<ast_decl> params = {};
	std::string return_type;

	std::string value_tostring() const
	{
		return "macro";
	}
};

struct ast_funcdef_expr
{
	std::vector<ast_decl> params = {};
	std::string return_type;
	bool is_extern = false;

	std::string value_tostring() const
	{
		if(this->is_extern)
		{
			return "extern func";
		}
		return "func";
	}
};

struct ast_expr;

struct ast_partial_callfunc
{
	std::string function_name;
	std::vector<ast_expr> params = {};
	bool awaiting_next_param = false;

	std::string value_tostring() const
	{
		return "partial call";
	}
};

struct ast_callfunc_expr
{
	std::string function_name;
	std::vector<ast_expr> params = {};

	std::string value_tostring() const;
};

struct ast_symbol_expr
{
	std::string symbol;

	std::string value_tostring() const
	{
		return "symbol";
	}
};

struct ast_structdef_expr
{
	std::string value_tostring() const
	{
		return "structdef";
	}
};

struct ast_enumdef_expr
{
	std::string underlying_type = "";
	std::string value_tostring() const
	{
		if(this->underlying_type.empty())
		{
			return "enum";
		}
		return std::format("enum({})", this->underlying_type);
	}
};

enum class biop_type
{
	plus,
	minus,
	mul,
	div,
	bitwise_and,
	bitwise_or,
	bitwise_exor,
	modulo,
	cast,
	field,
	ptr_field,
	at,
	compare_eq,
	compare_neq,
	assign,
	less_than,
	greater_than,
	less_than_or_equal,
	greater_than_or_equal,
	_count
};
constexpr std::array<unsigned int, static_cast<int>(biop_type::_count)> biop_precedence
{
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	2,
	3,
	3,
	3,
	1,
	1,
	0,
	1,
	1,
	1,
	1
};

struct ast_biop_expr
{
	box<ast_expr> lhs;
	biop_type type;
	box<ast_expr> rhs;

	std::string value_tostring() const
	{
		return std::format("{} biop",
		std::array<const char*, static_cast<int>(biop_type::_count)>
		{
			"plus",
			"minus",
			"multiply",
			"divide",
			"cast",
			"field",
			"compare_eq",
			"assign"
		}[static_cast<int>(this->type)]);
	}

	unsigned int precedence() const
	{
		return biop_precedence[static_cast<int>(this->type)];
	}
};

enum class unop_type
{
	minus,
	invert,
	ref,
	deref,
	atomic_deref,
	alias,
	_count
};

struct ast_unop_expr
{
	unop_type type;
	box<ast_expr> rhs;

	std::string value_tostring() const
	{
		return std::format("{} unop",
		std::array<const char*, static_cast<int>(unop_type::_count)>
		{
			"minus",
			"invert",
			"ref",
			"deref",
			"atomic_deref",
			"alias"
		}[static_cast<int>(this->type)]);
	}
};

struct ast_designator_stmt
{
	std::string name;
	box<ast_expr> initialiser;

	std::string value_tostring(){return "designator";}
};

struct ast_blkinit_expr
{
	std::string type_name;
	std::vector<ast_designator_stmt> initialisers;

	std::string value_tostring() const
	{
		return "blockinit";
	}
};

struct ast_expr
{
	std::variant
	<
		ast_literal_expr,
		ast_zero_expr,
		ast_null_expr,
		ast_macrodef_expr,
		ast_funcdef_expr,
		ast_callfunc_expr,
		ast_symbol_expr,
		ast_structdef_expr,
		ast_enumdef_expr,
		ast_biop_expr,
		ast_unop_expr,
		ast_blkinit_expr
	> expr_;
	bool parenthesised = false;

	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(expr_)>>
		{
			"literal",
			"zero",
			"null",
			"macrodef",
			"funcdef",
			"callfunc",
			"symbol",
			"structdef",
			"enumdef",
			"biop",
			"unop",
			"blockinit"
		}[this->expr_.index()];
	}

	unsigned int precedence() const
	{
		if(this->parenthesised)
		{
			return 4;
		}
		if(IS_A(expr_, ast_biop_expr))
		{
			return AS_A(expr_, ast_biop_expr).precedence();
		}
		return 0;
	}

	std::string value_tostring() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
				{
					ret = arg.value_tostring();
				}, this->expr_);
		return ret;
	}
};


std::string ast_callfunc_expr::value_tostring() const
{
	return std::format("call({} params)", params.size());
}

struct ast_decl
{
	std::string type_name;
	std::string name;
	std::optional<ast_expr> initialiser = std::nullopt;

	std::string value_tostring() const
	{
		if(this->initialiser.has_value())
		{
			return this->initialiser->value_tostring();
		}
		return "<no initialiser>";
	}
};

struct ast_decl_stmt
{
	ast_decl decl;

	std::string value_tostring()
	{
		return this->decl.value_tostring();
	}
};

struct ast_expr_stmt
{
	ast_expr expr;

	std::string value_tostring()
	{
		return this->expr.value_tostring();
	}
};

struct ast_return_stmt
{
	std::optional<ast_expr> retval;

	std::string value_tostring()
	{
		return std::format("return {}", retval.has_value() ? retval->value_tostring() : "");
	}
};

struct ast_yield_stmt
{
	ast_expr retval;

	std::string value_tostring()
	{
		return std::format("yield {}", retval.value_tostring());
	}
};

struct ast_stmt;
struct ast_blk_stmt
{
	bool capped = false;
	bool introduce_scope = true;

	std::string value_tostring(){return "";}
};

struct ast_metaregion_stmt
{
	std::string name;
	bool capped = false;

	std::string value_tostring(){return std::format("{} meta-region", this->name);}
};

struct ast_if_stmt
{
	ast_expr condition;
	bool is_static;

	std::string value_tostring()
	{
		if(this->is_static)
		{
			return "static-if-statement";
		}
		return "if-statement";
	}
};

struct ast_while_stmt
{
	ast_expr condition;

	std::string value_tostring()
	{
		return "while-statement";
	}
};

struct ast_for_stmt
{
	ast_expr init;
	ast_expr cond;
	ast_expr iter;

	std::string value_tostring()
	{
		return "for-statement";
	}
};

using attributes_t = string_map<std::optional<ast_expr>>;

struct ast_stmt
{
	std::variant
	<
		ast_decl_stmt,
		ast_expr_stmt,
		ast_return_stmt,
		ast_yield_stmt,
		ast_blk_stmt,
		ast_metaregion_stmt,
		ast_designator_stmt,
		ast_if_stmt,
		ast_while_stmt,
		ast_for_stmt
	> stmt_;
	bool deferred = false;
	attributes_t attributes = {};
	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(stmt_)>>
		{
			"declaration",
			"expression",
			"return",
			"block",
			"metaregion",
			"designator",
			"if",
			"while",
			"for"
		}[this->stmt_.index()];
	}
	std::string value_tostring() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
				{
					using T = std::decay_t<decltype(arg)>;
					ret = const_cast<T&>(arg).value_tostring();
				}, this->stmt_);
		return ret;
	}
};

enum class partial_funcdef_stage
{
	defining_params,
	awaiting_next_param,
	awaiting_arrow,
	awaiting_return_type,
	awaiting_body
};

struct ast_partial_funcdef
{
	bool is_macro = false;
	std::vector<ast_decl> params = {};
	std::string return_type;
	partial_funcdef_stage stage = partial_funcdef_stage::defining_params;

	std::string value_tostring() const{return "";}
};

struct ast_macrodef
{
	ast_macrodef_expr macro;

	std::string value_tostring() const
	{
		return macro.value_tostring();
	}
};

struct ast_funcdef
{
	ast_funcdef_expr func;

	std::string value_tostring() const
	{
		return func.value_tostring();
	}
};

struct ast_attribute
{
	std::string key;
	std::optional<ast_expr> value = std::nullopt;

	std::string value_tostring() const
	{
		return std::format("[[{}]]", this->key);
	}
};

using node_payload = std::variant
<
	std::monostate,
	ast_token,
	ast_translation_unit,
	ast_expr,
	ast_partial_callfunc,
	ast_decl,
	ast_stmt,
	ast_partial_funcdef,
	ast_funcdef,
	ast_macrodef,
	ast_attribute
>;
std::array<const char*, std::variant_size_v<node_payload>> node_names
{
	"<error>",
	"_unparsed_token",
	"translation_unit",
	"expression",
	"partial function call",
	"declaration",
	"statement",
	"partial function definition",
	"function definition",
	"macro definition",
	"attribute"
};

struct node
{
	// payload, data varies depending on what type of node we are.
	node_payload payload = std::monostate{};
	std::vector<node> children = {};
	// where does this node begin in the source code
	srcloc begin_location = {};
	// where does the node end in the source code
	srcloc end_location = {};
	// variable that xor's with the resultant hash. useful if you want nodes of the same payload type to still vary in their hash.
	int hash_morph = 0;
	// types defined at the level of this node.
	mutable std::optional<sval> val = std::nullopt;

	constexpr std::int64_t hash() const
	{
		return std::hash<std::size_t>{}(this->payload.index()) ^ std::hash<int>{}(this->hash_morph);
	}

	bool is_null() const
	{
		return payload.index() == payload_index<std::monostate, node_payload>();
	}

	void verbose_print(std::string_view full_source, std::string prefix = "") const
	{
		std::string extra = "";
		if(this->payload.index() == payload_index<ast_token, node_payload>())
		{
			extra = std::format("<{}>", token_traits[static_cast<int>(std::get<ast_token>(this->payload).tok)].name);
		}
		else if(this->payload.index() == payload_index<ast_stmt, node_payload>())
		{
			extra = std::format("({})", std::get<ast_stmt>(this->payload).type_name());
		}
		std::string value_as_string;
		std::visit([&value_as_string](auto&& arg)
				{
					using T = std::decay_t<decltype(arg)>;
					if constexpr(!std::is_same_v<T, std::monostate>)
					{
						value_as_string = const_cast<T&>(arg).value_tostring();
					}
				}, this->payload);
		std::println("{}{}{} {} [{}, {}] - [{}, {}]", prefix, node_names[this->payload.index()], extra, value_as_string, this->begin_location.line, this->begin_location.column, this->end_location.line, this->end_location.column);
		for(const auto& child : this->children)
		{
			child.verbose_print(full_source, prefix + "\t");
		}
	}

	// when shifting (grabbing a token from the lexer, that is uh not an ast node so how do we add it to the parse stack?
	// easy! use this to wrap it up and add this to the parse stack instead.
	static node wrap_token(const lex_output& lex, std::size_t idx)
	{
		slice s = lex.lexemes[idx];
		token tok = lex.tokens[idx];
		return node
		{
			.payload = ast_token
			{
				.tok = tok,
				.lexeme = {lex.source.data() + s.offset, s.length}
			},
			.begin_location = lex.begin_locations[idx],
			.end_location = lex.end_locations[idx],
			// remember, ast tokens of different types (e.g colon and comments) should return different hashes, or chords can't select a specific token type.
			.hash_morph = static_cast<int>(tok)
		};
	}

	static node wildcard()
	{
		return {};
	}

	static node wrap_imaginary_token(token tok)
	{
		// this node should never be put into an AST, but used as a way to create a node hash based upon an arbitrary token.
		return node
		{
			.payload = ast_token
			{
				.tok = tok
			},
			.hash_morph = static_cast<int>(tok)
		};
	}
};

std::int64_t wildcard_hash()
{
	return node::wildcard().hash();
}

template<std::size_t Begin, std::size_t End>
void static_for(std::function<void(std::size_t)> function)
{
	if constexpr(Begin < End)
	{
		function(std::integral_constant<std::size_t, Begin>{});
		static_for<Begin + 1, End>(function);
	}
}

void iterate_all_hashes(std::function<void(std::int64_t)> function)
{
	// invoke function(hash) for every single possible hash of a token/node that exists. implementation of chord wildcards.
	auto lambda = [&function]<int B, int E>(auto& lambda_ref)constexpr
	{
		using T = std::variant_alternative_t<B, node_payload>;
		if constexpr(std::is_same_v<T, ast_token>)
		{
			for(int i = 0; i < static_cast<int>(token::_count); i++)
			{
				function(node::wrap_imaginary_token(static_cast<token>(i)).hash());
			}
		}
		else
		{
			function(node{.payload = T{}}.hash());
		}
		if constexpr(B < E - 1)
		{
			lambda_ref.template operator()<B + 1, E>(lambda_ref);
		}
	};
	lambda.template operator()<0, std::variant_size_v<node_payload>>(lambda);
}

// whenever a chord function is invoked, it should return some kind of hint as to what happens next.
enum class parse_action
{
	// i couldn't make a decision, i need another token
	shift,
	// i have performed a reduction on the parse stack.
	reduce,
	// i think you should attempt to reduce again, but offset the nodes by reduction_result_offset.
	recurse,
	// the chord function has reported a parse error, and the parser state is no longer trustworthy.
	error,
	// the reduction results should be set as children of the final AST node. 
	commit,
};
struct chord_result
{
	parse_action action = parse_action::error;
	slice nodes_to_remove = {};
	std::size_t reduction_result_offset = 0;
	std::vector<node> reduction_result = {};
};

struct parser_state;
using chord_function = chord_result(*)(std::span<node> subtrees, parser_state& state);

struct chord_extra_info
{
	bool extensible = false;
	bool overrideable = false;
};
struct parse_table_entry
{
	chord_function chord_fn = nullptr;
	const char* description = "";
	chord_extra_info extra;
	std::unordered_map<std::int64_t, parse_table_entry> children = {};
};
std::unordered_map<std::int64_t, parse_table_entry> parse_table = {};

using entry_fn = std::function<void(parse_table_entry&)>;
void foreach_entry_from_hashed_subtrees(std::span<const node> subtrees, entry_fn fn, parse_table_entry* base = nullptr, bool skip_extensible = false)
{
	if(base == nullptr)
	{
		panic_ifnt(subtrees.size() > 0, "attempted to find empty from hashes of a set of subtrees, but the set of subtrees was empty.");
		std::int64_t first_hash = subtrees.front().hash();
		panic_ifnt(first_hash != wildcard_hash(), "the first token/node in a chord state must *not* be a wildcard");
		subtrees = subtrees.subspan<1>();
		base = &parse_table[first_hash];
	}
	std::span<const node> subtree_cpy = subtrees;
	for(const node& n : subtrees)
	{
		std::int64_t hash = n.hash();
		if(!skip_extensible && base->extra.extensible)
		{
			break;
		}
		if(hash == wildcard_hash())
		{
			// for *EVERY* possible hash, recurse.
			iterate_all_hashes([subtree_cpy, &fn, base, skip_extensible](std::int64_t hash)
			{
				auto& child = base->children[hash];	
				foreach_entry_from_hashed_subtrees(subtree_cpy.subspan<1>(), fn, &child, skip_extensible);
			});
			return;
		}
		else
		{
			base = &base->children[hash];
		}
		subtree_cpy = subtree_cpy.subspan<1>();
	}
	fn(*base);
}
/*
parse_table_entry& find_entry_from_hashed_subtrees(std::span<const node> subtrees)
{
	panic_ifnt(subtrees.size() > 0, "attempted to find empty from hashes of a set of subtrees, but the set of subtrees was empty.");
	std::int64_t first_hash = subtrees.front().hash();
	subtrees = subtrees.subspan<1>();
	parse_table_entry& entry = parse_table[first_hash];
	for(const node& n : subtrees)
	{
		entry = entry.children[n.hash()];
	}
	return entry;
}
*/

std::size_t total_number_of_chords = 0;
void add_chord(std::span<const node> subtrees, const char* description, chord_function fn, chord_extra_info extra = {})
{
	bool any_wildcards = false;
	for(const node& n : subtrees)
	{
		if(n.hash() == wildcard_hash())
		{
			any_wildcards = true;
		}
	}
	foreach_entry_from_hashed_subtrees(subtrees, [fn, description, any_wildcards, extra](parse_table_entry& entry)
	{
		total_number_of_chords++;
		if(any_wildcards)
		{
			if(entry.chord_fn == nullptr)
			{
				entry.chord_fn = fn;
				entry.description = description;
				entry.extra = extra;
			}
		}
		else
		{
			panic_ifnt(entry.extra.overrideable || (entry.chord_fn == nullptr || entry.chord_fn == fn), "redefinition of chord function {}", entry.description);
			entry.chord_fn = fn;
			entry.description = description;
			entry.extra = extra;
		}
	}, nullptr, true);
}

#define CHORD_BEGIN add_chord(
#define CHORD_END );
#define EXTENSIBLE , {.extensible = true}
#define OVERRIDEABLE , {.overrideable = true}
#define EXTENSIBLE_AND_OVERRIDEABLE , {.extensible = true, .overrideable = true}

#define TOKEN(x) node::wrap_imaginary_token(token::x)
#define NODE(x) node{.payload = x{}}
#define WILDCARD node::wildcard()
#define FN [](std::span<node> nodes, parser_state& state)->chord_result
#define FAKEFN(name) chord_result name(std::span<node> nodes, parser_state& state)
#define STATE(...) [](){return std::vector{node{.payload = ast_translation_unit{}}, __VA_ARGS__};}(), "translation_unit, " STRINGIFY(__VA_ARGS__)
// the difference between STATE and LOOSE state is that STATE means your chord function will only target nodes/tokens right at the beginning of the parse state
// LOOKAHEAD_STATE means it could be offsetted deep in the parse state.
#define LOOKAHEAD_STATE(...) [](){return std::vector{__VA_ARGS__};}(), STRINGIFY(__VA_ARGS__)
void populate_chords();

#define chord_error(msg, ...) error_nonblocking(nodes.front().begin_location, msg, __VA_ARGS__); return chord_result{.action = parse_action::error}

struct parser_state
{
	const lex_output& in;
	std::vector<node> nodes = {};
	std::size_t token_cursor = 0;
	std::size_t recursive_offset = 0;
	bool recursing = false;

	std::size_t chord_invocation_count = 0;
	std::size_t shift_count = 0;
	std::size_t reduce_count = 0;
	std::size_t recurse_count = 0;
	std::size_t commit_count = 0;

	bool shift()
	{
		if(this->token_cursor >= this->in.tokens.size())
		{
			return false;
		}
		nodes.push_back(node::wrap_token(this->in, this->token_cursor++));
		return true;
	}
};

node parse(const lex_output& impl_in, bool verbose_parse)
{
	parser_state state{.in = impl_in};
	srcloc tu_begin = impl_in.begin_locations.front();
	srcloc tu_end = impl_in.end_locations.back();
	state.nodes = {node{.payload = ast_translation_unit{.path = impl_in.source_file}, .begin_location = tu_begin, .end_location = tu_end}};

	// only exit the loop if:
	// token cursor is at the end AND we have exactly 1 node
	// therefore, keep doing the loop if token cursor is not at the end OR we dont have one node
	while(state.token_cursor != state.in.tokens.size() || state.nodes.size() != 1)
	{
		std::span<const node> nodes_view = state.nodes;
		foreach_entry_from_hashed_subtrees(nodes_view.subspan(state.recursive_offset),
		[&state, was_recursing = state.recursing, verbose_parse](parse_table_entry& entry)
		{
			if(was_recursing != state.recursing)
			{
				// a previous call to this callback has triggered a recurse.
				// stop what we're doing
				return;
			}
			if(entry.chord_fn == nullptr)
			{
				std::size_t begin_idx = state.recursive_offset;
				if(!state.recursing)
				{
					begin_idx = 1;
				}

				std::string formatted_src = format_source(state.in.source, state.nodes[begin_idx].begin_location, state.nodes.back().end_location);
				std::string ast_dump;
				error_nonblocking(state.nodes[begin_idx].begin_location, "invalid syntax\n{}\n", formatted_src);
				if(verbose_parse)
				{
					for(const node& n : state.nodes)
					{
						n.verbose_print(state.in.source);
					}
				}
				crash();
			}
			std::span<node> nodes = state.nodes;
			// subspan<1> so it doesnt work on the translation unit initial node.
			if(state.recursing)
			{
				nodes = nodes.subspan(state.recursive_offset);
			}
			else
			{
				nodes = nodes.subspan<1>();
			}
			chord_result result = entry.chord_fn(nodes, state);
			state.chord_invocation_count++;
			if(verbose_parse && nodes.size())
			{
				std::print("{}\n{}{}\n\t=> ", nodes.front().begin_location, entry.description, entry.extra.extensible ? ", ..." : "");
			}
			switch(result.action)
			{
				case parse_action::shift:
					state.shift();
					if(verbose_parse)
					{
						std::println("shift");
					}
					state.shift_count++;
					break;
				break;
				case parse_action::reduce:
				{
					slice rem = result.nodes_to_remove;
					// remove everything according to the slice
					std::size_t begin_offset = rem.offset + 1;
					std::size_t end_offset = rem.offset + rem.length;
					if(state.recursing)
					{
						begin_offset = rem.offset + state.recursive_offset;
						end_offset = rem.offset + rem.length + state.recursive_offset - 1;
					}
					const srcloc begin_loc = state.nodes[begin_offset].begin_location;
					const srcloc end_loc = state.nodes[end_offset].end_location;
					// todo: deal with begin/end loc better, rright now we just set that for all of the results.
					for(node& n : result.reduction_result)
					{
						n.begin_location = begin_loc;
						n.end_location = end_loc;
					}
					state.nodes.erase(state.nodes.begin() + begin_offset, state.nodes.begin() + begin_offset + rem.length);
					// then add the result(s).
					state.nodes.insert(state.nodes.begin() + begin_offset, result.reduction_result.begin(), result.reduction_result.end());
					state.recursive_offset = 0;
					state.recursing = false;
					if(verbose_parse)
					{
						std::println("reduce");
					}
					state.reduce_count++;
					break;
				}
				break;
				case parse_action::recurse:
					state.recursive_offset += result.reduction_result_offset;
					state.recursing = true;
					if(!was_recursing)
					{
						// additional offset of 1 as the chord included a secret translation unit node.
						state.recursive_offset++;
					}
					if(verbose_parse)
					{
						std::println("recurse({})", result.reduction_result_offset);
					}
					state.recurse_count++;
					return;
				break;
				case parse_action::commit:
				{
					slice rem = result.nodes_to_remove;
					// remove everything according to the slice
					const srcloc begin_loc = state.nodes[rem.offset + 1].begin_location;
					const srcloc end_loc = state.nodes[rem.offset + rem.length].end_location;
					// todo: deal with begin/end loc better, rright now we just set that for all of the results.
					for(node& n : result.reduction_result)
					{
						n.begin_location = begin_loc;
						n.end_location = end_loc;
					}
					state.nodes.erase(state.nodes.begin() + rem.offset + 1, state.nodes.begin() + rem.offset + 1 + rem.length);
					// then add the result(s).
					for(const node& n : result.reduction_result)
					{
						state.nodes.front().children.push_back(n);
					}
					if(verbose_parse)
					{
						std::println("commit");
					}
					state.commit_count++;
				}
				break;
				case parse_action::error:
				{
					std::size_t begin_idx = state.recursive_offset;
					if(!state.recursing)
					{
						begin_idx = 1;
					}
					std::string formatted_src = format_source(state.in.source, state.nodes[begin_idx].begin_location, state.nodes.back().end_location);
					std::print("{}\n{}\n", formatted_src, verbose_parse ? entry.description : "");
					crash();
				}
				break;
				default:
					panic("parse chord function returned unknown parse action");
				break;
			}
		});
	}

	auto sz = state.nodes.size();
	panic_ifnt(sz == 1, "expected one final ast node, but there are {} nodes", sz);

	if(verbose_parse)
	{
		constexpr auto verbose_parse_print = R"(+======
	parse table size = {}
	chords = {}
	shifts = {}
	reductions = {}
	recursions = {}
	commits = {}
+======)";
		std::println(verbose_parse_print, total_number_of_chords, state.chord_invocation_count, state.shift_count, state.reduce_count, state.recurse_count, state.commit_count);
	}
	return state.nodes.front();
}


//////////////////////////// CODEGEN -> LLVM-IR ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE codegen

#define codegen_logic_begin {auto impl_codegen_time_ = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
#define codegen_logic_end time_codegen += std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() - impl_codegen_time_;}

void codegen_initialise(std::filesystem::path build_file)
{
	auto dir = build_file.parent_path().string();
	auto name = build_file.filename().string();
	codegen.ctx = std::make_unique<llvm::LLVMContext>();
	codegen.mod = std::make_unique<llvm::Module>(name, *codegen.ctx);
	codegen.ir = std::make_unique<llvm::IRBuilder<>>(*codegen.ctx);

	codegen.debug = std::make_unique<llvm::DIBuilder>(*codegen.mod);
	codegen.dbg = codegen.debug->createCompileUnit(llvm::dwarf::DW_LANG_C, codegen.debug->createFile(name, dir), "Psy Compiler", false, "", 0, "");
}

void codegen_verbose_print()
{
	if(codegen.mod != nullptr)
	{
		std::string ir;
		llvm::raw_string_ostream os{ir};
		codegen.mod->print(os, nullptr);
		std::println("llvm IR is as follows:\n\033[1;34m{}\033[0m", ir);
	}
}


void codegen_verify()
{
	std::string err_msg;
	llvm::raw_string_ostream output(err_msg);
	bool broken = llvm::verifyModule(*codegen.mod, &output);
	if(broken)
	{
		codegen_verbose_print();
	}
	error_ifnt(!broken, {}, "llvm verify failed: {}", err_msg);
}

void generate_object_file(std::string object_path, llvm::TargetMachine* targetMachine);

enum class linker_type
{
	msvc_like,
	gnu_like
};
linker_type divine_linker_type(const compile_args& args);

std::filesystem::path codegen_generate(compile_args& args)
{
	// tell the LLVM optimiser not to detect "hand-rolled" memset/memcpy and optimise them to CRT memset/memcpy.
	// this optimisation assumes these functions are always available i.e link against CRT. that's absolutely not the case. go away.
	// https://github.com/llvm/llvm-project/issues/56467/
	// fun fact: in my current code this does indeed stop memset generation, but not memcpy. :)
	llvm::DisableLIRP::Memset = true;
	llvm::DisableLIRP::Memcpy = true;

	if(!std::filesystem::exists(args.output_dir))
	{
		std::filesystem::create_directory(args.output_dir);
	}
	llvm::TargetMachine* targetMachine = nullptr;
	codegen_logic_begin

	codegen.debug->finalize();

	// Initialize all targets and related components
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Get the target triple
	if(args.target_triple.empty())
	{
		args.target_triple = llvm::sys::getDefaultTargetTriple();
	}
    codegen.mod->setTargetTriple(args.target_triple);
	if(divine_linker_type(args) == linker_type::msvc_like)
	{
		codegen.mod->setModuleFlag(llvm::Module::Error, "CodeView", 1);
	}

    std::string error;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(args.target_triple, error);

    if (!target) {
        llvm::errs() << "Error: " << error << "\n";
        return {};
    }

    // Create a TargetMachine
    llvm::TargetOptions opt;
    targetMachine = target->createTargetMachine(args.target_triple, "generic", "", opt, llvm::Reloc::PIC_, std::nullopt, llvm::CodeGenOptLevel::None);

    codegen.mod->setDataLayout(targetMachine->createDataLayout());

	llvm::PassBuilder passBuilder(targetMachine);

    llvm::LoopAnalysisManager loopAnalysisManager;
    llvm::FunctionAnalysisManager functionAnalysisManager;
    llvm::CGSCCAnalysisManager cgsccAnalysisManager;
    llvm::ModuleAnalysisManager moduleAnalysisManager;

    passBuilder.registerModuleAnalyses(moduleAnalysisManager);
    passBuilder.registerCGSCCAnalyses(cgsccAnalysisManager);
    passBuilder.registerFunctionAnalyses(functionAnalysisManager);
    passBuilder.registerLoopAnalyses(loopAnalysisManager);
    passBuilder.crossRegisterProxies(
        loopAnalysisManager, functionAnalysisManager, cgsccAnalysisManager, moduleAnalysisManager);

	llvm::OptimizationLevel opt_level;
	switch(args.optimisation_level)
	{
		case 0:
			opt_level = llvm::OptimizationLevel::O0;
		break;
		case 1:
			opt_level = llvm::OptimizationLevel::O1;
		break;
		case 2:
			opt_level = llvm::OptimizationLevel::O2;
		break;
		case 3:
			opt_level = llvm::OptimizationLevel::O3;
		break;
	}
    llvm::ModulePassManager modulePassManager = passBuilder.buildPerModuleDefaultPipeline(
        opt_level);

    // Run the optimization passes
    modulePassManager.run(*codegen.mod, moduleAnalysisManager);

	codegen_logic_end
	std::string object_path = args.output_name + ".o";
	generate_object_file((args.output_dir / object_path).string(), targetMachine);
	return object_path;
}

void codegen_terminate(bool verbose_print)
{
	if(verbose_print)
	{
		codegen_verbose_print();
	}

	for(std::size_t i = 0; i < codegen.global_variable_storage.size(); i++)
	{
		// llvm is so incredibly annoying
		// the optimisation passes may or may not have invalidated some or all of these
		// it seems there's no obvious way to know, so deleting them all is likely to crash
		// well fuck you, i'll just leak them.
		auto ptr = codegen.global_variable_storage[i].release();
		(void)ptr;
	}
	codegen.global_variable_storage.clear();
	codegen.ir = nullptr;
	codegen.debug = nullptr;
	codegen.mod = nullptr;
	codegen.ctx = nullptr;
}

//////////////////////////// ASSEMBLE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE assemble
#define assemble_logic_begin {auto impl_assemble_time_ = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
#define assemble_logic_end time_assemble += std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() - impl_assemble_time_;}

void generate_object_file(std::string object_path, llvm::TargetMachine* targetMachine)
{
	assemble_logic_begin
    // Emit the object file
	std::error_code err;
	llvm::raw_fd_ostream object_file(object_path, err, llvm::sys::fs::OF_None);

	if(err)
	{
		auto msg = err.message();
		error({}, "error while generating {}: {}", object_path, msg);
	}
    llvm::legacy::PassManager codeGenPassManager;
    if (targetMachine->addPassesToEmitFile(
            codeGenPassManager, object_file, nullptr, llvm::CodeGenFileType::ObjectFile)) {
        llvm::errs() << "The target machine cannot emit an object file.\n";
        return;
    }

    codeGenPassManager.run(*codegen.mod);
    object_file.flush();
	assemble_logic_end
}

//////////////////////////// LINK ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE link
#define link_logic_begin {auto impl_link_time_ = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
#define link_logic_end time_link += std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() - impl_link_time_;}

std::string get_default_linker()
{
#ifdef _WIN32
	return "\"link.exe\"";
#else
	return "ld";
#endif
}

std::string get_default_archiver()
{
#ifdef _WIN32
	return "\"lib.exe\"";
#else
	return "ar";
#endif
}

std::string get_linker()
{
	const char* val = std::getenv("PSYC_LINKER");
	if(val == nullptr)
	{
		return get_default_linker();
	}
	return val;
}

std::string get_archiver()
{
	const char* val = std::getenv("PSYC_ARCHIVER");
	if(val == nullptr)
	{
		return get_default_archiver();
	}
	return val;
}

linker_type divine_linker_type(const compile_args& args)
{
	const char* lnkty = std::getenv("PSYC_LINKER_FLAVOUR");
	if(lnkty != nullptr)
	{
		std::string lnktystr = lnkty;
		if(lnktystr == "msvc")
		{
			return linker_type::msvc_like;
		}
		else if(lnktystr == "gnu")
		{
			return linker_type::gnu_like;
		}
		else
		{
			error({}, "unknown linker flavour \"{}\". Expected \"msvc\" or \"gnu\"", lnktystr);
		}
	}
#ifdef _WIN32
	if(args.target_triple.contains("msvc"))
	{
		return linker_type::msvc_like;
	}
	return linker_type::gnu_like;
#else
	return linker_type::gnu_like;
#endif
}

void link(std::filesystem::path object_file_path, const compile_args& args)
{
	link_logic_begin
	if(args.output_type == target::object)
	{
		msg({}, "No linkage as you have opted for objects only.\n  \"{}\" written.", object_file_path);
		return;
	}
	std::string link_libs;
	for(const auto path : args.link_libraries)
	{
		link_libs += std::format(" {}", path.filename());
	}
	std::string linker = get_linker();
	std::string archiver = get_archiver();
	linker_type type = divine_linker_type(args);

	std::string lnk_args;

	std::string cmd = std::format("cd {} && ", args.output_dir);
	if(args.output_type == target::executable)
	{
		if(type == linker_type::msvc_like)
		{
			lnk_args = std::format(" {} /ENTRY:{} /NODEFAULTLIB /subsystem:console /OUT:{}{} {}", object_file_path, "_psymain", args.output_name + ".exe", link_libs, args.debug_symbols ? "/DEBUG" : "");
		}
		else
		{
			lnk_args = std::format(" {} -e {} -o {}{}", object_file_path, "_psymain", args.output_name + ".out", link_libs);
		}
		cmd += std::format("{}{}", linker, lnk_args);
	}
	else if(args.output_type == target::library)
	{
		if(type == linker_type::msvc_like)
		{
			lnk_args = std::format(" {} /OUT:{}", object_file_path, args.output_name + ".lib");
		}
		else
		{
			lnk_args = std::format(" rvs {} {}", args.output_name + ".a", object_file_path);
		}
		cmd += std::format("{}{}", archiver, lnk_args);
	}
	else
	{
		panic("unrecognised output target type. expected \"object\", \"executable\" or \"library\"");
	}

	if(args.verbose_link)
	{
		msg({}, "{}", cmd);
	}
	int ret = std::system(cmd.c_str());
	if(ret != 0)
	{
		error({}, "link failed");
	}
	link_logic_end
}

//////////////////////////// SEMAL ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE semal

sval wrap_type(type_t t)
{
	if(t.qual & typequal_static)
	{
		auto name = t.name();
		error({}, "attempt to wrap around static type \"{}\" without static value", name);
	}
	return {.ty = t};
}

const node* try_get_block_child(const node& n)
{
	if(n.children.size() == 1)
	{
		const auto& first_child = n.children.front();
		if(first_child.payload.index() == payload_index<ast_stmt, node_payload>())
		{
			const auto& stmt = std::get<ast_stmt>(first_child.payload);
			if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
			{
				// yes the first child is a block statement
				return &first_child;
			}
		}
	}
	return nullptr;
}

node* try_get_block_child(node& n)
{
	if(n.children.size() == 1)
	{
		auto& first_child = n.children.front();
		if(first_child.payload.index() == payload_index<ast_stmt, node_payload>())
		{
			const auto& stmt = std::get<ast_stmt>(first_child.payload);
			if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
			{
				// yes the first child is a block statement
				return &first_child;
			}
		}
	}
	return nullptr;
}

node* try_find_build_metaregion(node& ast)
{
	if(ast.payload.index() == payload_index<ast_stmt, node_payload>())
	{
		const auto& stmt = std::get<ast_stmt>(ast.payload);
		if(stmt.stmt_.index() == payload_index<ast_metaregion_stmt, decltype(stmt.stmt_)>())
		{
			const auto& metaregion = std::get<ast_metaregion_stmt>(stmt.stmt_);
			if(metaregion.name == "build")
			{
				return &ast;
			}
		}
	}
	for(auto& child : ast.children)
	{
		node* maybe_region = try_find_build_metaregion(child);
		if(maybe_region != nullptr)
		{
			return maybe_region;
		}
	}
	return nullptr;
}

std::filesystem::path get_compiler_path()
{
	std::string ret;
	ret.resize(1024);
	#ifdef _WIN32
	DWORD len = GetModuleFileNameA(nullptr, ret.data(), ret.size());
	ret.resize(len);
	return ret;
	#else
	return std::filesystem::canonical("/proc/self/exe");
	#endif
}

void handle_defer(std::span<node> children)
{
	if(children.empty())
	{
		return;
	}
	std::size_t pivot_offset = 0;
	auto pivot = children.end();
	auto last = children.back();
	if(IS_A(last.payload, ast_stmt))
	{
		auto last_stmt = AS_A(last.payload, ast_stmt);
		if(IS_A(last_stmt.stmt_, ast_return_stmt))
		{
			pivot = children.begin() + children.size() - 1;
			pivot_offset = 1;
		}
	}
	for(auto iter = children.begin(); iter != pivot && iter != children.end(); iter++)
	{
		auto& p = iter->payload;
		if(IS_A(p, ast_stmt))
		{
			if(AS_A(p, ast_stmt).deferred)
			{
				AS_A(p, ast_stmt).deferred = false;
				std::rotate(iter, iter + 1, pivot);
				iter = children.begin();
				pivot = children.begin() + children.size() - (++pivot_offset);
			}
		}
	}
}

void verify_semal_result(const semal_result& result, const node& n, std::string_view source)
{
	if(result.t == semal_type::err)
	{
		std::string quote = format_source(source, n.begin_location, n.end_location);
		error(n.begin_location, "invalid code\n{}\n{}", result.label, quote);
	}
}

llvm::DIScope* debug_get_scope(const node& n)
{
	llvm::DIScope* scope;
	if(lexical_blocks.empty())
	{
		if(debug_files.empty() || n.begin_location.file.empty())
		{
			return codegen.dbg;
		}
		else
		{
			return debug_files.at(n.begin_location.file);
		}
	}
	else
	{
		return lexical_blocks.back();
	}
}

void emit_debug_location(const node& n)
{
	llvm::DIScope* scope = debug_get_scope(n);
	codegen.ir->SetCurrentDebugLocation(llvm::DILocation::get(scope->getContext(), n.begin_location.line, n.begin_location.column, scope));
}

void emit_empty_debug_location()
{
	codegen.ir->SetCurrentDebugLocation(llvm::DebugLoc());
}

sval zero_as(const type_t& ty);

semal_result semal_literal_expr(const ast_literal_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	sval ret{.val = expr.value, .ty = expr.get_type()};
	if(do_codegen)
	codegen_logic_begin
		ret.ll = ret.llvm();
	codegen_logic_end
	return
	{
		.t = semal_type::misc,
		.label = "literal",
		.val = ret
	};
}

semal_result semal_decl(const ast_decl& decl, node& n, std::string_view source, semal_local_state*& local, bool do_codegen, const attributes_t& attributes = {});

semal_result semal_macrodef_expr(const ast_macrodef_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	local->unfinished_types.push_back({.t = semal_type::macro_decl});	
	return { .t = semal_type::macro_decl, .val = {.usrdata = new ast_macrodef_expr{expr}, .usrdata2 = new node{n}} };
}


semal_result semal_funcdef_expr(const ast_funcdef_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	// todo: foreach static param, generate a semal_result, push it, and the caller decl will somehow deal with all of them.
	// for now, static params are ignored and we only generate one unfinished function.
	fn_ty ty
	{
		.return_ty = local->parse_type(expr.return_type)
	};
	// if we go ahead and do the decls for these params, they have no obvious way of knowing that they are function params
	// they will just be local variables in the parent scope which is wrong.
	// to fix this, we will push a temporary unfinished function, and then pop it when the params are declared.
	local->unfinished_types.push_back({.t = semal_type::function_decl});	

	for(const ast_decl& param : expr.params)
	{
		semal_result param_result = semal_decl(param, n, source, local, do_codegen);
		if(param_result.is_err())
		{
			return param_result;
		}
		// so we need this decl to be visible in the *next* scope (if its not extern)
		// to do this, we use "pending types"
		if(!expr.is_extern)
		{
			local->pending_variables.emplace(param.name, param_result.val);
		}
		ty.params.push_back(param_result.val.ty);
	}
	semal_result ret = {.t = semal_type::function_decl, .val = {.ty = {.payload = ty}}};
	// update the last unfinished type, otherwise it wont have the params when it registers the function in local/global scopes.
	local->unfinished_types.back() = ret;
	// if the function is extern then we're not expecting a block - remove the unfinished type now.
	if(expr.is_extern)
	{
		local->unfinished_types.pop_back();
	}

	if(do_codegen)
	{
		std::vector<llvm::Type*> llvm_params;
		std::vector<llvm::DIType*> llvm_debug_params;
		for(std::size_t i = 0; i < ty.params.size(); i++)
		{
			llvm_params.push_back(ty.params[i].llvm());
			llvm_debug_params.push_back(ty.params[i].debug_llvm());
		}
		llvm::FunctionType* llvm_fn = llvm::FunctionType::get(ty.return_ty->llvm(), llvm_params, false);
		llvm::Function* llvm_func = llvm::Function::Create(llvm_fn, llvm::Function::ExternalLinkage, "unnamed_fn", codegen.mod.get());

		std::vector<llvm::Metadata*> el_tys = {};
		llvm::DISubroutineType* debug_fn_ty = nullptr;
		// first element should be return type???
		el_tys.push_back(ty.return_ty->debug_llvm());
		for(const type_t& param_ty : ty.params)
		{
			el_tys.push_back(param_ty.debug_llvm());
		}
		debug_fn_ty = codegen.debug->createSubroutineType(codegen.debug->getOrCreateTypeArray(el_tys));

		llvm::DIFile* file = debug_files.at(n.begin_location.file);
		llvm::DISubprogram* debug = codegen.debug->createFunction(file, llvm::StringRef(), llvm::StringRef(), file, n.begin_location.line, debug_fn_ty, n.begin_location.column, llvm::DINode::FlagPrototyped, llvm::DISubprogram::SPFlagDefinition);
		emit_empty_debug_location();

		if(!expr.is_extern)
		{
			llvm_func->setSubprogram(debug);
			lexical_blocks.push_back(debug);
			local->unfinished_types.back().val.ll = llvm_func;
			llvm::BasicBlock* bb = llvm::BasicBlock::Create(*codegen.ctx, "entry", llvm_func);
			codegen.ir->SetInsertPoint(bb);
			std::size_t counter = 0;
			for(llvm::Argument& arg : llvm_func->args())
			{
				std::string name = expr.params[counter].name;
				arg.setName(name + "_param");
				llvm::AllocaInst* llvm_param_var = codegen.ir->CreateAlloca(llvm_params[counter], nullptr, name);
				codegen.ir->CreateStore(&arg, llvm_param_var);
				local->pending_variables.at(std::string{name}).ll = llvm_param_var;

				llvm::DILocalVariable* dbg_var = codegen.debug->createParameterVariable(debug, name, counter + 1, file, n.begin_location.line, llvm_debug_params[counter], true);
				codegen.debug->insertDeclare(llvm_param_var, dbg_var, codegen.debug->createExpression(), llvm::DILocation::get(debug->getContext(), n.begin_location.line, 0, debug), codegen.ir->GetInsertBlock());

				counter++;
			}
			emit_debug_location(n);
		}
		ret.val.ll = llvm_func;
	}
	return ret;
}

semal_result semal_expr(const ast_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen);

bool expr_is_alias(const ast_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result param = semal_expr(expr, n, source, local, do_codegen);
	if(param.is_err())
	{
		return false;
	}
	return param.val.is_type;
}

bool expr_is_type(const ast_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result param = semal_expr(expr, n, source, local, do_codegen);
	if(param.is_err())
	{
		auto sym = AS_A(expr.expr_, ast_symbol_expr);
		return !local->parse_type(sym.symbol).is_badtype();
	}
	return param.val.is_type;
}

bool expr_is_struct(const ast_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result param = semal_expr(expr, n, source, local, do_codegen);
	if(param.is_err())
	{
		auto sym = AS_A(expr.expr_, ast_symbol_expr);
		return local->parse_type(sym.symbol).is_struct();
	}
	return param.val.ty.is_struct();
}

semal_result semal_call_builtin(const ast_callfunc_expr& call, node& n, std::string_view source, semal_local_state*& local, bool do_codegen);

semal_result semal(node& n, std::string_view source, semal_local_state* parent = nullptr, bool do_codegen = false);

semal_result semal_callfunc_expr(const ast_callfunc_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	fn_ty callee = {.return_ty = type_t::badtype()};
	emit_debug_location(n);
	// if you get around to static params and are emitting specific implementations, now is probably the time to do it.
	// use expr.function_name to create fake semal_decls of the implementation type(s) and re-use the functions here instead of searching for them.
	semal_result builtin_result = semal_call_builtin(expr, n, source, local, do_codegen);
	if(!builtin_result.is_null())
	{
		return builtin_result;
	}
	llvm::Value* llvm_fnval = nullptr;

	auto [local_iter, global_iter] = local->find_function(expr.function_name);
	if(local_iter != nullptr)
	{
		callee = local_iter->ty;
	}
	// you are allowed to call functions defined in other source files, so check the global iter too.
	else if(global_iter != nullptr)
	{
		callee = global_iter->ty;
		if(!global_iter->globally_visible)
		{
			return semal_result::err("call to inaccessible function \"{}\" (defined {})", expr.function_name, global_iter->loc);
		}
	}
	else
	{
		auto [mac_local, mac_global] = local->find_macro(expr.function_name);
		const semal_state2::macro_data_t* mac = nullptr;
		if(mac_global != nullptr)
		{
			mac = mac_global;
		}
		if(mac_local != nullptr)
		{
			mac = mac_local;
		}
		if(mac != nullptr)
		{
			// its a macro.
			// call it by just invoking its code right now.
			semal_result macroret = semal_result::null();
			const auto& macrodef_expr = *reinterpret_cast<const ast_macrodef_expr*>(mac->macrodef_expr);
			local->current_macros.push_back({mac->macrodef_expr, &expr});
			if(macrodef_expr.params.size() != expr.params.size())
			{
				return semal_result::err("macro expected {} params, you provided {}", macrodef_expr.params.size(), expr.params.size());
			}
			auto* nodeptr = try_get_block_child(*reinterpret_cast<node*>(mac->node));
			std::optional<srcloc> found_yield = std::nullopt;
			for(node& child : nodeptr->children)
			{
				// todo: add pending_macro_calls if its deferred.
				const auto& stmt = AS_A(child.payload, ast_stmt);
				if(IS_A(stmt.stmt_, ast_yield_stmt))
				{
					if(found_yield.has_value())
					{
						return semal_result::err("detected multiple yield statements in the same macro. previous was {}", found_yield.value());
					}
					const auto& yield = AS_A(stmt.stmt_, ast_yield_stmt);
					macroret = semal_expr(yield.retval, n, source, local, do_codegen);
					if(macroret.is_err())
					{
						return macroret;
					}
					type_t retty = local->parse_type(macrodef_expr.return_type);
					if(retty.is_void())
					{
						return semal_result::err("you cannot yield in a macro that yields v0");
					}
					if(macrodef_expr.return_type != deduced_type && !macroret.val.ty.is_convertible_to(retty))
					{
						return semal_result::err("yielded value was of type \"{}\" which is not convertible to the macro's yield type \"{}\"", macroret.val.ty.name(), retty.name());
					}
					found_yield = child.begin_location;
				}
				else
				{
					if(stmt.deferred)
					{
						local->pending_macro_calls.push_back(&child);
					}
					else
					{
						semal(child, source, local, do_codegen);
					}
				}
			}
			local->current_macros.pop_back();
			return macroret;
		}
		else
		{
			// ok this thing doesnt exist.
			// let's treat it as a symbol expression.
			ast_symbol_expr sym{.symbol = expr.function_name};
			ast_expr symexpr{.expr_ = ast_symbol_expr{.symbol = expr.function_name}};
			semal_result result = semal_expr(symexpr, n, source, local, do_codegen);
			if(result.is_err())
			{
				return result;
			}
			result.load_if_variable();
			if(result.val.ty.is_ptr())
			{
				auto pointee = *AS_A(result.val.ty.payload, ptr_ty).underlying_ty;
				if(pointee.is_fn())
				{
					// this is a function ptr. use ll as a function.
					llvm_fnval = result.val.ll;
					callee = AS_A(pointee.payload, fn_ty);
				}
			}
			if(llvm_fnval == nullptr)
			{
				return semal_result::err("unknown function \"{}\"", expr.function_name);
			}
		}
	}
	// ok we have a function to call.
	// result is just whatever the return type is.
	std::span<const ast_expr> actual_params = expr.params;
	std::span<const type_t> expected_params = callee.params;
	if(actual_params.size() != expected_params.size())
	{
		return semal_result::err("function \033[1;34m{} : {}\033[0m called with {} params when it expects {}", expr.function_name, callee.name(), actual_params.size(), expected_params.size());
	}
	std::vector<llvm::Value*> llvm_params;
	for(std::size_t i = 0; i < actual_params.size(); i++)
	{
		const type_t& expected_ty = expected_params[i];

		const ast_expr& actual = actual_params[i];
		semal_result actual_result = semal_expr(actual, n, source, local, do_codegen);
		if(actual_result.is_zero())
		{
			actual_result.val = zero_as(expected_ty);
		}
		if(actual_result.is_err())
		{
			return actual_result;
		}
		if(actual_result.val.is_type)
		{
			return semal_result::err("parameter {} in call to function \"{}\" was a type. function parameters cannot be types", i, expr.function_name);
		}
		const type_t& actual_ty = actual_result.val.ty;
		if(!actual_ty.is_convertible_to(expected_ty))
		{
			return semal_result::err("invalid parameter {} within call to function {} - provided type \"{}\" which does not convert to the param type \"{}\"", i, expr.function_name, actual_ty.name(), expected_ty.name());
		}
		if(do_codegen)
		{
			actual_result.load_if_variable();
			llvm_params.push_back(actual_result.val.convert_to(expected_ty));
		}
	}
	sval ret = wrap_type(*callee.return_ty);
	if(do_codegen)
	{
		// call function by name.
		const auto [local_fn, global_fn] = local->find_function(expr.function_name);
		llvm::Function* llvm_fn = nullptr;
		if(llvm_fnval == nullptr)
		{
			if (local_fn != nullptr)
			{
				llvm_fn = local_fn->location;
			}
			else
			{
				if (global_fn == nullptr)
				{
					panic("codegen: failed to call \"{}\" coz the function location was not found in local/global state. however, an error should already have been emitted if it wasnt defined by the user.", expr.function_name);
				}
				llvm_fn = global_fn->location;
			}
			ret.ll = codegen.ir->CreateCall(llvm_fn, llvm_params);
		}
		else
		{
			// call function by pointer.
			std::vector<llvm::Type*> llvm_param_types;
			for(const auto& param : callee.params)
			{
				llvm_param_types.push_back(param.llvm());
			}
			llvm::FunctionType* functy = llvm::FunctionType::get(callee.return_ty->llvm(), llvm_param_types, false);
			ret.ll = codegen.ir->CreateCall(functy, llvm_fnval, llvm_params);
		}
	}
	return
	{
		.t = semal_type::misc, .val = ret
	};
}

semal_result semal_symbol_expr(const ast_symbol_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	std::string_view symbol = expr.symbol;
	// so this could be alot of things.
	// option 1: a macro parameter. not treated like function params.
	for(const auto& [label, body] : local->current_macros)
	{
		const auto& macrodef = *reinterpret_cast<const ast_macrodef_expr*>(label);
		const auto& macro_call = *reinterpret_cast<const ast_callfunc_expr*>(body);
		for(std::size_t i = 0; i < macro_call.params.size(); i++)
		{
			const auto& param = macro_call.params[i];
			const auto& def = macrodef.params[i];
			type_t ty = local->parse_type(def.type_name);
			if(def.name == symbol)
			{
				semal_result res = semal_expr(param, n, source, local, do_codegen);
				if(def.type_name != deduced_type && !res.val.ty.is_convertible_to(ty))
				{
					return semal_result::err("parameter {} in call to macro is invalid. cannot convert from {} to {}", i, res.val.ty.name(), ty.name());
				}
				return res;
			}
		}
	}
	// option 2: a local variable.
	const auto [local_iter, global_iter] = local->find_variable(symbol);
	if(local_iter != nullptr)
	{
		return
		{
			.t = semal_type::variable_use,
			.label = std::string{symbol},
			.val = local_iter->var
		};
	}
	else if(global_iter != nullptr)
	{
		if(!global_iter->globally_visible)
		{
			return semal_result::err("use of inaccessible variable \"{}\" (declared {})", symbol, global_iter->loc);
		}
		return
		{
			.t = semal_type::variable_use,
			.label = std::string{symbol},
			.val = global_iter->var
		};
	}
	else
	{
		// perhaps you're trying to refer to a function as a variable?
		// that should yield a pointer to the function.
		const auto [fn_local, fn_global] = local->find_function(symbol);
		if(fn_local != nullptr)
		{
			fn_ty fnty = fn_local->ty;
			return
			{
				.t = semal_type::misc,
				.label = std::format("fnptr {}", symbol),
				.val =
				{
					.val = {},
					.ty = type_t::create_pointer_type(type_t{.payload = fnty}),
					.ll = fn_local->location
				}
			};
		}
		else if(fn_global != nullptr)
		{
			fn_ty fnty = fn_global->ty;
			return
			{
				.t = semal_type::misc,
				.label = std::format("fnptr {}", symbol),
				.val =
				{
					.val = {},
					.ty = type_t::create_pointer_type(type_t{.payload = fnty}),
					.ll = fn_global->location
				}
			};
		}
	}
	const auto [al_local, al_global] = local->find_alias(symbol);
	if(al_local != nullptr)
	{
		return
		{
			.t = semal_type::misc,
			.label = std::format("type"),
			.val =
			{
				.val = {},
				.ty = *al_local,
				.is_type = true
			}
		};
	}
	if(al_global != nullptr)
	{
		return
		{
			.t = semal_type::misc,
			.label = std::format("type"),
			.val =
			{
				.val = {},
				.ty = *al_global,
				.is_type = true
			}
		};
	}
	// i give up.
	return semal_result::err("unknown symbol \"{}\"", symbol);
}

semal_result semal_structdef_expr(const ast_structdef_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result ret = {.t = semal_type::struct_decl, .val = {.ty = {.payload = meta_ty{}}}};
	local->unfinished_types.push_back(ret);
	return ret;
}

semal_result semal_enumdef_expr(const ast_enumdef_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result ret = {.t = semal_type::enum_decl, .val = {.ty = {.payload = meta_ty{}}}};
	local->unfinished_types.push_back(ret);
	return ret;
}

semal_result semal_at_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	// rhs must be an array type.
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(lhs_result.is_err())
	{
		return lhs_result;
	}
	type_t lhs_ty = lhs_result.val.ty;
	type_t element_ty = type_t::badtype();
	std::optional<std::size_t> array_size = std::nullopt;
	if(lhs_ty.is_arr())
	{
		auto arr = AS_A(lhs_ty.payload, arr_ty);
		element_ty = *arr.underlying_ty;
		array_size = arr.array_length;
	}
	else if(lhs_ty.is_ptr())
	{
		element_ty = *AS_A(lhs_ty.payload, ptr_ty).underlying_ty;
	}
	else
	{
		return semal_result::err("lhs of 'at' operator is not an array or pointer type, but a \"{}\"", lhs_ty.name());
	}

	semal_result rhs_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(rhs_result.is_zero())
	{
		rhs_result.val = zero_as(type_t::create_primitive_type(prim_ty::type::u64));
	}
	if(rhs_result.is_err())
	{
		return rhs_result;
	}
	type_t rhs_ty = rhs_result.val.ty;
	if(!(rhs_ty.is_prim() && AS_A(rhs_ty.payload, prim_ty).is_integral()))
	{
		return semal_result::err("rhs of 'at' operator is not an type, but a \"{}\"", rhs_ty.name());
	}

	sval retval = wrap_type(type_t::create_pointer_type(element_ty));
	if(rhs_result.val.has_val() && array_size.has_value())
	{
		auto array_index = AS_A(AS_A(rhs_result.val.val, literal_val), std::int64_t);
		if(array_index >= array_size.value())
		{
			return semal_result::err("array index \"{}\" is out-of-bounds of the array (length {})", array_index, array_size.value());
		}
	}
	if(do_codegen)
	{
		rhs_result.load_if_variable();
		llvm::Value* idxlist[2];

		idxlist[0] = llvm::ConstantInt::get(*codegen.ctx, llvm::APInt{64, 0});
		idxlist[1] = rhs_result.val.ll;

		if(lhs_ty.is_ptr())
		{
			// it's not immediately clear to my why i dont load if its an array, but i do if its a pointer.
			// however, this is what works. im sure you'll figure it all out, sweetheart.
			lhs_result.load_if_variable();
			retval.ll = codegen.ir->CreateGEP(element_ty.llvm(), lhs_result.val.ll, rhs_result.val.ll);
		}
		else
		{
			retval.ll = codegen.ir->CreateGEP(lhs_result.val.ty.llvm(), lhs_result.val.ll, idxlist);
		}
	}
	return semal_result
	{
		.t = semal_type::misc,
		.label = std::format("{} at", lhs_result.label),
		.val = retval
	};
}

semal_result semal_cast_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	// x@y
	// x is always going to be some kind of expression.
	// y MUST be a symbol expression.
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(lhs_result.is_err())
	{
		return lhs_result;
	}
	if(IS_A(expr.rhs->expr_, ast_symbol_expr))
	{
		std::string_view symbol = AS_A(expr.rhs->expr_, ast_symbol_expr).symbol;
		type_t casted_to = type_t::badtype();
		if(symbol == "_")
		{
			if(lhs_result.is_zero() && lhs_result.val.ty.is_badtype())
			{
				return semal_result::err("it is invalid to perform the decay-idiom (@_) directly on a zero-init");
			}
			casted_to = lhs_result.val.ty.add_weak();
			if(casted_to.is_badtype())
			{
				return semal_result::err("could not deduct type to add weakness", symbol);
			}
		}
		else
		{
			casted_to = local->parse_type(symbol);
			if(casted_to.is_badtype())
			{
				return semal_result::err("invalid cast to unknown type \"{}\"", symbol);
			}
			if(lhs_result.is_zero())
			{
				lhs_result.val = zero_as(casted_to);
				return lhs_result;
			}
			
		}
		lhs_result.val.ty = lhs_result.val.ty.add_weak();
		type_t cast_from = lhs_result.val.ty;
		if(cast_from.is_convertible_to(casted_to))
		{
			// cool
			if(do_codegen)
			{
				lhs_result.load_if_variable();
				lhs_result.convert_to(casted_to);
			}
			lhs_result.val.ty = casted_to;
			return lhs_result;
		}
		else
		{
			return semal_result::err("invalid cast as \"{}\" cannot be explicitly converted to \"{}\"", cast_from.name(), casted_to.name());
		}
	}
	else
	{
		return semal_result::err("rhs of cast expression was a \"{} expression\" whereas a \"symbol expression\" was expected.", expr.rhs->type_name());
	}
}

semal_result semal_arith_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(lhs_result.is_zero())
	{
		return semal_result::err("zero-init cannot be the lhs of any arithmetic/bitwise operation");
	}
	if(lhs_result.is_err())
	{
		return lhs_result;
	}
	type_t lhs_ty = lhs_result.val.ty;
	// enums are allowed in case of bitwise operators.
	if(lhs_ty.is_enum() && (expr.type == biop_type::bitwise_and || expr.type == biop_type::bitwise_or || expr.type == biop_type::bitwise_exor))
	{
		auto enumty = AS_A(lhs_ty.payload, enum_ty);
		lhs_ty = *enumty.underlying_ty;
		lhs_ty.qual = lhs_ty.qual | typequal_weak;
	}
	if(!lhs_ty.is_prim())
	{
		return semal_result::err("lhs of arithmetic operation is not a primitive type, but a \"{}\"", lhs_ty.name());
	}
	auto lhs_prim = AS_A(lhs_ty.payload, prim_ty);
	if(!lhs_prim.is_numeric())
	{
		return semal_result::err("lhs of arithmetic operation is not a numeric type, but a \"{}\"", lhs_ty.name());
	}
	semal_result rhs_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(rhs_result.is_zero())
	{
		rhs_result.val = zero_as(lhs_result.val.ty);
	}
	if(rhs_result.is_err())
	{
		return rhs_result;
	}
	type_t rhs_ty = rhs_result.val.ty;
	// enums are allowed in case of bitwise operators.
	if(rhs_ty.is_enum() && (expr.type == biop_type::bitwise_and || expr.type == biop_type::bitwise_or || expr.type == biop_type::bitwise_exor))
	{
		auto enumty = AS_A(rhs_ty.payload, enum_ty);
		rhs_ty = *enumty.underlying_ty;
		rhs_ty.qual = rhs_ty.qual | typequal_weak;
	}
	if(!rhs_ty.is_prim())
	{
		return semal_result::err("rhs of arithmetic operation is not a primitive type, but a \"{}\"", rhs_ty.name());
	}
	auto rhs_prim = AS_A(rhs_ty.payload, prim_ty);
	if(!rhs_prim.is_numeric())
	{
		return semal_result::err("rhs of arithmetic operation is not a numeric type, but a \"{}\"", rhs_ty.name());
	}

	// ensure they are comparable.
	if(!lhs_ty.is_convertible_to(rhs_ty))
	{
		return semal_result::err("operands \"{}\" and \"{}\" of arithmetic operation are not convertible.", lhs_ty.name(), rhs_ty.name());
	}
	// if either are floating point, use that.
	// prefer bigger types too.
	prim_ty result_prim = lhs_prim;
	auto lhs_size = std::max(lhs_prim.integral_size(), lhs_prim.floating_point_size());
	auto rhs_size = std::max(rhs_prim.integral_size(), rhs_prim.floating_point_size());
	if(!lhs_prim.is_floating_point() && rhs_prim.is_floating_point())
	{
		result_prim = rhs_prim;
	}
	else if(rhs_size > lhs_size)
	{
		result_prim = rhs_prim;
	}

	sval result_val{.ty = type_t{.payload = result_prim}};
	if(lhs_ty.qual & typequal_weak || rhs_ty.qual & typequal_weak)
	{
		result_val.ty.qual = typequal_weak;
	}

	if(lhs_ty.qual & typequal_static && rhs_ty.qual & typequal_static)
	{
		result_val.ty.qual = result_val.ty.qual | typequal_static;
		double lhs_val;
		if(lhs_prim.is_floating_point())
		{
			lhs_val = std::get<double>(std::get<literal_val>(lhs_result.val.val));
		}
		else
		{
			lhs_val = std::get<std::int64_t>(std::get<literal_val>(lhs_result.val.val));
		}
		double rhs_val;
		if(rhs_prim.is_floating_point())
		{
			rhs_val = std::get<double>(std::get<literal_val>(rhs_result.val.val));
		}
		else
		{
			rhs_val = std::get<std::int64_t>(std::get<literal_val>(rhs_result.val.val));
		}

		if(lhs_prim.is_floating_point() || rhs_prim.is_floating_point())
		{
			// floating points aren't allowed with the bitwise operators.
			if(expr.type == biop_type::bitwise_and || expr.type == biop_type::bitwise_or || expr.type == biop_type::bitwise_exor)
			{
				return semal_result::err("floating point numbers are not allowed in bitwise operators. you have provided \"{}\" and \"{}\"", lhs_ty.name(), rhs_ty.name());
			}
		}

		double retval;
		using enum biop_type;
		switch(expr.type)
		{
			case plus:
				retval = lhs_val + rhs_val;
			break;
			case minus:
				retval = lhs_val - rhs_val;
			break;
			case mul:
				retval = lhs_val * rhs_val;
			break;
			case div:
				if(rhs_val == 0.0f)
				{
					return semal_result::err("divide by zero detected at compile time!");
				}
				retval = lhs_val / rhs_val;
			break;
			case bitwise_and:
				retval = static_cast<std::int64_t>(lhs_val) & static_cast<std::int64_t>(rhs_val);
			break;
			case bitwise_or:
				retval = static_cast<std::int64_t>(lhs_val) | static_cast<std::int64_t>(rhs_val);
			break;
			case bitwise_exor:
				retval = static_cast<std::int64_t>(lhs_val) ^ static_cast<std::int64_t>(rhs_val);
			break;
			case modulo:
				retval = static_cast<std::int64_t>(lhs_val) % static_cast<std::int64_t>(rhs_val);
			break;
			default:
				panic("aaah what arithmetic biop type is this???");
			break;
		}

		if(result_prim.is_floating_point())
		{
			result_val.val = literal_val{static_cast<double>(retval)};
		}
		else
		{
			result_val.val = literal_val{static_cast<std::int64_t>(retval)};
		}
	}

	if(do_codegen)
	{
		lhs_result.load_if_variable();
		rhs_result.load_if_variable();
		lhs_result.val.ll = lhs_result.val.convert_to(result_val.ty);
		rhs_result.val.ll = rhs_result.val.convert_to(result_val.ty);
		if(result_prim.is_integral())
		{
			switch(expr.type)
			{
				case biop_type::plus:
					result_val.ll = codegen.ir->CreateAdd(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::minus:
					result_val.ll = codegen.ir->CreateSub(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::mul:
					result_val.ll = codegen.ir->CreateMul(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::div:
					if(result_prim.is_signed_integral())
					{
						result_val.ll = codegen.ir->CreateSDiv(lhs_result.val.ll, rhs_result.val.ll);
					}
					else
					{
						result_val.ll = codegen.ir->CreateUDiv(lhs_result.val.ll, rhs_result.val.ll);
					}
				break;
				case biop_type::bitwise_and:
					result_val.ll = codegen.ir->CreateAnd(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::bitwise_or:
					result_val.ll = codegen.ir->CreateOr(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::bitwise_exor:
					result_val.ll = codegen.ir->CreateXor(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::modulo:
					if(result_prim.is_signed_integral())
					{
						result_val.ll = codegen.ir->CreateSRem(lhs_result.val.ll, rhs_result.val.ll);
					}
					else
					{
						result_val.ll = codegen.ir->CreateURem(lhs_result.val.ll, rhs_result.val.ll);
					}
				break;
				default:
					panic("aaah what arithmetic biop type is this???");
				break;
			}
		}
		else
		{
			panic_ifnt(result_prim.is_floating_point(), "waaah");
			switch(expr.type)
			{
				case biop_type::plus:
					result_val.ll = codegen.ir->CreateFAdd(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::minus:
					result_val.ll = codegen.ir->CreateFSub(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::mul:
					result_val.ll = codegen.ir->CreateFMul(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::div:
					result_val.ll = codegen.ir->CreateFDiv(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::modulo:
					return semal_result::err("modulo is not supported for floating-point types.");
				break;
				default:
					panic("aaah what arithmetic biop type is this???");
				break;
			}
		}
	}
	return semal_result
	{
		.t = semal_type::misc,
		.val = result_val
	};
}

semal_result semal_ptr_field_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	// expect lhs to be ptr to struct.
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(lhs_result.is_err())
	{
		return lhs_result;
	}
	semal_result cpy = lhs_result;
	type_t ty = lhs_result.val.ty;
	if(!ty.is_ptr())
	{
		return semal_result::err("expected pointer-type as lhs of -> expression, but got a \"{}\"", ty.name());
	}
	type_t underlying_ty = *AS_A(ty.payload, ptr_ty).underlying_ty;
	if(!underlying_ty.is_struct())
	{
		return semal_result::err("expected lhs of -> expression to be a pointer-to-struct, but got a \"{}\"", ty.name());
	}
	auto strty = AS_A(underlying_ty.payload, struct_ty);
	lhs_result.val.val = {};

	// absolutely *must* treat b as a symbol.
	if(IS_A(expr.rhs->expr_, ast_callfunc_expr))
	{
		// note there's nothing stopping you from copying over the callfunc functionality from semal_field...
		// i've chosen not to do it right now because it's a little complex.
		return semal_result::err("calling a function via struct-access ptr-field expression is not yet implemented. use the normal field expression (dot) instead.");
	}
	if(!IS_A(expr.rhs->expr_, ast_symbol_expr))
	{
		return semal_result::err("struct-access ptr-field expression is invalid. rhs of the ptr-field expression must be a symbol expression, but instead it is a {} expression", expr.rhs->type_name());
	}
	std::string_view rhs_symbol = AS_A(expr.rhs->expr_, ast_symbol_expr).symbol;

	auto iter = std::find(strty.member_order.begin(), strty.member_order.end(), rhs_symbol);
	panic_ifnt(iter != strty.member_order.end(), "wtf member order didnt contain member");
	std::size_t member_id = std::distance(strty.member_order.begin(), iter);

	lhs_result.val.ty = *strty.members.at(std::string{rhs_symbol});
	lhs_result.val.ty.qual = underlying_ty.qual;

	if(do_codegen)
	{
		cpy.load_if_variable();
		lhs_result.val.ll = codegen.ir->CreateStructGEP(underlying_ty.llvm(), cpy.val.ll, member_id);
	}
	return lhs_result;
}

semal_result semal_field_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	// two possible cases:
	// a.) some_struct_val.member (struct access)
	// b.) biop_type.cast (enum entry lvalue)
	// how do we know which one it is?
	// could try treating lhs as an expression (a). if it fails then (b). *but* gotta be careful if code is erroneous.
	// alternate method: if the expr is a symbol expression then treat the symbol as a direct typename. if the typename exists then (b) else (a)
	// 		but: alternative could absolutely have a lhs symbol expression. so we'd have to rely on typename parsing failure
	// they both arent bulletproof, but i think initial method is more ideal.
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(!lhs_result.is_err())
	{
		// lhs is a valid expression, assume it's a struct access.
		type_t ty = lhs_result.val.ty;
		if(!ty.is_struct())
		{
			return semal_result::err("lhs of field expression was itself a valid expression, which makes me think this field expression is a struct access. however, the lhs is of type \"{}\", which is not a struct. therefore the code is wrong.", ty.name());
		}
		auto strty = AS_A(ty.payload, struct_ty);
		// absolutely *must* treat b as a symbol.
		std::string_view rhs_symbol;
		bool is_function = false;
		std::optional<fn_ty> as_function = std::nullopt;
		std::span<const ast_expr> function_call_params = {};
		if(IS_A(expr.rhs->expr_, ast_callfunc_expr))
		{
			// okay
			const auto& call = AS_A(expr.rhs->expr_, ast_callfunc_expr);
			rhs_symbol = call.function_name;
			function_call_params = call.params;
			is_function = true;
		}
		else if(IS_A(expr.rhs->expr_, ast_symbol_expr))
		{
			rhs_symbol = AS_A(expr.rhs->expr_, ast_symbol_expr).symbol;
		}
		else
		{
			return semal_result::err("struct-access field expression is invalid. rhs of the field expression must be a symbol expression, but instead it is a {} expression", expr.rhs->type_name());
		}
		auto iter = strty.members.find(rhs_symbol);
		if(iter == strty.members.end())
		{
			return semal_result::err("struct-access field expression is invalid. struct \"{}\" has no member named \"{}\"", ty.name(), rhs_symbol);
		}
		// remember the struct member will inherit the qualifiers of the struct value.
		type_t member_ty = *iter->second;
		member_ty.qual = ty.qual;

		sval member{.ty = member_ty};
		if(ty.qual & typequal_static)
		{
			auto name = ty.name();
			panic_ifnt(IS_A(lhs_result.val.val, sval::struct_val), "sval of struct type \"{}\" {} did not have struct_val sval payload.", name, n.begin_location);
			auto structval = AS_A(lhs_result.val.val, sval::struct_val);
			auto iter = structval.find(rhs_symbol);
			panic_ifnt(iter != structval.end(), "static value of struct type \"{}\"'s struct_val did not contain an entry for member \"{}\"", name, rhs_symbol);
			member.val = iter->second.val;
		}
		if(is_function)
		{
			if(!member.ty.is_ptr())
			{
				return semal_result::err("attempt to call non-function-pointer member \"{}\" as function.", rhs_symbol);
			}
			auto member_ptr = AS_A(member.ty.payload, ptr_ty);
			auto member_underlying = *member_ptr.underlying_ty;
			if(!member_underlying.is_fn())
			{
				return semal_result::err("attempt to call non-function-pointer member \"{}\" as function.", rhs_symbol);
			}
			auto member_fnty = AS_A(member_underlying.payload, fn_ty);
			as_function = member_fnty;
			member = wrap_type(*member_fnty.return_ty);
		}
		if(do_codegen)
		{
			// expect lhs to be a struct.
			// need the member id
			auto iter = std::find(strty.member_order.begin(), strty.member_order.end(), rhs_symbol);
			panic_ifnt(iter != strty.member_order.end(), "wtf member order didnt contain member");
			std::size_t member_id = std::distance(strty.member_order.begin(), iter);
			// is the thing a variable?
			if(lhs_result.t == semal_type::variable_use)
			{
				// yes, use GEP
				member.ll = codegen.ir->CreateStructGEP(ty.llvm(), lhs_result.val.ll, member_id);
			}
			else
			{
				// no, use extractvalue.
				member.ll = codegen.ir->CreateExtractValue(lhs_result.val.ll, member_id);
			}

			if(is_function)
			{
				lhs_result.t = semal_type::misc;
				// load as ptr.
				llvm::Type* llvm_ptr_ty = llvm::PointerType::get(*codegen.ctx, 0);
				member.ll = codegen.ir->CreateLoad(llvm_ptr_ty, member.ll);
				// call it.
				std::vector<llvm::Value*> llvm_params;
				std::vector<llvm::Type*> llvm_param_types;
				for(const type_t& param_ty : as_function->params)
				{
					llvm_param_types.push_back(param_ty.llvm());
				}
				for(std::size_t i = 0; i < function_call_params.size(); i++)
				{
					const type_t& expected_ty = as_function->params[i];

					const ast_expr& param = function_call_params[i];
					semal_result actual_result = semal_expr(param, n, source, local, do_codegen);
					if(actual_result.is_zero())
					{
						actual_result.val = zero_as(expected_ty);
					}
					if(actual_result.is_err())
					{
						return actual_result;
					}
					const type_t& actual_ty = actual_result.val.ty;
					if(!actual_ty.is_convertible_to(expected_ty))
					{
						return semal_result::err("invalid parameter {} within call to function {} - provided type \"{}\" which does not convert to the param type \"{}\"", i, rhs_symbol, actual_ty.name(), expected_ty.name());
					}
					actual_result.load_if_variable();
					llvm_params.push_back(actual_result.val.convert_to(expected_ty));
				}

				llvm::FunctionType* functy = llvm::FunctionType::get(as_function->return_ty->llvm(), llvm_param_types, false);
				member.ll = codegen.ir->CreateCall(functy, member.ll, llvm_params);
			}
		}
		return
		{
			.t = lhs_result.t,
			.label = std::format("{}::{}", ty.name(), rhs_symbol),
			.val = member
		};
	}
	else
	{
		// we assume its an enum value.
		if(!IS_A(expr.lhs->expr_, ast_symbol_expr))
		{
			return semal_result::err("lhs of field expression was not a valid expression nor was it a symbol expression - the field expression is therefore invalid.");
		}
		std::string_view lhs_symbol = AS_A(expr.lhs->expr_, ast_symbol_expr).symbol;
		type_t ty = local->parse_type(lhs_symbol);
		if(ty.is_badtype())
		{
			return semal_result::err("lhs of field expression was a symbol expression \"{}\" but it did not yield a valid type", lhs_symbol);
		}
		if(!ty.is_enum())
		{
			return semal_result::err("lhs symbol expression \"{}\" within field expression is not an enum type. if you intended a struct-access, then the lhs is not a valid expression representing a struct either.", lhs_symbol);
		}
		auto enty = AS_A(ty.payload, enum_ty);

		// get the rhs.
		if(!IS_A(expr.rhs->expr_, ast_symbol_expr))
		{
			return semal_result::err("enum-access field expression is invalid. rhs of the field expression must be a symbol expression, but instead it is a {} expression", expr.rhs->type_name());
		}
		std::string_view rhs_symbol = AS_A(expr.rhs->expr_, ast_symbol_expr).symbol;
		auto iter = enty.entries.find(rhs_symbol);
		if(iter == enty.entries.end())
		{
			return semal_result::err("enum-access field expressionis invalid. enum \"{}\" has no entry named \"{}\"", lhs_symbol, rhs_symbol);
		}
		sval retval = wrap_type(ty);
		retval.val = literal_val{iter->second};
		retval.ty.qual = retval.ty.qual | typequal_static;
		if(do_codegen)
		{
			llvm::GlobalVariable* gvar = codegen.mod->getGlobalVariable(std::format("{}.{}", lhs_symbol, rhs_symbol));
			retval.ll = codegen.ir->CreateLoad(enty.underlying_ty->llvm(), gvar);
		}
		return
		{
			.t = semal_type::misc,
			.label = std::format("{}.{}", lhs_symbol, rhs_symbol),
			.val = retval
		};
	}
	std::unreachable();
}

semal_result semal_assign_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(lhs_result.is_zero())
	{
		return semal_result::err("the zero-initialiser cannot be assigned to");
	}
	if(lhs_result.is_err())
	{
		return lhs_result;
	}
	const type_t& lhs_ty = lhs_result.val.ty;
	if(!(lhs_ty.qual & typequal_mut))
	{
		return semal_result::err("attempt to assign to non-mut type \"{}\"", lhs_ty.name());
	}

	semal_result rhs_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(rhs_result.is_zero())
	{
		rhs_result.val = zero_as(lhs_ty);
	}
	if(rhs_result.is_err())
	{
		return rhs_result;
	}
	const type_t& rhs_ty = rhs_result.val.ty;

	if(!rhs_ty.is_convertible_to(lhs_ty))
	{
		return semal_result::err("assignment is invalid because rhs type \"{}\" cannot be converted to lhs type \"{}\"", rhs_ty.name(), lhs_ty.name());
	}

	if(do_codegen)
	{
		rhs_result.load_if_variable();
		rhs_result.convert_to(lhs_ty);
		bool is_atomic = false;
		if(lhs_result.label.starts_with("atomic deref") || lhs_result.label.starts_with("deref"))
		{
			if(lhs_result.label.starts_with("atomic deref"))
			{
				is_atomic = true;
			}
			// probably a deref
			// val.ll will contain the loaded value
			// but if a deref expr is the lhs of an assign, then we actually want the original ptr
			// which in this special case we keep tucked into usrdata2.
			panic_ifnt(lhs_result.val.usrdata2 != nullptr, "deref semal_result didn't contain usrdata2 pointing to a valid store location");
			lhs_result.val.ll = static_cast<llvm::Value*>(lhs_result.val.usrdata2);
		}
		codegen.ir->CreateStore(rhs_result.val.ll, lhs_result.val.ll);
		/*
		auto store_inst = codegen.ir->CreateStore(rhs_result.val.ll, lhs_result.val.ll);
		if(is_atomic)
		{
			store_inst->setAtomic(llvm::AtomicOrdering::Monotonic);
		}
		*/
	}
	lhs_result.val.ty = rhs_result.val.ty;
	lhs_result.val.val = rhs_result.val.val;

	return lhs_result;
}

semal_result semal_compare_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result lhs_result = semal_expr(*expr.lhs, n, source, local, do_codegen);
	if(lhs_result.is_zero())
	{
		return semal_result::err("zero-init cannot be on the lhs of any comparison");
	}
	if(lhs_result.is_err())
	{
		return lhs_result;
	}

	semal_result rhs_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(rhs_result.is_zero())
	{
		rhs_result.val = zero_as(lhs_result.val.ty);
	}
	if(rhs_result.is_err())
	{
		return rhs_result;
	}


	if(!rhs_result.val.ty.is_convertible_to(lhs_result.val.ty))
	{
		return semal_result::err("comparison invalid - cannot convert rhs \"{}\" type to lhs \"{}\"", rhs_result.val.ty.name(), lhs_result.val.ty.name());
	}

	if((lhs_result.val.ty.qual & typequal_static) && (rhs_result.val.ty.qual & typequal_static))
	{
		bool retbool;
		switch(expr.type)
		{
			case biop_type::compare_eq:
				retbool = lhs_result.val.val == rhs_result.val.val;
			break;
			case biop_type::compare_neq:
				retbool = lhs_result.val.val != rhs_result.val.val;
			break;
			case biop_type::less_than:
			{
				if(!lhs_result.val.ty.is_prim())
				{
					return semal_result::err("cannot pass non-primitives as operands to biop operator <");
				}
				auto prim = AS_A(lhs_result.val.ty.payload, prim_ty);
				if(prim.is_floating_point())
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), double) < AS_A(AS_A(rhs_result.val.val, literal_val), double);
				}
				else if(prim.p == prim_ty::type::boolean)
				{
					return semal_result::err("operator < is not valid with booleans");
				}
				else
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), std::int64_t) < AS_A(AS_A(rhs_result.val.val, literal_val), std::int64_t);
				}
			}
			break;
			case biop_type::greater_than:
			{
				if(!lhs_result.val.ty.is_prim())
				{
					return semal_result::err("cannot pass non-primitives as operands to biop operator <");
				}
				auto prim = AS_A(lhs_result.val.ty.payload, prim_ty);
				if(prim.is_floating_point())
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), double) > AS_A(AS_A(rhs_result.val.val, literal_val), double);
				}
				else if(prim.p == prim_ty::type::boolean)
				{
					return semal_result::err("operator > is not valid with booleans");
				}
				else
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), std::int64_t) > AS_A(AS_A(rhs_result.val.val, literal_val), std::int64_t);
				}
			}
			break;
			case biop_type::less_than_or_equal:
			{
				if(!lhs_result.val.ty.is_prim())
				{
					return semal_result::err("cannot pass non-primitives as operands to biop operator <");
				}
				auto prim = AS_A(lhs_result.val.ty.payload, prim_ty);
				if(prim.is_floating_point())
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), double) <= AS_A(AS_A(rhs_result.val.val, literal_val), double);
				}
				else if(prim.p == prim_ty::type::boolean)
				{
					return semal_result::err("operator <= is not valid with booleans");
				}
				else
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), std::int64_t) <= AS_A(AS_A(rhs_result.val.val, literal_val), std::int64_t);
				}
			}
			break;
			case biop_type::greater_than_or_equal:
			{
				if(!lhs_result.val.ty.is_prim())
				{
					return semal_result::err("cannot pass non-primitives as operands to biop operator <");
				}
				auto prim = AS_A(lhs_result.val.ty.payload, prim_ty);
				if(prim.is_floating_point())
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), double) >= AS_A(AS_A(rhs_result.val.val, literal_val), double);
				}
				else if(prim.p == prim_ty::type::boolean)
				{
					return semal_result::err("operator >= is not valid with booleans");
				}
				else
				{
					retbool = AS_A(AS_A(lhs_result.val.val, literal_val), std::int64_t) >= AS_A(AS_A(rhs_result.val.val, literal_val), std::int64_t);
				}
			}
			break;
			default:
				panic("unexpected biop type passed to semal_compare_...");
			break;
		}
		return semal_literal_expr({.value = retbool}, n, source, local, do_codegen);
	}

	sval retval
	{
		.ty = type_t::create_primitive_type(prim_ty::type::boolean)
	};
	if(do_codegen)
	{
		type_t ty = lhs_result.val.ty;
		lhs_result.load_if_variable();
		rhs_result.load_if_variable();
		rhs_result.convert_to(ty);
		if(ty.is_enum())
		{
			auto enty = AS_A(ty.payload, enum_ty);
			ty = *enty.underlying_ty;
		}

		if(ty.is_struct())
		{
			// member-wise compare
			panic("codegen: dont know how to compare structs");
		}
		else if(ty.is_ptr())
		{
			// ptr value comparison
			// note: icmp works on pointers too, so just use icmp
			switch(expr.type)
			{
				case biop_type::compare_eq:
					retval.ll = codegen.ir->CreateICmpEQ(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::compare_neq:
					retval.ll = codegen.ir->CreateICmpNE(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::less_than:
					retval.ll = codegen.ir->CreateICmpULT(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::greater_than:
					retval.ll = codegen.ir->CreateICmpUGT(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::less_than_or_equal:
					retval.ll = codegen.ir->CreateICmpULE(lhs_result.val.ll, rhs_result.val.ll);
				break;
				case biop_type::greater_than_or_equal:
					retval.ll = codegen.ir->CreateICmpUGE(lhs_result.val.ll, rhs_result.val.ll);
				break;
				default:
					std::unreachable();
				break;
			}
		}
		else if(ty.is_prim())
		{
			auto prim = AS_A(ty.payload, prim_ty);
			if(prim.is_integral() || prim.p == prim_ty::type::boolean)
			{
				bool is_signed = prim.is_signed_integral() || prim.p == prim_ty::type::boolean;
				switch(expr.type)
				{
					case biop_type::compare_eq:
						retval.ll = codegen.ir->CreateICmpEQ(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::compare_neq:
						retval.ll = codegen.ir->CreateICmpNE(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::less_than:
						if(is_signed)
						{
							retval.ll = codegen.ir->CreateICmpSLT(lhs_result.val.ll, rhs_result.val.ll);
						}
						else
						{
							retval.ll = codegen.ir->CreateICmpULT(lhs_result.val.ll, rhs_result.val.ll);
						}
					break;
					case biop_type::greater_than:
						if(is_signed)
						{
							retval.ll = codegen.ir->CreateICmpSGT(lhs_result.val.ll, rhs_result.val.ll);
						}
						else
						{
							retval.ll = codegen.ir->CreateICmpUGT(lhs_result.val.ll, rhs_result.val.ll);
						}
					break;
					case biop_type::less_than_or_equal:
						if(is_signed)
						{
							retval.ll = codegen.ir->CreateICmpSLE(lhs_result.val.ll, rhs_result.val.ll);
						}
						else
						{
							retval.ll = codegen.ir->CreateICmpULE(lhs_result.val.ll, rhs_result.val.ll);
						}
					break;
					case biop_type::greater_than_or_equal:
						if(is_signed)
						{
							retval.ll = codegen.ir->CreateICmpSGE(lhs_result.val.ll, rhs_result.val.ll);
						}
						else
						{
							retval.ll = codegen.ir->CreateICmpUGE(lhs_result.val.ll, rhs_result.val.ll);
						}
					break;
					default:
						std::unreachable();
					break;
				}
			}
			else if(prim.is_floating_point())
			{
				switch(expr.type)
				{
					case biop_type::compare_eq:
						retval.ll = codegen.ir->CreateFCmpUEQ(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::compare_neq:
						retval.ll = codegen.ir->CreateFCmpUNE(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::less_than:
						retval.ll = codegen.ir->CreateFCmpULT(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::greater_than:
						retval.ll = codegen.ir->CreateFCmpUGT(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::less_than_or_equal:
						retval.ll = codegen.ir->CreateFCmpULE(lhs_result.val.ll, rhs_result.val.ll);
					break;
					case biop_type::greater_than_or_equal:
						retval.ll = codegen.ir->CreateFCmpUGE(lhs_result.val.ll, rhs_result.val.ll);
					break;
					default:
						std::unreachable();
					break;
				}
			}
			else
			{
				auto name = ty.name();
				panic("codegen: dont know how to compare two \"{}\"'s {}", name, n.begin_location);
			}
		}
	}
	return
	{
		.t = semal_type::misc,
		.val = retval
	};

}

semal_result semal_biop_expr(const ast_biop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	using enum biop_type;
	switch(expr.type)
	{
		case cast:
			return semal_cast_biop_expr(expr, n, source, local, do_codegen);
		break;
		case plus:
		[[fallthrough]];
		case minus:
		[[fallthrough]];
		case mul:
		[[fallthrough]];
		case div:
		[[fallthrough]];
		case bitwise_and:
		[[fallthrough]];
		case bitwise_or:
		[[fallthrough]];
		case bitwise_exor:
		[[fallthrough]];
		case modulo:
			return semal_arith_biop_expr(expr, n, source, local, do_codegen);
		break;
		case field:
			return semal_field_biop_expr(expr, n, source, local, do_codegen);
		break;
		case assign:
			return semal_assign_biop_expr(expr, n, source, local, do_codegen);
		break;
		case compare_eq:
		[[fallthrough]];
		case compare_neq:
		[[fallthrough]];
		case less_than:
		[[fallthrough]];
		case greater_than:
		[[fallthrough]];
		case less_than_or_equal:
		[[fallthrough]];
		case greater_than_or_equal:
			return semal_compare_biop_expr(expr, n, source, local, do_codegen);
		break;
		case ptr_field:
		{
			return semal_ptr_field_biop_expr(expr, n, source, local, do_codegen);
			/*
			ast_unop_expr deref
			{
				.type = unop_type::deref,
				.rhs = expr.lhs
			};
			ast_biop_expr deref_expr = expr;
			deref_expr.lhs = ast_expr{.expr_ = deref};
			return semal_field_biop_expr(deref_expr, n, source, local, false);
			*/
		}
		break;
		case at:
			return semal_at_biop_expr(expr, n, source, local, do_codegen);
		break;
		default:
			panic("unknown biop type detected {}", n.begin_location);
		break;
	}
	std::unreachable();
}

semal_result semal_minus_unop_expr(const ast_unop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(result.is_err())
	{
		return result;
	}
	const type_t& ty = result.val.ty;
	if(!ty.is_prim())
	{
		return semal_result::err("attempt to negate non-primitive type \"{}\"", ty.name());
	}
	auto prim = AS_A(ty.payload, prim_ty);
	if(!prim.is_numeric())
	{
		return semal_result::err("attempt to negate non-numeric primitive \"{}\"", ty.name());
	}
	if(ty.qual & typequal_static)
	{
		auto& lit = AS_A(result.val.val, literal_val);
		if(IS_A(lit, std::int64_t))
		{
			lit = -AS_A(lit, std::int64_t);
		}
		else if(IS_A(lit, double))
		{
			lit = -AS_A(lit, double);
		}
		else
		{
			panic("failed to negate static value payload {}, as it wasn't a double nor int64", n.begin_location);
		}
	}
	if(do_codegen)
	{
		result.load_if_variable();
		if(prim.is_integral())
		{
			result.val.ll = codegen.ir->CreateNeg(result.val.ll);
		}
		else
		{
			panic_ifnt(prim.is_floating_point(), "waaah");
			result.val.ll = codegen.ir->CreateFNeg(result.val.ll);
		}
	}
	return result;
}

semal_result semal_invert_unop_expr(const ast_unop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result expr_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(expr_result.is_err())
	{
		return expr_result;
	}
	auto ty = expr_result.val.ty;
	if(!ty.is_prim())
	{
		return semal_result::err("operand of invert unary operator must be a bool or other integral type, you have provided a non-primitive \"{}\"", ty.name());
	}
	auto prim = AS_A(ty.payload, prim_ty);
	if(!prim.is_integral() && prim.p != prim_ty::type::boolean)
	{
		return semal_result::err("operand of invert unary operator must be a bool or other integral type. you have provided a \"{}\"", ty.name());
	}

	if(ty.qual & typequal_static)
	{
		auto& lit = AS_A(expr_result.val.val, literal_val);
		if(IS_A(lit, std::int64_t))
		{
			lit = ~AS_A(lit, std::int64_t);
		}
		else if(IS_A(lit, bool))
		{
			lit = !AS_A(lit, bool);
		}
		else
		{
			panic("failed to invert static value payload {}, as it wasn't a bool nor int64", n.begin_location);
		}
	}

	if(do_codegen)
	{
		expr_result.load_if_variable();
		expr_result.val.ll = codegen.ir->CreateNot(expr_result.val.ll);
	}
	return expr_result;
}

semal_result semal_ref_unop_expr(const ast_unop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	semal_result expr_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(expr_result.is_err())
	{
		return expr_result;
	}
	if(expr_result.t != semal_type::variable_use)
	{
		return semal_result::err("cannot ref a non-lvalue");
	}
	// todo: do not allow ref on an rvalue
	return
	{
		.t = expr_result.t == semal_type::variable_use ? semal_type::variable_ref : semal_type::misc,
		.label = std::format("ref {}", expr_result.label),
		.val = 
		{
			.val = std::monostate{},
			.ty = type_t::create_pointer_type(expr_result.val.ty),
			.ll = expr_result.val.ll
		}
	};
}

semal_result semal_deref_unop_expr(const ast_unop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool atomic, bool do_codegen)
{
	semal_result expr_result = semal_expr(*expr.rhs, n, source, local, do_codegen);
	if(expr_result.is_err())
	{
		return expr_result;
	}
	type_t ty = expr_result.val.ty;
	if(!ty.is_ptr())
	{
		return semal_result::err("cannot deref non-pointer-type \"{}\"", ty.name());
	}
	auto tyname = ty.name();
	ptr_ty ptr = AS_A(ty.payload, ptr_ty);
	ty = *ptr.underlying_ty;
	if (ty.is_void())
	{
		return semal_result::err("illegal deref of type \"{}\"", tyname);
	}

	semal_result ret
	{
		.t = semal_type::misc,
		.label = std::format("{} {}", atomic ? "atomic deref" : "deref", expr_result.label),
		.val =
		{
			.val = std::monostate{},
			.ty = ty
		}
	};
	if(do_codegen)
	{
		expr_result.load_if_variable();
		auto load_inst = codegen.ir->CreateLoad(ty.llvm(), expr_result.val.ll);
		if(atomic)
		{
			load_inst->setAtomic(llvm::AtomicOrdering::Monotonic);
		}
		ret.val.ll = load_inst;
		ret.val.usrdata2 = expr_result.val.ll;
	}
	return ret;
}

semal_result semal_alias_unop_expr(const ast_unop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	const auto& thing = *expr.rhs;
	if(!IS_A(thing.expr_, ast_symbol_expr))
	{
		return semal_result::err("rhs of 'alias' expr must be a symbol expression, but instead it is a {}", thing.type_name());
	}
	std::string_view tyname = AS_A(thing.expr_, ast_symbol_expr).symbol;

	return semal_result
	{
		.t = semal_type::alias_decl,
		.label = std::string{tyname},
		.val =
		{
			.val = {},
			.ty = local->parse_type(tyname),
			.is_type = true
		}
	};
}

semal_result semal_unop_expr(const ast_unop_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	using enum unop_type;
	switch(expr.type)
	{
		case minus:
			return semal_minus_unop_expr(expr, n, source, local, do_codegen);
		break;
		case invert:
			return semal_invert_unop_expr(expr, n, source, local, do_codegen);
		break;
		case ref:
			return semal_ref_unop_expr(expr, n, source, local, do_codegen);
		break;
		case deref:
			return semal_deref_unop_expr(expr, n, source, local, false, do_codegen);
		break;
		case atomic_deref:
			return semal_deref_unop_expr(expr, n, source, local, true, do_codegen);
		break;
		case alias:
			return semal_alias_unop_expr(expr, n, source, local, do_codegen);
		break;
		default:
			panic("unknown unop type detected {}", n.begin_location);
		break;
	}
	std::unreachable();
}

semal_result semal_designator_stmt(const ast_designator_stmt& designator_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen);

semal_result semal_blkinit_expr(const ast_blkinit_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	type_t ty = local->parse_type(expr.type_name);
	if(ty.is_badtype())
	{
		return semal_result::err("unknown type of blkinit expression \"{}\"", expr.type_name);
	}
	if(!ty.is_struct())
	{
		return semal_result::err("blkinit expression of type \"{}\" encountered, which is not a struct type. you can only blkinit struct types.", expr.type_name);
	}
	sval empty_structinit = wrap_type(ty);
	sval::struct_val table = {};
	semal_result blk{.t = semal_type::blkinit, .label = expr.type_name, .val = empty_structinit};
	local->unfinished_types.push_back(blk);
	auto& structinit = blk.val;
	bool is_static = true;
	// codegen will need to do a bunch of work here to actually create the structinit.
	std::unordered_map<std::size_t, llvm::Value*> llvm_initialisers = {};
	for(const ast_designator_stmt& desig : expr.initialisers)
	{
		semal_result desig_result = semal_designator_stmt(desig, n, source, local, do_codegen);
		if(desig_result.is_err())
		{
			return desig_result;
		}
		if(!(desig_result.val.ty.qual & typequal_static))
		{
			is_static = false;
		}
		if(desig_result.val.has_val())
		{
			table[desig.name] = desig_result.val;
		}
		if(desig_result.is_err())
		{
			return desig_result;
		}
		if(do_codegen)
		{
			// which member index are we?
			auto struct_type = AS_A(ty.payload, struct_ty);
			auto iter = std::find(struct_type.member_order.begin(), struct_type.member_order.end(), desig.name);
			panic_ifnt(iter != struct_type.member_order.end(), "wtf member order didnt contain member");
			std::size_t member_id = std::distance(struct_type.member_order.begin(), iter);

			desig_result.load_if_variable();
			desig_result.convert_to(*struct_type.members.at(desig.name));
			llvm_initialisers.emplace(member_id, desig_result.val.ll);
		}
	}
	if(is_static)
	{
		structinit.val = table;
		structinit.ty.qual = structinit.ty.qual | typequal_static;
		/*
		todo: enable this when static structval is supported
		dont do the runtime codegen unless we're not static at that point.
		if(do_codegen)
		{
			blk.val.ll = structinit.llvm();
		}
		*/
	}
	//else
	//{
		if(do_codegen)
		{
			// do it the old fashioned way.
			llvm::Type* llvm_struct_ty = structinit.ty.llvm();
			llvm::Value* structval = llvm::UndefValue::get(llvm_struct_ty);
			for(const auto& [member_id, val] : llvm_initialisers)
			{
				auto name = structinit.ty.name();
				panic_ifnt(val != nullptr, "detected nullptr llvm initialiser for member {} of struct {}", member_id, name);
				structval = codegen.ir->CreateInsertValue(structval, val, member_id);
			}
			blk.val.ll = structval;
		}
	//}
	local->unfinished_types.pop_back();
	return blk;
}

semal_result semal_expr(const ast_expr& expr, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	if(IS_A(expr.expr_, ast_literal_expr))
	{
		return semal_literal_expr(AS_A(expr.expr_, ast_literal_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_zero_expr))
	{
		return semal_result::zero();
	}
	else if(IS_A(expr.expr_, ast_null_expr))
	{
		return semal_result::zero();
	}
	else if(IS_A(expr.expr_, ast_funcdef_expr))
	{
		return semal_funcdef_expr(AS_A(expr.expr_, ast_funcdef_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_macrodef_expr))
	{
		return semal_macrodef_expr(AS_A(expr.expr_, ast_macrodef_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_callfunc_expr))
	{
		return semal_callfunc_expr(AS_A(expr.expr_, ast_callfunc_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_symbol_expr))
	{
		return semal_symbol_expr(AS_A(expr.expr_, ast_symbol_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_structdef_expr))
	{
		return semal_structdef_expr(AS_A(expr.expr_, ast_structdef_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_enumdef_expr))
	{
		return semal_enumdef_expr(AS_A(expr.expr_, ast_enumdef_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_biop_expr))
	{
		return semal_biop_expr(AS_A(expr.expr_, ast_biop_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_unop_expr))
	{
		return semal_unop_expr(AS_A(expr.expr_, ast_unop_expr), n, source, local, do_codegen);
	}
	else if(IS_A(expr.expr_, ast_blkinit_expr))
	{
		return semal_blkinit_expr(AS_A(expr.expr_, ast_blkinit_expr), n, source, local, do_codegen);
	}
	else
	{
		return semal_result::err("dont know how to semal_expr a \"{}\"", expr.type_name());
	}
	return semal_result::err("unreachable code hit within semal_expr (is one of the cases not returning as it should?)");
}

semal_result semal_decl(const ast_decl& decl, node& n, std::string_view source, semal_local_state*& local, bool do_codegen, const attributes_t& attributes)
{
	// i will need to parse types, give me access to the type system.
	// if we are in a local scope then use it from there
	bool maybe_globally_visible = true;

	sval val = wrap_type(type_t::badtype());
	semal_result init_result;
	bool is_zero_init = false;
	bool init_is_static = false;
	if(decl.initialiser.has_value())
	{
		init_result = semal_expr(decl.initialiser.value(), n, source, local, do_codegen);
		if(init_result.is_zero())
		{
			is_zero_init = true;
			if(decl.type_name == deduced_type)
			{
				return semal_result::err("if a decl has a zero-initialiser, then you must explicitly define its type.");
			}
		}
		if(init_result.is_err())
		{
			return init_result;
		}
		if(init_result.val.ty.is_void())
		{
			return semal_result::err("cannot initialise declaration with initialiser of type \"{}\"", init_result.val.ty.name());
		}
		init_is_static = (init_result.val.ty.qual & typequal_static);
		val = init_result.val;
	}
	if(decl.type_name != deduced_type)
	{
		// user has told us the type.
		auto [parse_ty, only_found_in_global] = local->parse_type_global_fallback(decl.type_name);
		if(parse_ty.is_badtype())
		{
			return semal_result::err("decl \"{}\"'s explicit type \"{}\" was unknown{}", decl.name, decl.type_name, !(parse_ty.is_badtype()) ? " \nnote: i could find this type globally but it is not accesible in this scope." : "");
		}
		if(is_zero_init)
		{
			val = zero_as(parse_ty);
			init_result.val = val;
		}
		else
		{
			if(decl.initialiser.has_value())
			{
				// make sure its convertible to the initialiser expression if it exists.
				if(!val.ty.is_convertible_to(parse_ty))
				{
					return semal_result::err("decl \"{}\" was defined with explicit type \"{}\", but the initialiser expression type \"{}\" is not implicitly convertible", decl.name, decl.type_name, val.ty.name());
				}
				if((parse_ty.qual & typequal_static) && !init_is_static)
				{
					return semal_result::err("decl \"{}\" of static type \"{}\" had non-static initialiser of type \"{}\"", decl.name, parse_ty.name(), init_result.val.ty.name());
				}
			}
			val.ty = parse_ty;
		}
	}
	if(attributes.contains("__force_mutable"))
	{
		// remove static and force mut
		val.ty.qual = static_cast<typequal>(val.ty.qual & ~typequal_static);
		val.ty.qual = val.ty.qual | typequal_mut;
	}
	if(val.ty.qual & typequal_static && !decl.initialiser.has_value())
	{
		return semal_result::err("decl \"{}\" is of static type \"{}\" but no initialiser. static variables cannot be left uninitialised.", decl.name, val.ty.name());
	}
	if(init_result.t == semal_type::alias_decl)
	{
		local->declare_alias(decl.name, local->parse_type(init_result.label));
		return init_result;
	}
	if(val.ty.is_badtype())
	{
		// initialiser did not have a type
		// this means the decl is either a macro definition or a alias
		if(init_result.t == semal_type::macro_decl)
		{
			auto* macrodef = reinterpret_cast<const ast_macrodef_expr*>(init_result.val.usrdata);
			auto* nodeptr = reinterpret_cast<node*>(init_result.val.usrdata2);
			local->declare_macro(decl.name, { .macrodef_expr = macrodef, .node = nodeptr });
			return init_result;
		}
		
		else
		{
			panic("fooey");
		}
	}
	panic_ifnt(!val.ty.is_badtype(), "did not expect semal_decl to return a bad type.");
	semal_result ret
	{
		.label = decl.name,
		.val = val,
	};
	if(val.ty.is_fn())
	{
		for(const auto& [name, maybe_expr] : attributes)
		{
			if(name == "private")
			{
				maybe_globally_visible = false;
			}
			else
			{
				warning(n.begin_location, "irrelevant attribute \"{}\" ignored", name);
			}
		}
		panic_ifnt(init_result.t == semal_type::function_decl, "noticed decl \"{}\" {} with initialiser being a function definition, but the expression did not correctly register itself as a function decl.", decl.name, n.begin_location);
		auto* llvm_func = static_cast<llvm::Function*>(init_result.val.ll);
		llvm_func->setName(decl.name);
		//llvm_func->getSubprogram()->setName(decl.name);
		// we are declaring a function!
		ret.t = semal_type::function_decl;
		auto ty = std::get<fn_ty>(init_result.val.ty.payload);

		local->declare_function(decl.name, ty, llvm_func, n.begin_location, maybe_globally_visible);

		// if its not extern then we expect an unfinished_type with no label.
		// if there is one, let's update its name as it will be unlabeled (the funcdef_expr doesnt know its own name)
		if(local->unfinished_types.size())
		{
			auto& last = local->unfinished_types.back();
			if(last.t == semal_type::function_decl)
			{
				panic_ifnt(last.label.empty(), "did not expect a non-empty label for an unfinished type produced what i expect to be a funcdef_expr. i want to fill in its name. please dont give it a name or change me.");
				// note: nothing relies on this, i am just doing this to improve debuggability (which function is this unfinished type representing???)
				last.label = decl.name;	
			}
		}
	}
	else
	{
		emit_debug_location(n);
		if(val.ty.is_type())
		{
			for(const auto& [name, maybe_expr] : attributes)
			{
				warning(n.begin_location, "irrelevant attribute \"{}\" ignored", name);
			}
			using enum semal_type;
			panic_ifnt(init_result.t == struct_decl || init_result.t == enum_decl, "noticed decl \"{}\" {} with initialiser being a struct or enum, but the expression did not correctly register itself as a struct/enum decl.", decl.name, n.begin_location);
			if(!decl.initialiser.has_value())
			{
				auto meta = AS_A(val.ty.payload, meta_ty);
				return semal_result::err("detected lack of initialiser/explicit typing when defining a struct/enum. this is not how you declare a {}. remove the explicit typename (i.e no \" : {}\") and defer it instead via ::=", meta.underlying_typename, decl.type_name);
			}
			local->unfinished_types.back().label = decl.name;
			switch(init_result.t)
			{
				case semal_type::struct_decl:
					ret.t = struct_decl;
					local->declare_struct(decl.name, struct_ty{}, n.begin_location);
				break;
				case semal_type::enum_decl:
					ret.t = enum_decl;
					local->declare_enum(decl.name, enum_ty{.underlying_ty = type_t::create_primitive_type(prim_ty::type::s64)}, n.begin_location);
				break;
				default:
					std::unreachable();
				break;
			}
			// ret.t = semal_result::type::struct_decl or enum_decl?
			// not sure if i need to do anything else here. your call, future harry.
		}
		else
		{
			const bool has_parent = local->parent != nullptr;
			const bool could_be_struct_member = has_parent && local->parent->unfinished_types.size();
			std::string maybe_struct_parent = "";
			if(could_be_struct_member)
			{
				const auto& last_unfinished = local->parent->unfinished_types.back();
				if(last_unfinished.t == semal_type::struct_decl)
				{
					maybe_struct_parent = last_unfinished.label;
				}
			}
			if(maybe_struct_parent.size())
			{
				bool added = local->struct_add_member(maybe_struct_parent, decl.name, ret.val.ty);
				if(!added) [[unlikely]]
				{
					panic("attempted to add member \"{}\" to non-existent struct \"{}\" {}. the logic of this code path implies the struct *must* exist. please submit a bug report.", decl.name, maybe_struct_parent, n.begin_location);
				}
			}
			else if(local->unfinished_types.size())
			{
				// could be that we're a parameter of a function currently being defined.
				const auto& last_unfinished = local->unfinished_types.back();
				if (last_unfinished.t == semal_type::function_decl)
				{
					// dont need to do anything here. you would think "why not add us as a param?"
					// well, at this point, we are called by semal_funcdef_expr which doesnt know its own name
					// so we let it deal with that, and just make sure we dont register this as a local variable here.
				}
			}
			else
			{
				bool external_linkage = false;
				bool is_thread_local = false;
				for (const auto& [name, maybe_expr] : attributes)
				{
					if(name == "public_linkage")
					{
						external_linkage = true;
					}
					else if(name == "thread_local")
					{
						is_thread_local = true;
					}
					else if(name == "private")
					{
						maybe_globally_visible = false;
					}
					else if(!name.starts_with("__"))
					{
						warning(n.begin_location, "irrelevant attribute \"{}\" ignored", name);
					}
				}
				if(do_codegen)
				{
					if (decl.initialiser.has_value())
					{
						init_result.load_if_variable();
						init_result.val.ll = init_result.val.convert_to(ret.val.ty);
					}
					
					llvm::DIFile* file = debug_files.at(n.begin_location.file);
					if(local->scope == scope_type::translation_unit)
					{
						// this is a global variable.
						if(init_result.val.ty.is_badtype())
						{
							init_result.val.ty = ret.val.ty;
						}
						ret.val.ll = codegen.declare_global_variable(decl.name, ret.val.ty, init_result.val, external_linkage, is_thread_local);
						llvm::DIGlobalVariable* dbg_var = codegen.debug->createTempGlobalVariableFwdDecl(file, decl.name, decl.name, file, n.begin_location.line, ret.val.ty.debug_llvm(), true);
					}
					else
					{
						llvm::AllocaInst* llvm_var = codegen.ir->CreateAlloca(ret.val.ty.llvm(), nullptr, decl.name);

						if(decl.initialiser.has_value())
						{
							codegen.ir->CreateStore(init_result.val.ll, llvm_var);
						}
						ret.val.ll = llvm_var;

						llvm::DILocalVariable* dbg_var = codegen.debug->createAutoVariable(debug_get_scope(n), decl.name, file, n.begin_location.line, ret.val.ty.debug_llvm());
						const semal_result* parent_fn = local->try_find_parent_function();
						auto* debug = static_cast<llvm::Function*>(parent_fn->val.ll)->getSubprogram();
						codegen.debug->insertDeclare(llvm_var, dbg_var, codegen.debug->createExpression(), llvm::DILocation::get(debug->getContext(), n.begin_location.line, 0, debug), codegen.ir->GetInsertBlock());
					}
				}

				local->declare_variable(decl.name, ret.val, n.begin_location, maybe_globally_visible, attributes.contains("__force_mutable"));
				ret.t = semal_type::variable_decl;
			}
		}
	}
	return ret;
}

semal_result semal_decl_stmt(const ast_decl_stmt& decl_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen, const attributes_t& attributes)
{
	return semal_decl(decl_stmt.decl, n, source, local, do_codegen, attributes);
}

semal_result semal_expr_stmt(const ast_expr_stmt& expr_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	return semal_expr(expr_stmt.expr, n, source, local, do_codegen);
}

semal_result semal_return_stmt(const ast_return_stmt& return_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	const semal_result* maybe_parent = local->try_find_parent_function();
	if(maybe_parent == nullptr)
	{
		return semal_result::err("detected return statement but we are not in a function definition.");
	}
	std::string_view parent_function_name = maybe_parent->label;
	type_t expected_return_ty = *AS_A(maybe_parent->val.ty.payload, fn_ty).return_ty;
	if(return_stmt.retval.has_value())
	{
		semal_result ret = semal_expr(return_stmt.retval.value(), n, source, local, do_codegen);
		if(ret.is_zero())
		{
			ret.val = zero_as(expected_return_ty);
		}
		if(ret.is_err())
		{
			return ret;
		}
		if(!ret.val.ty.is_convertible_to(expected_return_ty))
		{
			return semal_result::err("return value is of type \"{}\" which is not convertible to the enclosing function \"{}\"'s return type of \"{}\"", ret.val.ty.name(), parent_function_name, expected_return_ty.name());
		}
		if(do_codegen)
		{
			ret.load_if_variable();
			ret.convert_to(expected_return_ty);
			ret.val.ll = codegen.ir->CreateRet(ret.val.ll);
		}
		return ret;
	}
	else
	{
		if(do_codegen)
		{
			codegen.ir->CreateRetVoid();
		}
		return {.label = "return"};
	}
}

semal_result semal_metaregion_stmt(const ast_metaregion_stmt& metaregion_stmt, node& n, std::string_view source, semal_local_state*& local)
{
	// time to define some functions.
	type_t string_literal = type_t::create_pointer_type(type_t::create_primitive_type(prim_ty::type::u8));
	fn_ty stringparam_noret
	{
		.params = 
		{
			string_literal
		},
		.return_ty = type_t::create_void_type()
	};

	local->pending_functions.emplace("add_link_library", stringparam_noret);
	local->pending_functions.emplace("add_source_file", stringparam_noret);
	local->pending_functions.emplace("run_command", stringparam_noret);
	local->pending_functions.emplace("set_library", stringparam_noret);
	local->pending_functions.emplace("set_object", stringparam_noret);
	local->pending_functions.emplace("set_executable", stringparam_noret);
	local->pending_functions.emplace("set_optimisation", fn_ty{.params = {type_t::create_primitive_type(prim_ty::type::u64).add_weak()}, .return_ty = type_t::create_void_type()});
	local->pending_functions.emplace("set_output_directory", stringparam_noret);
	local->pending_functions.emplace("bundle_file", stringparam_noret);
	local->pending_functions.emplace("bundle_directory", stringparam_noret);
	local->pending_functions.emplace("enable_debug_symbols", fn_ty{.params = {type_t::create_primitive_type(prim_ty::type::boolean)}, .return_ty = type_t::create_void_type()});
	return semal_result::null();
}

semal_result semal_designator_stmt(const ast_designator_stmt& designator_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	std::string_view desig_name = designator_stmt.name;
	const ast_expr& desig_expr = *designator_stmt.initialiser;
	// so a designator has a different meaning depending on its context
	if(local->unfinished_types.size() && local->unfinished_types.back().t == semal_type::blkinit)
	{
		const auto& data = local->unfinished_types.back();
		// option A: a designator is a part of a block-initialiser (struct). in which case we should search for an unfinished blkinit in *this* local scope
		std::string_view struct_tyname = data.label;
		type_t ty = local->parse_type(struct_tyname);
		panic_ifnt(ty.is_struct(), "waaaoh blkinit for non-struct type even though i checked it earlier");
		const string_map<box<type_t>>& members = AS_A(ty.payload, struct_ty).members;
		// let's make sure the member exists.
		auto iter = members.find(designator_stmt.name);
		if(iter == members.end())
		{
			return semal_result::err("struct \"{}\" has no member named \"{}\"", struct_tyname, desig_name);
		}
		// type-check the member.
		type_t expected_ty = *iter->second;
		semal_result actual_result = semal_expr(desig_expr, n, source, local, do_codegen);
		if(actual_result.is_zero())
		{
			actual_result.val = zero_as(expected_ty);
		}
		if(actual_result.is_err())
		{
			return actual_result;
		}
		actual_result.load_if_variable();
		if(!actual_result.val.ty.is_convertible_to(expected_ty))
		{
			return semal_result::err("designator \"{}::{}\" is given expression of type \"{}\", which is not convertible to the actual type \"{}\"", struct_tyname, desig_name, actual_result.val.ty.name(), expected_ty.name());
		}
		if(do_codegen)
		{
			actual_result.convert_to(expected_ty);
		}
		// todo: codegen needs to do a conversion here if the types are convertible but dont exactly match.
		if(actual_result.val.ty.qual & typequal_static)
		{
			// if the init expr of a designator is static, then the designator itself should yield static
			// this is safe as (T static) can convert to T.
			// and in some cases if all designators are static then some useful optimisation can occur.
			expected_ty.qual = expected_ty.qual | typequal_static;
		}
		actual_result.val.ty = expected_ty;
		return actual_result;
	}
	// option B: a designator is part of an enum definition. in which case we should  search for an unfinite enum_decl in the *parent* of this local scope.
	error_ifnt(local->parent != nullptr, n.begin_location, "designator statement did not have a preceding local context. you should only provide a designator statement in a block-initialiser or an enum-definition.");
	semal_local_state& parent = *local->parent;
	error_ifnt(parent.unfinished_types.size(), n.begin_location, "designator statement's parent local state did not have any unfinished types. is this a blkinit that i haven't covered yet?");
	const semal_result& parent_result = parent.unfinished_types.front();
	// give me the type of the expr.
	// no codegen here for enum values because we codegen later (see the end of semal(...) impl when we pop context)
	semal_result entry_value = semal_expr(desig_expr, n, source, local, false);
	if(entry_value.is_err())
	{
		return entry_value;
	}
	switch(parent_result.t)
	{
		case semal_type::enum_decl:
			parent.enum_add_entry(parent_result.label, designator_stmt.name, entry_value.val);
		break;
		default:
			// option C: code is wrong.
			return semal_result::err("invalid designator statement. expected to be within either an enum definition or a block-initialiser. this appears to be in neither.");
		break;
	}
	return semal_result::null();
}

semal_result semal_if_stmt(const ast_if_stmt& if_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	semal_result cond_result = semal_expr(if_stmt.condition, n, source, local, do_codegen);
	if(cond_result.is_err())
	{
		return cond_result;
	}
	// go through all child nodes
	// if they are decl statements, do them now.
	// this is for codegen reasons.
	// what we really really do not want to do is alloca in a loop. i know an if statement is not a loop but we will do this for while/for so lets be consistent.
	/*
	node* blk = try_get_block_child(n);
	if(blk != nullptr && do_codegen)
	{
		for(auto iter = blk->children.begin(); iter != blk->children.end();)
		{
			auto& child = *iter;
			if(IS_A(child.payload, ast_stmt))
			{
				auto child_stmt = AS_A(child.payload, ast_stmt);
				if(IS_A(child_stmt.stmt_, ast_decl_stmt))
				{
					auto child_decl_stmt = AS_A(child_stmt.stmt_, ast_decl_stmt);
					semal_decl_stmt(child_decl_stmt, child, source, local, do_codegen, child_stmt.attributes);
					iter = blk->children.erase(iter);
					continue;
				}
			}
			iter++;
		}
	}
	*/
	type_t expected_cond_ty = type_t::create_primitive_type(prim_ty::type::boolean);
	const type_t& actual_cond_ty = cond_result.val.ty;
	const semal_result* maybe_parent = local->try_find_parent_function();
	if(if_stmt.is_static)
	{
		expected_cond_ty.qual = typequal_static;
	}
	else
	{
		if(maybe_parent == nullptr)
		{
			return semal_result::err("detected if statement but we are not in a function definition.");
		}
	}
	if(!actual_cond_ty.is_convertible_to(expected_cond_ty))
	{
		return semal_result::err("if-statement condition is of invalid type \"{}\" as it is not convertible to a \"\"", actual_cond_ty.name(), expected_cond_ty.name());
	}
	if(if_stmt.is_static)
	{
		// clear out all child nodes if the condition is false.
		bool cond = AS_A(AS_A(cond_result.val.val, literal_val), bool);
		if(!cond)
		{
			n.children.clear();
		}
	}
	else
	{
		if(do_codegen && !if_stmt.is_static)
		{
			cond_result.load_if_variable();
			cond_result.convert_to(expected_cond_ty);
			llvm::Function* parent_fn = static_cast<llvm::Function*>(maybe_parent->val.ll);
			llvm::BasicBlock* true_blk = llvm::BasicBlock::Create(*codegen.ctx, "then", parent_fn);
			llvm::BasicBlock* cont_blk = llvm::BasicBlock::Create(*codegen.ctx, "ifcont");

			parent_fn->insert(parent_fn->end(), cont_blk);
			codegen.ir->CreateCondBr(cond_result.val.ll, true_blk, cont_blk);
			codegen.ir->SetInsertPoint(true_blk);

			bool true_blk_contains_ret = false;
			node* blk = try_get_block_child(n);
			if(blk == nullptr)
			{
				panic("if statement didnt have a block child?");
			}
			for(const auto& child : blk->children)
			{
				if(IS_A(child.payload, ast_stmt))
				{
					const ast_stmt& child_stmt = AS_A(child.payload, ast_stmt);
					if(IS_A(child_stmt.stmt_, ast_return_stmt))
					{
						true_blk_contains_ret = true;	
					}
				}
			}
			sval checker_val
			{
				.ll = cont_blk
			};
			if(true_blk_contains_ret)
			{
				checker_val.val = literal_val{};
			}
			local->unfinished_types.push_back({.t = semal_type::if_stmt, .val = checker_val});
		}
	}
	return semal_result::null();
}

void hoist_variable_decls(node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	if(do_codegen)
	{
		for(auto iter = n.children.begin(); iter != n.children.end();)
		{
			auto& child = *iter;
			if(IS_A(child.payload, ast_stmt))
			{
				auto child_stmt = AS_A(child.payload, ast_stmt);
				if(IS_A(child_stmt.stmt_, ast_decl_stmt))
				{
					auto child_decl_stmt = AS_A(child_stmt.stmt_, ast_decl_stmt);
					child_stmt.attributes =
					{
						{"__force_mutable", std::nullopt}
					};
					type_t decl_init_ty = type_t::badtype();
					auto decl_init = child_decl_stmt.decl.initialiser;
					child_decl_stmt.decl.initialiser = std::nullopt;
					if(decl_init.has_value())
					{
						auto init_result = semal_expr(decl_init.value(), child, source, local, false);
						if(init_result.is_err())
						{
							panic("booey");
						}
						decl_init_ty = init_result.val.ty;
					}
					if(child_decl_stmt.decl.type_name == deduced_type)
					{
						// little hack here.
						// if the decl_init is a function pointer, don't include any qualifiers in the name.
						// trust me on this one.
						if(decl_init_ty.is_ptr())
						{
								auto underlying = AS_A(decl_init_ty.payload, ptr_ty);
								if(underlying.underlying_ty->is_fn())
								{
									decl_init_ty.qual = typequal_none;
								}
						}
						child_decl_stmt.decl.type_name = decl_init_ty.name();
					}
					semal_decl_stmt(child_decl_stmt, child, source, local, do_codegen, child_stmt.attributes);
					// convert the actual child to an *assignment*
					if(decl_init.has_value())
					{
						AS_A(child.payload, ast_stmt).stmt_ = ast_expr_stmt
						{
							.expr = ast_expr
							{
								.expr_ = ast_biop_expr
								{
									.lhs = ast_expr{.expr_ = ast_symbol_expr{.symbol = child_decl_stmt.decl.name}},
									.type = biop_type::assign,
									.rhs = decl_init.value()
								},
							}
						};
					}
					else
					{
						iter = n.children.erase(iter);
					}
					continue;
				}
			}
			hoist_variable_decls(child, source, local, do_codegen);
			iter++;
		}
	}
}

semal_result semal_while_stmt(const ast_while_stmt& while_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	semal_result cond_result = semal_expr(while_stmt.condition, n, source, local, do_codegen);
	if(cond_result.is_err())
	{
		return cond_result;
	}
	// go through all child nodes
	// if they are decl statements, do them now.
	// this is for codegen reasons.
	// what we really really do not want to do is alloca in a loop. remember allocas last till the end of the scope, *not* basic block.
	hoist_variable_decls(*try_get_block_child(n), source, local, do_codegen);

	type_t expected_cond_ty = type_t::create_primitive_type(prim_ty::type::boolean);
	const type_t& actual_cond_ty = cond_result.val.ty;
	const semal_result* maybe_parent = local->try_find_parent_function();
	if(!actual_cond_ty.is_convertible_to(expected_cond_ty))
	{
		return semal_result::err("while-statement condition is of invalid type \"{}\" as it is not convertible to a \"\"", actual_cond_ty.name(), expected_cond_ty.name());
	}
	if(do_codegen)
	{
		cond_result.load_if_variable();
		cond_result.convert_to(expected_cond_ty);
		llvm::Function* parent_fn = static_cast<llvm::Function*>(maybe_parent->val.ll);
		llvm::BasicBlock* true_blk = llvm::BasicBlock::Create(*codegen.ctx, "then", parent_fn);
		llvm::BasicBlock* cont_blk = llvm::BasicBlock::Create(*codegen.ctx, "whilecont");

		parent_fn->insert(parent_fn->end(), cont_blk);
		codegen.ir->CreateCondBr(cond_result.val.ll, true_blk, cont_blk);
		codegen.ir->SetInsertPoint(true_blk);

		bool true_blk_contains_ret = false;
		node* blk = try_get_block_child(n);
		if(blk == nullptr)
		{
			panic("while statement didnt have a block child?");
		}
		for(const auto& child : blk->children)
		{
			if(IS_A(child.payload, ast_stmt))
			{
				const ast_stmt& child_stmt = AS_A(child.payload, ast_stmt);
				if(IS_A(child_stmt.stmt_, ast_return_stmt))
				{
					true_blk_contains_ret = true;	
				}
			}
		}
		sval checker_val
		{
			.ll = cont_blk,
			.usrdata = &while_stmt,
			.usrdata2 = true_blk
		};
		if(true_blk_contains_ret)
		{
			checker_val.val = literal_val{};
		}
		local->unfinished_types.push_back({.t = semal_type::while_stmt, .val = checker_val});
	}
	return semal_result::null();
}

semal_result semal_for_stmt(const ast_for_stmt& for_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	semal_result init_result = semal_expr(for_stmt.init, n, source, local, do_codegen);
	if(init_result.is_err())
	{
		return init_result;
	}
	semal_result cond_result = semal_expr(for_stmt.cond, n, source, local, do_codegen);
	if(cond_result.is_err())
	{
		return cond_result;
	}
	
	hoist_variable_decls(*try_get_block_child(n), source, local, do_codegen);

	type_t expected_cond_ty = type_t::create_primitive_type(prim_ty::type::boolean);
	const type_t& actual_cond_ty = cond_result.val.ty;
	const semal_result* maybe_parent = local->try_find_parent_function();
	if(!actual_cond_ty.is_convertible_to(expected_cond_ty))
	{
		return semal_result::err("for-statement condition is of invalid type \"{}\" as it is not convertible to a \"\"", actual_cond_ty.name(), expected_cond_ty.name());
	}
	if(do_codegen)
	{
		cond_result.load_if_variable();
		cond_result.convert_to(expected_cond_ty);
		llvm::Function* parent_fn = static_cast<llvm::Function*>(maybe_parent->val.ll);
		llvm::BasicBlock* true_blk = llvm::BasicBlock::Create(*codegen.ctx, "then", parent_fn);
		llvm::BasicBlock* cont_blk = llvm::BasicBlock::Create(*codegen.ctx, "whilecont");

		parent_fn->insert(parent_fn->end(), cont_blk);
		codegen.ir->CreateCondBr(cond_result.val.ll, true_blk, cont_blk);
		codegen.ir->SetInsertPoint(true_blk);

		bool true_blk_contains_ret = false;
		node* blk = try_get_block_child(n);
		if(blk == nullptr)
		{
			panic("for statement didnt have a block child?");
		}
		for(const auto& child : blk->children)
		{
			if(IS_A(child.payload, ast_stmt))
			{
				const ast_stmt& child_stmt = AS_A(child.payload, ast_stmt);
				if(IS_A(child_stmt.stmt_, ast_return_stmt))
				{
					true_blk_contains_ret = true;	
				}
			}
		}
		sval checker_val
		{
			.ll = cont_blk,
			.usrdata = &for_stmt,
			.usrdata2 = true_blk
		};
		if(true_blk_contains_ret)
		{
			checker_val.val = literal_val{};
		}
		local->unfinished_types.push_back({.t = semal_type::for_stmt, .val = checker_val});
	}
	return semal_result::null();
}

semal_result semal_blk_stmt(const ast_blk_stmt& blk_stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	emit_debug_location(n);
	if(!blk_stmt.introduce_scope)
	{
		return semal_result::null();
	}
	semal_local_state* parent = local;
	local = &global.locals.emplace_back();
	local->scope = scope_type::block;
	local->parent = parent;
	// parent might have "pending variables"
	// this happens for stuff like function parameters which need to be put into a block after they appear
	// just define them as variables right now and clear the pendings (they only get handled once even if multiple children).
	for(const auto& [name, val] : parent->pending_variables)
	{
		local->declare_variable(name, val);
	}
	parent->pending_variables.clear();
	for(const auto& [name, val] : parent->pending_functions)
	{
		local->declare_function(name, val);
	}
	parent->pending_functions.clear();
	return semal_result::null();
}

semal_result semal_stmt(const ast_stmt& stmt, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	if(IS_A(stmt.stmt_, ast_decl_stmt))
	{
		return semal_decl_stmt(AS_A(stmt.stmt_, ast_decl_stmt), n, source, local, do_codegen, stmt.attributes);
	}
	else if(IS_A(stmt.stmt_, ast_expr_stmt))
	{
		return semal_expr_stmt(AS_A(stmt.stmt_, ast_expr_stmt), n, source, local, do_codegen);
	}
	else if(IS_A(stmt.stmt_, ast_return_stmt))
	{
		return semal_return_stmt(AS_A(stmt.stmt_, ast_return_stmt), n, source, local, do_codegen);
	}
	else if(IS_A(stmt.stmt_, ast_yield_stmt))
	{
		return semal_result::err("unexpected yield statement. yield statements are only valid within macros. did you mean to use a return statement instead?");
	}
	else if(IS_A(stmt.stmt_, ast_blk_stmt))
	{
		return semal_blk_stmt(AS_A(stmt.stmt_, ast_blk_stmt), n, source, local, do_codegen);
	}
	else if(IS_A(stmt.stmt_, ast_metaregion_stmt))
	{
		n.children.clear();
		return semal_result::null();
	}
	else if(IS_A(stmt.stmt_, ast_designator_stmt))
	{
		return semal_designator_stmt(AS_A(stmt.stmt_, ast_designator_stmt), n, source, local, do_codegen);
	}
	else if(IS_A(stmt.stmt_, ast_if_stmt))
	{
		return semal_if_stmt(AS_A(stmt.stmt_, ast_if_stmt), n, source, local, do_codegen);
	}
	else if(IS_A(stmt.stmt_, ast_while_stmt))
	{
		return semal_while_stmt(AS_A(stmt.stmt_, ast_while_stmt), n, source, local, do_codegen);
	}
	else if(IS_A(stmt.stmt_, ast_for_stmt))
	{
		return semal_for_stmt(AS_A(stmt.stmt_, ast_for_stmt), n, source, local, do_codegen);
	}
	else
	{
		return semal_result::err("dont know how to semal_stmt a \"{}\"", stmt.type_name());
	}
}

semal_result semal(node& n, std::string_view source, semal_local_state* parent, bool do_codegen)
{
	semal_local_state* local = parent;

	// rearrange deferred statements now so we can just go through them all in-order.
	handle_defer(n.children);
	semal_result res = semal_result::null();

	if(IS_A(n.payload, ast_translation_unit))
	{
		auto tu = AS_A(n.payload, ast_translation_unit);
		panic_ifnt(parent == nullptr, "why is parent semal_local_state of TU not null???");
		local = &global.locals.emplace_back();
		local->scope = scope_type::translation_unit;
		local->parent = parent;
		global.compiled_source_files.insert(tu.path);
	}
	else
	{
		panic_ifnt(parent != nullptr, "why is parent semi_local_state null when i am not a translation unit AST node???");
		if(IS_A(n.payload, ast_stmt))
		{
			res = semal_stmt(AS_A(n.payload, ast_stmt), n, source, local, do_codegen);
		}
		else
		{
			res = semal_result::err("dont know how to semal ast node \"{}\"", node_names[n.payload.index()]);
		}
	}
	verify_semal_result(res, n, source);

	if(res.t != semal_type::macro_decl && n.children.size())
	{
		auto pivot = n.children.end();
		if(IS_A(n.children.back().payload, ast_stmt))
		{
			auto last_stmt = AS_A(n.children.back().payload, ast_stmt);
			if(IS_A(last_stmt.stmt_, ast_return_stmt))
			{
				pivot = n.children.begin() + n.children.size() - 1;
			}
		}
		for(std::size_t i = 0; i < n.children.size(); i++)
		{
			semal(n.children[i], source, local, do_codegen);
			while(local->pending_macro_calls.size())
			{
				void* deferred_code = local->pending_macro_calls.back();
				local->pending_macro_calls.pop_back();
				pivot = n.children.insert(pivot, *reinterpret_cast<const node*>(deferred_code));
			}
			local->pending_macro_calls.clear();
		}
	}

	// special logic for block statements AFTER its done its children (pop context)
	// this is the only thing that does logic after it ends, so im just plodding it here ugly-like just this once.
	if(IS_A(n.payload, ast_stmt))
	{
		const auto& stmt = AS_A(n.payload, ast_stmt);
		if(IS_A(stmt.stmt_, ast_blk_stmt))
		{
			// do: pop context
			if(parent->unfinished_types.size())
			{
				semal_result last = parent->unfinished_types.back();
				switch(last.t)
				{
					case semal_type::struct_decl:
						if(do_codegen)
						{
							std::string_view structname = last.label;
							semal_state2::struct_value* structval = std::get<0>(local->find_struct(structname));
							panic_ifnt(structval != nullptr, "waaah");
							struct_ty ty = *structval;
							std::vector<llvm::Type*> member_tys;
							std::vector<llvm::Metadata*> member_debug_tys;


							for(std::string member_name : ty.member_order)
							{
								type_t memty = *ty.members.at(member_name);
								member_tys.push_back(memty.llvm());
							}
							llvm::StructType* llvm_ty =  llvm::StructType::create(member_tys, structname);
							global.llvm_structs[ty] = llvm_ty;
							auto struct_size = 8 * static_cast<std::int64_t>(codegen.mod->getDataLayout().getTypeAllocSize(llvm_ty));
							auto align_size = codegen.mod->getDataLayout().getABITypeAlign(llvm_ty).value() * 8;

							const auto& dlayout = codegen.mod->getDataLayout();
							auto* structlayout = dlayout.getStructLayout(llvm_ty);
							std::size_t counter = 0;
							llvm::DIFile* file = debug_files.at(n.begin_location.file);
							for(std::string member_name : ty.member_order)
							{
								type_t memty = *ty.members.at(member_name);
								auto offset = static_cast<std::int64_t>(structlayout->getElementOffsetInBits(counter));
								auto member_size = static_cast<std::int64_t>(codegen.mod->getDataLayout().getTypeAllocSizeInBits(memty.llvm()));
								auto member_align = 0;
								member_debug_tys.push_back(codegen.debug->createMemberType(file, member_name, file, n.begin_location.line, member_size, member_align, offset, llvm::DINode::DIFlags::FlagPublic, memty.debug_llvm()));
								counter++;
							}

							global.llvm_debug_structs[ty] = codegen.debug->createStructType(file, structname, file, n.begin_location.line, struct_size, align_size, llvm::DINode::DIFlags::FlagPublic, nullptr, codegen.debug->getOrCreateArray(member_debug_tys));
						}
					break;
					case semal_type::macro_decl:
					{
					}
					break;
					case semal_type::function_decl:
					{
						// should emit an empty return.
						std::string_view funcname = last.label;
						semal_state2::function_value* funcval = std::get<1>(local->find_function(funcname));
						type_t return_ty = *funcval->ty.return_ty;
						if(IS_A(return_ty.payload, prim_ty))
						{
							auto return_prim = AS_A(return_ty.payload, prim_ty);
							if(return_prim.p == prim_ty::type::v0)
							{
								// let's add a fake return.
								semal_return_stmt(ast_return_stmt{.retval = std::nullopt}, n, source, local, do_codegen);
							}
						}
						panic_ifnt(lexical_blocks.size() == 1, "whaat why didnt hte function lexical block exist");
						lexical_blocks.pop_back();
					}
					break;
					case semal_type::enum_decl:
						if(do_codegen)
						{
							std::string_view enumname = last.label;
							semal_state2::enum_value* enumval = std::get<0>(local->find_enum(enumname));
							panic_ifnt(enumval != nullptr, "waaah but an enum");
							enum_ty ty = *enumval;
							for(const auto& [name, entryval] : ty.entries)
							{
								sval val{.val = literal_val{entryval}, .ty = *ty.underlying_ty};
								val.ty.qual = val.ty.qual | typequal_static;
								val.ll = val.llvm();
								codegen.declare_global_variable(std::format("{}.{}", enumname, name), val.ty, val, true);
							}
						}
					break;
					case semal_type::if_stmt:
					{
						auto* cont_blk = static_cast<llvm::BasicBlock*>(last.val.ll);
						bool doesnt_need_branch = IS_A(last.val.val, literal_val);
						if(!doesnt_need_branch)
						{
							codegen.ir->CreateBr(cont_blk);
						}
						codegen.ir->SetInsertPoint(cont_blk);
					}
					break;
					case semal_type::while_stmt:
					{
						auto* cont_blk = static_cast<llvm::BasicBlock*>(last.val.ll);
						auto* true_blk = static_cast<llvm::BasicBlock*>(last.val.usrdata2);
						bool doesnt_need_branch = IS_A(last.val.val, literal_val);
						auto* stmt = static_cast<const ast_while_stmt*>(last.val.usrdata);
						semal_result cond_result = semal_expr(stmt->condition, n, source, local, do_codegen);
						cond_result.load_if_variable();
						cond_result.convert_to(type_t::create_primitive_type(prim_ty::type::boolean));
						codegen.ir->CreateCondBr(cond_result.val.ll, true_blk, cont_blk);
						codegen.ir->SetInsertPoint(cont_blk);
					}
					break;
					case semal_type::for_stmt:
					{
						auto* cont_blk = static_cast<llvm::BasicBlock*>(last.val.ll);
						auto* true_blk = static_cast<llvm::BasicBlock*>(last.val.usrdata2);
						bool doesnt_need_branch = IS_A(last.val.val, literal_val);
						auto* stmt = static_cast<const ast_for_stmt*>(last.val.usrdata);
						semal_result iter_result = semal_expr(stmt->iter, n, source, local, do_codegen);
						verify_semal_result(iter_result, n, source);

						semal_result cond_result = semal_expr(stmt->cond, n, source, local, do_codegen);
						cond_result.load_if_variable();
						cond_result.convert_to(type_t::create_primitive_type(prim_ty::type::boolean));
						codegen.ir->CreateCondBr(cond_result.val.ll, true_blk, cont_blk);
						codegen.ir->SetInsertPoint(cont_blk);
					}
					break;
					default:
						panic("when popping context (closing of block {}), last unfinished type was detected, but it was neither a non-extern function, enum nor struct", n.end_location);
					break;
				}
				parent->unfinished_types.pop_back();
			}
		}
	}
	return res;
}

sval zero_as(const type_t& ty)
{
	sval ret
	{
		.ty = ty
	};
	ret.ty.qual = ret.ty.qual | typequal_static;
	if(ty.is_ptr())
	{
		ret.val = sval_null_tag{};
	}
	if(ty.is_enum())
	{
		ret.val = literal_val{std::int64_t{0}};
	}
	else if(ty.is_struct())
	{
		const auto& sty = AS_A(ty.payload, struct_ty);
		sval::struct_val members;
		for(const auto& memname : sty.member_order)
		{
			members[memname] = zero_as(*sty.members.at(memname));
		}
		ret.val = members;
	}
	else if(ty.is_arr())
	{
		const auto& arrty = AS_A(ty.payload, arr_ty);
		sval::array_val elems;
		elems.resize(arrty.array_length, zero_as(*arrty.underlying_ty));
		ret.val = elems;
	}
	else if(ty.is_prim())
	{
		const auto& prim = AS_A(ty.payload, prim_ty);
		if(prim.is_floating_point())
		{
			ret.val = literal_val{0.0};
		}
		else if(prim.p == prim_ty::type::boolean)
		{
			ret.val = literal_val{false};
		}
		else
		{
			ret.val = literal_val{0};
		}
	}
	else if(ty.is_fn())
	{
		auto name = ty.name();
		error({}, "attempt to interpret zero-expr as a function type \"{}\". zero-expr cannot initialise a function", name);
	}
	ret.ll = ret.llvm();

	return ret;
}

void semal_verify(compile_args& args)
{
}

//////////////////////////// TYPE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE type

//////////////////////////// META ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE meta

//////////////////////////// ASSEMBLE -> OBJECTS ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE assemble

//////////////////////////// LINK -> EXECUTABLE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE link
// link objects -> executable


//////////////////////////// PARSE CHORDS ////////////////////////////
// we are back in parser land - this is where all the chord functions live. they sit here at the bottom because there is going to be *alot* of them.
#undef COMPILER_STAGE
#define COMPILER_STAGE parse

// chord function shared code

// how do i turn a particular token into an expr?
#define EXPRIFY_T(x) EXPRIFY_##x(nodes, state)
FAKEFN(EXPRIFY_integer_literal)
{
	auto front = std::get<ast_token>(nodes[0].payload).lexeme;
	std::int64_t val;
	std::from_chars_result result;
	if(front.starts_with("0b"))
	{
		// binary (base 2) literal
		result = std::from_chars(front.data() + 2, front.data() + front.size(), val, 2);
	}
	else if(front.starts_with("0x"))
	{
		// hex (base 16) literal
		result = std::from_chars(front.data() + 2, front.data() + front.size(), val, 16);
	}
	else
	{
		// base 10 integer literal
		result = std::from_chars(front.data(), front.data() + front.size(), val, 10);
	}
	panic_ifnt(result.ec == std::errc() && result.ptr != front.data(), "malformed integer literal");
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = val}}}}
	};
}
FAKEFN(EXPRIFY_decimal_literal)
{
	auto front = std::get<ast_token>(nodes[0].payload).lexeme;
	double val;
	auto result = std::from_chars(front.data(), front.data() + front.size(), val);
	panic_ifnt(result.ec == std::errc() && result.ptr != front.data(), "malformed decimal literal");
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = val}}}}
	};
}
FAKEFN(EXPRIFY_symbol)
{
	std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_symbol_expr{.symbol = symbol}}}}
	};
}
FAKEFN(EXPRIFY_string_literal)
{
	std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = symbol}}}}
	};
}
FAKEFN(EXPRIFY_char_literal)
{
	std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = symbol.front()}}}}
	};
}
FAKEFN(EXPRIFY_keyword_true)
{
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = true}}}}
	};
}
FAKEFN(EXPRIFY_keyword_false)
{
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = false}}}}
	};
}

FAKEFN(EXPRIFY_keyword_zero)
{
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_zero_expr{}}}}
	};
}

FAKEFN(EXPRIFY_keyword_null)
{
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_null_expr{}}}}
	};
}

std::unordered_set<token> unop_tokens{};

#define DEFINE_EXPRIFICATION_CHORDS(x) \
	CHORD_BEGIN\
		STATE(TOKEN(x)), FN\
		{\
			return {.action = parse_action::recurse};\
		}\
		EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x)), FN\
		{\
			return {.action = parse_action::shift};\
		}\
	CHORD_END\
	CHORD_BEGIN\
		STATE(TOKEN(x), TOKEN(semicol)), FN\
		{\
			return {.action = parse_action::recurse};\
		}\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(semicol)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(dot)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(keyword_at)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(arrow)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(compare)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(comparen)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(assign)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(cparen)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(cbrack)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(oanglebrack)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(canglebrack)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(loreq)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(goreq)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(comma)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(plus)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(dash)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(asterisk)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(fslash)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(bitwise_and)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(bitwise_or)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(bitwise_exor)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(modulo)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(cast)), FN\
		{\
			return EXPRIFY_T(x);\
		}\
	EXTENSIBLE\
	CHORD_END

#define DEFINE_UNOPIFICATION_CHORDS(x, unop_ty) \
	unop_tokens.insert(token::x);\
	CHORD_BEGIN\
		STATE(TOKEN(x)), FN\
		{\
			return {.action = parse_action::recurse};\
		}\
		EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x)), FN\
		{\
			return {.action = parse_action::shift};\
		}\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), NODE(ast_expr)), FN\
		{\
			const auto& expr = std::get<ast_expr>(nodes[1].payload);\
			return\
			{\
				.action = parse_action::reduce,\
				.nodes_to_remove = {.offset = 0, .length = 2},\
				.reduction_result = {node{.payload = ast_expr{.expr_ = ast_unop_expr\
					{\
						.type = unop_type::unop_ty,\
						.rhs = expr\
					}}}}\
			};\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(TOKEN(x), WILDCARD), FN\
		{\
			return {.action = parse_action::recurse, .reduction_result_offset = 1};\
		}\
	EXTENSIBLE\
	CHORD_END

#define DEFINE_BIOPIFICATION_CHORDS(x, biop_ty) \
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(x)), FN\
		{\
			return {.action = parse_action::shift};\
		}\
	OVERRIDEABLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(x), NODE(ast_expr)), FN\
		{\
			auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);\
			const auto& cast_to = std::get<ast_expr>(nodes[2].payload);\
			ast_expr* last_expr;\
			if(call.params.empty())\
			{\
				std::string_view tok = std::get<ast_token>(nodes[1].payload).lexeme;\
				chord_error("unexpected token {} detected before any params. move or remove this token.", tok);\
			}\
			last_expr = &call.params.back();\
			*last_expr = ast_expr{.expr_ = ast_biop_expr{.lhs = *last_expr, .type = biop_type::biop_ty, .rhs = cast_to}};\
			return\
			{\
				.action = parse_action::reduce,\
				.nodes_to_remove = {.offset = 1, .length = 2}\
			};\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(x), WILDCARD), FN\
		{\
			return\
			{\
				.action = parse_action::recurse,\
				.reduction_result_offset = 2\
			};\
		}\
	EXTENSIBLE_AND_OVERRIDEABLE\
	CHORD_END\
	CHORD_BEGIN\
		STATE(NODE(ast_expr), TOKEN(x)), FN\
		{\
			return {.action = parse_action::recurse};\
		}\
		EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(x), NODE(ast_expr)), FN\
		{\
			auto& lhs_expr = std::get<ast_expr>(nodes[0].payload);\
			auto& rhs_expr = std::get<ast_expr>(nodes[2].payload);\
			if(IS_A(lhs_expr.expr_, ast_biop_expr))\
			{\
				auto& lhs_biop_expr = std::get<ast_biop_expr>(lhs_expr.expr_);\
				unsigned int prec = biop_precedence[static_cast<int>(biop_type::biop_ty)];\
				if(prec > lhs_expr.precedence())\
				{\
					auto& actual_rhs_expr = *lhs_biop_expr.rhs;\
					actual_rhs_expr = ast_expr{.expr_ = ast_biop_expr\
						{\
							.lhs = actual_rhs_expr,\
							.type = biop_type::biop_ty,\
							.rhs = rhs_expr\
						}};\
				}\
			}\
			return\
			{\
				.action = parse_action::reduce,\
				.nodes_to_remove = {.offset = 0, .length = 3},\
				.reduction_result = {node{.payload = ast_expr{.expr_ = ast_biop_expr\
					{\
						.lhs = lhs_expr,\
						.type = biop_type::biop_ty,\
						.rhs = rhs_expr\
					}}}}\
			};\
		}\
	EXTENSIBLE_AND_OVERRIDEABLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(x), WILDCARD), FN\
		{\
			return {.action = parse_action::recurse, .reduction_result_offset = 2};\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(x), WILDCARD), FN\
		{\
			return {.action = parse_action::recurse, .reduction_result_offset = 2};\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		STATE(NODE(ast_decl), TOKEN(x)), FN\
		{\
			return {.action = parse_action::recurse};\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(x)), FN\
		{\
			return {.action = parse_action::shift};\
		}\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(x), NODE(ast_expr)), FN\
		{\
			auto& decl = std::get<ast_decl>(nodes[0].payload);\
			if(!decl.initialiser.has_value())\
			{\
				chord_error("ksdfkjdshkfjh");\
			}\
			ast_expr& lhs_expr = decl.initialiser.value();\
			const ast_expr& rhs_expr = AS_A(nodes[2].payload, ast_expr);\
			bool rearranged = false;\
			if(IS_A(lhs_expr.expr_, ast_biop_expr))\
			{\
				auto& lhs_biop_expr = std::get<ast_biop_expr>(lhs_expr.expr_);\
				if(biop_precedence[static_cast<int>(biop_type::biop_ty)] > lhs_expr.precedence())\
				{\
					lhs_biop_expr.rhs = ast_expr{.expr_ = ast_biop_expr\
						{\
							.lhs = *lhs_biop_expr.rhs,\
							.type = biop_type::biop_ty,\
							.rhs = rhs_expr\
						}};\
					rearranged = true;\
				}\
			}\
			if(!rearranged)\
			{\
				decl.initialiser.value() = ast_expr{.expr_ = ast_biop_expr\
				{\
					.lhs = decl.initialiser.value(),\
					.type = biop_type::biop_ty,\
					.rhs = std::get<ast_expr>(nodes[2].payload)\
				}};\
			}\
			return\
			{\
				.action = parse_action::reduce,\
				.nodes_to_remove = {.offset = 1, .length = 2}\
			};\
		}\
	EXTENSIBLE\
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(x), WILDCARD), FN\
		{\
			return {.action = parse_action::recurse, .reduction_result_offset = 2};\
		}\
	EXTENSIBLE\
	CHORD_END

// a chord is a single entry in the parse tree
// so let's say we are in the middle of the parsing process.
// - ours is a shift-reduce parser, meaning it somehow needs to know when to shift and when it can perform a reduction
// - instead of checking the entire subtree state manually everytime, we convert the current parse state to a set of hashes
// - we then use those hashes to find a chord function that tells the parser what to do in the case of that exact parse state
// - all chord functions are hand-written, but to manually put them in the right place in the table would be too insane.
// - so the macro hackery below will take the chord function and magically install it in the correct place in the parse table.
//
// CHORD_BEGIN (signifies the beginning of a chord)
// STATE(...) is how you specify what exact parse state this particular chord is meant to handle
// 		example: STATE(TOKEN(colon), TOKEN(eq)) means that the chord function handles the case that the source contains exactly ":="
// 		note: this example is unrealistic, as := is actually its own token (token::initialiser). ctrl-f above.
// directly after STATE(...), you should put FN and write your c++ code. you are now defining your chord function.
//
// how to write a chord function
// =============================
//
// - you have a writable array of the nodes within the variable "nodes". you should consider it a std::span<node>
// - you already know what type of nodes are in the array - as they will exactly match what you specified in STATE(...)
// - if your state represents a valid reduction, you should do the reduction and return parse_action::reduce
// - if your state represents a syntax error, you should call chord_error(msg, ...) directly in your function. no need to return anything in that case, the macro will handle it for you.
//
void populate_chords(){

// just the translation unit
CHORD_BEGIN
	STATE(), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

// declarations

CHORD_BEGIN
	STATE(TOKEN(symbol), TOKEN(colon)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), TOKEN(symbol)), FN
	{
		// x : y
		// this is a declaration
		ast_decl decl
		{
			.type_name = std::string{std::get<ast_token>(nodes[2].payload).lexeme},
			.name = std::string{std::get<ast_token>(nodes[0].payload).lexeme}
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = decl}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), TOKEN(initialiser)), FN
	{
		ast_decl decl
		{
			.type_name = deduced_type,
			.name = std::string{std::get<ast_token>(nodes[0].payload).lexeme}
		};
		// keep the initialiser
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = decl}}
		};
	}
CHORD_END

// decl with an explicit type that is a function type.
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), TOKEN(keyword_func)), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), NODE(ast_partial_funcdef)), FN
	{
		const auto& funcdef = AS_A(nodes[2].payload, ast_partial_funcdef);
		if(funcdef.stage == partial_funcdef_stage::awaiting_body)
		{
			std::string params_str;
			for(std::size_t i = 0; i < funcdef.params.size(); i++)
			{
				params_str += funcdef.params[i].type_name;
				if(i < (funcdef.params.size() - 1))
				{
					params_str += ',';
				}
			}
			std::string func_typename = std::format("func({}) -> {}", params_str, funcdef.return_type);
			ast_decl decl
			{
				.type_name = func_typename,
				.name = std::string{std::get<ast_token>(nodes[0].payload).lexeme}
			};
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = nodes.size()},
				.reduction_result = {node{.payload = decl}}
			};
		}
		else
		{
			return {.action = parse_action::recurse, .reduction_result_offset = 2};
		}
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), WILDCARD), FN
	{
		chord_error("syntax error, looks like a malformed declaration");
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(symbol), TOKEN(colon), WILDCARD), FN
	{
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 0
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(initialiser)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(initialiser)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(initialiser), WILDCARD), FN
	{
		return {.action = parse_action::recurse};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(initialiser), WILDCARD), FN
	{
		return {.action = parse_action::recurse};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(initialiser), WILDCARD), FN
	{
		auto& decl_node = nodes[0];
		auto& decl = std::get<ast_decl>(decl_node.payload);
		if(decl.initialiser.has_value())
		{
			chord_error("declaration {} appears to have more than one initialiser.", decl.name);
		}

		auto value_node = nodes[2];

		if(value_node.payload.index() == payload_index<ast_token, node_payload>())
		{
			auto value = std::get<ast_token>(value_node.payload);
			if(value.tok == token::keyword_zero)
			{
					decl.initialiser = ast_expr{.expr_ = ast_zero_expr{}};
			}
			else if(value.tok == token::keyword_null)
			{
					decl.initialiser = ast_expr{.expr_ = ast_null_expr{}};
			}
			else
			{
				if(value.tok == token::oparen || value.tok == token::keyword_func || value.tok == token::keyword_macro || value.tok == token::symbol || value.tok == token::keyword_struct || value.tok == token::keyword_enum || unop_tokens.contains(value.tok))
				{
						return {.action = parse_action::recurse, .reduction_result_offset = 2};
				}
				decl.initialiser = ast_expr{.expr_ = ast_literal_expr{}};
				auto& literal = std::get<ast_literal_expr>(decl.initialiser->expr_);

				switch(value.tok)
				{
					case token::integer_literal:
						literal.value = std::stoll(std::string{value.lexeme});
					break;
					case token::decimal_literal:
						literal.value = std::stod(std::string{value.lexeme});
					break;
					case token::char_literal:
					{
						std::string escaped_chars = escape(value.lexeme);
						std::size_t chars_size = escaped_chars.size();
						if(chars_size != 1)
						{
							error(value_node.begin_location, "char literals must contain only 1 character, '{}' contains {} characters", value.lexeme, chars_size);
						}
						literal.value = escaped_chars.front();
					}
					break;
					case token::string_literal:
						literal.value = escape(value.lexeme);
					break;
					case token::keyword_true:
						literal.value = true;
					break;
					case token::keyword_false:
						literal.value = false;
					break;
					default:
						chord_error("a {} is not a valid initialiser for a declaration", token_traits[static_cast<int>(value.tok)].name);
					break;
				}
			}
		}
		else
		{

			if(value_node.payload.index() == payload_index<ast_partial_funcdef, node_payload>())
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 2};
			}
			else if(value_node.payload.index() == payload_index<ast_funcdef, node_payload>())
			{
				auto funcdef = std::get<ast_funcdef>(value_node.payload);
				decl.initialiser = ast_expr{.expr_ = funcdef.func};
			}
			else if(value_node.payload.index() == payload_index<ast_macrodef, node_payload>())
			{
				auto macrodef = std::get<ast_macrodef>(value_node.payload);
				decl.initialiser = ast_expr{.expr_ = macrodef.macro};
			}
			else if(value_node.payload.index() == payload_index<ast_expr, node_payload>())
			{
				decl.initialiser = std::get<ast_expr>(value_node.payload);
			}
			else if(value_node.payload.index() == payload_index<ast_partial_callfunc, node_payload>())
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 2};
			}
			else
			{
				chord_error("not sure how to handle a {} as a decl initialiser", node_names[value_node.payload.index()]);
			}
		}
		decl_node.end_location = nodes.back().end_location;
		decl_node.children = value_node.children;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 2}
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen), TOKEN(symbol)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen), TOKEN(cparen)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		auto& func = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(func.stage == partial_funcdef_stage::defining_params)
		{
		}
		else
		{
			chord_error("syntax error while parsing function definition - expecting to be defining params");
		}
		func.stage = partial_funcdef_stage::awaiting_arrow;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 2},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen), NODE(ast_decl)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		auto& func = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(func.stage == partial_funcdef_stage::defining_params)
		{
			func.params.push_back(std::get<ast_decl>(nodes[2].payload));
		}
		else
		{
			chord_error("syntax error while parsing function definition - expecting to be defining params");
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 2},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(cparen)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::defining_params)
		{
			chord_error("unexpected ')' token while parsing function definition. you have already finished defining the params.");
		}
		def.stage = partial_funcdef_stage::awaiting_arrow;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(comma)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage == partial_funcdef_stage::defining_params)
		{
			def.stage = partial_funcdef_stage::awaiting_next_param;
		}
		else
		{
			chord_error("unexpected ',' token while parsing function definition. you have already finished defining the params/static params.");
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), NODE(ast_decl)), FN
	{
		auto next_param = std::get<ast_decl>(nodes[1].payload);
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage == partial_funcdef_stage::awaiting_next_param)
		{
			def.params.push_back(next_param);
			def.stage = partial_funcdef_stage::defining_params;
		}
		else
		{
			chord_error("unexpected decl named {} while parsing function definition. you have already finished defining the params/static params, or have you forgotten a comma?", next_param.name);
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(arrow)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_arrow)
		{
			chord_error("unexpected '->' token while parsing function definition. malformed function definition");
		}
		def.stage = partial_funcdef_stage::awaiting_return_type;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(symbol)), FN
	{
		auto return_typename = std::string{std::get<ast_token>(nodes[1].payload).lexeme};
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage == partial_funcdef_stage::awaiting_next_param)
		{
			// so a comma preceded us. we're the start of a decl representing the next param, but havent figured that out yet
			// recurse.
			return
			{
				.action = parse_action::recurse,
				.reduction_result_offset = 1
			};
		}
		if(def.stage != partial_funcdef_stage::awaiting_return_type)
		{
			chord_error("unexpected '{}' token while parsing function definition. i wasnt ready for the return type yet", return_typename);
		}
		def.stage = partial_funcdef_stage::awaiting_body;
		def.return_type = return_typename;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(initialiser)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_body)
		{
			chord_error("unexpected ':=' token while parsing function definition. this is presumably a typename, but i wasn't ready for the body yet (assuming you are trying to declare as extern)");
		}
		return {.action = parse_action::shift};
	}

CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		// function decl with empty body
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_body)
		{
			chord_error("unexpected '{' token(s) while parsing function definition. this is presumably a typename, but i wasn't ready for the body yet (assuming you are trying to define a function body)");
		}
		node retnode{.children = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}}};
		if(def.is_macro)
		{
			retnode.payload = ast_macrodef
			{
				.macro =
				{
					.params = std::move(def.params),
					.return_type = std::move(def.return_type),
				}
			};
		}
		else
		{
			retnode.payload = ast_funcdef
			{
				.func =
				{
					.params = std::move(def.params),
					.return_type = std::move(def.return_type),
					.is_extern = false
				}
			};
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {retnode}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrace), NODE(ast_stmt)), FN
	{
		// this is the start of a block statement.
		node ret = {.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}};
		ret.children.push_back(nodes[1]);
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {ret}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), NODE(ast_stmt)), FN
	{
		auto& defnode = nodes[0];
		const auto& def = std::get<ast_partial_funcdef>(defnode.payload);
		const auto& stmt = std::get<ast_stmt>(nodes[1].payload);
		if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			// this is a blk statement
			// remember, we can only collapse this into a full function definition if the block statement is capped (i.e ended with a cbrace)
			// if not, then we need to recurse.
			auto blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(!blk.capped)
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 1};
			}

			defnode.children = {nodes[1]};
			if(def.is_macro)
			{
				defnode.payload = ast_macrodef
				{
					.macro =
					{
						.params = std::move(def.params),
						.return_type = std::move(def.return_type),
					}
				};
			}
			else
			{
				defnode.payload = ast_funcdef
				{
					.func =
					{
						.params = std::move(def.params),
						.return_type = std::move(def.return_type),
						.is_extern = false
					}
				};
			}
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 1, .length = 1},
			};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("expected block statement, this is a {} statement", stmt_name);
		}

	}
CHORD_END


CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), NODE(ast_stmt), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(initialiser), TOKEN(keyword_extern)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_body)
		{
			chord_error("unexpected ':= extern' tokens while parsing function definition. this is presumably a typename, but i wasn't ready for the body yet (assuming you are trying to declare as extern)");
		}
		if(def.is_macro)
		{
			chord_error("macros cannot be marked as 'extern'. only functions.");
		}
		// aha, the function is extern
		ast_funcdef complete_funcdef
		{
			.func =
			{
				.params = std::move(def.params),
				.return_type = std::move(def.return_type),
				.is_extern = true
			}
		};

		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = complete_funcdef}}
		};

	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), TOKEN(symbol)), FN
	{
		// it should be the start of a decl (the first param)
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), TOKEN(cparen)), FN
	{
		// decl with no parameters
		ast_partial_funcdef func
		{
			.is_macro = false,
			.params = {},
			.return_type = "???",
			.stage = partial_funcdef_stage::awaiting_arrow
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), NODE(ast_decl)), FN
	{
		// we have the start of a function definition. finally.
		ast_partial_funcdef func
		{
			.is_macro = false,
			.params = {std::get<ast_decl>(nodes[2].payload)},
			.return_type = "???"
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

// macro
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_macro)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_macro), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_macro), TOKEN(oparen), TOKEN(symbol)), FN
	{
		// it should be the start of a decl (the first param)
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_macro), TOKEN(oparen), TOKEN(cparen)), FN
	{
		// decl with no parameters
		ast_partial_funcdef func
		{
			.is_macro = true,
			.params = {},
			.return_type = "???",
			.stage = partial_funcdef_stage::awaiting_arrow
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_macro), TOKEN(oparen), NODE(ast_decl)), FN
	{
		// we have the start of a function definition. finally.
		ast_partial_funcdef func
		{
			.is_macro = true,
			.params = {std::get<ast_decl>(nodes[2].payload)},
			.return_type = "???"
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_structdef_expr{}}, .children = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{.capped = true}}}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), NODE(ast_stmt)), FN
	{
		auto& stmt_node = nodes[1];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			const auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(blk.capped)
			{
				return
				{
					.action = parse_action::reduce,
					.nodes_to_remove = {.offset = 0, .length = nodes.size()},
					.reduction_result = {node{.payload = ast_expr{.expr_ = ast_structdef_expr{}}, .children = {stmt_node}}}
				};
			}
			else
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 1};
			}
		}
		else
		{
			const char* stmt_type = stmt.type_name();
			chord_error("struct keyword is followed by a statement. i only expect block statements to follow this keyword, but instead you've provided me with a {}", stmt_type);
		}
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), WILDCARD), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), WILDCARD, WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_enum)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_enum), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_enum), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_enumdef_expr{}}, .children = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{.capped = true}}}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_enum), NODE(ast_stmt)), FN
	{
		auto& stmt_node = nodes[1];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			const auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(blk.capped)
			{
				return
				{
					.action = parse_action::reduce,
					.nodes_to_remove = {.offset = 0, .length = nodes.size()},
					.reduction_result = {node{.payload = ast_expr{.expr_ = ast_enumdef_expr{}}, .children = {stmt_node}}}
				};
			}
			else
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 1};
			}
		}
		else
		{
			const char* stmt_type = stmt.type_name();
			chord_error("enum keyword is followed by a statement. i only expect block statements to follow this keyword, but instead you've provided me with a {}", stmt_type);
		}
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_enum), WILDCARD), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_enum), WILDCARD, WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

// block initialisers
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		// empty block initialiser
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_blkinit_expr
				{
					.type_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
					.initialisers = {}
				}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(obrace), TOKEN(dot)), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), NODE(ast_stmt)), FN
	{
		const auto& stmt_node = nodes[1];
		const auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			const auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(!blk.capped)
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 1};
			}
			ast_blkinit_expr init{.type_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme}};
			// go through the block, get all of the designator statements and fill a block initialiser with it.
			for(const node& child : stmt_node.children)
			{
				if(child.payload.index() == payload_index<ast_stmt, node_payload>())
				{
					const auto& child_stmt = std::get<ast_stmt>(child.payload);
					if(child_stmt.stmt_.index() == payload_index<ast_designator_stmt, decltype(child_stmt.stmt_)>())
					{
						init.initialisers.push_back(std::get<ast_designator_stmt>(child_stmt.stmt_));
					}
					else
					{
						chord_error("");
					}
				}
				else
				{
					chord_error("");
				}

			}
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = 2},
				.reduction_result = {node{.payload = ast_expr{.expr_ = init}}}
			};
			
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("unexpected {} statement following \"symbol{\". expected a block statement (to form a block initialiser) only.", stmt_name);
		}
	}
EXTENSIBLE
CHORD_END

// designators
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot), TOKEN(symbol)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot), TOKEN(symbol), TOKEN(initialiser)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot), TOKEN(symbol), TOKEN(initialiser), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot), TOKEN(symbol), TOKEN(initialiser), NODE(ast_expr), TOKEN(semicol)), FN
	{
		const auto& expr_node = nodes[3];
		std::string name{std::get<ast_token>(nodes[1].payload).lexeme};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 5},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_designator_stmt{.name = name, .initialiser = std::get<ast_expr>(expr_node.payload)}}}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot), TOKEN(symbol), TOKEN(initialiser), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 3};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(dot), TOKEN(symbol), TOKEN(initialiser), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 3};
	}
EXTENSIBLE
CHORD_END

//

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(semicol)), FN
	{
		return {.action = parse_action::recurse};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), TOKEN(cparen)), FN
	{
		// this is just a call with no params
		ast_callfunc_expr call
		{
			.function_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
			.params = {},
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_expr{.expr_ = call}, .end_location = nodes.back().end_location}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), NODE(ast_expr)), FN
	{
		// this is a call with at least one param
		ast_partial_callfunc call
		{
			.function_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
			.params = {std::get<ast_expr>(nodes[2].payload)},
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {node{.payload = call, .end_location = nodes.back().end_location}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), WILDCARD), FN
	{
		return{.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(comma)), FN
	{
		auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got ,");
		}
		call.awaiting_next_param = true;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), NODE(ast_expr)), FN
	{
		auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);
		if(!call.awaiting_next_param)
		{
			chord_error("syntax error while evaluating function call. unexpected expression which is presumably meant to be a parameter. did you forget a preceding comma?");
		}
		call.params.push_back(std::get<ast_expr>(nodes[1].payload));
		call.awaiting_next_param = false;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(oparen)), FN
	{
		// we've most likely only hit this path coz we just parsed the beginning of a function call with static params.
		// we just remove the oparen and continue on as if we had already specified a first param (push-back will save us)
		auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);
		call.awaiting_next_param = true;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(cparen)), FN
	{
		auto& call_node = nodes[0];
		auto& call = std::get<ast_partial_callfunc>(call_node.payload);
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got )");
		}
		ast_callfunc_expr complete_call
		{
			.function_name = call.function_name,
			.params = call.params
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_expr{.expr_ = complete_call}}}
		};
	}
CHORD_END

DEFINE_UNOPIFICATION_CHORDS(dash, minus)
DEFINE_UNOPIFICATION_CHORDS(invert, invert)
DEFINE_UNOPIFICATION_CHORDS(keyword_ref, ref)
DEFINE_UNOPIFICATION_CHORDS(keyword_deref, deref)
DEFINE_UNOPIFICATION_CHORDS(keyword_atomic_deref, atomic_deref)
DEFINE_UNOPIFICATION_CHORDS(keyword_alias, alias)

DEFINE_BIOPIFICATION_CHORDS(cast, cast)
DEFINE_BIOPIFICATION_CHORDS(plus, plus)
DEFINE_BIOPIFICATION_CHORDS(dash, minus)
DEFINE_BIOPIFICATION_CHORDS(asterisk, mul)
DEFINE_BIOPIFICATION_CHORDS(fslash, div)
DEFINE_BIOPIFICATION_CHORDS(bitwise_and, bitwise_and)
DEFINE_BIOPIFICATION_CHORDS(bitwise_or, bitwise_or)
DEFINE_BIOPIFICATION_CHORDS(bitwise_exor, bitwise_exor)
DEFINE_BIOPIFICATION_CHORDS(modulo, modulo)
DEFINE_BIOPIFICATION_CHORDS(dot, field)
DEFINE_BIOPIFICATION_CHORDS(arrow, ptr_field)
DEFINE_BIOPIFICATION_CHORDS(compare, compare_eq)
DEFINE_BIOPIFICATION_CHORDS(comparen, compare_neq)
DEFINE_BIOPIFICATION_CHORDS(assign, assign)
DEFINE_BIOPIFICATION_CHORDS(oanglebrack, less_than)
DEFINE_BIOPIFICATION_CHORDS(canglebrack, greater_than)
DEFINE_BIOPIFICATION_CHORDS(loreq, less_than_or_equal)
DEFINE_BIOPIFICATION_CHORDS(goreq, greater_than_or_equal)
DEFINE_BIOPIFICATION_CHORDS(keyword_at, at)

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(semicol)), FN
	{
		auto& decl_node = nodes[0];
		auto decl = std::get<ast_decl>(decl_node.payload);
		
		decl_node.payload = ast_stmt
		{
			.stmt_ = ast_decl_stmt{.decl = decl}
		};

		decl_node.end_location = nodes.back().end_location;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = nodes.size() - 1}
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_expr), TOKEN(semicol)), FN
	{
		return
		{
			.action = parse_action::recurse
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(semicol)), FN
	{
		auto& expr_node = nodes[0];
		auto expr = std::get<ast_expr>(expr_node.payload);
		
		expr_node.payload = ast_stmt
		{
			.stmt_ = ast_expr_stmt{.expr = expr}
		};

		expr_node.end_location = nodes.back().end_location;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = nodes.size() - 1}
		};
		
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

DEFINE_EXPRIFICATION_CHORDS(integer_literal)
DEFINE_EXPRIFICATION_CHORDS(decimal_literal)
DEFINE_EXPRIFICATION_CHORDS(char_literal)
DEFINE_EXPRIFICATION_CHORDS(string_literal)
DEFINE_EXPRIFICATION_CHORDS(symbol)
DEFINE_EXPRIFICATION_CHORDS(keyword_true)
DEFINE_EXPRIFICATION_CHORDS(keyword_false)
DEFINE_EXPRIFICATION_CHORDS(keyword_zero)
DEFINE_EXPRIFICATION_CHORDS(keyword_null)

CHORD_BEGIN
	STATE(TOKEN(keyword_defer)), FN
	{
		chord_error("you can only defer a statement that is in a block. this looks like a top-level statement");
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_defer)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_defer), NODE(ast_stmt)), FN
	{
		auto& stmt = std::get<ast_stmt>(nodes[1].payload);
		if(stmt.deferred)
		{
			chord_error("attempt to defer statement that is already deferred.");
		}
		stmt.deferred = true;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_defer), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_stmt)), FN
	{
		// statement is right at the beginning.
		// it should become the latest child of the translation unit node.
		const auto& stmt = std::get<ast_stmt>(nodes[0].payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			if(!std::get<ast_blk_stmt>(stmt.stmt_).capped)
			{
				return {.action = parse_action::recurse};
			}
		}
		else if(stmt.stmt_.index() == payload_index<ast_metaregion_stmt, decltype(stmt.stmt_)>())
		{
			if(!std::get<ast_metaregion_stmt>(stmt.stmt_).capped)
			{
				return {.action = parse_action::recurse};
			}
		}
		return
		{
			.action = parse_action::commit,
			.nodes_to_remove = {.offset = 0, .length = 1},
			.reduction_result = {nodes[0]}
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), TOKEN(cbrace)), FN
	{
		// cap off a block statement if its a block statement.
		// no idea if it isnt a block statement.
		auto& stmt_node = nodes[0];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(blk.capped)
			{
				chord_error("extraneous closing brace. already just capped off a block statement.");
			}
			blk.capped = true;
			stmt_node.end_location = nodes[1].end_location;
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 1, .length = 1}
			};
		}
		else if(payload_index<ast_designator_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			// we kinda expect designator statements to pop up here continually until it hits the block close brace.
			// in which case we should just recurse.
			return {.action = parse_action::recurse, .reduction_result_offset = 1};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("unexpected {} statement, expected block statement only.", stmt_name);
			// do nothing
		}
	}
CHORD_END


CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), TOKEN(obrace)), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), NODE(ast_stmt)), FN
	{
		// append to a block statement
		// no idea if it isnt a block statement.
		auto& stmt_node = nodes[0];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			stmt_node.children.push_back(nodes[1]);
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 1, .length = 1}
			};
		}
		else if(payload_index<ast_metaregion_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			// rhs stmt should be a block statement.
			auto& rhs_node = nodes[1];
			auto& rhs_stmt = std::get<ast_stmt>(rhs_node.payload);
			auto& lhs_metaregion = std::get<ast_metaregion_stmt>(stmt.stmt_);
			if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() != rhs_stmt.stmt_.index())
			{
				const char* stmt_name = rhs_stmt.type_name();
				chord_error("a metaregion stmt is followed by another statement. that rhs statement should always be a block statement, but instead you have provided a {}", stmt_name);
			}
			const auto& rhs_blk = std::get<ast_blk_stmt>(rhs_stmt.stmt_);
			if(rhs_blk.capped)
			{
				auto sz = stmt_node.children.size();
				panic_ifnt(stmt_node.children.empty(), "did not expect metaregion to have children already. it had {} children", sz);
				stmt_node.children = {rhs_node};
				lhs_metaregion.capped = true;
				stmt_node.end_location = rhs_node.end_location;
				return {.action = parse_action::reduce, .nodes_to_remove = {.offset = 1, .length = 1}};
			}
			return {.action = parse_action::recurse, .reduction_result_offset = 1};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("syntax error concerning two statements together. i've only really thought about this when the lhs is a block statement, but this time its a {} statement", stmt_name);
		}
		
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), WILDCARD), FN
	{
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 1
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(end_of_file)), FN
	{
		// translation unit -> end of file
		// this means the end.
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()}
		};
	}
CHORD_END

// wildcard chords begin

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), WILDCARD), FN
	{
		chord_error("invalid function definition syntax");
	}
CHORD_END

CHORD_BEGIN
	STATE(WILDCARD), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace), NODE(ast_stmt)), FN
	{
		// try to parse a block
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 1
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace), WILDCARD), FN
	{
		// try to parse a block
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 2
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), WILDCARD, WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

// parenthesised expressions (should allow you to avoid operator precedence issues)
CHORD_BEGIN
	STATE(TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(oparen), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(oparen), NODE(ast_expr), TOKEN(cparen)), FN
	{
		const auto& expr_node = nodes[1];
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {expr_node}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(oparen), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
	EXTENSIBLE
CHORD_END

// compositing field expressions at the end of declarations.

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(dot)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(arrow)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(dot), NODE(ast_expr)), FN
	{
		auto lhs_expr = std::get<ast_expr>(nodes[0].payload);
		while(!IS_A(lhs_expr.expr_, ast_symbol_expr))
		{
			// what if its a biop?
			if(IS_A(lhs_expr.expr_, ast_biop_expr))
			{
				auto biop = AS_A(lhs_expr.expr_, ast_biop_expr);
				lhs_expr = *biop.rhs;
			}
			else
			{
				const char* expr_name = lhs_expr.type_name();
				chord_error("lhs of expr.expr is always expected to be a symbol expr (for now). you have supplied a {} expression", expr_name);
			}
		}
		std::string_view symbol = std::get<ast_symbol_expr>(lhs_expr.expr_).symbol;
		auto& expr_node = nodes[2];
		auto& expr = std::get<ast_expr>(expr_node.payload);
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {node{.payload = ast_expr
			{
				.expr_ = ast_biop_expr
				{
					.lhs = lhs_expr,
					.type = biop_type::field,
					.rhs = expr
				}
			}}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(arrow), NODE(ast_expr)), FN
	{
		const auto& lhs_expr = std::get<ast_expr>(nodes[0].payload);
		if(lhs_expr.expr_.index() != payload_index<ast_symbol_expr, decltype(lhs_expr.expr_)>())
		{
			const char* expr_name = lhs_expr.type_name();
			chord_error("lhs of expr.expr is always expected to be a symbol expr (for now). you have supplied a {} expression", expr_name);
		}
		std::string_view symbol = std::get<ast_symbol_expr>(lhs_expr.expr_).symbol;
		auto& expr_node = nodes[2];
		auto& expr = std::get<ast_expr>(expr_node.payload);
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {node{.payload = ast_expr
			{
				.expr_ = ast_biop_expr
				{
					.lhs = lhs_expr,
					.type = biop_type::ptr_field,
					.rhs = expr,
				}
			}}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(dot), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(arrow), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(WILDCARD, TOKEN(end_of_file)), FN
	{
		auto wildcard_node = nodes.front();
		const char* node_name = node_names[wildcard_node.payload.index()];
		std::string_view wildcard_src = quote_source(state.in.source, wildcard_node.begin_location, wildcard_node.end_location);
		chord_error("unexpected end of file, was expecting more after {} (\"{}\")", node_name, wildcard_src);
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_return)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_yield)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_return), NODE(ast_stmt)), FN
	{
		// return expects an expression
		// but due to an ambiguous grammar we should also steal a statement if it is an expression statement.
		// if it's not then we're probably an error.
		const auto& stmt = std::get<ast_stmt>(nodes[1].payload);
		if(stmt.stmt_.index() == payload_index<ast_expr_stmt, decltype(stmt.stmt_)>())
		{
			// steal the expression and use that instead.
			const auto& retval = std::get<ast_expr_stmt>(stmt.stmt_).expr;
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = nodes.size()},
				.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_return_stmt{.retval = retval}}}}
			};
		}
		else
		{
			const char* stmt_type = stmt.type_name();
			chord_error("return is followed by a statement instead of an expression. this is acceptable if the statement is an expression statement, but instead you have provided a {}", stmt_type);
		}
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_yield), NODE(ast_stmt)), FN
	{
		// yield expects an expression
		// but due to an ambiguous grammar we should also steal a statement if it is an expression statement.
		// if it's not then we're probably an error.
		const auto& stmt = std::get<ast_stmt>(nodes[1].payload);
		if(stmt.stmt_.index() == payload_index<ast_expr_stmt, decltype(stmt.stmt_)>())
		{
			// steal the expression and use that instead.
			const auto& retval = std::get<ast_expr_stmt>(stmt.stmt_).expr;
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = nodes.size()},
				.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_yield_stmt{.retval = retval}}}}
			};
		}
		else
		{
			const char* stmt_type = stmt.type_name();
			chord_error("yield is followed by a statement instead of an expression. this is acceptable if the statement is an expression statement, but instead you have provided a {}", stmt_type);
		}
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_return), NODE(ast_expr), TOKEN(semicol)), FN
	{
		const auto& retval = std::get<ast_expr>(nodes[1].payload);
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_return_stmt{.retval = retval}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_yield), NODE(ast_expr), TOKEN(semicol)), FN
	{
		const auto& retval = std::get<ast_expr>(nodes[1].payload);
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_yield_stmt{.retval = retval}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_return), TOKEN(semicol)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_return_stmt{}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_return), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_yield), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(compare), TOKEN(symbol)), FN
	{
		return {.action = parse_action::recurse};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(compare), TOKEN(symbol)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(compare), TOKEN(symbol), TOKEN(compare)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(compare), TOKEN(symbol), TOKEN(compare), TOKEN(obrace)), FN
	{
		std::string_view name = std::get<ast_token>(nodes[1].payload).lexeme;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {
				node{.payload = ast_stmt{.stmt_ = ast_metaregion_stmt{.name = std::string{name}}}},
				node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}
			}
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), WILDCARD), FN
	{
		chord_error("unexpected token(s) directly following a decl, did you forget a semicolon?");
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(obrack), TOKEN(obrack)), FN
	{
		return {.action = parse_action::recurse};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack), TOKEN(obrack)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack), TOKEN(obrack), NODE(ast_expr), TOKEN(cbrack)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack), TOKEN(obrack), NODE(ast_expr), TOKEN(cbrack), TOKEN(cbrack)), FN
	{
		ast_attribute attr;
		const ast_expr& expr = AS_A(nodes[2].payload, ast_expr);
		if(IS_A(expr.expr_, ast_symbol_expr))
		{
			attr.key = std::string{AS_A(expr.expr_, ast_symbol_expr).symbol};
		}
		else if(IS_A(expr.expr_, ast_biop_expr))
		{
			const auto& biop = AS_A(expr.expr_, ast_biop_expr);
			if(biop.type != biop_type::assign)
			{
				chord_error("attribute contents that are a binary operation are only allowed to be assignments");
			}
			if(!IS_A(biop.lhs->expr_, ast_symbol_expr))
			{
				auto name = biop.lhs->type_name();
				chord_error("lhs of assignment within attribute contents is only allowed to be a symbol expression. yours is a {} expression.", name);
			}
			attr.key = std::string{AS_A(biop.lhs->expr_, ast_symbol_expr).symbol};
			attr.value = *biop.rhs;
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 5},
			.reduction_result = {node{.payload = attr}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack), TOKEN(obrack), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack), TOKEN(obrack), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrack), TOKEN(obrack), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

// attribute preceding statement.
CHORD_BEGIN
	STATE(NODE(ast_attribute), WILDCARD), FN
	{
		return {.action = parse_action::recurse};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_attribute)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_attribute), NODE(ast_stmt)), FN
	{
		const auto& attr = AS_A(nodes[0].payload, ast_attribute);
		auto& stmt = AS_A(nodes[1].payload, ast_stmt);
		if(stmt.attributes.contains(attr.key))
		{
			chord_error("attribute \"{}\" defined more than once for the same statement", attr.key);
		}
		stmt.attributes.emplace(attr.key, attr.value);

		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_attribute), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

// static if statements
CHORD_BEGIN
	STATE(TOKEN(keyword_static_if), WILDCARD), FN
	{
		return {.action = parse_action::recurse};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), NODE(ast_stmt)), FN
	{
		const auto& expr_node = nodes[2];
		const auto& expr = std::get<ast_expr>(expr_node.payload);
		auto& stmt_node = nodes[4];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(!blk.capped)
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 4};
			}
			// static if blocks do *not* introduce scope.
			blk.introduce_scope = false;
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = 5},
				.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_if_stmt
					{
						.condition = std::get<ast_expr>(nodes[2].payload),
						.is_static = true
					}}, .children = {stmt_node}}}
			};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("{} statement detected directly after a static-if-statement. you should provide a block statement instead.");
		}
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 6},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_if_stmt
				{
					.condition = std::get<ast_expr>(nodes[2].payload),
					.is_static = true
				}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 4};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_static_if), TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

// if statements
CHORD_BEGIN
	STATE(TOKEN(keyword_if), WILDCARD), FN
	{
		chord_error("if-statements are not allowed outside of a block. use a static-if-statement or place this if-statement inside of a function block/metaregion");
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), NODE(ast_stmt)), FN
	{
		const auto& expr_node = nodes[2];
		const auto& expr = std::get<ast_expr>(expr_node.payload);
		const auto& stmt_node = nodes[4];
		const auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			const auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(!blk.capped)
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 4};
			}
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = 5},
				.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_if_stmt
					{
						.condition = std::get<ast_expr>(nodes[2].payload),
						.is_static = false
					}}, .children = {stmt_node}}}
			};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("{} statement detected directly after an if-statement. you should provide a block statement instead.");
		}
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 6},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_if_stmt
				{
					.condition = std::get<ast_expr>(nodes[2].payload),
					.is_static = false
				}},
				.children = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}
			}}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 4};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_if), TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

// while statement
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), NODE(ast_stmt)), FN
	{
		const auto& expr_node = nodes[2];
		const auto& expr = std::get<ast_expr>(expr_node.payload);
		const auto& stmt_node = nodes[4];
		const auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			const auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(!blk.capped)
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 4};
			}
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = 5},
				.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_while_stmt
					{
						.condition = std::get<ast_expr>(nodes[2].payload),
					}}, .children = {stmt_node}}}
			};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("{} statement detected directly after an if-statement. you should provide a block statement instead.");
		}
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 6},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_while_stmt
				{
					.condition = std::get<ast_expr>(nodes[2].payload),
				}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 4};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_while), TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

// for statement
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 4};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 4};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 6};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(cparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 6};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		const auto& init_node = nodes[2];
		const auto& cond_node = nodes[4];
		const auto& iter_node = nodes[6];

		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 10},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_for_stmt
				{
					.init = AS_A(init_node.payload, ast_expr),
					.cond = AS_A(cond_node.payload, ast_expr),
					.iter = AS_A(iter_node.payload, ast_expr)
				}}, .children = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(cparen), NODE(ast_stmt)), FN
	{
		const auto& init_node = nodes[2];
		const auto& cond_node = nodes[4];
		const auto& iter_node = nodes[6];
		const auto& stmt_node = nodes[8];
		const auto& stmt = AS_A(stmt_node.payload, ast_stmt);
		if(!IS_A(stmt.stmt_, ast_blk_stmt))
		{
			const char* stmt_name = stmt.type_name();
			chord_error("{} statement detected directly after for-statement. you should provide a block statement instead.");
		}

		const auto& blk = AS_A(stmt.stmt_, ast_blk_stmt);
		if(!blk.capped)
		{
			return {.action = parse_action::recurse, .reduction_result_offset = 8};
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 9},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_for_stmt
				{
					.init = AS_A(init_node.payload, ast_expr),
					.cond = AS_A(cond_node.payload, ast_expr),
					.iter = AS_A(iter_node.payload, ast_expr)
				}}, .children = {stmt_node}}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_for), TOKEN(oparen), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(comma), NODE(ast_expr), TOKEN(cparen), TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 8};
	}
EXTENSIBLE
CHORD_END

// end of chords
}
//////////////////////////// BUILD SYSTEM ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE build_system

void compile_file(std::filesystem::path file, compile_args& args);
void compile_source(std::filesystem::path file, std::string source, compile_args& args)
{
	auto absolute = std::filesystem::absolute(file);
	std::string filename = absolute.filename().string();
	std::string directory = absolute.parent_path().string();
	llvm::DIFile* debug_file = codegen.debug->createFile(filename.c_str(), directory.c_str(), std::nullopt, source);
	debug_files.emplace(file, debug_file);

	timer_restart();
	lex_output tokens = lex_from_data(file, source);
	if(args.verbose_lex)
	{
		tokens.verbose_print();
	}

	time_lex += elapsed_time();
	timer_restart();

	node ast = parse(tokens, args.verbose_parse);
	if(args.verbose_ast)
	{
		ast.verbose_print(tokens.source);
	}

	time_parse += elapsed_time();
	timer_restart();
	auto now_cpy = now;
	auto codegen_cpy = time_codegen;

	node* build = try_find_build_metaregion(ast);
	if(build != nullptr)
	{
		semal_local_state local;
		semal_local_state* ptr = &local;
		semal_result metaregion_result = semal_metaregion_stmt(AS_A(AS_A(build->payload, ast_stmt).stmt_, ast_metaregion_stmt), *build, source, ptr);
		verify_semal_result(metaregion_result, *build, source);
		for(node& child : build->children)
		{
			semal_result child_result = semal(child, source, ptr);
		}
		while (global.added_source_files.size())
		{
			const auto [src_path, included_from_srcloc] = global.added_source_files.back();
			global.added_source_files.pop_back();
			if (!global.compiled_source_files.contains(src_path))
			{
				compile_file(src_path, args);
			}
		}
		for(const auto& [lib_path, included_from_srcloc] : global.added_link_libraries)
		{
			if(!global.registered_link_libraries.contains(lib_path))
			{
				args.link_libraries.push_back(lib_path);
				global.registered_link_libraries.insert(lib_path);
			}
		}
		/*
		auto temp_state = *types;
		semal(*build, temp_state, ctx);
		for(const auto& [newfile, loc] : temp_state.added_source_files)
		{
			error_ifnt(newfile != file, loc, "source file {} adds itself {}", file, loc);
			auto filename = newfile.filename();
			error_ifnt(std::filesystem::exists(newfile), {}, "could not find source file \"{}\" added {}", filename, loc);
			compile_file(newfile, args, types, false);
		}
		for(const auto& [libpath, loc] : temp_state.added_link_libraries)
		{
			auto filename = libpath.filename();
			error_ifnt(std::filesystem::exists(libpath), {}, "could not find link library \"{}\" added {}", filename, loc);
			args.link_libraries.push_back(libpath);
		}
		*/
	}
	//semal(ast, *types, ctx, true);
	if(global.compiled_source_files.contains(file))
	{
		return;
	}
	semal(ast, source, nullptr, true);

	auto codegen_diff = time_codegen - codegen_cpy;
	timer_restart();
	auto right_now = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
	time_semal += right_now - std::chrono::duration_cast<std::chrono::milliseconds>(now_cpy.time_since_epoch()).count();
	// some of the semal may have been spent codegen'ing. remove that.
	time_semal -= codegen_diff;

	// todo: codegen

	time_codegen = elapsed_time();
	timer_restart();
}

void compile_file(std::filesystem::path file, compile_args& args)
{
	/*
	if(include_preload)
	{
		compile_source("preload.psy", get_preload_source(), args);
	}
	*/
	compile_source(file, read_file(file), args);
}

// entry point

int main(int argc, char** argv)
{
	timer_restart();
	populate_chords();

	std::vector<std::string_view> cli_args(argv + 1, argv + argc);
	compile_args args = parse_args(cli_args);
	std::filesystem::path cli_output_dir = args.output_dir;
	global.args = &args;
	if(args.should_print_help)
	{
		print_help();
	}
	if(args.build_file == std::filesystem::path{})
	{
		return 0;
	}

	time_setup = elapsed_time();
	timer_restart();

	codegen_initialise(std::filesystem::absolute(args.build_file));
	compile_source("preload.psy", get_preload_source(), args);
	compile_file(args.build_file, args);
	if(args.output_type == target::executable)
	{
		#ifdef _WIN32
		compile_source("entrypoint.psy", 
		"ExitProcess ::= func(uExitCode : u32 weak) -> v0 := extern;\
		_psymain ::= func() -> v0\
		{\
			ret ::= main();\
			ExitProcess(ret);\
		};",
		args);
		#else
		panic("todo: linux support");
		#endif
	}

	semal_verify(args);
	codegen_verify();
	if(cli_output_dir != ".")
	{
		// this means that CLI specified -o for an output directory
		// this should always override anything specified in the code.
		warning({}, "\"-o {}\" overrides path \"{}\" set by build system", cli_output_dir, args.output_dir);
		args.output_dir = cli_output_dir;
	}
	std::filesystem::path object_file_path = codegen_generate(args);
	codegen_terminate(args.verbose_codegen);

	link(object_file_path, args);

	std::print("\nsetup:    {}s\nlex:      {}s\nparse:    {}s\nsemal:    {}s\ncodegen:  {}s\nassemble: {}s\nlink:     {}s\ntotal:    {}s\n\n", time_setup / 1000.0f, time_lex / 1000.0f, time_parse / 1000.0f, time_semal / 1000.0f, time_codegen / 1000.0f, time_assemble / 1000.0f, time_link / 1000.0f, (time_setup + time_lex + time_parse + time_semal + time_codegen + time_assemble + time_link) / 1000.0f);
}

semal_result semal_call_builtin(const ast_callfunc_expr& call, node& n, std::string_view source, semal_local_state*& local, bool do_codegen)
{
	auto get_as_string = [&n, &source, &local](const ast_expr& expr) -> std::string
	{
		semal_result result = semal_expr(expr, n, source, local, false);
		return std::get<std::string>(std::get<literal_val>(result.val.val));
	};
	auto get_as_integer = [&n, &source, &local](const ast_expr& expr) -> std::int64_t
	{
		semal_result result = semal_expr(expr, n, source, local, false);
		return std::get<std::int64_t>(std::get<literal_val>(result.val.val));
	};
	type_t strlit = type_t::create_pointer_type(type_t::create_primitive_type(prim_ty::type::u8));
	strlit.qual = typequal_static;

	if(call.function_name == "add_link_library")
	{
		std::filesystem::path path = get_as_string(call.params.front());
		global.added_link_libraries.emplace(path, n.begin_location);
	}
	else if(call.function_name == "add_source_file")
	{
		std::filesystem::path path = get_as_string(call.params.front());
		if(!path.is_absolute())
		{
			path = n.begin_location.file.parent_path() / path;
		}
		if(!std::filesystem::exists(path))
		{
			return semal_result::err("cannot find source file \"{}\"", path);
		}
		global.added_source_files.emplace_back(path, n.begin_location);
	}
	else if(call.function_name == "run_command")
	{
		std::string str = get_as_string(call.params.front());
		int ret = system(str.c_str());
		if(ret != 0)
		{
			error(n.begin_location, "run_command \033[1;34m{}\033[0m returned exit code \"{}\"", str, ret);
		}
	}
	else if(call.function_name == "set_object")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		std::string name = get_as_string(call.params.front());
		global.args->output_name = name;
		global.args->output_type = target::object;
	}
	else if(call.function_name == "set_library")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		std::string name = get_as_string(call.params.front());
		global.args->output_name = name;
		global.args->output_type = target::library;
	}
	else if(call.function_name == "set_executable")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		std::string name = get_as_string(call.params.front());
		global.args->output_name = name;
		global.args->output_type = target::executable;
	}
	else if(call.function_name == "set_optimisation")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		int opt = get_as_integer(call.params.front());
		global.args->optimisation_level = opt;
	}
	else if(call.function_name == "set_output_directory")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		std::string name = get_as_string(call.params.front());
		global.args->output_dir = name;
	}
	else if(call.function_name == "bundle_file")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		std::filesystem::path path = get_as_string(call.params.front());
		error_ifnt(std::filesystem::exists(path), {}, "bundle file \"{}\" does not exist", path);
		error_ifnt(std::filesystem::is_regular_file(path), {}, "bundle file \"{}\" is not a regular file, did you mean to bundle_directory?", path);
		std::error_code ec;
		std::string errmsg;
		std::filesystem::copy(path, global.args->output_dir, std::filesystem::copy_options::recursive | std::filesystem::copy_options::overwrite_existing, ec);
		errmsg = ec.message();
		error_ifnt(!ec, {}, "bundle file copy failed: \"{}\"", errmsg);
		auto full_path = std::filesystem::absolute(global.args->output_dir / path.filename());
		msg({}, "bundle \"{}\" => \"{}\"", path, full_path);
	}
	else if(call.function_name == "bundle_directory")
	{
		panic_ifnt(global.args != nullptr, "compiler dev forgot to set global.args :)");
		std::filesystem::path path = get_as_string(call.params.front());
		error_ifnt(std::filesystem::exists(path), {}, "bundle directory \"{}\" does not exist", path);
		error_ifnt(std::filesystem::is_directory(path), {}, "\"{}\" is not a directory", path);
		std::error_code ec;
		std::string errmsg;
		std::filesystem::path bundle_output_path = global.args->output_dir / path.filename();
		if(!std::filesystem::exists(bundle_output_path))
		{
			std::filesystem::create_directories(bundle_output_path, ec);
			errmsg = ec.message();
			error_ifnt(!ec, {}, "bundle directory copy failed: \"{}\"", errmsg);
		}
		std::filesystem::copy(path, bundle_output_path, std::filesystem::copy_options::recursive | std::filesystem::copy_options::overwrite_existing, ec);
		errmsg = ec.message();
		error_ifnt(!ec, {}, "bundle directory copy failed: \"{}\"", errmsg);
		auto full_path = std::filesystem::absolute(global.args->output_dir / path.filename());
		msg({}, "bundle \"{}\" => \"{}\" (recursively)", path, full_path);
	}
	else if(call.function_name == "enable_debug_symbols")
	{
		semal_result result = semal_expr(call.params.front(), n, source, local, false);
		auto debug = std::get<bool>(std::get<literal_val>(result.val.val));
		global.args->debug_symbols = debug;
	}
	else if(call.function_name == "__atomic_add")
	{
		semal_result ptr = semal_expr(call.params[0], n, source, local, true);
		if(!ptr.val.ty.is_ptr())
		{
			return semal_result::err("first parameter to __atomic_add must be a pointer. you have provided a {}", ptr.val.ty.name());
		}
		ptr.load_if_variable();
		semal_result addval = semal_expr(call.params[1], n, source, local, true);
		if(!addval.val.ty.is_prim())
		{
			return semal_result::err("second parameter to __atomic_add must be an integral type. you have provided a {}", addval.val.ty.name());
		}
		if(!(addval.val.ty.qual & typequal_static))
		{
			return semal_result::err("second parameter to __atomic_add must be static. you have provided a {}", addval.val.ty.name());
		}
		return semal_result
		{
			.t = semal_type::misc,
			.val = sval
			{
				.ty = type_t::create_void_type(),
				.ll = codegen.ir->CreateAtomicRMW
				(
					llvm::AtomicRMWInst::BinOp::Add,
					ptr.val.ll,
					addval.val.llvm(),
					llvm::MaybeAlign(),
					llvm::AtomicOrdering::Monotonic
				)
			}
		};
	}
	else if(call.function_name == "__message")
	{
		std::string msg = get_as_string(call.params.front());
		#define OLD_COMPILER_STAGE COMPILER_STAGE
		#undef COMPILER_STAGE
		#define COMPILER_STAGE meta
		msg(n.begin_location, "{}", msg);
		#undef COMPILER_STAGE
		#define COMPILER_STAGE OLD_COMPILER_STAGE
		#undef OLD_COMPILER_STAGE
	}
	else if(call.function_name == "__warning")
	{
		std::string msg = get_as_string(call.params.front());
		#define OLD_COMPILER_STAGE COMPILER_STAGE
		#undef COMPILER_STAGE
		#define COMPILER_STAGE meta
		warning(n.begin_location, "{}", msg);
		#undef COMPILER_STAGE
		#define COMPILER_STAGE OLD_COMPILER_STAGE
		#undef OLD_COMPILER_STAGE
	}
	else if(call.function_name == "__error")
	{
		std::string msg = get_as_string(call.params.front());
		#define OLD_COMPILER_STAGE COMPILER_STAGE
		#undef COMPILER_STAGE
		#define COMPILER_STAGE meta
		error(n.begin_location, "{}", msg);
		#undef COMPILER_STAGE
		#define COMPILER_STAGE OLD_COMPILER_STAGE
		#undef OLD_COMPILER_STAGE
	}
	else if(call.function_name == "__env")
	{
		std::string envvar_name = get_as_string(call.params.front());
		const char* envval = std::getenv(envvar_name.c_str());
		std::string envstr = "";
		if(envval != nullptr)
		{
			envstr = envval;
		}
		return
		{
			.t = semal_type::misc,
			.label = std::format("__env({})", envvar_name),
			.val =
			{
				.val = literal_val{envstr},
				.ty = strlit
			}
		};
	}
	else if(call.function_name == "__concat")
	{
		std::string lhs = get_as_string(call.params[0]);
		std::string rhs = get_as_string(call.params[1]);
		std::string result = lhs + rhs;

		return semal_literal_expr({.value = result}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__strlen")
	{
		std::string str = get_as_string(call.params.front());
		sval val
		{
			.val = literal_val{static_cast<std::int64_t>(str.size())},
			.ty = type_t::create_primitive_type(prim_ty::type::s64)
		};
		val.ll = val.llvm();
		return
		{
			.t = semal_type::misc,
			.label = "",
			.val = val
		};
	}
	else if(call.function_name == "__sizeof")
	{
		ast_expr expr = call.params.front();
		type_t ty = type_t::badtype();
		semal_result expr_result = semal_expr(expr, n, source, local, false);
		if(expr_result.is_err())
		{
			// it better be a sytmbol expression.
			if(IS_A(expr.expr_, ast_symbol_expr))
			{
				std::string_view symbol = AS_A(expr.expr_, ast_symbol_expr).symbol;
				ty = local->parse_type(symbol);
			}
			else
			{
				return semal_result::err("invalid parameter passed to __sizeof. expected a valid expression, or a typename.");
			}
		}
		else
		{
			ty = expr_result.val.ty;
		}
		if(ty.is_badtype())
		{
			return semal_result::err("unknown type passed to __sizeof.");
		}
		llvm::Type* llty = ty.llvm();
		ast_literal_expr lit
		{
			.value = static_cast<std::int64_t>(codegen.mod->getDataLayout().getTypeAllocSize(llty))
		};
		return semal_literal_expr(lit, n, source, local, do_codegen);
	}
	else if(call.function_name == "__alignof")
	{
		ast_expr expr = call.params.front();
		type_t ty = type_t::badtype();
		semal_result expr_result = semal_expr(expr, n, source, local, false);
		if(expr_result.is_err())
		{
			// it better be a sytmbol expression.
			if(IS_A(expr.expr_, ast_symbol_expr))
			{
				std::string_view symbol = AS_A(expr.expr_, ast_symbol_expr).symbol;
				ty = local->parse_type(symbol);
			}
			else
			{
				return semal_result::err("invalid parameter passed to __sizeof. expected a valid expression, or a typename.");
			}
		}
		else
		{
			ty = expr_result.val.ty;
		}
		if(ty.is_badtype())
		{
			return semal_result::err("unknown type passed to __sizeof.");
		}
		llvm::Type* llty = ty.llvm();
		ast_literal_expr lit
		{
			.value = static_cast<std::int64_t>(codegen.mod->getDataLayout().getABITypeAlign(llty).value() * 8)
		};
		return semal_literal_expr(lit, n, source, local, do_codegen);
		
	}
	else if(call.function_name == "__debugbreak")
	{
		return
		{
			.t = semal_type::misc,
			.label = "debugbreak",
			.val =
			{
				.ty = type_t::create_void_type(),
				.ll = codegen.ir->CreateCall(llvm::Intrinsic::getOrInsertDeclaration(codegen.mod.get(), llvm::Intrinsic::debugtrap))
			}
		};
	}
	else if(call.function_name == "__unreachable")
	{
		return
		{
			.t = semal_type::misc,
			.label = "unreachable",
			.val =
			{
				.ty = type_t::create_void_type(),
				.ll = codegen.ir->CreateUnreachable()
			}
		};
	}
	else if(call.function_name == "__is_a")
	{
		semal_result param = semal_expr(call.params.front(), n, source, local, false);
		param.val.ty.qual = typequal_none;
		type_t ty = local->parse_type(AS_A(call.params[1].expr_, ast_symbol_expr).symbol);
		ty.qual = typequal_none;
		return semal_literal_expr({.value = param.val.ty == ty}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__is_type")
	{
		return semal_literal_expr({.value = expr_is_type(call.params.front(), n, source, local, do_codegen)}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__is_alias")
	{
		return semal_literal_expr({.value = expr_is_alias(call.params.front(), n, source, local, do_codegen)}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__is_struct")
	{
		return semal_literal_expr({.value = expr_is_struct(call.params.front(), n, source, local, do_codegen)}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__typename")
	{
		semal_result param = semal_expr(call.params.front(), n, source, local, false);
		return semal_literal_expr({.value = param.val.ty.name()}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__enumname")
	{
		semal_result param = semal_expr(call.params.front(), n, source, local, true);
		param.load_if_variable();
		if(!param.val.ty.is_enum())
		{
			return semal_result::err("__enumname must be provided an enum value, but you have provided a \"{}\"", param.val.ty.name());
		}
		auto enumty = AS_A(param.val.ty.payload, enum_ty);
		ast_literal_expr badenum
		{
			.value = std::string{"badenum"}
		};
		if(param.val.ty.qual & typequal_static)
		{
			// let's do it real easy now.
			auto val = AS_A(AS_A(param.val.val, literal_val), std::int64_t);
			for(const auto& [name, value] : enumty.entries)
			{
				if(value == val)
				{
					return semal_literal_expr({.value = std::string{name}}, n, source, local, do_codegen);
				}
			}
			return semal_literal_expr(badenum, n, source, local, do_codegen);
		}

		//auto decl = semal_decl(ast_decl{.type_name = "u8& mut", .name = "_enumname_result", .initialiser = ast_expr{.expr_ = badenum}}, n, source, local, true);
		auto decl_stmt = ast_decl_stmt
		{
			.decl =
			{
				.type_name = "u8& mut",
				.name = "_enumname_result",
				.initialiser = ast_expr{.expr_ = badenum}
			}
		};
		attributes_t attr;
		attr["__force_mutable"] = {};
		semal_result variable = semal_decl_stmt(decl_stmt, n, source, local, true, attr);
		const semal_result* maybe_parent = local->try_find_parent_function();
		llvm::Function* parent_fn = static_cast<llvm::Function*>(maybe_parent->val.ll);
		for(const auto& [name, value] : enumty.entries)
		{
			semal_result lit = semal_literal_expr({.value = value}, n, source, local, true);
			llvm::Value* cond = codegen.ir->CreateICmpEQ(param.val.ll, lit.val.ll);
			if(maybe_parent == nullptr)
			{
				return semal_result::err("it is an error to call __enumname outside of a function implementation.");
			}
			llvm::BasicBlock* cond_blk = llvm::BasicBlock::Create(*codegen.ctx, std::format("__enum_name_{}", name), parent_fn);
			llvm::BasicBlock* cont_blk = llvm::BasicBlock::Create(*codegen.ctx, "__enumname cont");
			parent_fn->insert(parent_fn->end(), cont_blk);
			codegen.ir->CreateCondBr(cond, cond_blk, cont_blk);
			codegen.ir->SetInsertPoint(cond_blk);
			semal_result strlit = semal_literal_expr({.value = name}, n, source, local, true);
			codegen.ir->CreateStore(strlit.val.ll, variable.val.ll);
			codegen.ir->CreateBr(cont_blk);

			codegen.ir->SetInsertPoint(cont_blk);
		}
		return
		{
			.t = semal_type::misc,
			.val =
			{
				.ty = type_t::create_pointer_type(type_t::create_primitive_type(prim_ty::type::u8)),
				.ll = variable.val.load()
			}
		};
	}
	else if(call.function_name == "__embed")
	{
		#define OLD_COMPILER_STAGE COMPILER_STAGE
		#undef COMPILER_STAGE
		#define COMPILER_STAGE meta
		std::filesystem::path path = get_as_string(call.params.front());
		if(!std::filesystem::exists(path))
		{
			error(n.begin_location, "cannot \033[1;34m__embed\033[0m non-existent file \"{}\"", path);
		}
		if(!std::filesystem::is_regular_file(path))
		{
			error(n.begin_location, "cannot \033[1;34m__embed\033[0m \"{}\" as it is not a regular file", path);
		}
		std::string file_contents = read_file(path);

		#undef COMPILER_STAGE
		#define COMPILER_STAGE OLD_COMPILER_STAGE
		#undef OLD_COMPILER_STAGE
		return semal_literal_expr({.value = file_contents}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__file")
	{
		return semal_literal_expr({.value = n.begin_location.file.string()}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__function")
	{
		auto find_function_from_local = [](const semal_local_state& loc) -> std::string
		{
			if(loc.unfinished_types.empty())
			{
				return "";
			}
			for(std::size_t i = loc.unfinished_types.size() - 1; i > 0; i--)
			{
				if(loc.unfinished_types[i].t == semal_type::function_decl)
				{
					return loc.unfinished_types[i].label;
				}
			}
			return "";
		};
		std::string ret = "";
		const semal_local_state* l = local;
		while(l != nullptr && ret == "")
		{
			ret = find_function_from_local(*l);
			if(ret.empty())
			{
				l = l->parent;
			}
		}
		return semal_literal_expr({.value = ret}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__line")
	{
		return semal_literal_expr({.value = n.begin_location.line}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__column")
	{
		return semal_literal_expr({.value = n.begin_location.column}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__config")
	{
		return semal_literal_expr({.value = global.args->build_config}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__memcpy")
	{
		semal_result dst = semal_expr(call.params[0], n, source, local, true);
		dst.load_if_variable();
		dst.convert_to(local->parse_type("v0& weak"));
		semal_result src = semal_expr(call.params[1], n, source, local, true);
		src.load_if_variable();
		src.convert_to(local->parse_type("v0& weak"));
		semal_result count = semal_expr(call.params[2], n, source, local, true);
		count.load_if_variable();
		count.convert_to(local->parse_type("u64"));

		return semal_result
		{
			.t = semal_type::misc,
			.val = sval
			{
				.ty = type_t::create_void_type(),
				.ll = codegen.ir->CreateMemCpyInline(dst.val.ll, llvm::MaybeAlign(), src.val.ll, llvm::MaybeAlign(), count.val.ll)
			}
		};
	}
	else if(call.function_name == "__memset")
	{
		semal_result ptr = semal_expr(call.params[0], n, source, local, true);
		ptr.load_if_variable();
		ptr.convert_to(local->parse_type("v0& weak"));
		semal_result fill = semal_expr(call.params[1], n, source, local, true);
		fill.load_if_variable();
		fill.convert_to(local->parse_type("u8"));
		semal_result count = semal_expr(call.params[2], n, source, local, true);
		count.load_if_variable();
		count.convert_to(local->parse_type("u64"));

		return semal_result
		{
			.t = semal_type::misc,
			.val = sval
			{
				.ty = type_t::create_void_type(),
				.ll = codegen.ir->CreateMemSetInline(ptr.val.ll, llvm::MaybeAlign(), fill.val.ll, count.val.ll)
			}
		};
	}
	else if(call.function_name == "__arrlen")
	{
		semal_result arr = semal_expr(call.params[0], n, source, local, do_codegen);
		if(arr.is_err())
		{
			return arr;
		}
		if(!arr.val.ty.is_arr())
		{
			return semal_result::err("__arrlen expects an array type");
		}
		arr_ty arrty = AS_A(arr.val.ty.payload, arr_ty);
		return semal_literal_expr(ast_literal_expr{.value = literal_val{static_cast<std::int64_t>(arrty.array_length)}}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__union")
	{
		// represents an array of bytes of fixed-size (equal to the largest size of the types passed to the union)
		std::size_t size = 0;
		if(call.params.empty())
		{
			return semal_result::err("__union must be called with at least one parameter/typename");
		}
		for(const ast_expr& expr : call.params)
		{
			type_t cur_ty = type_t::badtype();
			semal_result expr_result = semal_expr(expr, n, source, local, do_codegen);
			if (expr_result.is_err())
			{
				// it better be a sytmbol expression.
				if (IS_A(expr.expr_, ast_symbol_expr))
				{
					std::string_view symbol = AS_A(expr.expr_, ast_symbol_expr).symbol;
					cur_ty = local->parse_type(symbol);
				}
				else
				{
					return semal_result::err("invalid parameter passed to __union. expected a valid expression, or a typename.");
				}
			}
			else
			{
				cur_ty = expr_result.val.ty;
			}
			if (cur_ty.is_badtype())
			{
				return semal_result::err("unknown type passed to __union.");
			}
			size = std::max(size, 8 * static_cast<std::size_t>(codegen.mod->getDataLayout().getTypeAllocSize(cur_ty.llvm())));
		}
		sval::array_val arr;
		auto element_ty = type_t::create_primitive_type(prim_ty::type::u8);
		arr.resize(size, sval{.val = sval_zero_tag{}, .ty = element_ty});

		auto ret_ty = type_t::create_array_type(element_ty, size);
		sval retval
		{
			.val = arr,
			.ty = type_t::create_array_type(element_ty, size),
		};
		retval.ll = retval.llvm();
		return semal_result
		{
			.t = semal_type::misc,
			.val = retval
		};
	}
	else if(call.function_name == "__arrcpy")
	{
		ast_expr expr = call.params[0];
		type_t element_ty = type_t::badtype();
		semal_result expr_result = semal_expr(expr, n, source, local, do_codegen);
		if (expr_result.is_err())
		{
			// it better be a sytmbol expression.
			if (IS_A(expr.expr_, ast_symbol_expr))
			{
				std::string_view symbol = AS_A(expr.expr_, ast_symbol_expr).symbol;
				element_ty = local->parse_type(symbol);
			}
			else
			{
				return semal_result::err("invalid first parameter passed to __array. expected a valid expression, or a typename.");
			}
		}
		else
		{
			element_ty = expr_result.val.ty;
		}
		if (element_ty.is_badtype())
		{
			return semal_result::err("unknown type passed to __array.");
		}

		ast_expr target = call.params[1];
		std::size_t arr_len = call.params.size() - 2;
		for(std::size_t i = 0; i < arr_len; i++)
		{
			ast_expr val = call.params[2 + i];
			// deref(target at i) = val
			ast_biop_expr target_ptr
			{
				.lhs = target,
				.type = biop_type::at,
				.rhs = ast_expr
				{
					.expr_ = ast_literal_expr{.value = literal_val{static_cast<int64_t>(i)}}
				}
			};
			ast_unop_expr deref
			{
				.type = unop_type::deref,
				.rhs = ast_expr
				{
					.expr_ = target_ptr
				}
			};
			ast_biop_expr assign
			{
				.lhs = ast_expr{.expr_ = deref},
				.type = biop_type::assign,
				.rhs = val
			};
			semal_expr(ast_expr{.expr_ = assign}, n, source, local, do_codegen);
		}
		return semal_literal_expr(ast_literal_expr{.value = static_cast<std::int64_t>(arr_len)}, n, source, local, do_codegen);
	}
	else if(call.function_name == "__array")
	{
		ast_expr expr = call.params.front();
		type_t element_ty = type_t::badtype();
		semal_result expr_result = semal_expr(expr, n, source, local, do_codegen);
		if (expr_result.is_err())
		{
			// it better be a sytmbol expression.
			if (IS_A(expr.expr_, ast_symbol_expr))
			{
				std::string_view symbol = AS_A(expr.expr_, ast_symbol_expr).symbol;
				element_ty = local->parse_type(symbol);
			}
			else
			{
				return semal_result::err("invalid first parameter passed to __array. expected a valid expression, or a typename.");
			}
		}
		else
		{
			element_ty = expr_result.val.ty;
		}
		if (element_ty.is_badtype())
		{
			return semal_result::err("unknown type passed to __array.");
		}

		element_ty.qual = element_ty.qual | typequal_static;
		if(element_ty.is_badtype())
		{
			return semal_result::err("invalid array element type \"{}\"", element_ty.name());
		}
		std::size_t arr_len = call.params.size() - 1;
		sval::array_val arr;
		arr.resize(arr_len);
		for(std::size_t i = 0; i < arr_len; i++)
		{
			semal_result res = semal_expr(call.params[1 + i], n, source, local, do_codegen);
			if(res.is_err())
			{
				return res;
			}
			if(!res.val.ty.is_convertible_to(element_ty))
			{
				return semal_result::err("fooey");
			}
			if(!(res.val.ty.qual & typequal_static))
			{
				return semal_result::err("all elements within a call to __array must be static");
			}
			arr[i] = res.val;
		}
		type_t arrty = type_t::create_array_type(element_ty, arr_len);
		arrty.qual = typequal_static;
		sval ret = sval
		{
			.val = arr,
			.ty = arrty
		};
		ret.ll = ret.llvm();
		return semal_result
		{
			.t = semal_type::misc,
			.val = ret
		};
	}
	else if(call.function_name.starts_with("__"))
	{
		return semal_result::err("unknown builtin \"{}\"", call.function_name);
	}
	if(call.function_name.starts_with("__"))
	{
		return semal_result{.t = semal_type::misc};
	}
	else
	{
		return semal_result::null();
	}
}

std::string get_preload_source()
{

	// some psy source code that is *always* compiled before any file. its API is available to everything.
	// so uh try not to make it code that compiles slow as fuck thanks
	static constexpr char preload_src[] = R"psy(
	__is_windows : bool static := {};
	__is_linux : bool static := {};
	__psyc ::= "{}";
	__cwd ::= "{}";
	//__chkstk ::= func() -> v0{{}};
	__pi ::= 3.14159;
	[[public_linkage]]
	_fltused : s32 := (0@s32);

	srcloc ::= struct
	{{
		file : u8&;
		function : u8&;
		line : u64;
		column : u64;
	}};
	srcloc_current ::= macro() -> srcloc
	{{
		yield srcloc
		{{
			.file := __file();
			.function := __function();
			.line := __line();
			.column := __column();
		}};
	}};
	if static(__config() != "debug")
	{{
	memcpy ::= func(dst : v0 mut& weak, src : v0& weak, count : u64 weak) -> v0
	{{
		__memcpy(dst, src, count);
	}};
	}}
	)psy"; 
#ifdef _WIN32
	constexpr bool windows = true;
	constexpr bool linux = false;
#else
	constexpr bool windows = false;
#ifdef __linux__
	constexpr bool linux = true;
#else
	constexpr bool linux = false;
#endif
#endif
	std::string cwd_path = std::filesystem::current_path().string();
	std::replace(cwd_path.begin(), cwd_path.end(), '\\', '/');
	std::string psyc_path = get_compiler_path().string();
	std::replace(psyc_path.begin(), psyc_path.end(), '\\', '/');
	return std::format(preload_src, windows ? "true" : "false", linux ? "true" : "false", psyc_path, cwd_path);

}
