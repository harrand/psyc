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
#include <array>
#include <charconv>
#include <variant>
#include <unordered_map>
#include <ranges>
#include <functional>
#include <unordered_set>
#include <chrono>
#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#define STRINGIFY(...) #__VA_ARGS__

std::string get_preload_source();

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

#define COMPILER_STAGE
#define error(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, true, std::make_format_args(__VA_ARGS__))
#define warning(loc, msg, ...) generic_warning(err::COMPILER_STAGE, msg, loc, std::make_format_args(__VA_ARGS__))
#define error_nonblocking(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, false, std::make_format_args(__VA_ARGS__))
#define error_ifnt(cond, loc, msg, ...) if(!(cond)){error(loc, msg, __VA_ARGS__);}

//////////////////////////// ARGPARSE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE argparse

struct compile_args
{
	bool should_print_help = false;
	bool verbose_lex = false;
	bool verbose_ast = false;
	bool verbose_parse = false;
	std::filesystem::path build_file = {};
};

compile_args parse_args(std::span<const std::string_view> args)
{
	compile_args ret;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const auto& arg = args[i];
		auto argnext = [allowed = i < args.size() - 1, i, &args](){if(!allowed){error({}, "argument missing value");} return args[i + 1];};

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
		else if(arg == "--verbose-all")
		{
			ret.verbose_lex = true;
			ret.verbose_ast = true;
			ret.verbose_parse = true;
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
			this->p == type::s8  ||
			this->p == type::u64 ||
			this->p == type::u32 ||
			this->p == type::u16 ||
			this->p == type::u8  ||
			
			this->p == type::f64 ||
			this->p == type::f32;
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
	std::unordered_map<std::string, box<type_t>> members = {};
	std::string name() const
	{
		return "struct";
	}
	bool operator==(const struct_ty& rhs) const = default;
};

struct enum_ty
{
	box<type_t> underlying_ty;
	std::vector<std::string> entries = {};
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
	bool operator==(const ptr_ty& rhs) const = default;
};

struct fn_ty
{
	std::vector<type_t> static_params;
	std::vector<type_t> params;
	box<type_t> return_ty;
	std::string name() const;
	bool operator==(const fn_ty& rhs) const = default;
};

struct meta_ty
{
	box<type_t> concrete;
	bool operator==(const meta_ty& rhs) const = default;

	std::string name() const;
};

#define meta_type "type"

struct type_t
{
	using payload_t = std::variant
	<
		std::monostate,
		prim_ty,
		struct_ty,
		enum_ty,
		ptr_ty,
		fn_ty,
		meta_ty
	>;
	payload_t payload;
	typequal qual = typequal_none;

	static type_t create_void_type()
	{
		return type_t{.payload = prim_ty{.p = prim_ty::type::v0}};
	}

	static type_t create_meta_type(type_t ty = type_t::badtype())
	{
		return type_t{.payload = meta_ty{.concrete = ty}};
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
					return false;
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
			}
		}
		else if(lhs_is(meta_ty))
		{
			if(rhs_is(meta_ty))
			{
				const auto& lhs_ty = std::get<meta_ty>(this->payload);
				const auto& rhs_ty = std::get<meta_ty>(rhs.payload);

				// if either of them have badtype as concrete, then it is a template type and the conversion is okay.
				if(lhs_ty.concrete->is_badtype() || rhs_ty.concrete->is_badtype())
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

				// pointers always convert to one-another if they are the same or one of them is weak.
				// many of these conversions will be unsafe unless care is taken, like C.
				return lhs_ptr == rhs_ptr || either_is_weak;
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
		// different type of types.
		// if we're not on the second attempt, try the other way around.
		if(!second_attempt)
		{
			return rhs.is_convertible_to(*this, true);
		}
		panic("dont know if {} can be converted to {}", lhs_name, rhs_name);
		return false;
	}

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

	bool is_fn() const
	{
		return this->payload.index() == payload_index<fn_ty, decltype(this->payload)>();
	}

	bool operator==(const type_t& rhs) const = default;
};

std::string ptr_ty::name() const
{
	return std::format("{}&", this->underlying_ty->name());
}

std::string fn_ty::name() const
{
	std::string sparams_str = "";
	if(this->static_params.size())
	{
		sparams_str = "<";
		for(std::size_t i = 0; i < this->static_params.size(); i++)
		{
			const auto& param_ty = this->static_params[i];
			sparams_str += param_ty.name();
			if(i < (this->static_params.size() - 1))
			{
				sparams_str += ", ";
			}
		}
		sparams_str += ">";
	}
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
	return std::format("func{}({}) -> {}", sparams_str, params_str, this->return_ty->name());
}

std::string meta_ty::name() const
{
	if(this->concrete->is_badtype())
	{
		return meta_type;
	}
	return std::format(meta_type" (aka {})", this->concrete->name());
}

struct ast_funcdef_expr;
using literal_val = std::variant<std::int64_t, double, char, std::string, bool>;

struct sval
{
	std::variant<std::monostate, literal_val, std::unordered_map<std::string, sval>> val = std::monostate{};
	type_t ty;
};

struct semal_state
{
	std::unordered_map<std::string, prim_ty> primitives = {};
	std::unordered_map<std::string, struct_ty> structs = {};
	std::unordered_map<std::string, enum_ty> enums = {};
	std::unordered_map<std::string, fn_ty> functions = {};
	std::unordered_map<std::string, const ast_funcdef_expr*> function_locations = {};
	std::unordered_map<std::string, sval> variables = {};

	std::unordered_map<std::filesystem::path, srcloc> added_source_files = {};

	ptr_ty create_pointer_ty(type_t pointee) const
	{
		return {.underlying_ty = {pointee}};
	}

	type_t parse(std::string_view type_name) const
	{
		if (type_name == meta_type)
		{
			return type_t::create_meta_type();
		}

		// typenames can get very complicated so this isnt trivial at all.

		// Initialize the type to be parsed
		type_t current_type = type_t::badtype();
		
		std::string_view tyname = type_name;
		while(!tyname.empty())
		{
			// skip whitespace
			if(std::isspace(tyname.front()))
			{
				tyname.remove_prefix(1);
				continue;
			}
			if(tyname.front() == '&')
			{
				error_ifnt(!current_type.is_badtype(), {}, "type {} is malformed? saw pointer symbol before i found the base type", type_name);
				current_type = type_t{.payload = this->create_pointer_ty(current_type)};
				tyname.remove_prefix(1);
				continue;
			}
			std::size_t till_next_thing = 0;
			for(std::size_t i = 0; i < tyname.size(); i++)
			{
				if(!std::isalnum(tyname[i]))
				{
					break;
				}
				till_next_thing++;
			}
			std::string_view word = (till_next_thing == tyname.size()) ? tyname : tyname.substr(0, till_next_thing);
			if(word == "mut")
			{
				current_type.qual = current_type.qual | typequal_mut;
			}
			else if(word == "static")
			{
				current_type.qual = current_type.qual | typequal_static;
			}
			else if(word == "weak")
			{
				current_type.qual = current_type.qual | typequal_weak;
			}
			else
			{
				// im gonna assume this is the base type now then.
				for (const auto& [name, prim] : this->primitives)
				{
					if (name == word)
					{
						current_type.payload = prim;
						break;
					}
				}

				// Or with structs
				for (const auto& [name, structval] : this->structs)
				{
					if (name == word)
					{
						current_type.payload = structval;
						break;
					}
				}

				// Or with enums
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

	semal_state coalesce(const semal_state& other) const
	{
		semal_state ret = *this;
		for(const auto& [name, prim] : other.primitives)
		{
			ret.primitives[name] = prim;
		}

		for(const auto& [name, structval] : other.structs)
		{
			ret.structs[name] = structval;
		}

		for(const auto& [name, enumval] : other.enums)
		{
			ret.enums.emplace(name, enumval);
		}

		for(const auto& [name, fn] : other.functions)
		{
			ret.functions.emplace(name, fn);
		}
		for(const auto& [name, node] : other.function_locations)
		{
			ret.function_locations[name] = node;
		}
		for(const auto& [name, ty] : other.variables)
		{
			ret.variables[name] = ty;
		}
		return ret;
	}

	void feed_forward(semal_state& other)
	{
		// specifically bring back enums. only enums.
		// enums are given members in their own block which needs to be fed-back to its parent
		// structs probably need to do this too.
		for(const auto& [name, structval] : this->structs)
		{
			if(!other.structs.contains(name))
			{
				other.structs.emplace(name, structval);
			}
			else
			{
				other.structs.at(name) = structval;
			}
		}

		/*
		for(const auto& [name, funcval] : this->functions)
		{
			other.functions.emplace(name, funcval);
			other.function_locations.emplace(name, this->function_locations.at(name));
		}
		*/

		for(const auto& [file, loc] : this->added_source_files)
		{
			other.added_source_files.emplace(file, loc);
		}

		for(const auto& [name, enumval] : this->enums)
		{
			if(!other.enums.contains(name))
			{
				other.enums.emplace(name, enumval);
			}
			else
			{
				other.enums.at(name) = enumval;
			}
		}
	}
};

semal_state create_basic_type_system()
{
	semal_state ret;

	for(int i = 0; i < static_cast<int>(prim_ty::type::_count); i++)
	{
		auto primty = static_cast<prim_ty::type>(i);
		const char* name = prim_ty::type_names[i];
		ret.primitives[name] = {.p = primty};
	}
	return ret;
}

semal_state create_build_metaregion_context()
{
	semal_state ret;
	ret.functions.emplace("add_source_file", fn_ty
			{
				.params =
				{
					type_t{.payload =ret.create_pointer_ty(type_t
					{
						.payload = prim_ty{.p = prim_ty::type::u8}
					})}
				},
				.return_ty = type_t::create_void_type()
			});
	ret.function_locations.emplace("add_source_file", nullptr);
	return ret;
}

semal_state create_empty_type_system()
{
	return {};
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
	oanglebrack,
	canglebrack,
	keyword_static_if,
	keyword_if,
	keyword_while,
	keyword_for,
	keyword_return,
	keyword_func,
	keyword_extern,
	keyword_struct,
	keyword_enum,
	keyword_ref,
	keyword_deref,
	keyword_defer,
	keyword_true,
	keyword_false,
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
			std::size_t string_length = state.advance_until([](std::string_view next){return next.starts_with("\"");});
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
		.trivial = true
	},

	tokeniser
	{
		.name = "if keyword",
		.front_identifier = "if",
		.trivial = true
	},

	tokeniser
	{
		.name = "while keyword",
		.front_identifier = "while",
		.trivial = true
	},

	tokeniser
	{
		.name = "for keyword",
		.front_identifier = "for",
		.trivial = true
	},

	tokeniser
	{
		.name = "return keyword",
		.front_identifier = "return",
		.trivial = true
	},

	tokeniser
	{
		.name = "func keyword",
		.front_identifier = "func",
		.trivial = true
	},

	tokeniser
	{
		.name = "extern keyword",
		.front_identifier = "extern",
		.trivial = true
	},

	tokeniser
	{
		.name = "struct keyword",
		.front_identifier = "struct",
		.trivial = true
	},

	tokeniser
	{
		.name = "enum keyword",
		.front_identifier = "enum",
		.trivial = true
	},

	tokeniser
	{
		.name = "ref keyword",
		.front_identifier = "ref",
		.trivial = true
	},

	tokeniser
	{
		.name = "deref keyword",
		.front_identifier = "deref",
		.trivial = true
	},

	tokeniser
	{
		.name = "defer keyword",
		.front_identifier = "defer",
		.trivial = true
	},

	tokeniser
	{
		.name = "true keyword",
		.front_identifier = "true",
		.trivial = true
	},

	tokeniser
	{
		.name = "false keyword",
		.front_identifier = "false",
		.trivial = true
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
					return !(std::isalnum(next.front()) || next.front() == '_' || next.front() == '&'
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
			std::size_t cursor_before = state.cursor;
			state.advance(trait.front_identifier);
			out.tokens.push_back(tok);
			out.lexemes.push_back({.offset = cursor_before, .length = std::strlen(trait.front_identifier)});
			return true;
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

struct ast_translation_unit{std::string value_tostring(){return "";}};

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

struct ast_decl;

struct ast_funcdef_expr
{
	std::vector<ast_decl> static_params = {};
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
	std::vector<ast_expr> static_params = {};
	std::vector<ast_expr> params = {};
	bool on_static_params = false;
	bool awaiting_next_param = false;

	std::string value_tostring() const
	{
		return "partial call";
	}
};

struct ast_callfunc_expr
{
	std::string function_name;
	std::vector<ast_expr> static_params = {};
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
	cast,
	field,
	_count
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
			"field"
		}[static_cast<int>(this->type)]);
	}
};

enum class unop_type
{
	minus,
	invert,
	ref,
	deref,
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
			"deref"
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
		ast_funcdef_expr,
		ast_callfunc_expr,
		ast_symbol_expr,
		ast_structdef_expr,
		ast_enumdef_expr,
		ast_biop_expr,
		ast_unop_expr,
		ast_blkinit_expr
	> expr_;

	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(expr_)>>
		{
			"literal",
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
	return std::format("call<{}, {}>", static_params.size(), params.size());
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

struct ast_stmt;
struct ast_blk_stmt
{
	bool capped = false;

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

struct ast_stmt
{
	std::variant
	<
		ast_decl_stmt,
		ast_expr_stmt,
		ast_return_stmt,
		ast_blk_stmt,
		ast_metaregion_stmt,
		ast_designator_stmt,
		ast_if_stmt
	> stmt_;
	bool deferred = false;
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
			"if"
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
	defining_static_params,
	awaiting_next_static_param,
	defining_params,
	awaiting_next_param,
	awaiting_arrow,
	awaiting_return_type,
	awaiting_body
};

struct ast_partial_funcdef
{
	std::vector<ast_decl> static_params = {};
	std::vector<ast_decl> params = {};
	std::string return_type;
	partial_funcdef_stage stage = partial_funcdef_stage::defining_params;

	std::string value_tostring() const{return "";}
};

struct ast_funcdef
{
	ast_funcdef_expr func;

	std::string value_tostring() const
	{
		return func.value_tostring();
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
	ast_funcdef
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
	"function definition"
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
	mutable semal_state types = create_empty_type_system();

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
#define STATE(...) [](){return std::array{node{.payload = ast_translation_unit{}}, __VA_ARGS__};}(), "translation_unit, " STRINGIFY(__VA_ARGS__)
// the difference between STATE and LOOSE state is that STATE means your chord function will only target nodes/tokens right at the beginning of the parse state
// LOOKAHEAD_STATE means it could be offsetted deep in the parse state.
#define LOOKAHEAD_STATE(...) [](){return std::array{__VA_ARGS__};}(), STRINGIFY(__VA_ARGS__)
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
	state.nodes = {node{.payload = ast_translation_unit{}, .begin_location = tu_begin, .end_location = tu_end}};

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
			if(verbose_parse)
			{
				std::print("{}{}\n\t=> ", entry.description, entry.extra.extensible ? ", ..." : "");
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
					if(state.recursing)
					{
						begin_offset = rem.offset + state.recursive_offset;
					}
					const srcloc begin_loc = state.nodes[begin_offset].begin_location;
					std::size_t end_offset = rem.offset + rem.length;
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

void call_builtin_function(const ast_callfunc_expr& call, semal_state& state, srcloc loc)
{
	if(call.function_name == "add_source_file")
	{
		const ast_expr& path_expr = call.params.front();
		std::filesystem::path path{std::get<std::string>(std::get<ast_literal_expr>(path_expr.expr_).value)};
		if(!std::filesystem::exists(path))
		{
			// assume its a standard library source file.
			path = get_compiler_path().parent_path() / path;
		}
		state.added_source_files.emplace(path, loc);
	}
	else
	{
		panic("{}, detected call to a function i know about, but i cant find its implementation. its location is nullptr implying that its a builtin function, but i dont recognise \"{}\" as a valid builtin", loc, call.function_name);
	}
}


struct semal_context
{
	enum class type
	{
		in_function,
		in_struct,
		in_enum,
		in_metaregion,
		in_other_statement,
		undefined
	};
	struct entry
	{
		type t;
		std::string name;
	};
	std::vector<entry> entries = {};
	std::unordered_map<std::string, sval> variables_to_exist_in_next_scope = {};

	const entry* try_get_parent_function() const
	{
		auto iter = std::find_if(this->entries.rbegin(), this->entries.rend(),
				[](const entry& e)->bool
				{
					return e.t == semal_context::type::in_function;
				});
		if(iter != this->entries.rend())
		{
			return &*iter;
		}
		return nullptr;
	}
	const entry* try_get_parent_struct() const
	{
		auto iter = std::find_if(this->entries.rbegin(), this->entries.rend(),
				[](const entry& e)->bool
				{
					return e.t == semal_context::type::in_struct;
				});
		if(iter != this->entries.rend())
		{
			return &*iter;
		}
		return nullptr;
	}
	const entry* try_get_parent_enum() const
	{
		auto iter = std::find_if(this->entries.rbegin(), this->entries.rend(),
				[](const entry& e)->bool
				{
					return e.t == semal_context::type::in_enum;
				});
		if(iter != this->entries.rend())
		{
			return &*iter;
		}
		return nullptr;
	}
	const entry* try_get_parent_metaregion() const
	{
		auto iter = std::find_if(this->entries.rbegin(), this->entries.rend(),
				[](const entry& e)->bool
				{
					return e.t == semal_context::type::in_metaregion;
				});
		if(iter != this->entries.rend())
		{
			return &*iter;
		}
		return nullptr;
	}
};

std::optional<sval> decl_get_type(const ast_decl& decl, semal_state& types, srcloc loc, semal_context& ctx);

std::optional<sval> expr_get_type(const ast_expr& expr, semal_state& types, srcloc loc, semal_context& ctx)
{
	if(expr.expr_.index() == payload_index<ast_literal_expr, decltype(expr.expr_)>())
	{
		const auto& lit = std::get<ast_literal_expr>(expr.expr_);
		sval value
		{
			.val = lit.value,
		};

		value.ty.qual = typequal_static;
		if(lit.value.index() == payload_index<std::int64_t, decltype(lit.value)>())
		{
			value.ty.payload = prim_ty{.p = prim_ty::type::s64};
		}
		else if(lit.value.index() == payload_index<double, decltype(lit.value)>())
		{
			value.ty.payload = prim_ty{.p = prim_ty::type::f64};
		}
		else if(lit.value.index() == payload_index<char, decltype(lit.value)>())
		{
			value.ty.payload = prim_ty{.p = prim_ty::type::u8};
		}
		else if(lit.value.index() == payload_index<std::string, decltype(lit.value)>())
		{
			value.ty.qual = typequal_none;
			value.ty.payload = prim_ty{.p = prim_ty::type::u8};
			value.ty = type_t{.payload = types.create_pointer_ty(value.ty), .qual = typequal_static};
		}
		else if(lit.value.index() == payload_index<bool, decltype(lit.value)>())
		{
			value.ty.payload = prim_ty{.p = prim_ty::type::boolean};
		}
		else
		{
			panic("dont know how to typecheck this specific literal expression");
		}
		return value;
	}
	else if(expr.expr_.index() == payload_index<ast_funcdef_expr, decltype(expr.expr_)>())
	{
		const auto& def = std::get<ast_funcdef_expr>(expr.expr_);

		fn_ty ty{.return_ty = type_t::badtype()};
		ty.static_params.resize(def.static_params.size());
		std::transform(def.static_params.begin(), def.static_params.end(), ty.static_params.begin(),
		[&types, &loc, &def, &ctx](const ast_decl& decl)
		{
			auto sparam = decl_get_type(decl, types, loc, ctx);
			error_ifnt(sparam.has_value(), loc, "failed to semal static parameter {} of function", decl.name);

			error_ifnt(!sparam->ty.is_badtype(), loc, "unknown type of static parameter {} of function", decl.name);
			if(!def.is_extern)
			{
				ctx.variables_to_exist_in_next_scope[decl.name] = sparam.value();
			}
			return sparam->ty;
		});

		ty.params.resize(def.params.size());
		std::transform(def.params.begin(), def.params.end(), ty.params.begin(),
		[&types, &loc, &def, &ctx](const ast_decl& decl)
		{
			auto param = decl_get_type(decl, types, loc, ctx);
			error_ifnt(param.has_value(), loc, "failed to semal parameter {} of function", decl.name);

			error_ifnt(!param->ty.is_badtype(), loc, "unknown type of parameter {} of function", decl.name);
			if(!def.is_extern)
			{
				ctx.variables_to_exist_in_next_scope[decl.name] = param.value();
			}
			return param->ty;
		});

		ty.return_ty = types.parse(def.return_type);
		error_ifnt(!ty.return_ty->is_badtype(), loc, "unknown return type \"{}\" of function", def.return_type);
		return wrap_type(type_t{.payload = ty, .qual = typequal_static});
	}
	else if(expr.expr_.index() == payload_index<ast_callfunc_expr, decltype(expr.expr_)>())
	{
		const auto& call = std::get<ast_callfunc_expr>(expr.expr_);
		auto iter = types.functions.find(call.function_name);	
		if(iter != types.functions.end())
		{
			fn_ty fn = iter->second;
			// make sure the call matches the fn
			auto call_sparams = call.static_params.size();
			auto fn_sparams = fn.static_params.size();
			error_ifnt(call_sparams == fn_sparams, loc, "function {} called with {} static parameters when it expects {}", call.function_name, call_sparams, fn_sparams);
			for(std::size_t i = 0; i < call_sparams; i++)
			{
				//auto call_param_ty = expr_get_type(call.static_params[i], types, loc, ctx);
				auto call_param = expr_get_type(call.static_params[i], types, loc, ctx);
				error_ifnt(call_param.has_value(), loc, "failed to semal static argument {} in call to function {}", i, call.function_name);
				const std::string call_expr_tyname = call_param->ty.name();
				const std::string fn_param_tyname = fn.static_params[i].name();
				error_ifnt(call_param->ty.is_convertible_to(fn.static_params[i]), loc, "static param {} of function {} expects type \"{}\". your \"{}\" is not convertible.", i, call.function_name, fn_param_tyname, call_expr_tyname);
				// todo: generate new concrete function signatures based on these
				warning(loc, "static params are not yet implemented");
			}
			auto call_params = call.params.size();
			auto fn_params = fn.params.size();
			error_ifnt(call_params == fn_params, loc, "function {} called with {} arguments when it expects {}", call.function_name, call_params, fn_params);
			for(std::size_t i = 0; i < call_params; i++)
			{
				auto call_param = expr_get_type(call.params[i], types, loc, ctx);
				//auto call_param_ty = expr_get_type(call.params[i], types, loc, ctx);
				error_ifnt(call_param.has_value(), loc, "failed to semal argument {} in call to function {}", i, call.function_name);
				const std::string call_expr_tyname = call_param->ty.name();
				const std::string fn_param_tyname = fn.params[i].name();
				error_ifnt(call_param->ty.is_convertible_to(fn.params[i]), loc, "param {} of function {} expects type \"{}\". your \"{}\" is not convertible.", i, call.function_name, fn_param_tyname, call_expr_tyname);
			}

			auto* fnloc = types.function_locations.at(call.function_name);
			if(fnloc == nullptr)
			{
				call_builtin_function(call, types, loc);
			}
			return wrap_type(*fn.return_ty);
		}
		else
		{
			error(loc, "undefined function \"{}\"", call.function_name);
		}
	}
	else if(expr.expr_.index() == payload_index<ast_symbol_expr, decltype(expr.expr_)>())
	{
		const auto& symbol_expr = std::get<ast_symbol_expr>(expr.expr_);
		auto iter = types.variables.find(symbol_expr.symbol);
		if(iter != types.variables.end())
		{
			return iter->second;
		}
		else
		{
			auto func_iter = types.functions.find(symbol_expr.symbol);
			if(func_iter != types.functions.end())
			{
				// they are referring to a function name as a variable. that's perfectly fine.
				sval val = wrap_type(type_t{.payload = func_iter->second});
				// "value" is secretly a string literal even though the ty is a function
				val.val = symbol_expr.symbol;
				return val;
			}
			else
			{
				// perhaps they're specifically referring to a typename?
				type_t type_name = types.parse(symbol_expr.symbol);
				if(!type_name.is_badtype())
				{
					return wrap_type(type_t::create_meta_type(type_name));
				}
				else
				{
					error(loc, "undefined variable, type or function \"{}\"", symbol_expr.symbol);
				}
			}
		}
	}
	else if(expr.expr_.index() == payload_index<ast_structdef_expr, decltype(expr.expr_)>())
	{
		// we dont handle this here, as it's only valid to declare a structdef if its within a decl.
		return wrap_type(type_t::create_meta_type(type_t{.payload = struct_ty{}}));
	}
	else if(expr.expr_.index() == payload_index<ast_enumdef_expr, decltype(expr.expr_)>())
	{
		const auto& enumdef = std::get<ast_enumdef_expr>(expr.expr_);
		type_t underlying_ty = type_t{.payload = prim_ty{.p = prim_ty::type::s64}};
		if(!enumdef.underlying_type.empty())
		{
			underlying_ty = types.parse(enumdef.underlying_type);
		}
		// we dont handle this here, as it's only valid to declare a structdef if its within a decl.
		return wrap_type(type_t::create_meta_type(type_t{.payload = enum_ty{.underlying_ty = underlying_ty}}));
	}
	else if(expr.expr_.index() == payload_index<ast_biop_expr, decltype(expr.expr_)>())
	{
		const auto& biop = std::get<ast_biop_expr>(expr.expr_);
		auto lhs = expr_get_type(*biop.lhs, types, loc, ctx);
		//auto lhs_ty = expr_get_type(*biop.lhs, types, loc, ctx);
		switch(biop.type)
		{
			// all operators aside from cast (@) act the same
			// the type of the expression is equal to the lhs
			// for the cast, rhs *must* be a symbol expression and represent a typename.
			case biop_type::plus:
				[[fallthrough]];
			case biop_type::minus:
				[[fallthrough]];
			case biop_type::mul:
				[[fallthrough]];
			case biop_type::div:
			{
				auto rhs = expr_get_type(*biop.rhs, types, loc, ctx);
				error_ifnt(rhs.has_value(), loc, "rhs of biop is invalid");
				error_ifnt(lhs.has_value(), loc, "lhs of biop is invalid");
				error_ifnt(lhs->ty.is_convertible_to(rhs->ty), loc, "binary operator is invalid because the left and right expression types are not convertible.");
				double rhs_val = 0.0;
				if(rhs->ty.is_prim() && rhs->val.index() != payload_index<std::monostate, decltype(rhs->val)>())
				{
					// rhs has a static value.
					// we assume its numeric.
					// double will store double and int64 so lets stick with double.
					if(payload_index<literal_val, decltype(rhs->val)>())
					{
						auto& rhs_svalue = std::get<literal_val>(rhs->val);
						if(rhs_svalue.index() == payload_index<double, decltype(rhs_svalue)>())
						{
							rhs_val += std::get<double>(rhs_svalue);
						}
						else if(rhs_svalue.index() == payload_index<std::int64_t, decltype(rhs_svalue)>())
						{
							rhs_val += std::get<std::int64_t>(rhs_svalue);
						}
						else
						{
							panic("did not expecting non-double non-int64 rhs literal value. did they pass a bool/char to a arithmetic biop {}?", loc);
						}
					}
					else
					{
						panic("really expected a literal value for a primitive rhs");
					}
				}
				if(lhs->ty.is_prim())
				{
					auto& static_value = std::get<literal_val>(lhs->val);
					if(static_value.index() == payload_index<std::int64_t, literal_val>())
					{
						std::get<std::int64_t>(static_value) += rhs_val;
					}
				}
				else
				{
					std::string lhs_tyname = lhs->ty.name();
					error(loc, "arithmetic binary operators are only valid on numeric types, you have tried to use it on a \"{}\" which is invalid", lhs_tyname);
				}
				return lhs;
			}
			break;
			case biop_type::cast:
			{
				error_ifnt(lhs.has_value(), loc, "lhs of biop is invalid");
				const auto& rhs_expr = *biop.rhs;
				if(rhs_expr.expr_.index() != payload_index<ast_symbol_expr, decltype(rhs_expr.expr_)>())
				{
					const char* expr_name = rhs_expr.type_name();
					error(loc, "rhs of cast expression *must* be a symbol representing a typename. instead you have provided a {} expression", expr_name);
				}
				else
				{
					const auto& symbol_expr = std::get<ast_symbol_expr>(rhs_expr.expr_);
					type_t casted_to_ty = types.parse(symbol_expr.symbol);
					std::string casted_from_tyname = lhs->ty.name();
					std::string casted_to_tyname = casted_to_ty.name();
					error_ifnt(lhs->ty.add_weak().is_convertible_to(casted_to_ty), loc, "cannot explicitly convert {} to {}", casted_from_tyname, casted_to_tyname);
					// let's try to do the cast if the static value exists.
					sval ret = wrap_type(casted_to_ty);
					ret.val = lhs->val;
					if(ret.val.index() == payload_index<literal_val, decltype(ret.val)>())
					{
						const auto& lit = std::get<literal_val>(ret.val);
						// todo: magic static value conversion logic.
					}
					return ret;
				}
			}
			break;
			case biop_type::field:
			{
				error_ifnt(lhs_ty.has_value() && !lhs_ty.value().is_badtype(), loc, "lhs of cast expression did not yield a valid type");
				if(lhs_ty->is_type())
				{
					const auto& meta = std::get<meta_ty>(lhs_ty->payload);
					type_t concrete = *meta.concrete;
					lhs_ty = concrete;
				}
				std::string rhs_symbol;
				if(biop.rhs->expr_.index() == payload_index<ast_symbol_expr, decltype(biop.rhs->expr_)>())
				{
					rhs_symbol = std::get<ast_symbol_expr>(biop.rhs->expr_).symbol;
				}
				else
				{
					panic("dont know how to handle biop field expression where rhs is not a symbol expr.");
				}
				if(lhs_ty->is_struct())
				{
					const auto& ty = std::get<struct_ty>(lhs_ty->payload);
					// which member is it?
					auto iter = ty.members.find(rhs_symbol);
					if(iter == ty.members.end())
					{
						error(loc, "variable \"{}\" of struct type does not have a member named \"{}\"", rhs_symbol);
					}
					const type_t& member_ty = *iter->second;
					return member_ty;
				}
				else if(lhs_ty->is_enum())
				{
					const auto& ty = std::get<enum_ty>(lhs_ty->payload);
					if(!std::ranges::contains(ty.entries, rhs_symbol))
					{
						error(loc, "enum has no entry \"{}\"", rhs_symbol);
					}
					return type_t{.payload = ty};
				}
			}
			break;
			default:
				panic("unhandled biop type in semal");
			break;
		}
	}
	else if(expr.expr_.index() == payload_index<ast_unop_expr, decltype(expr.expr_)>())
	{
		const auto& unop = std::get<ast_unop_expr>(expr.expr_);
		auto rhs_ty = expr_get_type(*unop.rhs, types, loc, ctx);
		std::string op_name = unop.value_tostring();
		error_ifnt(rhs_ty.has_value() && !rhs_ty.value().is_badtype(), loc, "rhs of unary operator {} yielded an invalid type", op_name);
		std::string tyname = rhs_ty->name();
		switch(unop.type)
		{
			// all operators aside from cast (@) act the same
			// the type of the expression is equal to the lhs
			// for the cast, rhs *must* be a symbol expression and represent a typename.
			case unop_type::minus:
			[[fallthrough]];
			case unop_type::invert:
				return rhs_ty;
			break;
			case unop_type::ref:
			{
				// in theory you could ref literally anything
				// however if you ref something like a literal then you are almost 100% wrong
				// let's error out on it.
				if(unop.rhs->expr_.index() == payload_index<ast_literal_expr, decltype(unop.rhs->expr_)>()
				|| unop.rhs->expr_.index() == payload_index<ast_funcdef_expr, decltype(unop.rhs->expr_)>()
				)
				{
					const char* expr_name = unop.rhs->type_name();
					error(loc, "attempt to ref a {} expression, which is incorrect.", expr_name);
				}
				return type_t{.payload = types.create_pointer_ty(rhs_ty.value())};
			}
			break;
			case unop_type::deref:
				error_ifnt(rhs_ty->is_ptr(), loc, "cannot deref non-pointer-type \"{}\"", tyname);
				return *std::get<ptr_ty>(rhs_ty->payload).underlying_ty;
			break;
			default:
				panic("unhandled unop type in semal");
			break;
		}
	}
	else if(expr.expr_.index() == payload_index<ast_blkinit_expr, decltype(expr.expr_)>())
	{
		const auto& blkinit = std::get<ast_blkinit_expr>(expr.expr_);
		type_t ret = types.parse(blkinit.type_name);
		if(!ret.is_struct())
		{
			error(loc, "block initialiser was of non-struct-type \"{}\". block initialisers can only be used to initialise structs for now.", blkinit.type_name);
		}
		auto ty = std::get<struct_ty>(ret.payload);
		for(const auto& init : blkinit.initialisers)
		{
			const ast_expr& value = *init.initialiser;

			auto iter = ty.members.find(init.name);
			if(iter == ty.members.end())
			{
				error(loc, "struct \"{}\" has no member named \"{}\"", blkinit.type_name, init.name);
			}
			auto init_expr_ty = expr_get_type(value, types, loc, ctx);
			error_ifnt(init_expr_ty.has_value() && !init_expr_ty->is_badtype(), loc, "initialiser of member \"{]\" was of invalid type", init.name);
			const bool convertible = iter->second->is_convertible_to(init_expr_ty.value());
			std::string member_typename = iter->second->name();
			std::string expr_typename = init_expr_ty->name();
			error_ifnt(convertible, loc, "member \"{}\" is of type \"{}\". the initialiser value you have passed is of type \"{}\" which is not convertible to the aforementioned member type.", init.name, member_typename, expr_typename);
		}
		return ret;
	}
	else
	{
		const char* expr_name = expr.type_name();
		panic("dont know how to typecheck a {} expression", expr_name);
	}
	return std::nullopt;
}

std::optional<type_t> decl_get_type(const ast_decl& decl, semal_state& types, srcloc loc, semal_context& ctx)
{
	std::optional<type_t> ty = std::nullopt;
	if(decl.type_name == deduced_type)
	{
		error_ifnt(decl.initialiser.has_value(), {}, "decl {} with deduced type must have an initialiser", decl.name);
		ty = expr_get_type(decl.initialiser.value(), types, loc, ctx);
	}
	else
	{
		ty = types.parse(decl.type_name);
		if(ty.has_value() && ty.value().is_type())
		{
			// user has specifically typed "type" as the type of a variable
			// that has no concrete type information, so we're still going to take the deduction code path
			// if there is no initialiser, then its basically a c++ template.
			if(decl.initialiser.has_value())
			{
				ty = expr_get_type(decl.initialiser.value(), types, loc, ctx);
			}
		}
		if(!ty.has_value() || ty.value().is_badtype())
		{
			// could be referring to a variable (so long as that variable is a meta type)
			auto iter = types.variables.find(decl.type_name);
			if(iter != types.variables.end())
			{
				sval val = iter->second;
				ty = val.ty;
			}
		}
	}

	const bool expr_is_funcdef = decl.initialiser.has_value() && decl.initialiser.value().expr_.index() == payload_index<ast_funcdef_expr, decltype(decl.initialiser.value().expr_)>();
	const bool expr_is_structdef = decl.initialiser.has_value() && decl.initialiser.value().expr_.index() == payload_index<ast_structdef_expr, decltype(decl.initialiser.value().expr_)>();
	const bool expr_is_enumdef = decl.initialiser.has_value() && decl.initialiser.value().expr_.index() == payload_index<ast_enumdef_expr, decltype(decl.initialiser.value().expr_)>();
	if(ty.has_value())
	{
		if(ty.value().is_fn() && expr_is_funcdef)
		{
			// declaration initialiser is a function type.
			auto [_, actually_emplaced] = types.functions.emplace(decl.name, std::get<fn_ty>(ty.value().payload));
			if(!actually_emplaced)
			{
				error(loc, "duplicate definition of function \"{}\"", decl.name);
			}
			const auto& def = std::get<ast_funcdef_expr>(decl.initialiser.value().expr_);
			types.function_locations[decl.name] = &def;
			if(!def.is_extern)
			{
				ctx.entries.push_back({.t = semal_context::type::in_function, .name = decl.name});
			}
		}
		else if(ty.value().is_type() && expr_is_structdef)
		{
			auto& structdef = std::get<struct_ty>(std::get<meta_ty>(ty.value().payload).concrete->payload);
			// we expect this to be an empty struct because we dont have access to its members until we see its children.
			auto [_, actually_emplaced] = types.structs.emplace(decl.name, structdef);
			if(!actually_emplaced)
			{
				error(loc, "duplicate definition of struct \"{}\"", decl.name);
			}
			ctx.entries.push_back({.t = semal_context::type::in_struct, .name = decl.name});
		}
		else if(ty.value().is_type() && expr_is_enumdef)
		{
			auto& enumdef = std::get<enum_ty>(std::get<meta_ty>(ty.value().payload).concrete->payload);
			auto [_, actually_emplaced] = types.enums.emplace(decl.name, enumdef);
			if(!actually_emplaced)
			{
				error(loc, "duplicate definition of enum \"{}\"", decl.name);
			}
			ctx.entries.push_back({.t = semal_context::type::in_enum, .name = decl.name});
		}
	}
	return ty;
}

std::optional<type_t> stmt_get_type(const ast_stmt& stmt, semal_state& types, srcloc loc, semal_context& ctx)
{
	if(stmt.stmt_.index() == payload_index<ast_decl_stmt, decltype(stmt.stmt_)>())
	{
		auto& decl = std::get<ast_decl_stmt>(stmt.stmt_).decl;
		auto ty = decl_get_type(decl, types, loc, ctx);
		error_ifnt(ty.has_value(), loc, "decl {} does not yield a type", decl.name);
		error_ifnt(!ty.value().is_badtype(), loc, "decl {} yielded an invalid type", decl.name);

		auto* maybe_parent_struct = ctx.try_get_parent_struct();
		auto* maybe_parent_fn = ctx.try_get_parent_function();
		if(maybe_parent_struct != nullptr && maybe_parent_fn == nullptr)
		{
			// we're in a struct and not a function - we are a data member.
			auto& structdef = types.structs.at(maybe_parent_struct->name);
			structdef.members.emplace(decl.name, ty.value());
		}
		else
		{
			// we're not in a struct so this isnt a data member, just a variable.
			// but if we're a function or struct type then we aren't a variable (dont worry, we've been registered elsewhere).
			if(!ty.value().is_fn() && !ty.value().is_type())
			{
				sval replace_me_with_above_scope = wrap_type(ty.value());
				auto [_, actually_emplaced] = types.variables.emplace(decl.name, replace_me_with_above_scope);
				if(!actually_emplaced)
				{
					error(loc, "duplicate definition of variable \"{}\"", decl.name);
				}
			}
		}
		return ty.value();
	}
	else if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
	{
		const auto* maybe_parent_metaregion = ctx.try_get_parent_metaregion();
		if(maybe_parent_metaregion != nullptr && maybe_parent_metaregion->name == "build")
		{
			// this block constitutes a build metaregion
			//error(loc, "block within build metaregion detected");
			types = types.coalesce(create_build_metaregion_context());
		}
		else if(maybe_parent_metaregion != nullptr)
		{
			error(loc, "unknown metaregion name \"{}\". did you mean \"build\"", maybe_parent_metaregion->name);
		}
		for(const auto& [name , ty] : ctx.variables_to_exist_in_next_scope)
		{
			types.variables.emplace(name, ty);
		}
		ctx.variables_to_exist_in_next_scope.clear();
	}
	else if(stmt.stmt_.index() == payload_index<ast_expr_stmt, decltype(stmt.stmt_)>())
	{
		auto& expr = std::get<ast_expr_stmt>(stmt.stmt_);
		auto ty = expr_get_type(expr.expr, types, loc, ctx);
		error_ifnt(ty.has_value(), loc, "expr does not yield a type");
		error_ifnt(!ty.value().is_badtype(), loc, "expr yielded an invalid type");
		return ty;
	}
	else if(stmt.stmt_.index() == payload_index<ast_return_stmt, decltype(stmt.stmt_)>())
	{
		const auto* parent_func = ctx.try_get_parent_function();
		error_ifnt(parent_func != nullptr, loc, "return statement is invalid because we are not in a function implementation.");
		const fn_ty& func = types.functions.at(parent_func->name);

		auto& ret = std::get<ast_return_stmt>(stmt.stmt_);
		if(ret.retval.has_value())
		{
			auto ty = expr_get_type(ret.retval.value(), types, loc, ctx);
			error_ifnt(ty.has_value(), loc, "expr does not yield a type");
			error_ifnt(!ty.value().is_badtype(), loc, "expr yielded an invalid type");

			std::string retval_tyname = ty.value().name();
			std::string fn_return_tyname = func.return_ty->name();
			error_ifnt(!func.return_ty->is_void(), loc, "return value is incorrect because the parent function \"{}\" returns v0", parent_func->name);
			error_ifnt(ty.value().is_convertible_to(*func.return_ty), loc, "return expression of type {} is not convertible to the parent function \"{}\"'s return type of {}", retval_tyname, parent_func->name, fn_return_tyname);
			return ty;
		}
		else
		{
			const auto return_tyname = func.return_ty->name();
			error_ifnt(func.return_ty->is_void(), loc, "detected empty return expression, which is only valid in a function that returns v0. the parent function (named \"{}\") returns a {}", parent_func->name, return_tyname);
			return type_t::create_void_type();
		}
	}
	else if(stmt.stmt_.index() == payload_index<ast_metaregion_stmt, decltype(stmt.stmt_)>())
	{
		const auto& metaregion = std::get<ast_metaregion_stmt>(stmt.stmt_);
		ctx.entries.push_back({.t = semal_context::type::in_metaregion, .name = metaregion.name});
		return type_t::create_void_type();
	}
	else if(stmt.stmt_.index() == payload_index<ast_designator_stmt, decltype(stmt.stmt_)>())
	{
		const auto* maybe_enum_parent = ctx.try_get_parent_enum();
		const auto& desig = std::get<ast_designator_stmt>(stmt.stmt_);

		if(maybe_enum_parent == nullptr)
		{
			error(loc, "unexpected designator. designators are currently only allowed within an enum definition, which is not where we are.");
		}

		auto& enumdef = types.enums.at(maybe_enum_parent->name);
		enumdef.entries.push_back(desig.name);
		type_t ret = *enumdef.underlying_ty;

		// make sure the initialiser of the designator actually matches the type.
		auto desig_init_ty = expr_get_type(*desig.initialiser, types, loc, ctx);
		error_ifnt(desig_init_ty.has_value() && !desig_init_ty.value().is_badtype(), loc, "initialiser expression of designator yielded an invalid type.");
		std::string desig_init_tyname = desig_init_ty->name();
		std::string enum_tyname = enumdef.name();
		std::string enum_underlying_tyname = ret.name();
		error_ifnt(desig_init_ty->is_convertible_to(ret), loc, "initialiser expression of designator is of type {}, which is not convertible to {} (underlying type: {})", desig_init_tyname, enum_tyname, enum_underlying_tyname);
		return ret;
	}
	else if(stmt.stmt_.index() == payload_index<ast_if_stmt, decltype(stmt.stmt_)>())
	{
		const auto& if_stmt = std::get<ast_if_stmt>(stmt.stmt_);
		auto cond_ty = expr_get_type(if_stmt.condition, types, loc, ctx);
		error_ifnt(cond_ty.has_value() && !cond_ty->is_badtype(), loc, "condition of if-statement is of invalid type");
		std::string cond_tyname = cond_ty->name();
		error_ifnt(cond_ty->is_convertible_to(type_t{.payload = prim_ty{.p = prim_ty::type::boolean}}), loc, "condition of if-statement is of type \"{}\", which is not convertible to \"bool\"", cond_tyname);
		if(if_stmt.is_static)
		{
			error_ifnt(cond_ty->qual & typequal_static, loc, "condition of a static-if-statement must be static, \"{}\" is not static", cond_tyname);
		}
		ctx.entries.push_back({.t = semal_context::type::in_other_statement});
	}
	else
	{
		const char* stmt_name = stmt.type_name();
		error(loc, "dont know how to typecheck a {} statement", stmt_name);
	}
	return std::nullopt;
}

void semal(node& ast, semal_state& types, semal_context& ctx)
{
	bool is_tu = false;
	bool pop_context = false;
	if(ast.payload.index() == payload_index<ast_translation_unit, node_payload>())
	{
		is_tu = true;
	}
	else if(ast.payload.index() == payload_index<ast_stmt, node_payload>())
	{
		auto& stmt = std::get<ast_stmt>(ast.payload);
		auto ty = stmt_get_type(stmt, types, ast.begin_location, ctx);
		std::println("{}", ty.value_or(type_t::badtype()).name());
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			// is a block statement, after all children we should pop context.
			pop_context = true;
		}
	}
	else
	{
		const char* node_name = node_names[ast.payload.index()];
		panic("dont know how to typecheck a {}", node_name);
	}
	ast.types = ast.types.coalesce(types);

	// check if any children are return statements
	auto maybe_return_stmt = std::find_if(ast.children.begin(), ast.children.end(),
			[](const node& child)->bool
			{
				if(child.payload.index() == payload_index<ast_stmt, node_payload>())
				{
					const auto& stmt = std::get<ast_stmt>(child.payload);
					return stmt.stmt_.index() == payload_index<ast_return_stmt, decltype(stmt.stmt_)>();
				}
				return false;
			});
	if(maybe_return_stmt != ast.children.end())
	{
		// if there is a return stmt, it better be the last child.
		error_ifnt(std::next(maybe_return_stmt) == ast.children.end(), maybe_return_stmt->begin_location, "return statement must be the last statement within a block. this one is not.");
	}
	// do an initial pass over the children
	// if any of it are statements that are deferred, send it to the end of the list of children.
	for(auto iter = ast.children.begin(); iter != ast.children.end(); iter++)
	{
		if(iter->payload.index() == payload_index<ast_stmt, node_payload>())
		{
			if(std::get<ast_stmt>(iter->payload).deferred)
			{
				// note: this will probably cause issues in the case that the last child is a return stmt. as the return stmt should always be the last stmt in a blk.
				std::rotate(iter, iter + 1, maybe_return_stmt);
			}
		}
	}
	// ok SORRY. actually semal the children now.
	for(auto& child : ast.children)
	{
		semal(child, ast.types, ctx);
		if(is_tu)
		{
			types = types.coalesce(ast.types);
		}
		else
		{
			ast.types.feed_forward(types);
		}
	}
	if(pop_context)
	{
		ctx.entries.pop_back();
	}
}

//////////////////////////// TYPE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE type

//////////////////////////// META ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE meta

//////////////////////////// CODEGEN -> LLVM-IR ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE codegen

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
	std::int64_t value = std::stol(std::string{std::get<ast_token>(nodes[0].payload).lexeme});
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = value}}}}
	};
}
FAKEFN(EXPRIFY_decimal_literal)
{
	double value = std::stod(std::string{std::get<ast_token>(nodes[0].payload).lexeme});
	return
	{
		.action = parse_action::reduce,
		.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
		.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = value}}}}
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
		LOOKAHEAD_STATE(TOKEN(x), TOKEN(cparen)), FN\
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
	CHORD_END\
	CHORD_BEGIN\
		LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(x), NODE(ast_expr)), FN\
		{\
			auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);\
			const auto& cast_to = std::get<ast_expr>(nodes[2].payload);\
			ast_expr* last_expr;\
			if(call.on_static_params)\
			{\
				if(call.static_params.empty())\
				{\
					std::string_view tok = std::get<ast_token>(nodes[1].payload).lexeme;\
					chord_error("unexpected token {} detected before any static params. move or remove this token.", tok);\
				}\
				last_expr = &call.static_params.back();\
			}\
			else\
			{\
				if(call.params.empty())\
				{\
					std::string_view tok = std::get<ast_token>(nodes[1].payload).lexeme;\
					chord_error("unexpected token {} detected before any params. move or remove this token.", tok);\
				}\
				last_expr = &call.params.back();\
			}\
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
	EXTENSIBLE\
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
			const auto& lhs_expr = std::get<ast_expr>(nodes[0].payload);\
			const auto& rhs_expr = std::get<ast_expr>(nodes[2].payload);\
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
			decl.initialiser.value() = ast_expr{.expr_ = ast_biop_expr\
			{\
				.lhs = decl.initialiser.value(),\
				.type = biop_type::biop_ty,\
				.rhs = std::get<ast_expr>(nodes[2].payload)\
			}};\
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
			if(value.tok == token::keyword_func || value.tok == token::symbol || value.tok == token::keyword_struct || value.tok == token::keyword_enum || unop_tokens.contains(value.tok))
			{
					return {.action = parse_action::recurse, .reduction_result_offset = 2};
			}
			decl.initialiser = ast_expr{.expr_ = ast_literal_expr{}};
			auto& literal = std::get<ast_literal_expr>(decl.initialiser->expr_);

			switch(value.tok)
			{
				case token::integer_literal:
					literal.value = std::stol(std::string{value.lexeme});
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
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(canglebrack)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::defining_static_params)
		{
			chord_error("unexpected '>' token while parsing function definition. you have already finished defining the static params.");
		}
		def.stage = partial_funcdef_stage::defining_params;
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
		else if(def.stage == partial_funcdef_stage::defining_static_params)
		{
			def.stage = partial_funcdef_stage::awaiting_next_static_param;
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
		else if(def.stage == partial_funcdef_stage::awaiting_next_static_param)
		{
			def.static_params.push_back(next_param);
			def.stage = partial_funcdef_stage::defining_static_params;
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
		if(def.stage == partial_funcdef_stage::awaiting_next_param || def.stage == partial_funcdef_stage::awaiting_next_static_param)
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
		ast_funcdef complete_funcdef
		{
			.func = 
			{
				.static_params = std::move(def.static_params),
				.params = std::move(def.params),
				.return_type = std::move(def.return_type),
				.is_extern = false
			}
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = complete_funcdef, .children = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}}}}
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

			ast_funcdef complete_funcdef
			{
				.func = 
				{
					.static_params = std::move(def.static_params),
					.params = std::move(def.params),
					.return_type = std::move(def.return_type),
					.is_extern = false
				}
			};
			defnode.payload = complete_funcdef;
			defnode.children = {nodes[1]};
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
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oanglebrack)), FN
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
			.static_params = {},
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
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oanglebrack), TOKEN(symbol)), FN
	{
		// it should be the start of a decl (the first param)
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), NODE(ast_decl)), FN
	{
		// we have the start of a function definition. finally.
		ast_partial_funcdef func
		{
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
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oanglebrack), NODE(ast_decl)), FN
	{
		// we have the start of a function definition. finally.
		ast_partial_funcdef func
		{
			.static_params = {std::get<ast_decl>(nodes[2].payload)},
			.params = {},
			.return_type = "???",
			.stage = partial_funcdef_stage::defining_static_params
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
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oanglebrack)), FN
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
			.static_params = {},
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
			.on_static_params = false
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
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oanglebrack), NODE(ast_expr)), FN
	{
		// this is a call with at least one static param
		ast_partial_callfunc call
		{
			.function_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
			.static_params = {std::get<ast_expr>(nodes[2].payload)},
			.on_static_params = true
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
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oanglebrack), WILDCARD), FN
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
		if(call.on_static_params)
		{
			call.static_params.push_back(std::get<ast_expr>(nodes[1].payload));
		}
		else
		{
			call.params.push_back(std::get<ast_expr>(nodes[1].payload));
		}
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
		call.on_static_params = false;
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
		if(call.on_static_params)
		{
			chord_error("syntax error while evaluating function call. did not expect a ) yet because i thought you were still providing static params. did you forget a >?");
		}
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got )");
		}
		ast_callfunc_expr complete_call
		{
			.function_name = call.function_name,
			.static_params = call.static_params,
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

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(canglebrack)), FN
	{
		auto& call_node = nodes[0];
		auto& call = std::get<ast_partial_callfunc>(call_node.payload);
		if(!call.on_static_params)
		{
			chord_error("syntax error while evaluating function call. did not expect a > yet because i didn't think you were providing static params at this point.");
		}
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got ,");
		}
		call.on_static_params = false;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
CHORD_END

DEFINE_UNOPIFICATION_CHORDS(dash, minus)
DEFINE_UNOPIFICATION_CHORDS(keyword_ref, ref)
DEFINE_UNOPIFICATION_CHORDS(keyword_deref, deref)

DEFINE_BIOPIFICATION_CHORDS(cast, cast)
DEFINE_BIOPIFICATION_CHORDS(plus, plus)
DEFINE_BIOPIFICATION_CHORDS(dash, minus)
DEFINE_BIOPIFICATION_CHORDS(asterisk, mul)
DEFINE_BIOPIFICATION_CHORDS(fslash, div)
DEFINE_BIOPIFICATION_CHORDS(dot, field)

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
DEFINE_EXPRIFICATION_CHORDS(string_literal)
DEFINE_EXPRIFICATION_CHORDS(symbol)
DEFINE_EXPRIFICATION_CHORDS(keyword_true)
DEFINE_EXPRIFICATION_CHORDS(keyword_false)

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

// uniform function call syntax
// foo.bar(1, 2)
// is equivalent too
// bar(foo, 1, 2)
CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(dot)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(dot), NODE(ast_expr)), FN
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
		if(expr.expr_.index() == payload_index<ast_callfunc_expr, decltype(expr.expr_)>())
		{
			// ufcs
			auto& call = std::get<ast_callfunc_expr>(expr.expr_);
			call.params.insert(call.params.begin(), ast_expr{.expr_ = ast_symbol_expr{.symbol = std::string{symbol}}});
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 0, .length = 2}
			};
		}
		else if(expr.expr_.index() == payload_index<ast_symbol_expr, decltype(expr.expr_)>())
		{
			std::string rhs = std::get<ast_symbol_expr>(expr.expr_).symbol;
			// ok rhs of the dot is not a call to a function
			// that means it must be a field expr
			// which requires a symbol expression.

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
		else
		{
			const char* expr_name = expr.type_name();
			chord_error("in a expr.expr reduction, did not expect rhs expr to be a {}, expected either a function call expression (UFCS), or a symbol expression (forming a field expression)", expr_name);
		}
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

// static if statements
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
				}}}}
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

// end of chords
}
//////////////////////////// BUILD SYSTEM ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE build_system

std::uint64_t time_setup = 0, time_lex = 0, time_parse = 0, time_semal = 0, time_codegen = 0;

void compile_file(std::filesystem::path file, const compile_args& args, semal_state* types = nullptr, bool include_preload = true);

void compile_source(std::filesystem::path file, std::string source, const compile_args& args, semal_state* types)
{
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

	semal_context ctx = {};
	node* build = try_find_build_metaregion(ast);
	if(build != nullptr)
	{
		auto temp_state = create_basic_type_system();
		semal(*build, temp_state, ctx);
		for(const auto& [newfile, loc] : temp_state.added_source_files)
		{
			error_ifnt(newfile != file, loc, "source file {} adds itself {}", file, loc);
			auto filename = newfile.filename();
			error_ifnt(std::filesystem::exists(newfile), {}, "could not find source file \"{}\" added {}", filename, loc);
			compile_file(newfile, args, types, false);
		}
	}
	semal(ast, *types, ctx);

	timer_restart();
	auto right_now = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
	time_semal += right_now - std::chrono::duration_cast<std::chrono::milliseconds>(now_cpy.time_since_epoch()).count();

	// todo: codegen

	time_codegen = elapsed_time();
	timer_restart();
}

void compile_file(std::filesystem::path file, const compile_args& args, semal_state* types, bool include_preload)
{
	semal_state new_types = create_basic_type_system();
	if(types == nullptr)
	{
		types = &new_types;
	}
	if(include_preload)
	{
		compile_source("preload.psy", get_preload_source(), args, types);
	}
	compile_source(file, read_file(file), args, types);
}

// entry point

int main(int argc, char** argv)
{
	timer_restart();
	populate_chords();

	std::vector<std::string_view> cli_args(argv + 1, argv + argc);
	compile_args args = parse_args(cli_args);
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

	compile_file(args.build_file, args);

	std::print("setup: {}\nlex:   {}\nparse: {}\nsemal: {}\ncodegen: {}\ntotal: {}", time_setup / 1000.0f, time_lex / 1000.0f, time_parse / 1000.0f, time_semal / 1000.0f, time_codegen / 1000.0f, (time_setup + time_lex + time_parse + time_semal + time_codegen) / 1000.0f);
}

std::string get_preload_source()
{

	// some psy source code that is *always* compiled before any file. its API is available to everything.
	// so uh try not to make it code that compiles slow as fuck thanks
	static constexpr char preload_src[] = R"psy(
	null ::= 0@u64@v0&;

	is_windows : bool static := {};
	is_linux : bool static := {};
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
	return std::format(preload_src, windows, linux);

}
