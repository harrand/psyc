#ifndef PSYC_SRCLOC_HPP
#define PSYC_SRCLOC_HPP
#include <filesystem>
#include <string>
#include <format>
#include <limits>

struct srcloc
{
	std::filesystem::path file;
	unsigned int line;
	unsigned int column;

	inline std::string to_string() const
	{
		return std::format("{}({}:{})", this->file.filename().string(), this->line, this->column);
	}

	static srcloc undefined()
	{
		return
		{
			.file = {},
			.line = std::numeric_limits<decltype(line)>::max(),
			.column = std::numeric_limits<decltype(column)>::max()	
		};
	}
	bool operator==(const srcloc& rhs) const = default;
};

#endif // PSYC_SRCLOC_HPP