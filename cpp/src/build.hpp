#ifndef PSYC_BUILD_HPP
#define PSYC_BUILD_HPP
#include "session.hpp"

namespace build
{
	enum class linkage_type
	{
		none,
		executable,
		library
	};
	struct info
	{
		std::size_t optimisation_level = 0;
		linkage_type link = linkage_type::executable;
		std::string output_name = "a.exe";
	};

	info gather(session& ses);
	void go(const session& ses, const info& info);	
}

#endif // PSYC_BUILD_HPP