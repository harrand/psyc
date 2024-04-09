#include <iostream>
#include <utility>

namespace diag
{
	template<typename... Ts>
	void print(Ts&&... ts)
	{
		((std::cout<<std::forward<Ts>(ts)<<""),...);
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
		std::cout << "\033[1;33m" << "warning: ";
		print(std::forward<Ts>(ts)...);
		std::cout << "\033[0m";
	}

	template<typename... Ts>
	void error(Ts&&... ts)
	{
		std::cout << "\033[0;31m" << "error: ";
		print(std::forward<Ts>(ts)...);
		std::cout << "\033[0m";
	}

	template<typename... Ts>
	void fatal_error(Ts&&... ts)
	{
		std::cout << "\033[1;31m" << "fatal error: ";
		print(std::forward<Ts>(ts)...);
		std::cout << "\033[0m";
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
}