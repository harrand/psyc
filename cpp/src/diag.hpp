#include <iostream>
#include <utility>

namespace diag
{
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
}