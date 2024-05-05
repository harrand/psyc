#include "timer.hpp"
#include <chrono>

namespace timer
{
	std::chrono::time_point<std::chrono::system_clock> now;

	void start()
	{
		now = std::chrono::system_clock::now();
	}

	std::uint64_t elapsed_millis()
	{
		auto right_now = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
		return right_now - std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count();
	}
}