#include <span>
#include <string_view>
#include <vector>

void parse_args(std::span<const std::string_view> args)
{

}

int main(int argc, char** argv)
{
	const std::vector<std::string_view> args(argv + 1, argv + argc);
	parse_args(args);
}
