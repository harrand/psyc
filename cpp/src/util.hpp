#ifndef PSYC_UTIL_HPP
#define PSYC_UTIL_HPP
#include <memory>
#include <filesystem>

namespace util
{
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

	template<typename ... Ts>                                                 // (7) 
	struct overload : Ts ... { 
		using Ts::operator() ...;
	};
	template<class... Ts> overload(Ts...) -> overload<Ts...>;

	std::filesystem::path get_this_executable_path();
}

#endif // PSYC_UTIL_HPP