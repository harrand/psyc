#include "profile.hpp"
#ifdef PSYC_PROFILE
#include <cstddef>
void * operator new ( std :: size_t count )
{
	auto ptr = malloc ( count );
	TracyAlloc ( ptr , count );
	return ptr ;
}
void operator delete ( void * ptr ) noexcept
{
	TracyFree ( ptr );
	free ( ptr );
}

#endif