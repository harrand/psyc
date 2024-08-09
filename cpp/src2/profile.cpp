#include "profile.hpp"
#ifdef PSYC_PROFILE
#include <cstddef>
#include "mimalloc.h"
void * operator new ( std :: size_t count )
{
	auto ptr = mi_malloc( count );
	TracyAlloc ( ptr , count );
	return ptr ;
}
void operator delete ( void * ptr ) noexcept
{
	TracyFree ( ptr );
	mi_free ( ptr );
}

#else
#include "mimalloc-new-delete.h"
#endif