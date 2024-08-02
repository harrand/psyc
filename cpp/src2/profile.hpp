#ifdef PSYC_PROFILE
#include "tracy/Tracy.hpp"
#include "tracy/TracyC.h"
#define CONCAT_PROF(a, b) CONCAT_INNER_PROF(a, b)
#define CONCAT_INNER_PROF(a, b) a ## b
#define MY_SPECIAL_STRINGIFY( x ) # x

#define PROFZONE(name) ZoneScopedN(name)
#define PROFNAME(name) ZoneName(name, 64)
#define PROFNAMEF(name, ...) ZoneNameF(name, __VA_ARGS__, 64)
#define PROFTEXT(text) ZoneText(text, 64)
#define PROFTEXTF(name, ...) ZoneTextF(name, __VA_ARGS__, 64)

#define PROFZONE_BEGIN(name) TracyCZoneN(CONCAT_PROF(ctx, name), MY_SPECIAL_STRINGIFY(name), true)
#define PROFZONE_END(name) TracyCZoneEnd(CONCAT_PROF(ctx, name))
#else
#define PROFZONE(name)
#define PROFNAME(name)
#define PROFNAMEF(name, ...)
#define PROFTEXT(text)
#define PROFTEXTF(name, ...)
#define PROFZONE_BEGIN(name)
#define PROFZONE_END(name)
#endif