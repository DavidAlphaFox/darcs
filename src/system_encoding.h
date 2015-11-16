#ifndef __SYSTEM_ENCODING__
#define __SYSTEM_ENCODING__

#ifndef WIN32
#include <langinfo.h>
#include <locale.h>
#endif

char* get_system_encoding();

#endif
