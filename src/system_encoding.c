#include "system_encoding.h"

char* get_system_encoding() {
#ifdef WIN32
  return "utf8";
#else
  setlocale(LC_ALL,"");
  return nl_langinfo(CODESET);
#endif
}
