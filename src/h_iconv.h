#include <iconv.h>

iconv_t darcs_iconv_open(const char *tocode, const char *fromcode);

void darcs_iconv_close(iconv_t cd);

size_t darcs_iconv(iconv_t cd, char **inbuf, size_t *inbytesleft,
                char **outbuf, size_t *outbytesleft);

