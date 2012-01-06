#include <ldmconfig.h>
#ifdef NO_SETENV
/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <string.h>
#include <stdlib.h>
#include "setenv.h"

/*
 * _findenv --
 *      Returns pointer to value associated with name, if any, else NULL.
 *      Sets offset to be the offset of the name/value combination in the
 *      environmental array, for use by setenv(3) and unsetenv(3).
 *      Explicitly removes '=' in argument name.
 *
 *      This routine *should* be a static; don't use it.
 */
static char *
_findenv(const char *name, size_t *offset)
{
        extern char **environ;
        size_t len;
        const char *cC;
        char *C;
        char **P;

        for (cC = name, len = 0; *cC && *cC != '='; ) {
            ++cC;
            ++len;
        }
        for (P = environ; *P; ++P)
                if (!strncmp(*P, name, len))
                        if (*(C = *P + len) == '=') {
                                *offset = (size_t)(P - environ);
                                return(++C);
                        }
        return(NULL);
}


/*
 * setenv --
 *      Set the value of the environmental variable "name" to be
 *      "value".  If rewrite is set, replace any current value.
 */
int
setenv(const char *name, const char *value, int rewrite)
{
        extern char **environ;
        static int alloced;                     /* if allocated space before */
        char *C;
        size_t l_value, offset;

        if (*value == '=')                      /* no `=' in value */
                ++value;
        l_value = strlen(value);
        if ((C = _findenv(name, &offset))) {    /* find if already exists */
                if (!rewrite)
                        return (0);
                if (strlen(C) >= l_value) {     /* old larger; copy over */
                        while (*C++ = *value++)
                            ; /* empty */
                        return (0);
                }
        } else {                                        /* create new slot */
                size_t  cnt;
                char    **P;

                for (P = environ, cnt = 0; *P; ) {
                    ++P;
                    ++cnt;
                }
                if (alloced) {                  /* just increase size */
                        environ = (char **)realloc((char *)environ,
                            (size_t)(sizeof(char *) * (cnt + 2)));
                        if (!environ)
                                return (-1);
                }
                else {                          /* get new space */
                        alloced = 1;            /* copy old entries into it */
                        P = (char **)malloc((size_t)(sizeof(char *) *
                            (cnt + 2)));
                        if (!P)
                                return (-1);
                        (void)memcpy(P, environ,
                            (size_t)(cnt * sizeof(char *)));
                        environ = P;
                }
                environ[cnt + 1] = NULL;
                offset = cnt;
        }
        {
                const char *C;
                for (C = name; *C && *C != '='; )
                    ++C;        /* no `=' in name */
                if (!(environ[offset] =                 /* name + `=' + value */
                    malloc((size_t)(C - name) + l_value + 2))) /* no overflow */
                        return (-1);
        }
        for (C = environ[offset]; (*C = *name++) && *C != '='; ) /* "=" is OK */
            ++C;
                ;
        for (*C++ = '='; *C++ = *value++; )
                ; /* empty */
        return (0);
}
#else /* NO_SETENV *//* End of BSD copywrited inclusion */
void
_nada_setenv(void)
{
        /* Do nothing
         * except declare a function which does nothing
         * except shut up compilers and archivers
         * that complain about empty files.
         */
}
#endif /* NO_SETENV */
