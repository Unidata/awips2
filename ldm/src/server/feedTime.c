/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: feedTime.c,v 1.1.18.3 2008/04/15 16:34:13 steve Exp $ */

#include <ldmconfig.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "ldm.h"
#include "ldmprint.h"
#include "timestamp.h"
#include "ulog.h"

#include "feedTime.h"


#if 0
static char            _pathname[512];
static int             _initialized;
#endif


/*
 * Sets the product "from" time according to the last successful call to
 * ft_set_time(prod_class_t*) with the same argument.
 *
 * This function calls abort(3) if the environment variable LDMHOME is
 * unset.
 *
 * @param class               Product-class specification.  The "from" member
 *                            will be set if and only if this returns success.
 * @return 1                  if the product "from" time was successfuly set.
 * @return 0                  otherwise.
 */
/*ARGUSED*/
int
ft_get_time(prod_class_t* const class)
{
    int success = 0;                                    /* initial default */

#if 0
    /*
     * TODO: make this module work in the context of concurrent-access by
     * multiple processes to the same, persistent file.
     */

    if (!_initialized) {
        const char* const ldmhome = getenv("LDMHOME");

        assert(class != NULL);

        if (ldmhome == NULL) {
            uerror("Environment variable LDMHOME is unset");
            abort();
        }
        else {
            /*
             * Encode pathname of feed-time file based on LDMHOME and the
             * product-specification-array of the product-class specification.
             * The encoding is
             *
             *     <LDMHOME> "./{" <prod_spec> ["," ...] "}"
             *
             * Note that the pathname may contain embedded whitespace and
             * characters that are special to user-shells.
             */
            int len = strlen(ldmhome);
            int left = sizeof(_pathname);
            char* cp = _pathname;

            if (left > len) {
                (void)strcpy(cp, ldmhome);

                cp += len;
                left -= len;

                if (left > 3) {
                    (void)strcpy(cp, "/.{");

                    cp += 3;
                    left -= 3;

                    if (left > 0) {
                        int        i;
                        int        n = class->psa.psa_len;
                        prod_spec* ps = class->psa.psa_val;

                        for (i = 0; i < n; i++, ps++) {
                            if (i > 0) {
                                if (left <= 1)
                                    break;

                                (void)strcpy(cp, ",");

                                cp += 1;
                                left -= 1;
                            }

                            len = sprint_prod_spec(cp, left, ps);

                            if (len < 0)
                                break;

                            cp += len;
                            left -= len;
                        }

                        if (i == n) {
                            if (left > 1) {
                                (void)strcpy(cp, "}");

                                cp += 1;
                                left -= 1;
                                success = 1;            /* encoding success */
                            }
                        }
                    }
                }
            }

            if (!success) {
                uwarn("%s:%d: Couldn't encode pathname: \"%s\" \"%s\"",
                    __FILE__, __LINE__, ldmhome, s_prod_class(NULL, 0, class));
                uwarn("%s:%d: Feed-time cannot be saved");
            }
            else {
                /*
                 * Obtain feed-time from last ft_save() if it exists.
                 */
                FILE* file;

                success = 0;                            /* current default */
                *cp = '\0';                             /* terminate pathname */

                file = fopen(_pathname, "rb");

                if (file == NULL) {
                    uinfo("%s:%d: Feed-time file \"%s\" doesn't exist",
                        __FILE__, __LINE__, _pathname);
                }
                else {
                    timestampt time;

                    if (fread(&time, sizeof(time), 1, file) == 1) {
                        class->from = time;
                        success = 1;                    /* success */
                    }
                    else {
                        uwarn("%s:%d: Feed-time file \"%s\" is corrupt; "
                            "using TS_NONE", __FILE__, __LINE__, _pathname);
                    }

                    (void)fclose(file);
                }

                _initialized = 1;                       /* success */
            }
        }
    }
#endif

    return success;
}


/*
 * Sets the time of this module.
 *
 * @param time               The time for this module.
 */
/*ARGUSED*/
void
ft_set_time(const timestampt* const time)
{
#if 0
    if (_initialized) {
        FILE* file = fopen(_pathname, "wb");

        if (file == NULL) {
            serror("%s:%d: Couldn't open feed-time file \"%s\"",
                __FILE__, __LINE__, _pathname);
        }
        else {
            if (fwrite(time, sizeof(time), 1, file) != 1) {
                serror("%s:%d: Couldn't write feed-time file \"%s\"",
                    __FILE__, __LINE__, _pathname);
            }

            (void)fclose(file);
        }
    }
#endif
}
