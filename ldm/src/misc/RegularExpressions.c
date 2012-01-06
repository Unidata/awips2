/* $Id: RegularExpressions.c,v 1.1.2.1 2005/01/26 21:38:35 steve Exp $ */

#include "ldmconfig.h"

#include "RegularExpressions.h"

#include <string.h>


/*
 * Indicates if a regular-expression specification is pathological or not.
 * Pathological specifications are longer than two characters and have
 * a ".*" prefix.
 *
 * Arguments:
 *      spec    Pointer to 0-terminated regular expression specification.
 *
 * Returns:
 *      0       Specification is not pathological.
 *      1       Specification is pathological.
 */
int
re_isPathological(
    const char* const   spec)
{
    return strlen(spec) > 2 && (0 == strncmp(".*", spec, 2));
}


/*
 * Vets a regular-expression specification.  Converts pathological
 * specifications (ones with a ".*" prefix) to non-pathological ones.
 *
 * Arguments:
 *      spec    Pointer to 0-terminated regular expression specification.
 *              Must not be NULL.
 *
 * Returns:
 *      0       Specification was not adjusted.
 *      1       Specification was adjusted.
 */
int
re_vetSpec(
    char* const spec)
{
    int         wasConverted = 0;
    size_t      len = strlen(spec);

    while (len > 2 && (0 == strncmp(".*", spec, 2))) {
        (void)memmove(spec, spec+2, len-1);

        len -= 2;
        wasConverted = 1;
    }

    return wasConverted;
}
