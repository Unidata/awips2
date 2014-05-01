#include "ldmconfig.h"

#include <errno.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "pattern.h"
#include "RegularExpressions.h"


struct Pattern {
    char*       string;
    regex_t     reg;
    int         ignoreCase;
};

static Pattern* matchAll;


/*
 * Arguments:
 *      pat             Pointer to memory that will be set to point to an
 *                      allocated Pattern.  Set on and only on success.
 *      ere             The extended regular expression.  Caller may free upon
 *                      return.  If the expression is a pathological 
 *                      regular-expression, then it will be repaired.
 *      ignoreCase      Whether or not to ignore case in matches.
 *
 * Returns:
 *      NULL    Success.  *pat is set.
 *      else    Error object.  err_code() values:
 *                      1       System error.  *pat not set.
 *                      2       Invalid ERE.  *pat not set.
 */
ErrorObj*
pat_new(
    Pattern** const             pat,
    const char* const           ere,
    const int                   ignoreCase)
{
    ErrorObj*   errObj = NULL;  /* success */
    int         isTrivial = 0 == strcmp(".*", ere);

    if (isTrivial && NULL != matchAll) {
        *pat = matchAll;
    }
    else {
        Pattern* const  ptr = (Pattern*)malloc(sizeof(Pattern));

        if (NULL == ptr) {
            errObj = ERR_NEW2(1, NULL, "Couldn't allocate %lu bytes: %s",
                (unsigned long)sizeof(Pattern), strerror(errno));
        }
        else {
            ptr->string = strdup(ere);

            if (NULL == ptr->string) {
                errObj = ERR_NEW2(1, NULL, "Couldn't copy string \"%s\": %s",
                    ere, strerror(errno));
            }
            else {
                int     err;

                (void)re_vetSpec(ptr->string);

                if (err = regcomp(&ptr->reg, ptr->string,
                    REG_EXTENDED | REG_NOSUB | (ignoreCase ? REG_ICASE : 0))) {

                    char        buf[512];

                    (void)regerror(err, &ptr->reg, buf, sizeof(buf));

                    errObj = ERR_NEW2((REG_ESPACE == err) ? 1 : 2, NULL,
                        "Couldn't compile ERE \"%s\": %s", ptr->string, buf);
                }
                else {
                    ptr->ignoreCase = ignoreCase;
                    *pat = ptr;

                    if (isTrivial && NULL == matchAll)
                        matchAll = ptr;
                }                       /* ERE compiled */

                if (errObj)
                    free(ptr->string);
            }                           /* ptr->string allocated */

            if (errObj)
                free(ptr);
        }                               /* "ptr" allocated */
    }                                   /* non-trivial ERE or matchAll==NULL */

    return errObj;
}


/*
 * Clones a pattern.
 *
 * Arguments:
 *      dst             Pointer to pointer to be set to the clone.  Set on and
 *                      only on success.
 *      src             The pattern to be cloned.
 * Returns:
 *      NULL            Success.
 *      else            Error object.
 */
ErrorObj*
pat_clone(
    Pattern** const             dst,
    const Pattern* const        src)
{
    return pat_new(dst, src->string, src->ignoreCase);
}


/*
 * Arguments:
 *      pat     Pointer to Pattern set by pat_new().
 * Returns:
 *      0       String doesn't match pattern
 *      1       String matches pattern
 */
int
pat_isMatch(
    const Pattern* const        pat,
    const char* const           string)
{
    return
        matchAll == pat
            ? 1
            : 0 == regexec(&pat->reg, string, 0, NULL, 0);
}


/*
 * Arguments:
 *      pat     Pointer to Pattern set by pat_new().
 *
 * Returns:
 *      Pointer to the extended regular expression of this pattern.  If the
 *      initializing expression was pathological, then then returned string is
 *      the repaired form of the expression.
 */
const char*
pat_getEre(
    const Pattern* const        pat)
{
    return 
        matchAll == pat
            ? ".*"
            : pat->string;
}


/*
 * Releases the resources of a Pattern.
 *
 * Arguments:
 *      pat     Pointer to Pattern.  May be NULL.
 */
void
pat_free(
    Pattern* const      pat)
{
    if (pat && matchAll != pat) {
        Pattern*        p = (Pattern*)pat;

        regfree(&p->reg);
        free(p->string);
        free(p);
    }
}
