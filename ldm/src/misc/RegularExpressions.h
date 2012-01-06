#ifndef REGULAR_EXPRESSIONS_H
#define REGULAR_EXPRESSIONS_H


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
    const char* const   spec);


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
    char* const spec);

#endif
