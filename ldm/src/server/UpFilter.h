#ifndef UP_FILTER_H
#define UP_FILTER_H

#include "pattern.h"

typedef struct UpFilter	UpFilter;

ErrorObj*
upFilter_new(
    UpFilter** const		upFilter);

ErrorObj*
upFilter_addComponent(
    UpFilter* const		upFilter,
    const feedtypet		feedtype,
    const Pattern* const	okPattern,
    const Pattern* const	notPattern);

int
upFilter_isMatch(
    const UpFilter* const	upFilter,
    const prod_info* const	info);


/*
 * Returns the number of components in an UpFilter.
 *
 * Arguments:
 *	upFilter	Pointer to the UpFilter.
 * Returns:
 *	The number of components in "upFilter".
 */
unsigned
upFilter_getComponentCount(
    const UpFilter* const	upFilter);


/*
 * Returns a string representation of an UpFilter.
 *
 * Arguments:
 *	upFilter	The UpFilter whose string representation is to be
 *			returned.
 * Returns:
 *	NULL		Failure to create a string representation of "upFilter".
 *	else		A pointer to a string representation of "upFilter".
 */
const char*
upFilter_toString(
    UpFilter* const	upFilter);

void
upFilter_free(
    UpFilter* const		upFilter);

#endif
