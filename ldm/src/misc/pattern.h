#ifndef PATTERN_H
#define PATTERN_H

#include "error.h"

typedef struct Pattern Pattern;

ErrorObj*
pat_new(
    Pattern**		pat,
    const char*		re,
    const int		ignoreCase);

ErrorObj*
pat_clone(
    Pattern**		dst,
    const Pattern*	src);

int
pat_isMatch(
    const Pattern*	pat,
    const char*		string);

const char*
pat_getEre(
    const Pattern*	pat);

void
pat_free(
    Pattern*	pat);

#endif
