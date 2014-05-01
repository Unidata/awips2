#ifndef DOWN_HELP_H
#define DOWN_HELP_H

#include "ldm.h"
#include "pq.h"

void
dh_setInfo(
    prod_info* const		newInfo,
    const prod_info* const	oldInfo,
    const char* const		hostId);

int
dh_saveDataProduct(
    struct pqueue*		pq,
    const prod_info* const	info,
    void* const			data,
    const int			wasHereis,
    const int			notifyAutoShift);

#endif
