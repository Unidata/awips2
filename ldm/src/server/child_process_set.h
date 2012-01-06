#ifndef CPS_H
#define CPS_H

#include <sys/types.h>  /* pid_t */

int       cps_new();
int       cps_add(pid_t);
void      cps_remove(pid_t);
int       cps_contains(pid_t);
unsigned  cps_count(void);

#endif
