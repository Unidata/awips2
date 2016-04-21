/*
 *  function findints - used as intermediary between the Fortran
 *                      call from ss_input.f and the find_in_ts
 *                      c program.
 */
#include <stdio.h>

void    findints(name, type, delta_t, ts_float, ts_char, status)

char    *name;    /* time series name */
char    *type;    /* time series data type */
char    ts_char[][4]; /* time series character data */
float   ts_float[];   /* time series floating point data */
int     *delta_t;     /* sample time interval */
int     *status;      /* function status */
{
  *status = find_in_ts(name, type, *delta_t, ts_float, ts_char);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/findints.c,v $";
 static char rcs_id2[] = "$Id: findints.c,v 1.1 1995/09/08 15:00:14 page Exp $";}
/*  ===================================================  */

}
