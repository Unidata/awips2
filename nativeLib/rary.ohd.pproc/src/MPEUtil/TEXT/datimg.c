#include <stdio.h>
#include <time.h>
#include "create_fortran_link.h"

/* function datimg - get greenwich time in int form */

create_fortran_link( void, datimg, (int time_out[6]), (time_out))

{
 struct  tm   *time_pointer;
 time_t       tp;

   time(&tp);
   time_pointer = gmtime(&tp);

   time_out[0] = time_pointer->tm_year;
   time_out[2] = time_pointer->tm_yday+1;
   time_out[2] = time_pointer->tm_mon + 1;
   time_out[3] = time_pointer->tm_mday;
   time_out[4] = time_pointer->tm_hour;
   time_out[5] = time_pointer->tm_min;


}
