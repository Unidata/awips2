#include "light_data.h"

/*
   Main program to read lightning data in netcdf file and
   to insert them into "lightning" table.
*/

/*******************************************************************/
int lightning_proc_main (int argc, const char **argv)
/*******************************************************************/
{

   /*** Declare variables to get the time */
   char fn[150];
   int rrr;

   /*** Declare object of time structure */
   time_t t;
   struct tm *dt;

   printf("******Enter the lightning_proc.\n");

   /*** Called function time */
   t=time(NULL);
   dt=localtime (&t);

   /*** Copy filename string to contain fn array */
   strcpy(fn, argv[1]);

   /*** Called function ntcdfrd to read the file yyyymmdd_hhhh */

   rrr=ntcdfrd(fn);

  /* Get min max latitude and longitude */

   get_mmll_site();

   /*** Called lightning_count() function to load data into database */

   write_lightning(rrr);

   return 0;
}


