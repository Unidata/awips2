#include "stdio.h"
#include "cex25.h"
/*
 * File:FillDecPlaceArray.c
 *
 * Fills the decimal_places array of DecPlace structures
 *
 */

void FillDecPlaceArray()
{
   int      i, j;            /* counters     */
   FILE     *dp;             /* File pointer */
   char     sys_path[80];    /* path for system files */
   int      len, len2;       /* length of strings for get_apps_defaults */

   /* call routine to get the path name for sys files */
   memset(sys_path, '\0', 80);
   len = strlen("ifp_sys_dir");
   get_apps_defaults("ifp_sys_dir", &len, sys_path, &len2);
   strcat(sys_path, "/decimalplaces");

   dp = fopen(sys_path, "r");
   fscanf(dp, "%*s%*s%*s%*s%*s%*s");
   for(i=0; i<NUM_DATATYPES; i++)
   {
      fscanf(dp, "%s %*s %d", decimal_places[i].datatype,
	     &decimal_places[i].num_dp);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/FillDecPlaceArray.c,v $";
 static char rcs_id2[] = "$Id: FillDecPlaceArray.c,v 1.1 1995/09/08 14:56:48 page Exp $";}
/*  ===================================================  */

} /* end of FillDecPlaceArray */
