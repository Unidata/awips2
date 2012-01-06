#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "LineSegs.h"
#include "mpe_log_utils.h"

static int maxib = 0;

int get_basin_data (const char *basin_file,
  	            const char *hrap_file,
		    struct map map[],
		    struct tag tag[])
{
   FILE *fr = NULL;
   FILE *fp = NULL;
   char message[GAGEQC_MESSAGE_LEN];

   char *p = NULL, ibuf[200];
   char where_clause[200];
   int ib, ier, l, numpts, ip, x, y, ip2, ip3, ip4, zones, i, dum;
   int hrap_basin_flag = 1;
   double lat, lon;
   char bchar[10];
   int m;
   int mm;
   int num_points;

   LineSegs *pLineSegs = NULL;
   LineSegs *pLineSegNode = NULL;

   /* Only the WHFS basin file format can be used. */
   if (basin_file[0] == 0)
   {
      /* A basin file must be specified. */
      memset ( message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf (message, "No basin file specified\n");
      logMessage ( message );
      return DAILYQC_FAILED;
   }

   if (hrap_file[0] == 0)
   {
     logMessage ("no hrap basin file specified\n");
      hrap_basin_flag = 0;
   }

   /*----------------------------------------------*/
   /*   read file containing info for drawing basins  */
   /*   filename read from config file                */
   /*----------------------------------------------*/


   /* This file is one generated from ArcView.  It contains the lat/lon points
    * of each vertex defining the path of the basin. */
   fr = fopen (basin_file, "r");

   if (fr == NULL)
   {
      memset (message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf (message, "Could not open file: %s\n", basin_file);
      logMessage (message);
      return DAILYQC_FAILED;
   }
   else
   {
      memset (message, '\0', GAGEQC_MESSAGE_LEN);
      sprintf (message, "Opened file: %s\n", basin_file);
      logMessage (message);
   }
/*--------------------------------------------------------*/
/*   read file containing HRAP coordinates in each basin  */
/*   note that some basins have "lower", "middle" and "upper" portions */
/*   filename read from config file                       */
/*--------------------------------------------------------*/

   /* If this file does not exist then use linesegs and assume that the
    * basins are not split up into subareas. */
   if (hrap_basin_flag == 1)
   {
      fp = fopen (hrap_file, "r");

      if (fp == NULL)
      {
	 memset (message, '\0', GAGEQC_MESSAGE_LEN);
	 sprintf (message, "Could not open file: %s  Using LineSegs.\n",
		  hrap_file);
	 logMessage (message);
	 hrap_basin_flag = 0;
      }
      else
      {
	 memset (message, '\0', GAGEQC_MESSAGE_LEN);
	 sprintf (message, "Opened file: %s\n", hrap_file);
	 logMessage (message);
      }
   }

   ib = 0;

   /* Assume the WHFS basin file format. */
   p = fgets (ibuf, 200, fr);

   while (p != NULL)
   {
      ier = sscanf (ibuf, "%s %*s %d %d", map[ib].hb5, &dum, &numpts);
      map[ib].basin_points = numpts;
      map[ib].basin = calloc (numpts, sizeof (struct lcoord));

      for (l = 0; l < numpts; l++)
      {
         p = fgets (ibuf, 200, fr);
         ier = sscanf (ibuf, "%lf %lf", &lat, &lon);

         /* Ensure that the longitude is negative. */
         lon = -lon;

         map[ib].basin[l].lon = lon;
         map[ib].basin[l].lat = lat;
      }

      ib++;
      p = fgets (ibuf, 200, fr);
   }

   map[ib].hb5[0] = 0;
   maxib = ib;

   /* Initialize basin area elements in the map structure. */
   for (ib = 0; ib < maxib; ib++)
   {
      map[ib].gz = (float * )calloc (200, sizeof (float));
      map[ib].uz = (float * )calloc (200, sizeof (float));
      map[ib].mz = (float * )calloc (200, sizeof (float));
      map[ib].lz = (float * )calloc (200, sizeof (float));
      map[ib].gzc = (float * )calloc (24, sizeof (float));
      map[ib].uzc = (float * )calloc (24, sizeof (float));
      map[ib].mzc = (float * )calloc (24, sizeof (float));
      map[ib].lzc = (float * )calloc (24, sizeof (float));
      map[ib].zgz = (float * )calloc (200, sizeof (float));
      map[ib].zuz = (float * )calloc (200, sizeof (float));
      map[ib].zmz = (float * )calloc (200, sizeof (float));
      map[ib].zlz = (float * )calloc (200, sizeof (float));
      map[ib].tgz = (float * )calloc (200, sizeof (float));
      map[ib].tuz = (float * )calloc (200, sizeof (float));
      map[ib].tmz = (float * )calloc (200, sizeof (float));
      map[ib].tlz = (float * )calloc (200, sizeof (float));
      map[ib].maps_done = (int * )calloc (200, sizeof (int));
      map[ib].tmaps_done = (int * )calloc (200, sizeof (int));
      map[ib].zmaps_done = (int * )calloc (200, sizeof (int));

      for (m = 0; m < 200; m++)
      {
	 map[ib].gz[m] = -1;
	 map[ib].uz[m] = -1;
	 map[ib].mz[m] = -1;
	 map[ib].lz[m] = -1;
	 map[ib].zgz[m] = -1;
	 map[ib].zuz[m] = -1;
	 map[ib].zmz[m] = -1;
	 map[ib].zlz[m] = -1;
	 map[ib].tgz[m] = -999;
	 map[ib].tuz[m] = -999;
	 map[ib].tmz[m] = -999;
	 map[ib].tlz[m] = -999;
	 map[ib].maps_done[m] = -1;
	 map[ib].tmaps_done[m] = -1;
	 map[ib].zmaps_done[m] = -1;

      }
   }

   /* Now read the HRAP grid to basin definitions.  If the user has not supplied
    * a file containing this mapping, then use LineSeqs. */
   if ((hrap_basin_flag == 1) && (fp != NULL))
   {
      for (ib = 0; ib < maxib; ib++)
      {

	 p = fgets (ibuf, 80, fp);


	 if (p == NULL)
	 {
	    break;
	 }

	 p = strchr (ibuf, '\n');

	 if (p != NULL)
	 {
	    *p = 0;
	 }

	 ier = sscanf (ibuf, "%d %d %*s %s", &numpts, &zones, bchar);

	 strcpy (map[ib].bchar, bchar);
	 map[ib].hrap_points = numpts;
	 map[ib].hrap_data = calloc (numpts, sizeof (struct hrap_data));

	 for (mm = 0; mm < 4; mm++)
	 {
	    map[ib].zones[mm] = -1;
	 }

	 for (l = 0; l < numpts; l++)
	 {

	    for (mm = 0; mm < 4; mm++)
	    {
	       map[ib].hrap_data[l].zone[mm] = -1;
	    }

	    p = fgets (ibuf, 100, fp);

	    if (p == NULL)
	       break;

	    ier =
	       sscanf (ibuf, "%d %d %d %d %d %d\n", &x, &y, &ip, &ip2, &ip3,
		       &ip4);

	    if (ip < 0 || ip > 4)
	    {
               memset(message, '\0', GAGEQC_MESSAGE_LEN);
	       sprintf(message, "HRAP error in read_basin_data routine.\n");
               logMessage(message );
               return DAILYQC_FAILED;
	    }

	    map[ib].hrap_data[l].x = x;
	    map[ib].hrap_data[l].y = y;
	    map[ib].hrap_data[l].zone[ip - 1] = 1;
	    map[ib].zones[0] = 1;

	    if (ier >= 4)
	    {
	       if (ip2 < 0 || ip2 > 4)
	       {
                  memset ( message, '\0', GAGEQC_MESSAGE_LEN );
		  sprintf (message, "HRAP error in read_basin_data routine.\n");
                  logMessage ( message );
                  return DAILYQC_FAILED;
	       }

	       map[ib].hrap_data[l].zone[ip2 - 1] = 1;
	       map[ib].zones[ip2 - 1] = 1;

	    }


	    if (ier >= 5)
	    {

	       if (ip3 < 0 || ip3 > 4)
	       {
                  memset ( message, '\0', GAGEQC_MESSAGE_LEN );
		  sprintf (message, "HRAP error in read_basin_data routine.\n");
                  logMessage ( message );
                  return DAILYQC_FAILED;
	       }

	       map[ib].hrap_data[l].zone[ip3 - 1] = 1;
	       map[ib].zones[ip3 - 1] = 1;

	    }

	    if (ier >= 6)
	    {

	       if (ip4 < 0 || ip4 > 4)
	       {
                  memset ( message, '\0', GAGEQC_MESSAGE_LEN );
		  sprintf (message, "HRAP error in read_basin_data routine.\n");
                  logMessage ( message );
                  return DAILYQC_FAILED;
	       }

	       map[ib].hrap_data[l].zone[ip4 - 1] = 1;
	       map[ib].zones[ip4 - 1] = 1;

	    }

	 }

      }
   }
   else
   {
      /* Read Linesegs to get the basin definitions. Basins are assumed not to
       * have subareas. */
      /* Loop over each basin.  Retrieve the lineseg data for it if it is
       * available. */
      for (ib = 0; ib < maxib; ib++)
      {
	 /* Build the linesegs where clause. */
	 sprintf (where_clause, "WHERE area_id = '%s' ORDER BY hrap_row ASC, "
		  "hrap_beg_col ASC", map[ib].hb5);

	 /* Retrieve the linesegs data. */
	 pLineSegs = GetLineSegs (where_clause);

	 /* Assume a bchar of HZZZ. */
	 strcpy (map[ib].bchar, "HZZZ");

	 if (pLineSegs != NULL)
	 {
	   pLineSegNode = pLineSegs;

	    map[ib].hrap_points = 0;
	    map[ib].hrap_data = NULL;
            l = 0;

	    while (pLineSegNode != NULL)
	    {
	       num_points = (pLineSegNode->hrap_end_col -
			     pLineSegNode->hrap_beg_col) + 1;
	       map[ib].hrap_points += num_points;
	       map[ib].hrap_data = (struct hrap_data * ) realloc 
                                            (map[ib].hrap_data,
					    (map[ib].hrap_points *
					     sizeof (struct hrap_data)));

	       if (map[ib].hrap_data == NULL)
	       {
		      flogMessage (stdout, "Could not realloc memory for "
			                   "map[%d].hrap_data.\n", ib);
		      exit (1);
	       }

	       for (i = pLineSegNode->hrap_beg_col;
		        i <= pLineSegNode->hrap_end_col; ++i)
	       {
           
		      map[ib].hrap_data[l].x = i;
		      map[ib].hrap_data[l].y = pLineSegNode->hrap_row;
		      map[ib].hrap_data[l].zone[0] = 1;
		      map[ib].hrap_data[l].zone[1] = 0;
		      map[ib].hrap_data[l].zone[2] = 0;
		      map[ib].hrap_data[l].zone[3] = 0;
		      map[ib].zones[0] = 1;
                      map[ib].zones[1] = 0;
                      map[ib].zones[2] = 0;
                      map[ib].zones[3] = 0;
          
              ++l;
	       }

	       pLineSegNode = (LineSegs *) ListNext (&pLineSegNode->node);
	    }

	    FreeLineSegs (pLineSegs);
	    pLineSegs = NULL;

	 }
	 else
	 {
	   logMessage ("Could not retrieve LineSeg data for basin %s.\n",
		    map[ib].hb5);
	 }

      }

   }

   if (fr != NULL)
   {
      fclose (fr);
      fr = NULL;
   }

   if (fp != NULL)
   {
      fclose (fp);
      fp = NULL;
   }

   /* initialize basins as always on */

   for (ib = 0; ib < maxib; ib++)
   {
      map[ib].owner = 9999;
   }

   return DAILYQC_OK;
}

int get_num_basins ( )
{
   return maxib;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
