/* This routine should be called only once to read and store the information
   in the topography file. */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GeneralUtil.h"
#include "mpe_log_utils.h"
#include "mpe_topo.h"

static struct topo topo;

const struct topo * get_topo_coord ()
{
   static char *geo_data_token = "whfs_geodata_dir";
   char geo_data_file[150];
   int color;
   static int file_found = 0;
   static int first = 1;
   int i, j, ier, maxj, maxi, max_value, k;
   int len;
   int reply_len;
   int tdi;
   float lat, lon;
   FILE *fp = NULL;
   char *p = NULL;
   char header_buf [250];
   char * kbuf = NULL;
   float max_lat, max_lon, delta_lat, delta_lon, total_lat, total_lon;

   if (first == 1)
   {
      first = 0;

      max_value = 0;

      /* Open the topograhy file. */
      len = strlen (geo_data_token);
      get_apps_defaults (geo_data_token, &len, geo_data_file, &reply_len);

      if (reply_len == 0)
      {
	 /* Could not retrieve the whfs geo data directory. */
	 return NULL;
      }

      strcat (geo_data_file, "/topography");

      fp = fopen (geo_data_file, "r");

      if (fp == NULL)
      {
	logMessage ("could not open %s\n", geo_data_file);
         return NULL;
      }

      /* Read the first record of the topography file.  It contains the
         header information including the latitude/longitude bounds
         of the data in the file and the courseness of the data. */
      p = fgets (header_buf, 80, fp);

      ier = sscanf (header_buf, "%f %f %f %f %f %f", &max_lat, &max_lon, &total_lat,
		    &total_lon, &delta_lat, &delta_lon);

      /* Convert the latitude and longitude increments from degrees into 
         minutes. */
      delta_lat = delta_lat / 60.;
      delta_lon = delta_lon / 60.;

      /* Determine the total number of longitude increments (in minutes)
         and the total number of latitude increments. */
      maxi = ceil (total_lon / delta_lon);
      maxj = ceil (total_lat / delta_lat);

      /* Assign the latitude and longitude information read from the
         topography file into the topography structure. */
      topo.maxi = maxi;
      topo.maxj = maxj;
      topo.max_lat = max_lat;
      topo.max_lon = max_lon;
      topo.total_lat = total_lat;
      topo.total_lon = total_lon;
      topo.delta_lat = delta_lat;
      topo.delta_lon = delta_lon;

      /* Dynamically create the two dimensional array. */
      /* Maxi corresponds to longitude. */
      topo.coord = (struct lcoord **) calloc (maxi, sizeof (struct lcoord *));
      topo.value = (short int **) calloc (maxi, sizeof (short int *));
      topo.color = (char ** ) calloc ( maxi, sizeof ( char * ));

      if (topo.coord == NULL || topo.value == NULL || topo.color == NULL )
      {
	logMessage ("no memory for topo array\n");
	 exit (1);
      }

      for (i = 0; i < maxi; i++)
      {

	 /* maxj corresponds to latitude. */
	 topo.coord[i] =
	    (struct lcoord *) calloc (maxj, sizeof (struct lcoord));

	 topo.value[i] = (short *) calloc (maxj, sizeof (short));

         topo.color[i] = ( char * ) calloc ( maxj, sizeof ( char ));

	 if (topo.coord[i] == NULL || topo.value[i] == NULL ||
             topo.color[i] == NULL )
	 {

	   logMessage ("no memory for topo array\n");
	    exit (1);

	 }

      }

     logMessage ("****topo maxi %d maxj %d\n", maxi, maxj);

      /* Dynamically create the kbuf array. */
      kbuf = ( char * ) malloc ( sizeof ( char ) * ( maxi * 4 + 1 ) );

      if ( kbuf == NULL )
      {
        logMessage ( "No memory to hold topo array record.\n");
         exit(1);
      }

      for (j = 0; j < maxj; j++)
      {

	 fread (kbuf, sizeof (char), maxi * 4, fp);
	 kbuf[maxi * 4] = 0;
	 k = 0;

	 for (i = 0; i < maxi; i++)
	 {
	    topo.value[i][j] = atoi (&kbuf[k]);
	    lat = lon = topo.coord[i][j].lat = max_lat - (float) j *delta_lat;

	    /* Be sure to negate the longitude.  Map library expects
	       Western Hemisphere longitudes to be negative. */
	    topo.coord[i][j].lon = -1 * (max_lon - (float) i * delta_lon);
	    k = k + 4;
	 }

      }

      for (j = 0; j < maxj - 1; j++)
      {
	 for (i = 0; i < maxi - 1; i++)
         {
            /* Determine the color index of this topo cell. */
            if (topo.value[i][j] > 0)
            {

               if (topo.value[i][j] < 80 )
                  color = 3;

               else if (topo.value[i][j] < 160 )
                  color = 10;

               else if (topo.value[i][j] < 240 )
                  color = 17;

               else if (topo.value[i][j] < 320 )
                  color = 24;

               else
                  color = 31;

               /* Determine the gradient from southwest to northeast.
                  Tdi will be negative for a downhill gradient.  Tdi will
                  be positive for an uphill gradient. */
               tdi = -topo.value[i][j + 1] / 3 + topo.value[i + 1][j] / 3;
               if (tdi > 3)
                  tdi = 3;

               if (tdi < -3)
                  tdi = -3;

               /* Adjust the color to give the illusion of illumination from
                  the southwest. */
               color = color + tdi;

               topo.color[i][j] = ( char ) color;
            }
            else
            {
               topo.color[i][j] = ( char ) 0;
            }

         }
      }


      if ( kbuf != NULL )
      {
         free ( kbuf );
         kbuf = NULL;
      }

      fclose (fp);
      fp = NULL;
      file_found = 1;
   }

   if ( file_found == 1 )
   {
      return &topo;
   }
   else
   {
      return NULL;
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/get_topo_coord.c,v $";
 static char rcs_id2[] = "$Id: get_topo_coord.c,v 1.3 2007/05/23 20:54:46 whfs Exp $";}
/*  ===================================================  */

}
