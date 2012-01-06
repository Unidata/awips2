/*******************************************************************************
* FILENAME:            read_topo.c
* DESCRIPTION:         Crops a national topographic data field to one which
*                      is large enough to completely contain an office's MPE
*                      forecast area.
*
* ORIGINAL AUTHOR:     Bryon Lawrence (based on code from Craig Peterson)
* CREATION DATE:       March 2006
* ORGANIZATION:        HSEB/OHD
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2006   B. Lawrence       Initial Coding
*   Jan   2007   B. Lawrence       Added logic to read Canadian topo file.
*                                  This file will provide topo data for the
*                                  Canadian portion of the NWRFC basin.
********************************************************************************
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GeneralUtil.h"
#include "HydroStatus.h"
#include "rfcwide.h"
#include "stage3.h"
#include "Swap2Bytes.h"

/* Macro for finding bounding lat and lon values
   which form a box which completely encloses the office's
   MPE forecast area. */
#define LAT_LON_TEST   if ( lat < min_lat ) \
                       {                    \
                          min_lat = lat;    \
                       }                    \
                                            \
                       if ( lat > max_lat ) \
                       {                    \
                          max_lat = lat;    \
                       }                    \
                                            \
                       if ( lon < min_lon ) \
                       {                    \
                          min_lon = lon;    \
                       }                    \
                                            \
                       if ( lon > max_lon ) \
                       {                    \
                          max_lon = lon;    \
                       }                    \

/* Variables defining the 30-arcsecond ALASKA topography file (w180n90)
   from the USGS. */
#define ALASKA_TOPO_CELLSIZE 0.00833333333
#define ALASKA_TOPO_NODATA -9999
#define MAXIMUM_ALASKA_ELEVATION 6098   /* Meters */
#define NUMBER_ALASKA_TOPO_ROWS  6000
#define NUMBER_ALASKA_TOPO_COLUMNS  4800
#define UPPER_LEFT_CORNER_LATITUDE 89.995833
#define LOWER_LEFT_CORNER_LATITUDE 40.000
#define UPPER_LEFT_CORNER_LONGITUDE -179.995833

typedef enum TopoFileNames { SOUTHWEST, NORTHWEST, EAST, CANADA,
                             ALASKA, NUM_TOPO_FILES} TopoFileNames;

int read_topo_main (int argc, const char **argv)
{

   static char * geo_data_token = "whfs_geodata_dir";
   FILE *fp = NULL, *fw = NULL, *fr[NUM_TOPO_FILES];
   short int **  elev = NULL;
   char dbuf[4000], buf[10];
   double cell_size;
   double max_lat, min_lat, max_lon, min_lon, olat, olon;
   double lower_left_lat;
   double lower_left_lon;
   int i, j, k, h, n, maxi, maxj, mesh_size, l, di, dj;
   char dummy_string [ 100 ];
   char latd[10], latm[10], lond[10], lonm[10];
   float lat, lon, dlat, dlon;
   char fname[100];
   int itemp;
   char directory[100];
   HRAP latlon;
   int ilen, slen;
   int ix, iy;
   int maxx;
   int maxy;
   int missing_value;
   int num_cols;
   int num_rows;
   int status;
   int tpoints;
   int xelev = 0;
   int nelev = 9999;
   int xor;
   int yor;
   point hrap;
   short * pAlaskaTopoRecord = NULL;
   size_t num_topo_columns = NUMBER_ALASKA_TOPO_COLUMNS;

   max_lat = -9999.;
   min_lat = 9999.;
   max_lon = -9999.;
   min_lon = 9999.;

   printf ("starting read_topo\n");

   ilen = strlen ( geo_data_token );

   /* The raw topography files are expected to be in the directory
      specified by the whfs_geodata_dir token. */
   get_apps_defaults ( geo_data_token, &ilen, directory, &slen);

   if (slen == 0)
   {
      printf ("could not find a value for token %s.\n", geo_data_token);
      return 1;
   }

   /* Build the path and open the southwest topo file. */
   strcpy (fname, directory);
   strcat (fname, "/");
   strcat (fname, "toposw");

   fr[SOUTHWEST] = fopen (fname, "r");

   if (fr[SOUTHWEST] == NULL)
   {
      printf ("could not open %s\n", fname);
      return 1;
   }


   /* Build the path and open the northwest topo file. */
   strcpy (fname, directory);
   strcat (fname, "/");
   strcat (fname, "toponw");

   fr[NORTHWEST] = fopen (fname, "r");

   if (fr[NORTHWEST] == NULL)
   {
      printf ("could not open %s\n", fname);
      return 1;
   }

   /* Build the path and open the eastern topo file. */
   strcpy (fname, directory);
   strcat (fname, "/");
   strcat (fname, "topoea");

   fr[EAST] = fopen (fname, "r");

   if (fr[EAST] == NULL)
   {
      printf ("could not open %s\n", fname);
      return 1;
   }

   /* Build the path and open the canadian topo file. */
   strcpy (fname, directory);
   strcat (fname, "/");
   strcat (fname, "topocanada");

   fr[CANADA] = fopen(fname, "r" );

   if ( fr[CANADA] == NULL )
   {
      printf ("could not open %s\n", fname );
      return 1;
   }

   /* Build the path and open the Alaskan topo file. */
   strcpy  (fname, directory);
   strcat (fname, "/");
   strcat (fname, "topoalaska");

   fr[ALASKA] = fopen(fname, "r");

  if ( fr[ALASKA] == NULL )
  {
     printf ("could not open %s\n", fname );
  }

   /* The topography file will be cropped to fit the office's MPE forecast
      area.  This is determined by reading the MPE Coordinate File. */
   status = read_mpe_coordinate_file ( & xor, & yor, & maxx, & maxy );

   if ( status != HydroStatus_OK )
   {
      fprintf ( stderr, "Could not read the MPE coordinate file.\n" );
      return 1;
   }

   /* Determine the latitude/longitude box which will completely
      enclose the office's MPE forecast area. */

   /* Process the southwest corner. */
   hrap.x = xor;
   hrap.y = yor;

   latlon = HrapToLatLongMpe ( hrap );

   lat = latlon.y;
   lon = latlon.x;

   LAT_LON_TEST

   /* Process the northwest corner. */
   hrap.x = xor;
   hrap.y = yor + maxy;

   latlon = HrapToLatLongMpe ( hrap );

   lat = latlon.y;
   lon = latlon.x;

   LAT_LON_TEST

   /* Process the northeast corner. */
   hrap.x = xor + maxx;
   hrap.y = yor + maxy;

   latlon = HrapToLatLongMpe ( hrap );

   lat = latlon.y;
   lon = latlon.x;

   LAT_LON_TEST

   /* Process the southeast corner. */
   hrap.x = xor + maxx;
   hrap.y = yor;

   latlon = HrapToLatLongMpe ( hrap );

   lat = latlon.y;
   lon = latlon.x;

   LAT_LON_TEST;

   /* Round the lat/lon values up to the next nearest whole lat/lon. */
   min_lat = round_to_places ( (min_lat - 0.5), 0 );
   max_lat = round_to_places ( (max_lat + 0.5), 0 );
   min_lon = round_to_places ( (min_lon - 0.5), 0 );
   max_lon = round_to_places ( (max_lon + 0.5), 0 );

   mesh_size = 1;

   dlat = (float) (mesh_size * 30) / 60;

   di = mesh_size;
   dj = mesh_size;

   tpoints = 120 / mesh_size;

   dlon = dlat;

   maxj = ( (max_lat - min_lat) * 120 );
   maxi = ( (max_lon - min_lon) * 120 );

   printf ("maxi %d maxj %d\n", maxi, maxj);

   /* Dynamically allocate memory for the read_topo array. */
   elev = ( short ** ) malloc ( ( maxi + 1 ) * ( sizeof ( short * ) ) );

   if ( elev == NULL )
   {
      fprintf ( stderr, "Could not allocate memory for the elev "
                        "array.\n" );
      return 1;
   }

   for ( i = 0; i <= maxi; ++i )
   {
      elev [ i ] = ( short * ) malloc ( ( maxj + 1 ) * sizeof ( short ) );

      if ( elev [ i ] == NULL )
      {
         fprintf ( stderr, "Could not allocate memory for the elev "
                           "array.\n" );
         return 1;
      }
   }

   /* Allocate space for the array representing a record from
      the Alaska topo file. */
   pAlaskaTopoRecord = ( short * ) malloc ( sizeof ( short ) *
                                   NUMBER_ALASKA_TOPO_COLUMNS );

   if ( pAlaskaTopoRecord == NULL )
   {
      printf ( "Could not allocate memory to contain a record "
               "from the Alaska topo file.\n" );
      return 1;
   }

   printf ("di %d dj %d\n", di, dj);


   olat = -1;
   olon = -1;
   h = -30;
   k = 0;

   for (j = 0; j < maxj; j++)
   {
      for (i = 0; i < maxi; i++)
      {
	 elev[i][j] = 0;

      }
   }

   /* Read each of the four available dem files for the CONUS and
      Canada and Alaska. */
   for (l = 0; l < NUM_TOPO_FILES; l++)
   {

      printf ("reading %d\n", l);

      fp = fr[l];

      if ( ( l != CANADA ) && ( l != ALASKA ) )
      {
         for (;;)
         {
            /* Skip the dataset name. */
	    n = fread (dbuf, 12, sizeof (char), fp);

	    if (n == 0)
	       break;

            /* Read the maximum (northern most) latitude (deg, min) and
               western most longitude (deg, min). */
	    fread (latd, 3, sizeof (char), fp);
	    fread (latm, 3, sizeof (char), fp);
	    fread (lond, 4, sizeof (char), fp);
	    fread (lonm, 3, sizeof (char), fp);

            /* Convert latitude and longitude from degrees/minutes to
               degrees/decimal. */
	    lat = atof (latd) + atof (latm) / 60;
	    lon = atof (lond) + atof (lonm) / 60;

            /* Read and dispose blank characters. */
	    fread (dbuf, 95, sizeof (char), fp);

            /* Compute base offsets assuming 0.5 arc-minute mesh. */
	    iy = ( (max_lat - lat) * 120);
	    ix = ( (max_lon - lon) * 120);

	    for (j = 0; j < 30; j++)
	    {
	       for (i = 0; i < 30; i++)
	       {

	          fscanf (fp, "%4c", buf);
	          buf[4] = 0;

	          sscanf (buf, "%d", &itemp);

	          if (ix + i < 0 || ix + i > maxi || iy + j < 0 ||
                      iy + j > maxj)
		     continue;

	          elev[i + ix][j + iy] = itemp / 10;

	          if ( elev[i + ix][j + iy] > 6000 )
	          {
		     printf ("error %d %d %d\n", elev[i + ix][j + iy],
		   	  i + ix, j + iy);
		     exit (1);
	          }
	       }
	    }
         }
      }
      else if ( l == CANADA )
      {
         /* Special logic to process the canadian topography file. */
         /* Read the number of columns and number of rows in this topo data
            set. */
         fscanf ( fp, "%s %d", dummy_string, & num_cols );
         fscanf ( fp, "%s %d", dummy_string, & num_rows );

         /* Read the minimum (southernmost) latitude and the
            maximum (westernmost) longitude. */
         fscanf ( fp, "%s %lf", dummy_string, & lower_left_lon );
         fscanf ( fp, "%s %lf", dummy_string, & lower_left_lat );

         lower_left_lon *= -1;

         /* Read the cell size. */
         fscanf ( fp, "%s %lf", dummy_string, & cell_size );

         /* Read the missing value. */
         fscanf ( fp, "%s %d", dummy_string, & missing_value );

         /* Compute the maximum (northernmost) latitude. */
         lat = lower_left_lat + ( cell_size * num_rows );

         /* Compute base offsets assuming 0.5 arc-minute mesh. */
         iy = ( (max_lat - lat) * 120);
         ix = ( (max_lon - lower_left_lon) * 120);

         /* Read each record in.   Store the values in the elevation array
            when necessary. */

         for (j = 0; j < num_rows; j++)
         {
            for (i = 0; i < num_cols; i++)
	    {
               /* Read the next value from the Canadian topo file. */
               n = fscanf ( fp, "%d", &itemp );

               if ( n == 0 )
               {
                  /* The end of the topography file has been reached. */
                  break;
               }

               /* Skip the value if it is missing. */
               if ( itemp == missing_value )
               {
                  continue;
               }

               /* Is the value within bounds? */
	       if (ix + i < 0 || ix + i > maxi || iy + j < 0 ||
                   iy + j > maxj)
               {
	          continue;
               }

               /* Convert itemp from meters to decameters. */
	       elev[i + ix][j + iy] = itemp / 10;
            }
         }

      }
      else  /* Process Alaska topo data. */
      {
         num_rows = NUMBER_ALASKA_TOPO_ROWS;
         num_cols = NUMBER_ALASKA_TOPO_COLUMNS;

         /* Compute the lower left latitude. */
         lower_left_lon = UPPER_LEFT_CORNER_LONGITUDE * (-1);

         /* Read the missing value. */
         missing_value = ALASKA_TOPO_NODATA;

         /* Compute the maximum (northernmost) latitude. */
         lat = UPPER_LEFT_CORNER_LATITUDE;

         /* Compute base offsets assuming 0.5 arc-minute mesh. */
         iy = ( (max_lat - lat) * 120);
         ix = ( (max_lon - lower_left_lon) * 120);

         /* Read each record in.   Store the values in the elevation array
            when necessary. */

         for (j = 0; j < num_rows; j++)
         {
            fread ( pAlaskaTopoRecord, sizeof ( short ),
                    NUMBER_ALASKA_TOPO_COLUMNS, fp );

            /* Flip the bytes in this array from Big Endian
               to Little Endian. */
            Swap2Bytes_ ( pAlaskaTopoRecord, & num_topo_columns );

            for (i = 0; i < num_cols; i++)
	    {
               /* Read the next value from the Canadian topo file. */
               itemp = pAlaskaTopoRecord [ i ];

               /* Skip the value if it is missing. */
               if ( itemp == missing_value )
               {
                  continue;
               }

               /* Is the value within bounds? */
	       if (ix + i < 0 || ix + i > maxi || iy + j < 0 ||
                   iy + j > maxj)
               {
	          continue;
               }

               /* Convert itemp from meters to decameters. */
	       elev[i + ix][j + iy] = itemp / 10;
            }
         }

      }

      fclose (fp);
      fp = NULL;
   }

   if ( pAlaskaTopoRecord != NULL )
   {
      free ( pAlaskaTopoRecord );
      pAlaskaTopoRecord = NULL;
   }

   printf ("finished read\n");

   strcpy (fname, directory);
   strcat (fname, "/");
   strcat (fname, "topography");

   fw = fopen (fname, "w");

   fprintf (fw, "%f %f %f %f %f %f\n", max_lat, max_lon, max_lat - min_lat,
	    max_lon - min_lon, dlat, dlon);

   xelev = 0;
   nelev = 9999;

   for (j = 0; j < maxj; j = j + dj)
   {
      for (i = 0; i < maxi; i = i + di)
      {
	 if (elev[i][j] > 6000)
	 {
	    printf ("error %d\n", elev[i][j]);
	    elev[i][j] = 0;
	 }
	 else
	 {

	    if (elev[i][j] > xelev)
	       xelev = elev[i][j];

	    if (elev[i][j] < nelev)
	       nelev = elev[i][j];

	 }

	 fprintf (fw, "%3d ", elev[i][j]);

      }

   }

   fclose (fw);

   printf ("read_topo done - output in %s %d %d\n", fname, xelev, nelev);

   /* Free the memory used by the elev array. */
   for ( i = 0; i < maxi + 1; ++i )
   {
       free ( elev [ i ] );
   }

   free ( elev );
   elev = NULL;

   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc/src/read_topo/RCS/read_topo.c,v $";
 static char rcs_id2[] = "$Id: read_topo.c,v 1.5 2007/02/23 21:24:37 lawrence Exp $";}
/*  ===================================================  */

}
