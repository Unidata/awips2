/*******************************************************************************
* FILENAME:           create_freezing_station_list.c
* DESCRIPTION:        Creates a list of freezing stations for use in
*                     the DailyQC portion of MPE.
*
* ORIGINAL AUTHOR:    Bryon Lawrence
* CREATION DATE:      April 3, 2006
* ORGANIZATION:       HSEB
* MACHINE:            Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2006   B. Lawrence       Initial Coding
********************************************************************************
*/

#include <string.h>
#include <stdio.h>

#include "GeneralUtil.h"
#include "HydroStatus.h"
#include "rfcwide.h"
#include "stage3.h"
#include "netcdf.h"

/*******************************************************************************
* MODULE NAME:  main
* PURPOSE:      Locate all RUC80 grid points which fall within the
*               office's MPE forecast area.  Determine the lat/lon
*               of these points and assign arbitrary ids starting with
*               Z0000. Create a file in which each record is a freezing
*               station with the format:
*
*               <HB5> <Lat> <Lon> <Elev> <Tip/Weigh Flag> <Name>
*
*               The elevation is always 0.  The Tip Weigh Flag is
*               always 0.
*
*
*
* RETURNS:
*   VALUE                        DESCRIPTION
*       0                        Program functioned successfully.
*       1                        Program failed.
*
* APIs UTILIZED:
*   NAME                      HEADER FILE    DESCRIPTION
*   get_apps_defaults         GeneralUtil.h  Retrieve Apps Defaults Token
*                                            values.
*   LatLongToHrapMpe          stage3.h       Converts Lat/Long to coordinate
*                                            on national grid.
*   read_mpe_coordinate_file  rfcwide.h      Reads the coord.dat file.
*   w3fb12_                   None.          NCEP routine for converting
*                                            points on Lambert Conformal
*                                            projection to lat/lon.
*
*
* DATA FILES AND/OR DATABASE:
* Produces a file named <site_id>_freezing_station_list.  Site id is given
* by token mpe_site_id.  The path to the file is given by token
* mpe_station_list_dir:
*
* <mpe_station_list_dir>/<site_id>_freezing_station_list
*
*
********************************************************************************
*/

/* Prototype for routine which converts points on lambert conformal
   projection to lat/lon values. */
void w3fb12_(const float * i_point_coord,
             const float * j_point_coord,
             const float * ALAT1,
             const float * ELON1,
             const float * DX,
             const float * ELONV,
             const float * ALATAN,
             float * lat,
             float * lon,
             int * ier);

int create_freezing_station_list_main ( int argc, const char ** argv )
{
	char hb5 [ 7 ];
	   const char * PEDTSEP = "HZIRZZZ";
	   const char * mpe_site_id = "mpe_site_id";
	   const char * mpe_station_list_dir = "mpe_station_list_dir";
	   const char * ruc_model_data_dir = "ruc_model_data_dir";

	   char freezing_station_path [150];
	   char mpe_site_id_val [20];
	   char mpe_station_list_dir_val [120];
	   char ruc_model_data_dir_val [256];
	   char ruc_model_data_template[256];
	   char num_points [ 10 ];

	   /* Parameters needed in the Lambert Conformal projection converstion
	        and those common to RUC80 and RUC13 are initialized */

	      float ALAT1 = 16.28119;
	      float ELON1 = 360.0 - 126.1378;

	      const float ELONV = 360.0 - 95.000;
	      const float ALATAN = 25.000;

	      float DX;

	      float *lat00, *lon00;

	      int cdfid, f_len;
	      nc_type f_type;

   /* The number of points in the grid. */
   int NX;
   int NY;

   const int elev = 0;
   const int tip_weigh = 0;

   FILE * pFile = NULL;

   float i_point_coord;
   float j_point_coord;
   float lat;
   float lon;

   HRAP hrap;
   HydroStatus status;

   int ier;
   int i;
   int j;
   int maxx;
   int maxy;
   int point_count = 0;
   int reply_len;
   int string_len;
   int xor;
   int yor;

   /* Retrieve the HRAP coordinates of the MPE forecast area. */
   status = read_mpe_coordinate_file ( &xor, &yor, &maxx, &maxy );

   if ( status != HydroStatus_OK )
   {
      fprintf ( stderr, "Could not read the MPE coordinate file.\n" );
      return 1;
   }

   /* Read the tokens required to build the freezing station list. */
   string_len = strlen ( mpe_site_id );

   get_apps_defaults ( (char *) mpe_site_id, &string_len,
                       mpe_site_id_val, & reply_len );

   if ( reply_len == 0 )
   {
      fprintf ( stderr, "Could not retrieve a value for token %s. Stopping.\n",
                        mpe_site_id );
      return 1;
   }

   string_len = strlen ( ruc_model_data_dir );

   get_apps_defaults ( ( char * ) ruc_model_data_dir, & string_len,
		               ruc_model_data_dir_val, & reply_len );

   if ( reply_len == 0 )
   {
      fprintf ( stderr, "Could not retrieve a value for token %s. Stopping.\n",
    		  ruc_model_data_dir );
      return 1;
   }

   string_len = strlen ( mpe_station_list_dir );

   get_apps_defaults ( ( char * ) mpe_station_list_dir, & string_len,
                          mpe_station_list_dir_val, & reply_len );

   if ( reply_len == 0 )
   {
      fprintf ( stderr, "Could not retrieve a value for token %s. Stopping.\n",
                         mpe_station_list_dir_val );
      return 1;
   }

   sprintf ( freezing_station_path, "%s/%s_freezing_station_list",
                                    mpe_station_list_dir_val,
                                    mpe_site_id_val );

   /* Open the file the freezing station list will be written to. Open
      for writing and updating. */
   pFile = fopen ( freezing_station_path, "w" );

   if ( pFile == NULL )
   {
      fprintf ( stderr, "Could not open file %s. Stopping.\n",
                        freezing_station_path );
      return 1;
   }


   sprintf ( num_points, "%-5d    ", point_count);
   fprintf ( pFile, "%s\n", num_points );

   if(strstr(ruc_model_data_dir_val,"GRID130")!=NULL)
   {
       /* RUC13 */


    /* in order to generalize this, need to read the lat00 and lon00 from the NetCDF template file in the RUC 13
       NetCDf area on AWIPS.
    */

       strcpy(ruc_model_data_template, ruc_model_data_dir_val);
       strcat( ruc_model_data_template,"/template");

       cdfid=ncopen(ruc_model_data_template,NC_NOWRITE);

       if (cdfid==-1)
       {
    	   fprintf(stderr, "\n ERROR: Could not open the netcdf file: %s\n", ruc_model_data_template );
    	   fprintf(stderr, "\n In order to create the freezing level stations using RUC 13 data, ");
    	   fprintf(stderr, "\n first check to ensure the token ruc_model_data_dir is correct." );
    	   fprintf(stderr, "\n For RUC 13, it is normally set to /data/fxa/Grid/SBN/netCDF/GRID130/RUC.");
    	   fprintf(stderr, "\n However, this could be different on your system. ");
    	   fprintf(stderr, "\n If correct, check your AWIPS setup to ensure you are receiving the RUC 13 data correctly. \n");
    	   fprintf(stderr, "\n If not, ask the NCF for assistance as you may need to relocalize your system \n");
    	   fprintf(stderr, "\n with a -grids localization and change other configuration files as well.");
    	   return 1;
       }

       /* Read the lat00 and lon00 globabl attribute values, which are the SW lat/lon corner points */

       ncattinq(cdfid,NC_GLOBAL,"lat00",&f_type, &f_len);

       lat00 = (float *) malloc(f_len * nctypelen(f_type));

       ncattget(cdfid,NC_GLOBAL,"lat00",(void *) lat00);

       ncattinq(cdfid,NC_GLOBAL,"lon00",&f_type, &f_len);

       lon00 = (float *) malloc(f_len * nctypelen(f_type));

       ncattget(cdfid,NC_GLOBAL,"lon00",(void *) lon00);

       ncclose(cdfid);

       ALAT1 = *lat00;

       ELON1 = 360.0 + (*lon00);  /* lon00 is a negative value in the NetCDF file */



       DX = 13545;  /* note - may need to read this as well from the template file */
       NX = 175;
       NY = 175;
   }
   else /* RUC80 */
   {
       DX = 81270.5;
       NX = 76;
       NY = 57;
   }

   for ( j = 1; j <= NY; ++j )
   {
      for ( i = 1; i <= NX; ++i )
      {
         i_point_coord = (float)i;
         j_point_coord = (float)j;

         w3fb12_(&i_point_coord,
                 &j_point_coord,
                &ALAT1,
                &ELON1,
                &DX,
                &ELONV,
                &ALATAN,
                &lat,
                &lon,
                &ier);

        if ( ier != 0 )
        {
           fprintf ( stderr, "Error processing point (%d,%d) (i,j). "
                             "Skipped...\n", i, j );
        }
        else
        {
           /* Determine if this point is inside the MPE area. */
           /* Must subtract 360 to convert from East Longitude. */
           lon -= 360.00;
           lon *= -1;
           hrap = LatLongToHrapMpe ( lat, lon );

           if ( ( hrap.x >= xor ) &&
                ( hrap.x <= xor + maxx ) &&
                ( hrap.y >= yor ) &&
                ( hrap.y <= yor + maxy ) )
           {
              /* This RUC grid point is within the MPE forecast box. */
              sprintf ( hb5, "Z%05d", point_count );
              fprintf ( pFile, "%s %s %8.4f %9.4f %d %d %s\n",
                               hb5, PEDTSEP, lat, lon, elev, tip_weigh,
                               hb5 );
              ++ point_count;
           }
        }
      }
   }

   /* Rewind the file to the position of the freezing station record
      count place holder.  */
   rewind ( pFile );

   sprintf ( num_points, "%-5d    ", point_count);
   fprintf ( pFile, "%s\n", num_points );

   /* Close the file. */
   fclose ( pFile );
   pFile = NULL;

   return 0;
}
