/*******************************************************************************
* FILENAME:             convert_coord_file.c
* NUMBER OF MODULES:    2
* GENERAL INFORMATION:
*   MODULE 1:           HrapToLatLongMpe
* DESCRIPTION:          Converts an HRAP coordinate to a latitude/longitude
*                       coordinate.
*   MODULE 2:           main
* DESCRIPTION:          The main routine.  Processes command line arguments,
*                       reads the coord_host.dat data file, and converts the
*                       HRAP coords in the data to determine the
*                       lat/lon coords of each corner of the MPE forecast
*                       area rectangle.
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        February 11, 2005
* ORGANIZATION:         OHD-11, HSEB
* MACHINE:              Redhat Linux.
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        Feb 11, 2005 Bryon Lawrence    Original Coding/documenting
*
* This program has been written to not have any dependencies on in house
* WHFS libraries.  This was done so it could be easily rebuilt anyplace
* that has an ANSI C compiler.
*          2       Sep 21, 2007  David T. Miller   added a check for HPE
*                                                  which uses this routine in the
*                                                  same way as MPE for the HRAP
*                                                  grid corners but four times
*                                                  the max X and Y grid coordinates
********************************************************************************
*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "GeneralUtil.h"


/* computed constants required for HRAP to Lat/Lon conversion.
RE = ((earthrad * (1 + sin(tlat))) / mesh_len) = 2496.3404
GI_SQUARED = ((earthrad * (1 + sin(tlat))) / mesh_len) = 2496.3404 */

#define DEGRAD      0.017453293
#define GI_SQUARED  6231715.29
#define RADDEG      57.2957795
#define RE          2496.3404
#define STDLON      105.0
#define TOKEN_VALUE_LEN 64
#define HPE_GRID_FACTOR  4

/* description of notable constants:
   stdlat    - float; standard latitude  = 60.
   STDLON    - float; standard longitude = 105.
   earthrad  - float; earth's radius (km) = 6371.2
   mesh_len  - float; mesh length at stdlat (60 deg North) = 4.7625
   RADDEG    - float; conversion from radians to degrees = 57.29577951
   DEGRAD    - float; conversion degrees to radians = 0.017453293;
   tlat      - float; standard latitude in radians = stdlat/RADDEG
   */

/* Structure Definitions */
struct HRAP
{
   float x;
   float y;
};

struct Point
{
   int x;
   int y;
};

/***************************************************************************/
/*       FUNCTION:   convert from HRAP coordinates to latitude-longitude   */
/***************************************************************************/

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:  HrapToLatLongMpe
* PURPOSE:      Convert an HRAP coordinate to a lat/lon coordinate.
*
* ARGUMENTS:
*   TYPE   DATA TYPE    NAME      DESCRIPTION/UNITS
*   I/O    struct Point hrap      Contains the HRAP row and column
*                                 coordinates (relative to the national
*                                 HRAP grid)
* RETURNS:
*   DATA TYPE                     DESCRIPTION
*   struct HRAP                   Contains the latitude (y) and longitude (x)
*                                 coordinates cooresponding the HRAP
*                                 coordinate.
* APIs UTILIZED:
*   Only system routines are used.
*
* DATA FILES AND/OR DATABASE:
*  None.
*
* ERROR HANDLING:
*  None.
********************************************************************************
*/
static struct HRAP HrapToLatLongMpe ( struct Point hrap )
{
   /* local variables */

   double x, y, rr, ang;
   struct HRAP  ll;


   /* apply offsets */

   x = (double )hrap.x - 401.;
   y = (double )hrap.y - 1601.;

   rr = x*x + y*y;


   /* determine the y coordinate */

   ll.y = asin((GI_SQUARED - rr) / (GI_SQUARED + rr)) * RADDEG;


   /* determine the x coordinate */

   ang = atan2(y, x) * RADDEG;
   if (ang < 0) ang = ang + 360.;

   ll.x = 270 + STDLON - ang;


   /* adjust if needed */

   if (ll.x < 0)
      ll.x = ll.x + 360;

   else if (ll.x > 360)
      ll.x = ll.x - 360;


   /* return with the resultant structure */

   return ll;
}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    main
* PURPOSE:        The main routine.  Takes two command line arguments, the
*                 first specifying the the coord_host.dat filename and path,
*                 the second specifying the name of the file to which to write
*                 the latitude/longitude coordinates of each corner of the
*                 MPE forecast box.
*
*  Usage:
*       convert_coord_file <coord_file> <output_file>
*
*
* RETURNS:
*   0 upon success
*   1 upon failure.
*
*   Possible causes of failure:
*     - Incorrect number of arguments
*     - Cannot open coord file for reading
*     - Cannot open output file for writing
*
* APIs UTILIZED:
*   NAME                            DESCRIPTION
*   HrapToLatLongMpe                Converts HRAP coordinates to Latitude
*                                   Longitude coordinates.
* DATA FILES AND/OR DATABASE:
*   Requires as input a coordinate file with the following four lines of
*   information:
*   HRAPX    - The national HRAP col of the southwest corner of the MPE
*              forecast area.
*   HRAPY    - The national HRAP row of the southwest corner of the MPE
*              forecast area.
*   NUMCOLS  - The number of HRAP columns in the MPE forecast area.
*   NUMROWS  - The number of HRAP rows in the MPE forecast area.
*
********************************************************************************
*/
int convert_coord_file_main ( int argc , char ** argv)
{
   char * pCoordFilename = NULL;
   char * pOutputFilename = NULL;
   FILE * pFile = NULL;
   struct HRAP lat_lon_ne;
   struct HRAP lat_lon_nw;
   struct HRAP lat_lon_se;
   struct HRAP lat_lon_sw;
   int maxx;
   int maxy;
   int xor;
   int yor;
   struct Point hrap_point;
   char tokenvalue[TOKEN_VALUE_LEN] = {'\0'};

   /* Check to make sure the user has supplied the path and name of
      the coordinate file and the output lat/lon file. */
   if ( argc != 3 )
   {
      fprintf ( stderr, "Usage:\n"
                        "\tconvert_coord_file <hrap coordinate file> "
			"<converted lat/lon file>\n" ) ;
      return 1 ;
   }

   pCoordFilename = argv [ 1 ] ;
   pOutputFilename = argv [ 2 ] ;

   /* Open the Coord file. */
   pFile = fopen ( pCoordFilename , "r" ) ;

   if ( pFile == NULL )
   {
      fprintf ( stderr, "Could not open file %s for reading.\n",
                pCoordFilename ) ;
      return 1 ;
   }

   /* Read the Coord file. */
   fscanf ( pFile, "%d", &xor );
   fscanf ( pFile, "%d", &yor );
   fscanf ( pFile, "%d", &maxx );
   fscanf ( pFile, "%d", &maxy );

   /* Close the file. */
   fclose ( pFile );
   pFile = NULL ;

   /* Get the lat, lon of the southwest corner. */
   hrap_point.x = ( float ) xor;
   hrap_point.y = ( float ) yor;
   lat_lon_sw = HrapToLatLongMpe ( hrap_point );

   /* Get the lat, lon of the northwest corner. */
   hrap_point.y = ( float ) ( yor + maxy );
   lat_lon_nw = HrapToLatLongMpe ( hrap_point );

   /* Get the lat, lon of the northeast corner. */
   hrap_point.x = ( float ) ( xor + maxx );
   lat_lon_ne = HrapToLatLongMpe ( hrap_point );

   /* Get the lat, lon of the southeast corner. */
   hrap_point.y = ( float ) yor ;
   lat_lon_se = HrapToLatLongMpe ( hrap_point );

   /* Write the corners and number of points out to a text file which
      also resides in the coord file directory. */
   pFile = fopen ( pOutputFilename , "w" );

   if ( pFile == NULL )
   {
      fprintf ( stderr, "Could not open file %s for writing.\n" ,
                        pOutputFilename );
      return 1;
   }

   /* Added to check if this is for HPE.
   *
   *   If so, then get the grid factor and multiply the max x and y grid point values
   *   by the grid factor.  Default is by 4 but check to see if this is the normal HRAP.
   *   Note that setting may still cause a problem in displaying in AWIPS because
   *   localization uses the values here to reset the CDL file for the NetCDF files.
   *   So if a site really does want the normal HRAP grid, they will have to relocalize.
   *
   */
   if (strstr(pOutputFilename,"HPE") != NULL)
   {
      int value = HPE_GRID_FACTOR ;
      int len = strlen("hpe_hrap_grid_factor") ;
      int tokenlen = 0;
      int status = get_apps_defaults("hpe_hrap_grid_factor",&len,tokenvalue,&tokenlen) ;
       if( status == 0 && isdigit(tokenvalue[0]) !=0 )
	{
	  int itokenvalue = atoi(tokenvalue);
	  if (itokenvalue == 1 || itokenvalue == 4)
	  {
	     maxx *= itokenvalue;
	     maxy *= itokenvalue;
	  }
	  else
	  {
             maxx *= value;
             maxy *= value;
	  }
	}
	else
	{
	   maxx *= value;
	   maxy *= value;
	}
   }
   fprintf ( pFile, "Columns: %d\n" , maxx  ) ;
   fprintf ( pFile, "Rows: %d\n" , maxy ) ;
   fprintf ( pFile, "Southwest Corner: %7.5f %8.5f\n", lat_lon_sw.y ,
                                                     -1 * lat_lon_sw.x ) ;
   fprintf ( pFile, "Northwest Corner: %7.5f %8.5f\n", lat_lon_nw.y ,
                                                     -1 * lat_lon_nw.x ) ;
   fprintf ( pFile, "Northeast Corner: %7.5f %8.5f\n", lat_lon_ne.y ,
                                                     -1 * lat_lon_ne.x ) ;
   fprintf ( pFile, "Southeast Corner: %7.5f %8.5f\n", lat_lon_se.y ,
                                                     -1 * lat_lon_se.x ) ;
   fclose ( pFile ) ;

   pFile = NULL ;

   return 0 ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
