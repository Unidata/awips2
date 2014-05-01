/*******************************************************************************
* FILENAME:             load_geodata.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           load_geodata
* DESCRIPTION:          Reads geodata files and loads their contents into
*                       the IHFS database GeoArea table.
*
* ORIGINAL AUTHOR:      Mark Glaudemans
* CREATION DATE:        Unknown
* ORGANIZATION:         HSEB/OHD
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        1/12/2004    Bryon Lawrence    Updated to use sscanf to
*                                                  read the latitude/longitude
*                                                  pairs from the data file
*                                                  instead of doing complicated
*                                                  string manipulations to
*                                                  parse this information. 
*                                                  Added documentation blocks.
*          1        10/8/2004    Moria Shebsovich  Updated to read geo data
*                                                  from ascii files and load the*                                                  GeoArea table. 
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "geo_dbutil.h"
#include "geo_header_file.h"
#include "geo_log.h"
#include "geoutil.h"

#include "List.h"
/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    load_geoarea
* PURPOSE:        Reads whfs geodata files and loads their contents into
*                 the IHFS database GeoArea table.
*      
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME     DESCRIPTION/UNITS
*   Input  FILE	*      infile   The name of the geodata file to load the data
*                               from.
*   Input  char	*      geotype  The type of the geoarea or geoline to be loaded.
*                               For geoareas this may be:
*                                  COUNTY,STATE,RESRVR,ZONE,BASIN
*                               For geolines this may be:
*                                  ROAD, STREAM
*
*   Input  int         rank      The feature rank.  This allows geodata
*                                features to be selectively displayed.
*   Input  int         geotable  This may be GEOAREA for geoarea data or
*                                GEOLINE for geoline data.
*
* RETURNS:
*   DATA TYPE     DESCRIPTION
*   int           Always return 0.  Not very useful.
*
* APIs UTILIZED:
*   NAME              HEADER FILE          DESCRIPTION
*   exit_geoutil      geo_header.h         Closes the data file and database
*                                          and exits the program.
*   log_geomsg        geo_log.h            Logs a message to a user-specified
*                                          log file.  If the supplied file
*                                          pointer is NULL, then the message
*                                          is logged to standard error. 
*   parse_geo_header  geo_header.h         Tokenizes the geodata block header
*                                          and parses them to determine
*                                          their values.
*   put_geoarea       geo_dbutil.h         Writes geoarea data to the IHFS
*                                          database GeoArea table.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME               DESCRIPTION
*   GeoAreaData                   Structure to contain geo information received
*                                 from the input ascii files. 
*   char       buf [ ]            Contains a file record read with
*                                 fgets ( ).
*   char *     fgets_ptr          Contains a pointer to the file record
*                                 read with fgets ( ).
*   char       id [ ]             Contains the id parsed from the geodata
*                                 block header.
*   char       msgstr [ ]         Messages sent on to the log stream 
*                                 are constructed in this array.
*   char       name [ ]           The geoarea or geoline name parsed
*                                 from the geodata block header.
*   double     intlat             The centroid latitude.
*   double     intlon             The centroid longitude.
*   double     lat [ ]            Array of latitudes corresponding to geodata
*                                 latitude/longitude pairs.
*   double     lon [ ]            Array of longitudes corresponding to geoline
*                                 latitude/longitude pairs.
*   FILE *     log_file           A pointer to the FILE structure representing
*                                 the log file.
*   int	       i                  A counter variable.
*   int	       linenum            Used to keep track of the line in the
*                                 file currently being read.
*   int        lines_in_block     Used to keep track of the number of lines
*                                 in a geoarea/geoline block in the data file.
*   int	       npts               The number of latitude/longitude pairs
*                                 defining a geoarea/geoline geo feature. 
*   int        order              The ranking feature which allows
*                                 certain geoarea or geoline objects to
*                                 have higher precedence over other objects.
*                                 Useful in displaying increasing amounts of
*                                 detail.
*   int        numblock           The number geodata blocks read so far.
*   int        numdups            The number of duplicate records found when
*                                 trying to insert into the IHFS GeoArea or
*                                 GeoLine table.
*   int	       numignore          The number of blocks of data ignored
*                                 due to errors.
*   int        num_items_read     Used to receive error conditions from the
*                                 sscanf routine.
*   int	       save_data_block    Indicates whether or not the block of
*                                 geodata just read should be saved.  If an
*                                 error was encountered while reading the
*                                 data block, then it will not be saved into
*                                 IHFS database.
*   int	       status             Contains the status of a call to put_geoarea.
*
* DATA FILES AND/OR DATABASE:
*   Requires a valid geodata in which the data is organized as described above.
*   For WHFS, the geodata files are normally located under 
*   ../whfs/local/data/geo and end in the extension  *.dat.
*
*   Requires the IHFS database which contains the GeoArea table.
*
* ERROR HANDLING:
*   This routine will exit if an unexpected end of file is encountered
*   while reading latitude/longitude pairs from the data file.
*
*   A block of data will be rejected for saving into the database (GeoArea
*   table) if an error is encountered reading one or more of the
*   latitude/longitude pairs.
********************************************************************************
*/
   
int load_geodata(FILE	* infile ,
		 char	* geotype ,
		 int	rank ,
		 int	geotable )
{
   char	id[LOC_ID_LEN + 1] ;
   char msgstr[240] ;
   FILE * log_file = stdout ;
   int	linenum ;
   int  numblock ;
   int  numdups ;
   int	numignore ;
   int	status = 0 ;

   
   GeoAreaData * pGeoAreaDataHead = NULL ;
   GeoAreaData * pGeoAreaDataNode = NULL ;

   /* Log_file is set to stdout so that all log information is written
      to stdout.  This was done so that the output of this routine would
      be captured by the process_geoarea and process_geoline scripts and
      placed into their respective log files. */
   
   /* initialize */
   linenum = 0;
   numblock = numignore = numdups = 0;
      
   /* Loop on reading the file records.
      Assume the first record is a block header record, which gives 
      the number of points that follow.  then read the number of records
      defined by the number of paired points.  then assume the next record
      is a block header record.  repeat until the end of the file */
      
      pGeoAreaDataHead = read_geodata(infile, geotype, rank, GEOAREA);
      if (pGeoAreaDataHead == NULL)
      {
         sprintf(msgstr,
                    "ERROR: Could not read data for geotype %s.\n", geotype); 
         log_geomsg(msgstr, log_file);
      } 
      pGeoAreaDataNode = (GeoAreaData * ) ListFirst ( &pGeoAreaDataHead->list); 
      
      while (pGeoAreaDataNode != NULL)
      {
         /* load the data into the database if good data */
         if (pGeoAreaDataNode->save_data_block == 1 )
         {
	    numblock++;
	 
	    if ( geotable == GEOAREA )
            { 
	       status = put_geoarea(pGeoAreaDataNode); 
            }
                     
	    if (status == DUPLICATE_IGNORED)
	    {
	       sprintf(msgstr,
		    "ERROR: Ignored duplicate record for %s.  "
		    "Make keys unique!\n", id);
	       numdups++;
	    
	       log_geomsg(msgstr, log_file);
	    }
	    else if (status == INSERT_FAILED)
	    {
	       sprintf(msgstr,
		    "ERROR: Database write failed for %s.\n", id);	    
	       log_geomsg(msgstr, log_file);
	    }
         }
      
         else
         {
	    sprintf( msgstr ,
		  "ERROR: Discarding block of data for id %s due to "
                  "errors.\n" , id ) ;
	    log_geomsg(msgstr, log_file); 
	    numignore++;
         } 
      
      pGeoAreaDataNode = (GeoAreaData * ) ListNext ( &pGeoAreaDataNode->node); 
   
   } /* end while */

   if (geotable == GEOAREA)
   {
      sprintf(msgstr, 
	      "Program completed.\n  Processed %d blocks.\n"
	      "  Discarded/Duplicate blocks: % d / %d\n",
	      numblock, numignore, numdups);
      log_geomsg(msgstr, log_file);
   }	      
   freeGeoAreaData (& pGeoAreaDataHead) ;
 
   return 0 ;
}
