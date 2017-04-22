/*******************************************************************************
* FILENAME:             read_geodata.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           read_geodata
* DESCRIPTION:          Reads geodata files and loads their contents into
*                       the geoAreaData structure.
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
*          1        10/8/2004    Moria Shebsovich  Updated to read from ascii 
*						   files, to save geo data in 
*						   the GeoAreaData internal 
*						   structure, and to load 
*						   geoarea data to the IHFS 
*						   GeoArea table.
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "geo_header_file.h"
#include "geo_log.h"
#include "geoutil.h"
#include "List.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    read_geoarea
* PURPOSE:        Reads whfs geodata files and loads their contents into
*                 the GeoAreaData structure. The data from the GeoAreaData 
*                 structure may be loaded to the GeoArea or LineSegs IHFS 
*                 table or used to create binary files. 
*      
*                 The geodata files contain latitude/longitude pairs defining
*                 geoarea polygons (such as basins or zones) or geoline
*                 vectors (such as rivers or streams).  Each geoarea polygon
*                 or geoline vector is represented by a block of data in 
*                 the geodata file. Each block of data has the following
*                 format:
*
*            <id> <name> <feature rank> <numpoints> [center lat] [center lon]
*            <lat> <lon>
*            <lat> <lon> 
*            <lat> <lon>
*            <lat> <lon>
*            <lat> <lon>
*            ...   ...
*            <lat> <lon>
*
*            where id is the 1-8 character id of the geoarea or geoline
*                  name is the name of the geoarea or geoline. It may be
*                       up to 20 characters long for a geoline and up to 40 
*                       characters long for a geoarea.
*                  feature rank is the order of the geoarea or geoline.
*                      This allows geographic features to be displayed 
*                      according to relative importance.  Lower numbers 
*                      take precedence over higher numbers.
*                  numpoints is the number of latitude/longitude pairs
*                      defining the geoarea or geoline.
*                  center lat is the centroid latitude. This applies only to
*                      geoarea polygons.
*                  center lon is the centroid longitude.  This applies only to
*                      geoarea polygons.
*                  
*            An example of a block of data in a geoarea data file containing 
*            basin data:
*
*    ALTO2 XXX -1 91 35.03123 99.37381
*    34.8585000 99.2419000
*    34.8585000 99.2752000
*    34.8668000 99.2752000
*    34.8668000 99.2919000
*    34.8835000 99.2919000
*    ...        ...
*    34.9002000 99.2502000
*    34.9002000 99.2419000
*    34.8585000 99.2419000
*
*            An example of a block of data in a geoline data file containing
*            river data:
*
*    UNKNOWN  NAME XXX -1 22
*    37.0450000 97.0060000
*    37.0360000 97.0100000
*    37.0330000 97.0210000
*    37.0390000 97.0280000
*    37.0390000 97.0390000
*    37.0420000 97.0510000
*    ...        ...
*    37.1330000 97.1480000
*    37.1420000 97.1470000
*    37.1430000 97.1470000
*
*    If a block of data is successfully read from a geoarea or geoline data
*    file, then it is stored GeoAreaData to be loaded into the GeoArea table 
*    for geoarea data or converted to the binary files for geoline data.
*
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
*   Input  int         geotable  This now is GEOAREA for geoarea data.
*
* RETURNS:
*   DATA TYPE     DESCRIPTION
*
* APIs UTILIZED:
*   NAME              HEADER FILE          DESCRIPTION
*   exit_geoutil      geo_header.h         Closes the data file and database
*                                          and exits the program.
*   exit_geoutil_file geo_header.h         Closes the data file and exits 
*                                          the program.
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
*   GeoAreaData                   Structure to hold geo data received from
*                                 the  input ascii files.
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

GeoAreaData * read_geodata(FILE	* infile ,
		 char	* geotype ,
		 int	rank ,
		 int	geotable )
{
   char buf [ MAX_RECORD_LEN ] ;
   char * fgets_ptr = NULL ;
   char	id[LOC_ID_LEN + 1] ;
   char msgstr[240] ;
   char name[LOC_AREANAME_LEN + 1] ;
   double intlat , intlon ;
   double *lat = NULL ;
   double * lon = NULL ;
   FILE * log_file = stdout ;
   int	i ;
   int	linenum , lines_in_block ;
   int	npts , order ;
   int  num_items_read ;
   int save_data_block ;
 
   GeoAreaData * pGeoAreaDataHead = NULL;
   GeoAreaData * pGeoAreaDataNode = NULL;
 
   /* Read the string and strip off any leading and trailing blanks 
      and compress consecutive blanks */
   fgets_ptr = fgets ( buf , MAX_RECORD_LEN , infile ) ;
   
   linenum = 0 ;
    
   while ( fgets_ptr != NULL )
   {
      /* initialize to save the data.  if error found that
	 tarnishes the data, reset variable so reading
	 will continue but the data block won't be saved */
      save_data_block = 1 ;
      linenum ++ ;

      parse_geo_header ( buf ,
                         infile ,
                         log_file ,
                         linenum ,
                         geotable ,
                         & save_data_block ,
                         id ,
                         name ,
                         & npts ,
                         & order ,
                         & intlat ,
                         & intlon ) ;
      
      /*-------------------------------------------*/
      /* now read the lat-lon pairs for this block.
         allow 0 number of point pairs to be processed */
      lines_in_block = 0 ;
            
      /* Dynamically allocated memory for lat/lon arrays. */
      lon = (double *) malloc (sizeof(double) * npts);
      if(lon == NULL)
      {
         sprintf(msgstr, "\nIn 'read_geodata':\n"
                 "Could not allocate memory for the longitude array\n");
         log_geomsg ( msgstr , log_file ) ;
         exit(1);
      }

      lat = (double *) malloc (sizeof(double) * npts);
      if(lat == NULL)
      {
         sprintf(msgstr, "\nIn 'read_geodata':\n"
                 "Could not allocate memory for the latitude array\n");
         log_geomsg ( msgstr , log_file ) ;
         exit(1);
      }

      for ( i = 0 ; i < npts ; ++ i )
      {
         memset( & buf , '\0' , MAX_RECORD_LEN ) ;
         fgets_ptr = fgets ( buf , MAX_RECORD_LEN , infile ) ;

	 if ( fgets_ptr == NULL )
	 {
	    sprintf ( msgstr ,
		      "ERROR: Unexpected end-of-file reached after line %d\n",
		      linenum ) ;
	    log_geomsg ( msgstr , log_file ) ;
	    fclose(infile);
            exit(1);
	 }
	 else	    
         {
	    linenum++;
         }
	 
         
         /* Extract the latitude and longitude values. */
         num_items_read = sscanf ( buf , "%lf %lf" , & lat [ lines_in_block ] ,
                                   & lon [ lines_in_block ] ) ;
         
         if ( num_items_read != 2 || num_items_read == EOF )
	 {
	    sprintf ( msgstr , "ERROR finding a latitude/longitude pair\n"
                               "for id %s in line %d: %s", id, linenum, buf);
	    log_geomsg(msgstr, log_file);
	    sprintf(msgstr, " (line %d of block)\n", lines_in_block + 1);
	    log_geomsg(msgstr, log_file);
	    
	    save_data_block = 0;
	 }
         else
         {

            /* Changed for GUAM by Mark on 8/24/2000
               Test the bounds of the longitude value.
	    if (lon[lines_in_block] <   0.  ||
	        lon[lines_in_block] > 180.) */
	     
	    if (lon[lines_in_block] < -180.  ||
	        lon[lines_in_block] > 180.)
	    {
	       sprintf(msgstr,
		       "ERROR reading or invalid lon for id %s in line %d: %s",
		        id, linenum, buf);
	       log_geomsg(msgstr, log_file);
	       sprintf(msgstr, " (line %d of block)\n", lines_in_block + 1);
	       log_geomsg(msgstr, log_file);
	       save_data_block = 0;
	    }
	 
	 
	    /* Test the bounds of the latitude value */
	    if (lat[lines_in_block] <  0. ||
	        lat[lines_in_block] > 90.)
	    {
	       sprintf(msgstr,
	   	       "ERROR reading or invalid lat for id %s in line %d: %s",
		       id, linenum, buf);
	       log_geomsg(msgstr, log_file);
	       sprintf(msgstr, " (line %d of block)\n", lines_in_block + 1);
	       log_geomsg(msgstr, log_file);
	    
	       save_data_block = 0;
	    }

         }

	 lines_in_block ++ ;
      }

      pGeoAreaDataNode = (GeoAreaData*) malloc (sizeof(GeoAreaData));
      
      if(pGeoAreaDataNode == NULL)
      {
         sprintf(msgstr, "\nIn 'read_geodata':\n"
                 "Could not allocate memory for a  GeoAreaData node\n");
         log_geomsg ( msgstr , log_file ) ;
         exit(1);
      }
      else
      { 
         strcpy(pGeoAreaDataNode->area_id, id);
         strcpy(pGeoAreaDataNode->name, name);
         strcpy(pGeoAreaDataNode->boundary_type,  geotype);
         pGeoAreaDataNode->interior_lat = intlat;
         pGeoAreaDataNode->interior_lon = intlon;
         pGeoAreaDataNode->lon = lon;
         pGeoAreaDataNode->lat = lat;
         pGeoAreaDataNode->npts = npts ;
	 pGeoAreaDataNode->save_data_block = save_data_block;
      } 

      if(pGeoAreaDataHead == NULL)
      {
         pGeoAreaDataHead = pGeoAreaDataNode;
         ListInit ( & pGeoAreaDataHead->list);
      }
      
      ListAdd(& pGeoAreaDataHead->list, & pGeoAreaDataNode->node);
   
      /* get the next block */

      fgets_ptr = fgets(buf, MAX_RECORD_LEN, infile);
   }  /* end of fgets_ptr while block */
  
   return pGeoAreaDataHead ;
}

void freeGeoAreaData (GeoAreaData  ** pGeoAreaDataHead )
{
   GeoAreaData * pGeoAreaDataNode = NULL ;
   GeoAreaData * nextPtr = NULL ;

   pGeoAreaDataNode = * pGeoAreaDataHead ;
       
   while ( pGeoAreaDataNode != NULL )
   {
       nextPtr = ( GeoAreaData * ) ListNext ( & pGeoAreaDataNode->node ) ;
             
       if ( pGeoAreaDataNode->lon != NULL )
       {
           free ( pGeoAreaDataNode->lon ) ;
           pGeoAreaDataNode->lon = NULL ;
       }
                
       if ( pGeoAreaDataNode->lat != NULL )
       {
           free ( pGeoAreaDataNode->lat ) ;
           pGeoAreaDataNode->lat = NULL ;
       }

       free ( pGeoAreaDataNode ) ;
       pGeoAreaDataNode = nextPtr ;
   }
   * pGeoAreaDataHead = NULL ;
}

