/****************************************************************************
                                                                               
 Function:  convert_ascii_to_latlon.c
 Purpose:   Reads the ascii file and writes to a binary file
	    in a lat/lon format.
 Written by:  Moria Shebsovich July 16, 2003
 Modified by: Bryon Lawrence, January 8, 2004
              Removed the use of fscanf to retrieve lat/lon pairs.
              Replaced it with the fgets routine to read each record
              containing the latitude/longitude pairs.  Sscanf is used
              to get the latitude/longitude pairs from these lat/lon
              records.

***************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "convert_ascii_to_latlon.h"
#include "DbmsDefs.h"
#include "geo_header_file.h"
#include "geo_log.h"
#include "stage3.h"

/******************************************************************************
                                                                                 
 Function:  convert_ascii_to_latlon()                            
                                                                                
   ***************************************************************************/

void convert_ascii_to_latlon ( char * ascii_fname ,
			       char * bin_fname )
{
   char header_string [ MAX_RECORD_LEN ] ;
   char  id [ LOC_ID_LEN + 1 ] ;
   char latlon_string [ MAX_RECORD_LEN ] ;
   char msgstr [ MAX_RECORD_LEN ] ;
   char  name [ LOC_NAME_LEN + 1 ] ;
   char * pFile = NULL ;
   double centroid_lat ;
   double centroid_lon ;
   FILE  * infile = NULL , * log_file = stdout , * outfile = NULL ;
   float xlat,xlon;
   HRAP  hrap[40000];
   int geotable = GEOAREA ;
   int i, linenum = 0 , order, npts;
   int len_id = 9 ;
   int len_name = 21 ;
   int save_data_block ;
   
   /* open the files */
   infile  = fopen ( ascii_fname , "r" ) ;

   if ( infile == NULL )
   {
      printf ( "\nError opening \"infile\", filename: %s.\n" , ascii_fname ) ;
      exit ( 1 ) ;
   }

   outfile = fopen ( bin_fname , "wb" ) ;

   if ( outfile == NULL )
   {
      printf ( "\nError opening \"outfile\", filename: %s.\n" , bin_fname ) ;
      fclose ( infile ) ;
      exit ( 1 ) ;
   }
   
   memset ( id,   '\0', LOC_ID_LEN + 1 ) ;
   memset ( name, '\0', LOC_AREANAME_LEN + 1 ) ;
   
   /* loop on reading the area headers until end of file */
   pFile = fgets ( header_string , MAX_RECORD_LEN , infile ) ;
   
   while ( pFile != NULL )
   {
      save_data_block = 1 ;
      ++ linenum ;
      parse_geo_header ( header_string ,
                         infile ,
                         log_file ,
                         linenum ,
                         geotable ,
                         & save_data_block ,
                         id , 
                         name ,
                         & npts ,
                         & order ,
                         & centroid_lat ,
                         & centroid_lon ) ;
                          
      /* read the points for the area and convert to hrap */
      for ( i = 0 ; i < npts ; ++ i )
      {
         pFile = fgets ( latlon_string , MAX_RECORD_LEN , infile ) ;
         sscanf ( latlon_string , "%f %f" , & xlat, & xlon ) ;
         ++ linenum ;
	 hrap[i].x = xlat ;
         hrap[i].y = xlon ;
      }
      
      if ( save_data_block == 1 )
      {
         /* write the binary data */
         if ( strlen ( id ) > len_id )
         {
            sprintf ( msgstr , "\nWARNING: The identifier, %s ,  is too long.\n"
                               "It will be truncated to %d characters when\n"
                               "written to output file %s.\n" , id ,
                                len_id , bin_fname ) ; 
            log_geomsg ( msgstr , log_file ) ;
         }

         if ( strlen ( name ) > len_name )
         {
            sprintf ( msgstr, "\nWARNING: The name, %s , is too long.\n"
                              "It will be truncated to %d characters when\n"
                              "written to output file %s.\n" , name ,
                              len_name , bin_fname ) ;
            log_geomsg ( msgstr , log_file ) ;
         }

         fwrite(id,     sizeof(char),  len_id,   outfile);
         fwrite(name,   sizeof(char),  len_name, outfile);
         fwrite(&order, sizeof(int),   1,        outfile);
         fwrite(&npts,  sizeof(int),   1,        outfile);
         fwrite(hrap,   sizeof(HRAP),   npts,    outfile);
      }
      else
      {
         sprintf ( msgstr , "\nWARNING: An error was encountered while\n"
                          "parsing the header for geoarea id: %s name: %s.\n"
                          "It will not be included in the output file %s.\n" , 
                          id , name , bin_fname ) ;
         log_geomsg ( msgstr , log_file ) ;
      }
      
      /* reset these values */
      memset ( id ,   '\0', LOC_ID_LEN + 1 ) ;
      memset ( name , '\0', LOC_AREANAME_LEN + 1 ) ;

      /* Retrieve the next header. */ 
      pFile = fgets ( header_string , MAX_RECORD_LEN , infile ) ;
   }

   /* Close the files. */
   if ( infile != NULL )
   {
      fclose ( infile ) ;
      infile = NULL ;
   }

   if ( outfile != NULL )
   {
      fclose ( outfile ) ;
      outfile = NULL ;
   } 
   
   return;
}
