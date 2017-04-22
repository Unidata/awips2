/*******************************************************************************
* FILENAME:             grid_to_area.c
* NUMBER OF MODULES:    2
* GENERAL INFORMATION:
*   MODULE 1:           create_grid_to_area
* DESCRIPTION:          This routine relates each HRAP grid in a MPE forecast
*                       area to the county or basin it resides in.
*   MODULE 2:           main
* DESCRIPTION:          The main routine of the grid_to_area program.
*
* ORIGINAL AUTHOR:      Unknown
* CREATION DATE:        Unknown
* ORGANIZATION:         HSEB - OHD
* MACHINE:              For HP-UX and Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/2003      Bryon Lawrence    Major overhaul of code.
*          2        11/2003      Bryon Lawrence    Major overhaul of code.
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "DbmsAccess.h"
#include "GeneralUtil.h"
#include "geoutil.h"
#include "GetOS.h"
#include "overlay.h"
#include "stage3.h"

/* Macros. */
/* The USAGE Macro for the grid_to_area program. */
#define USAGE fprintf ( stderr , "\nUseage:\n"           \
    "    %s -d<name of hydrological database>\n"         \
    "       -t<type of boundary> - COUNTY or BASINS\n"   \
    "         <output filename>\n\n" , argv[0] ) ;       \

/* Used in the main routine to free dynamically allocated memory. */
#define FREE_DBNAME if ( database_name != NULL )  \
                    {                             \
                       free ( database_name ) ;   \
                       database_name = NULL ;     \
                    }                             \

/* User defined types. */
/* A linked list of these structures is created for all of the basins
   or counties in the GeoArea table in the IHFS database. */
typedef struct CharStruct {
     Node node ;
     char area_id [ BASIN_ID_LEN + 1 ] ;
     char area_name [ COUNTY_LEN + 1 ] ;
     List list ;
} CharStruct ;

/* This enumeration is used to determine whether the COUNTY or BASIN
   boundary type is being processed. */
enum FileType { BasinFile , CountyFile } ;

/*******************************************************************************
* MODULE NUMBER: 1
*
* MODULE NAME:   create_grid_to_area
* PURPOSE:       This routine relates each HRAP bin in a site MPE area
*                to either a county name or a basin name.  It writes these
*                results out to a file which is later read by Hydroview/MPE.
*                Hydroview/MPE uses this file to display the county and
*                basin located underneath the mouse pointer.
*
* ARGUMENTS:
*
*   TYPE   DATA TYPE     NAME             DESCRIPTION/UNITS
*          enum FileType file_type        Indicates if Counties or Basins are
*                                         being processed.
*          char *        area_type        The character string representation
*                                         of the boundary type being processed
*                                         either "COUNTY" or "BASIN".
*          char *        output_file_name The name (but not the path) of the
*                                         output put the HRAP to area
*                                         conversion in.
*          int           max_name_length  The maximum number of characters
*                                         a basin id or county name may have.
*                                         This is for string copy and
*                                         dynamic memory allocation purposes.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         status                      The return status of this routine.
*                                           0 = it worked.  1 = it failed.
* APIs UTILIZED:
*   NAME                    HEADER FILE   DESCRIPTION
*   free_area_linesegs_list geoutil.h     Frees memory used by the LineSegs
*                                         information.
*   get_apps_defaults       GeneralUtil.h Retrieves the value of a
*                                         token or an environmental variable.
*   get_area_linesegs       geoutil.h     Retrieves the LineSegs information
*                                         corresponding to all of the
*                                         Counties or Basins.
*   ListFirst               List.h        Lists the first node in a linked
*                                         list.
*   ListNext                List.h        Lists the next node in a linked
*                                         list.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME              DESCRIPTION
*   char []           empty_string      Contains a strin of null characters
*   char []           geoascii_dir      The directory of the coordinate file.
*   char []           geobin_dir        The directory of the WHFS geodata files.
*   char **           loc_area          Contains the mapping of HRAP bins
*                                       to county names or basin ids.
*   char []           output_file       Contains the path and name of the
*                                       file to write the overlay data to.
*   char *            pChar             Used to point to the name of the
*                                       county or basin currently being
*                                       processed.
*   char []           rfcid             The id of the site this program
*                                       is being run at.
*   CharStruct *      pCharHead         Pointer to the head of the linked list
*                                       of basin or county names.
*   CharStruct *      pCharNode         Pointer to a node in the linked list
*                                       of basin or county names.
*   CharStruct *      pCharTemp         Temporary pointer for allocating
*                                       memory for nodes in the linked list
*                                       of basin or county names.
*   FILE *            inFilePtr         FILE pointer to coordinate file.
*   FILE *            outFilePtr        FILE pointer to output grid_to_area
*                                       file.
*   GeoAreaLineSegs * pLineSegsHead     Pointer to the head of LineSegs
*                                       information.
*   GeoAreaLineSegs * pLineSegsNode     Pointer to a node in the LineSegs
*                                       linked list.
*   int               col               The column in the site's MPE HRAP grid
*                                       currently being worked on.
*   int               i                 Loop index.
*   int               jcol              Used to loop over the columns in a row
*                                       of linesegs data.
*   int               position          The position in the loc_area area
*                                       to write county name or basin id
*                                       information into.
*   int               response_length   Get_apps_defaults returns the
*                                       length of token response in this.
*   int               row               The row in the siter's MPE HRAP grid
*                                       currently being worked on.
*   int               status            Used to contain error codes.
*   int               token_length      Tells get_apps_defaults how long
*                                       a token name is.
*
* DATA FILES AND/OR DATABASE:
*
*    This routine reads the following file:
*    /awips/hydroapps/geo/host/ascii/coord_host.dat
*
*    This routine writes to the following files:
*    If processing Basins:
*
*    /awips/hydroapps/whfs/local/data/geo/grid_to_basin_hrap.OS
*
*    If processing Counties:

*    /awips/hydroapps/whfs/local/data/geo/grid_to_county_hrap.OS
*
*    Where OS is the two character identifier of the operating system this
*    program is being run on:  HP for HP-UX, LX for Linux.
*
*    This routine relies on the LineSegs and GeoArea tables in the IHFS
*         database table.
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*             0                             This routine worked
*             1                             This routine failed.  The
*                                           reason for this failure is
*                                           written out to the standard
*                                           error stream.
*
********************************************************************************
*/
static int create_grid_to_area ( enum FileType file_type ,
                                 char * area_type ,
                                 char * output_file_name ,
                                 int max_name_length )
{
   char empty_string [ COUNTY_LEN + 1 ] ;
   char geoascii_dir [ 200 ] ;
   char geobin_dir [ 200 ] ;
   char ** loc_area = NULL ;
   char output_file [ 200 ] ;
   char * pChar = NULL ;
   char rfcid [ 5 ] ;

   CharStruct * pCharHead = NULL ;
   CharStruct * pCharNode = NULL ;
   CharStruct * pCharTemp = NULL ;

   FILE * inFilePtr = NULL ;
   FILE * outFilePtr = NULL ;

   GeoAreaLineSegs * pLineSegsHead = NULL ;
   GeoAreaLineSegs * pLineSegsNode = NULL ;

   int col ;
   int i ;
   int jcol ;
   int position ;
   int response_length ;
   int row ;
   int status = 0 ;
   int token_length ;

   memset ( empty_string , '\0' , COUNTY_LEN + 1 ) ;

   /*  Create the path name for the coordinate file containing the
       HRAP grid information for this site's forecast area. */
   token_length = strlen ( "geo_st3_ascii" ) ;
   status = get_apps_defaults ( "geo_st3_ascii" , & token_length ,
                                geoascii_dir , & response_length ) ;

   if ( status != 0 )
   {
      fprintf ( stderr , "Error in 'create_grid_to_area':\n"
                         "Could not retrieve the value of the geo_st3_ascii\n"
                         "token.\n" ) ;
      return 1 ;
   }

   token_length = strlen ( "st3_rfc" ) ;
   status = get_apps_defaults ( "st3_rfc" , & token_length , rfcid ,
                                & response_length ) ;

   if ( status != 0 )
   {
      fprintf ( stderr , "Error in routine 'create_grid_to_area':\n"
                         "Could not retrieve the value of the st3_rfc\n"
                         "token.\n" ) ;
      return 1 ;
   }

   strcat ( geoascii_dir , "/coord_" ) ;
   strcat ( geoascii_dir , rfcid ) ;
   strcat ( geoascii_dir , ".dat" ) ;

   fprintf ( stdout , "In 'create_grid_to_area':\n"
                      "The geoascii_dir is %s\n" , geoascii_dir ) ;

   if ( ( inFilePtr = fopen ( geoascii_dir , "rb" ) ) == NULL )
   {
      fprintf ( stderr , "Error in 'create_grid_to_area':\n"
                         "Coord file under %s can not be opened.\n" ,
                         geoascii_dir ) ;
      return 1 ;
   }

   fscanf ( inFilePtr , "%d" , & XOR ) ;
   fscanf ( inFilePtr , "%d" , & YOR ) ;
   fscanf ( inFilePtr , "%d" , & MAXX ) ;
   fscanf ( inFilePtr , "%d" , & MAXY ) ;
   fclose ( inFilePtr ) ;
   inFilePtr = NULL ;

   /* Locate the dir of input files for gridbin program on HP and LX.
      Construct the dir for output files for gridbin program on HP and LX */
   token_length = strlen ( "whfs_geodata_dir" ) ;
   status = get_apps_defaults ( "whfs_geodata_dir" , & token_length ,
                                geobin_dir , & response_length ) ;

   if ( status != 0 )
   {
      fprintf ( stderr , "In 'create_grid_to_area':\n"
                         "Could not retrieve the value of the\n"
                         "whfs_geodata_dir token.\n" ) ;
      return 1 ;
   }

   strcpy ( output_file , geobin_dir ) ;
   strcat ( output_file , "/" ) ;
   strcat ( output_file , output_file_name ) ;
   printf ( "The output_file is %s\n" , output_file ) ;

   loc_area  = ( char ** ) malloc ( MAXX * MAXY * sizeof ( char * ) ) ;

   if  ( loc_area == NULL )
   {
      /* The attempt to allocate memory has failed. */
      fprintf ( stderr , "\nIn 'create_grid_to_area':\n"
                         "Could not allocate %d bytes of memory for\n"
                         "the loc_area array.\n" ,
                         MAXX * MAXY * sizeof ( char ** ) ) ;
      return 1 ;
   }

   for ( i = 0 ; i < MAXY * MAXX ; ++ i )
   {
      loc_area [ i ] = empty_string ;
   }

   /* Open file for output grid_to_basin_hrap.OS */
   if ( ( outFilePtr = fopen ( output_file , "wb" ) ) == NULL )
   {
      fprintf ( stderr , "In 'create_grid_to_area':\n"
                         "Output_file %s not found.\n" , output_file ) ;
      exit ( 1 ) ;
   }

   /* Load the LineSegs information for the user specified area type. */
   pLineSegsHead = ( GeoAreaLineSegs * )
                   get_area_linesegs ( area_type ,
                                       sizeof ( GeoAreaLineSegs ) ) ;

   if ( pLineSegsHead == NULL )
   {
      fprintf ( stderr , "In 'create_grid_to_area':\n"
                         "Could not retrieve LineSegs information for\n"
                         "the %s area type. Cannot produce the grid_to_area\n"
                         "conversion for %s.\n" , area_type , area_type ) ;
      return 1 ;
   }

   /* Walk through the linked list */
   pLineSegsNode = ( GeoAreaLineSegs * ) ListFirst ( & pLineSegsHead->list ) ;

   while ( pLineSegsNode != NULL )
   {

      pCharTemp = ( CharStruct * ) malloc ( sizeof ( CharStruct ) ) ;

      if ( pCharTemp == NULL )
      {
         fprintf ( stderr , "\nIn 'create_grid_to_area':\n"
                            "Count not allocate memory for a node\n"
                            "in CharStruct linked list.\n" ) ;
         return 1 ;
      }

      if ( file_type == BasinFile )
      {
         memset ( pCharTemp->area_id , '\0' , BASIN_ID_LEN + 1 ) ;
         strncpy ( pCharTemp->area_id , pLineSegsNode->area_id ,
                   BASIN_ID_LEN ) ;
         pChar = pCharTemp->area_id ;
      }
      else
      {
         memset ( pCharTemp->area_name , '\0' , COUNTY_LEN + 1 ) ;
         strncpy ( pCharTemp->area_name , pLineSegsNode->name , COUNTY_LEN ) ;
         pChar = pCharTemp->area_name ;
      }

      if ( pCharHead == NULL )
      {
         pCharHead = pCharTemp ;
         ListInit ( & pCharHead->list ) ;
      }

      ListAdd ( & pCharHead->list , & pCharTemp->node ) ;

      /* Loop on the number of rows for this area type. */
      for ( i = 0 ; i < pLineSegsNode->numrows ; ++ i )
      {
         for ( jcol = pLineSegsNode->beg_cols [ i ] ;
               jcol <= pLineSegsNode->end_cols [ i ] ; ++ jcol )
         {
            row = pLineSegsNode->rows [ i ] - YOR ;
            col = jcol - XOR ;

            /* Check to make sure the box is within the site's area. */
            if ( ( row < MAXY ) &&
                 ( col < MAXX ) &&
                 ( row >= 0 ) && ( col >= 0 ) )
            {
               /* position = row * MAXX + col   ;  */
               /* position = ( MAXY - row ) * MAXX + col   ;  */
               position = ( col * MAXY ) + row ;
               * ( loc_area + position ) = pChar ;
            }

         }
      }

      pLineSegsNode = ( GeoAreaLineSegs * )
                      ListNext ( & pLineSegsNode->node ) ;
   }

   /* Free the LineSegs information. */
   free_area_linesegs_list ( ( void * ) pLineSegsHead ) ;

   /* Write the grid information to the output file. */
   for ( i = 0 ; i < MAXY * MAXX ; ++ i )
   {
      fwrite ( loc_area [ i ] , ( size_t ) max_name_length , ( size_t ) 1 ,
               outFilePtr ) ;
   }

   fclose ( outFilePtr ) ;
   outFilePtr = NULL ;

   /* Free the linked list of CharStructs. */
   pCharNode = pCharHead ;

   while ( pCharNode != NULL )
   {
      pCharTemp = ( CharStruct * ) ListNext ( & pCharNode->node ) ;
      free ( pCharNode ) ;
      pCharNode = pCharTemp ;
   }

   pCharHead = NULL ;

   free ( loc_area ) ;
   loc_area = NULL ;

   return 0 ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   main
* PURPOSE:       This is the main program for the grid_to_area executable.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME             DESCRIPTION/UNITS
*   input  int           argc             The number of command line arguments.
*   input  char **       argv             An array of command line arguments.
*
* RETURNS:
*   DATA TYPE   NAME                      DESCRIPTION
*   int         status                    Indicates to the operating system
*                                         if this program worked or not.
*
* APIs UTILIZED:
*    NAME                  HEADER FILE    DESCRIPTION
*    CloseDbms             DbmsAccess.h   Closes a connection to the database.
*    create_grid_to_area   N/A		  Associates HRAP grid bins to county
*                                         and basin names.
*    OpenDbms              DbmsAccess.h   Opens a connection to the database.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
int grid_to_area_main ( int argc , const char ** argv)
{
   char * database_name = NULL ;
   char area_type [ 10 ] ;
   extern char * optarg ;

   enum FileType file_type = BasinFile ;

   int c ;
   int database = 0 ;
   int max_name_length = 0 ;
   extern int optind , optopt ;
   int status = 0 ;
   int string_length ;
   int type = 0 ;

   memset ( area_type , '\0' , 10 ) ;

   /* Process any command line options.  The user must supply the -d and -t
      options. */
   while ( ( c = getopt ( argc , argv, ":d:t:" ) ) != -1 )
   {
      switch ( c )
      {
         case 'd' :

            /* Try to open the database. */
            string_length = strlen ( optarg ) ;
            database_name = ( char * ) malloc ( string_length * sizeof ( char )
                                                + 1 ) ;
            if ( database_name == NULL )
            {
               fprintf ( stderr , "In program '%s':\n"
                                  "Could not allocate %d bytes for\n"
                                  "the database_name array.\n" ,
                                  argv[0]  , string_length + 1 ) ;
               status = 1 ;
            }

            memset ( database_name , '\0' , string_length + 1 ) ;
            strcpy ( database_name , optarg ) ;
            database = 1 ;
            break ;

         case 't' :

            /* Set the type of area being processed. */
            if ( strcmp ( optarg , "BASIN" ) == 0 )
            {
               file_type = BasinFile ;
               max_name_length = BASIN_ID_LEN + 1 ;
               strcpy ( area_type , "BASIN" ) ;
            }
            else if ( strcmp ( optarg , "COUNTY" ) == 0 )
            {
               file_type = CountyFile ;
               max_name_length = COUNTY_LEN + 1 ;
               strcpy ( area_type , "COUNTY" ) ;
            }
            else
            {
               status = 1 ;
            }

            type = 1 ;
            break ;

         case ':' :

            status = 1 ;
            break ;

         case '?' :

            status = 1 ;
            break ;

         default :

            status = 1 ;
            break ;
      }
   }

   if ( ( status != 0 ) || ( optind != ( argc - 1 ) ) || ( database == 0 ) ||
        ( type == 0 ) )
   {
      USAGE ;
      FREE_DBNAME ;
      return 1 ;
   }

   /* Attempt to open the database. */
   status = OpenDbms ( database_name ) ;

   if ( status != 0 )
   {
      fprintf ( stderr , "\nIn program '%s':\n"
                         "Could not open database %s.\n" ,
                         argv[0] , database_name ) ;
      FREE_DBNAME ;
      return 1 ;
   }

   status = create_grid_to_area ( file_type , area_type , argv [ optind ] ,
                                  max_name_length ) ;

   FREE_DBNAME ;
   CloseDbms ( ) ;

   return 0 ;
}

/* The following block of code has been commented out due to a
   paradigm shift in how the grid_to_area program computes which Basins and
   Counties the bins in the HRAP grid belong to. BryonL 11/14/03. */

/* for ( i = 0 ; i < MAXX ; i++ )
{
      for ( j = 0 ; j < MAXY ; j++ )
      {
         memset ( loc_area [ j ] [ i ] , '\0' , max_name_length ) ;

         for ( k = 0 ; k < nummap ; k++ )
	 {
	    x = i + XOR + 0.5 ;
	    y = j + YOR + 0.5 ;
	    printf("The num of basin is %d and the basin name is %s\n",
	       k,mapbasin[k]->id);
	    in = InOut(x,y,k,mapbasin);

	    if (in == 1)
	    {
	       strcpy(loc_area[j][i],mapbasin[k]->id);
	       break;
	    }
	 }

	 fwrite(loc_area[j][i],max_name_length*sizeof(char),1,output_basin);

      }
   }
} */


/*End of main*/

/* Function of Inout  */

int InOut(x,y,j,data)
      float x,y;
      int j;
      overlay_struct **data;

   {
   int in,nc,i,ii;
   double a,b;

   in = 0;
   nc = 0;

   for (i=0; i < data[j]->npts; i++)
   {
      ii = i - 1 ;

      if (ii < 0) ii = data[j]->npts-1;

      if ((x >= data[j]->hrap[i].x && x < data[j]->hrap[ii].x ) ||
       	    ( x < data[j]->hrap[i].x && x >= data[j]->hrap[ii].x ))
      {
          b = (double) (data[j]->hrap[i].y - data[j]->hrap[ii].y) /  \
		      (double)(data[j]->hrap[i].x - data[j]->hrap[ii].x);
	  a = (double) data[j]->hrap[i].y - b * (double) data[j]->hrap[i].x;
	  if (y <= a + b*x)
	     nc++;
      }
   }

   if (nc > 2*(nc/2)) in = 1;

   return in;
}


/* function for read_overlay_data - not used anymore. */

#if 0

overlay_struct  **read_overlay_data(int *nc, FILE *file)
{
   int             n;
   int             i = 1;
   char            temp[9];
   overlay_struct  **data;


   data = (overlay_struct **) malloc(sizeof(overlay_struct *));
   memset(temp, '\0', 9);

   while( (n = fread(temp, sizeof(char), 9, file)) != 0)
   {
      data[i - 1] = (overlay_struct *) malloc(sizeof(overlay_struct));
      strncpy(data[i - 1]->id, temp, 9);

      memset(data[i - 1]->name, '\0', COUNTY_LEN + 1 );
      n = fread(data[i - 1]->name, sizeof(char), 21, file);

      n = fread(&data[i - 1]->order, sizeof(int), 1, file);
      n = fread(&data[i - 1]->npts, sizeof(int), 1, file);

      data[i - 1]->hrap = (HRAP *) malloc(data[i - 1]->npts * sizeof(HRAP));
      n = fread(data[i - 1]->hrap, sizeof(HRAP), data[i - 1]->npts, file);

      i++;
      data = (overlay_struct **) realloc(data, sizeof(overlay_struct *) * i);
      memset(temp, '\0', 9);
   }

   *nc = i - 1;
   fclose(file);
   return (data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

#endif
