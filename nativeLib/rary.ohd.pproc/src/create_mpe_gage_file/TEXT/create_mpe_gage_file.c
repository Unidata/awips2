
/*******************************************************************************
* FILENAME:           create_gage_file.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:    Bryon Lawrence
* CREATION DATE:      August 8, 2005
* ORGANIZATION:       OHD11/HSEB
* MACHINE:            Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "List.h"
#include "Location.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
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
#define FREE_MEMORY if ( pDBname != NULL )             \
                    {                                  \
                       free ( pDBname );               \
                       pDBname = NULL;                 \
                    }                                  \
                                                       \
                    if ( pPath != NULL )               \
                    {                                  \
                       free ( pPath );                 \
                       pPath = NULL;                   \
                    }                                  \
                                                       \
                    if ( pLocationHead != NULL )       \
                    {                                  \
                       FreeLocation ( pLocationHead ); \
                       pLocationHead = NULL;           \
                    }                                  \

#define USAGE printf ( "Usage:\n"                                            \
                       " create_gage_file -d[name of hydrological database] "\
                       " <station file path>\n" );                           \

static Location * get_gage_data ( )
{
   Location * pLocationHead = NULL;

   const static char * gage_query = "WHERE lat IS NOT NULL "
                                    "AND lon IS NOT NULL "
                                    "AND lid IN ( SELECT lid "
                                    "             FROM IngestFilter i "
                                    "             WHERE i.pe IN ( 'PC', 'PP') "
                                    "             AND i.stg2_input='T' ) "
                                    "ORDER BY lid asc;";
   pLocationHead = GetLocation ( gage_query );

   return pLocationHead;
}

static void  write_gage_data ( const char * path,
                               const Location * pLocationHead )
{
   bool is_slash = false;
   const char * filename = "mpe_gage_locations";
   char * pFilePath = NULL;
   FILE * pFile = NULL;
   int filename_len;
   int gage_count;
   int path_len;
   int filepath_len;
   Location * pLocationNode = NULL;

   if ( path == NULL || pLocationHead == NULL ) return;

   gage_count = ListCount ( ( List * ) & pLocationHead->list );

   if ( gage_count == 0 )
   {
      fprintf ( stderr, "In routine 'write_gage_data': "
                        "Found no gages to write.\n" );
      return;
   }

   /* Build the file path. */
   filename_len = strlen ( filename );
   path_len = strlen ( path );

   if ( path [ path_len - 1 ] != '/' )
   {
      ++ path_len;
   }
   else
   {
      is_slash = true;
   }

   filepath_len = path_len + filename_len + 1;

   pFilePath = ( char * ) malloc ( filepath_len * sizeof ( char ) );

   if ( pFilePath == NULL )
   {
      fprintf ( stderr, "In routine 'write_gage_data': "
                        "dynamic memory allocation failure.\n" );
      return;
   }

   if ( is_slash )
   {
      sprintf ( pFilePath, "%s%s", path, filename );
   }
   else
   {
      sprintf ( pFilePath, "%s/%s", path, filename );
   }

   /* Attempt to open the file for writing. */
   pFile = fopen ( pFilePath, "w" );

   if ( pFile == NULL )
   {
      if ( pFilePath != NULL )
      {
         free ( pFilePath );
         pFilePath = NULL;
      }

      fprintf ( stderr, "In routine 'write_gage_data': "
                        "Could not open file %s.\n", pFilePath );
      return;
   }

   /* Write the number of stations as the first record in the file. */
   fprintf ( pFile, "%d\n", gage_count );
   pLocationNode = ( Location * ) ListFirst ( & pLocationHead->list );

   while ( pLocationNode != NULL )
   {
      fprintf ( pFile, "%s %f %f\n", pLocationNode->lid,
                                     pLocationNode->lat,
                                     pLocationNode->lon );
      pLocationNode = ( Location * ) ListNext ( & pLocationNode->node );
   }

   if ( pFilePath != NULL )
   {
      free ( pFilePath );
      pFilePath = NULL;
   }

   fclose ( pFile );
   pFile = NULL;

   return;
}

int create_mpe_gage_file_main ( int argc, const char ** argv)
{
   char c;
   char * pDBname = NULL;
   char * pPath = NULL;
   int len;
   int status;
   Location * pLocationHead = NULL;

   /* Get the command line arguments. These include the database name
      and the name of the directory into which to place the generated
      gage file. */
   while (( c = getopt ( argc, argv, ":d:" ) ) != -1 )
   {
      switch ( c )
      {
         case 'd' :

            len = strlen ( optarg );
            pDBname = ( char * ) malloc ( ( len + 1 ) * sizeof ( char ) );

            if ( pDBname == NULL )
            {
               fprintf ( stderr, "Dynamic memory allocation failure\n" );
               return 1;
            }

            memset ( pDBname, '\0', len + 1 );
            strncpy ( pDBname, optarg, len );
            break;

          case ':' :

             fprintf ( stderr, "A database name must be supplied with the "
                               "'-d' option.\n" );
             USAGE;
             FREE_MEMORY;
             return 1;

          case '?' :

             fprintf ( stderr, "An invalid command line argumane has been "
                               "passed into this program.\n" );
             USAGE;
             FREE_MEMORY;
             return 1;

          default :

             fprintf ( stderr, "An invalid command line character, %c, was "
                               "found while parsing the command line options\n"
                               "passed into this routine.\n", c ) ;
             USAGE;
             FREE_MEMORY;
             return 1;
      }
   }

   if ( argv [ optind ] == NULL )
   {
      fprintf ( stderr, "Station file path not supplied.\n" );
      USAGE;
      FREE_MEMORY;
      return 1;
   }

   len = strlen ( argv [ optind ] );

   pPath = ( char * ) malloc ( ( len + 1 ) * sizeof ( char ) );

   if ( pPath == NULL )
   {
      fprintf ( stderr, "Dynamic memory allocation failure\n" );
      FREE_MEMORY;
      return 1;
   }

   memset ( pPath, '\0', len + 1 );
   strncpy ( pPath, argv [ optind ], len );

   /* Open the IHFS database. */
   status = OpenDbms ( pDBname );

   if ( status != Ok )
   {
     fprintf ( stderr, "Could not open database %s.\n", pDBname );
     FREE_MEMORY;
     return 1;
   }

   /* Get the gage data according to gage_query. */
   pLocationHead = get_gage_data ( );

   /* Write the gage data to a file. */
   write_gage_data ( pPath, pLocationHead );

   /* Close the IHFS database. */
   CloseDbms ( );

   /* Free dynamically allocated memory. */
   FREE_MEMORY;

   return 0;
}
