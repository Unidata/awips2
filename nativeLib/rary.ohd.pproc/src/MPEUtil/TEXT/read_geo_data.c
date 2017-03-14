/*=========================================================================*/
/*                    FILE PATH/NAME:   read_geo_data.c                    */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   get_geo_data()                     */
/*					read_geo_data_free_memory()        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include "HydroStatus.h"
#include "overlay.h"
#include "post_functions.h"
#include "rfcwide.h"
#include "stage3.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   get_geo_data                                          */
/*       FUNCTION:   read geographic overlay data files                    */
/***************************************************************************

Function type:
   HydroStatus (defined in HydroStatus.h in the mpe_utils directory)

   Returns "HydroStatus_NoCoordFile" if the coordinate file cannot be
   located.  Returns "HydroStatus_BadMalloc" if a memory allocation fails.
   Otherwise, this routine returns value "HydroStatus_OK".

Called by function:
   main_rfcwide (mpe)
   ReadParam (mpe_post_analysis)

Functions called:

Local variables:
   overlay_avail.xxxxx = overlay availability flags
                       = 0 -- overlay info not available
                       = 1 --    "     "   available

******************************** BEGIN get_geo_data *********/

char RFC [ MAX_RFC_LEN + 1 ];

HydroStatus get_geo_data ( )
{
   char   os_suffix[10];
   char   whfs_geodata_dir[128] ;
   char   whfs_gridbasin_path[128] ;
   char   whfs_gridcounty_path[128] ;
   FILE  *file = NULL ;
   int    i ;
   int    j ;
   int    len;
   HydroStatus status ;

   len = strlen("whfs_geodata_dir");
   get_apps_defaults("whfs_geodata_dir",&len,whfs_geodata_dir,&len);

   len = strlen("OS_SUFFIX");
   get_apps_defaults("OS_SUFFIX",&len,os_suffix,&len);

   sprintf(whfs_gridbasin_path,"%s/grid_to_basin_hrap%s",whfs_geodata_dir,
           os_suffix);
   sprintf(whfs_gridcounty_path,"%s/grid_to_county_hrap%s",whfs_geodata_dir,
           os_suffix);

   /* read coordinates of rectangle surrounding RFC area
      coordinates are on national HRAP grid */
   status = read_mpe_coordinate_file ( &XOR, &YOR, &MAXX, &MAXY ); 

   if ( status != HydroStatus_OK )
   {
           fprintf ( stderr, "\nIn routine \"read_geo_data\":\n"
                       "Could not open file \"coord_%s.dat\".\n"
                       "The MPE data grids cannot be viewed.\n" , RFC ) ;
           return HydroStatus_NoCoordFile ;
   } 

   /* Create an array of latitude and longitude coordinates corresponding
      to each point on a Hrap grid.  For each RFC, this should be fixed.
      By performing this computation "up front", valuable CPU time will be
      saved later when Hrap grid points need to be converted to lat/lon
      values before being converted to Pixmap cartesian coordinates for
      plotting purposes. */ 
   status = createLatLonGrid ( XOR , YOR , MAXX , MAXY ) ;

   if ( status == HydroStatus_BadMalloc )
   {
      fprintf ( stderr , "In routine read_geo_data:\n"
                         "The call to \"createLatLonGrid\" failed.  Dynamic\n"
                         "memory could not be allocated for the\n"
                         "\"hmap_to_lat_lon\" array.  This will likely cause\n"
                         "problems with the rest of the application, so\n"
                         "execution is being stopped.\n" ) ;
      exit ( 0 ) ;
   } 

   /*----------------------------------------------------------------*/
   /*  open and read overlay data files                              */
   /*----------------------------------------------------------------*/
   /* Read the RFC boundaries. */
   /* The RFC site boundary file is no longer read.  RFC boundaries are
      read from D2D. */
   overlay_avail.site_boundary=0;

   /* Read the locator data from the grid_to_basin_hrap.OS and
      grid_to_county_hrap.OS flat files. */
   if ( ( file = fopen ( whfs_gridbasin_path , "rb" ) ) == NULL )
   {
       overlay_avail.gridtobasin = 0 ;
       fprintf( stderr , "Warning: %s not found \n" , 
                whfs_gridbasin_path ) ;
   }
   else
   {
     overlay_avail.gridtobasin = 1 ;
  
     loc_basin = (char ***)malloc(MAXY*sizeof(char **));
 
     if ( loc_basin == NULL )
     {
     	fprintf ( stderr , "In routine \"read_geo_data\":\n"
                           "An error was encountered while attempting to\n"  
			   "dynamically allocate memory for \"loc_basin\"\n"
			   "data array.\n"
                           "This will likely cause problems with the rest\n"
                           "of the application, so execution is being\n"
                           "stopped.\n");

	read_geo_data_free_memory ( ) ;
	return HydroStatus_BadMalloc ; 
     }
     
     for (i=0; i<MAXY; i++)
     {
        loc_basin[i] = (char **)malloc(MAXX*sizeof(char *));

	if ( loc_basin[i] == NULL )
     	{
     		fprintf ( stderr , "In routine \"read_geo_data\":\n"
                          "An error was encountered while attempting to\n"  
			  "dynamically allocate memory for \"loc_basin[i]\"\n"
			  "data array.\n"
                          "This will likely cause problems with the rest \n"
                          "of the application, so execution is being stopped.\n");

		read_geo_data_free_memory ( ) ;
		return HydroStatus_BadMalloc ; 
     	}

        for (j=0; j<MAXX; j++)
        {
           loc_basin[i][j] = (char *)malloc(MAX_BASIN_NAME_LEN*sizeof(char));
	   if ( loc_basin[i][j] == NULL )
     	   {
     			fprintf ( stderr , "In routine \"read_geo_data\":\n"
                           "An error was encountered while attempting to\n"  
			   "dynamically allocate memory for \"loc_basin[i][j]\"\n"
			   "data array.\n"
                           "This will likely cause problems with the rest \n"
                           "of the application, so execution is being stopped.\n");

		read_geo_data_free_memory ( ) ;
		return HydroStatus_BadMalloc ; 
     	   }
        }
     }

     for (i=0; i<MAXX; i++)
     for (j=0; j<MAXY; j++)
     {
        fread(loc_basin[j][i],MAX_BASIN_NAME_LEN*sizeof(char),1,file);
     }

     fclose(file);

     if((file = fopen(whfs_gridcounty_path,"rb")) == NULL)
     {
       overlay_avail.gridtocounty=0;
       fprintf( stderr , "warning: %s not found \n" , whfs_gridcounty_path);
     }
     else
     {

        overlay_avail.gridtocounty=1;
        loc_cty = (char ***)malloc(MAXY*sizeof(char **));
	if ( loc_cty == NULL )
     	{
     		fprintf ( stderr , "In routine \"read_geo_data\":\n"
                           "An error was encountered while attempting to\n"  
			   "dynamically allocate memory for \"loc_cty\"\n"
			   "data array.\n"
                           "This will likely cause problems with the rest \n"
                           "of the application, so execution is being stopped.\n");

		read_geo_data_free_memory ( ) ;
		return HydroStatus_BadMalloc ; 
     	}
        for (i=0; i<MAXY; i++)
        {
           loc_cty[i] = (char **)malloc(MAXX*sizeof(char *));
	   if ( loc_cty[i] == NULL )
     	   {
     		fprintf ( stderr , "In routine \"read_geo_data\":\n"
                           "An error was encountered while attempting to\n"  
			   "dynamically allocate memory for \"loc_cty[i]\"\n"
			   "data array.\n"
                           "This will likely cause problems with the rest \n"
                           "of the application, so execution is being stopped.\n");

		read_geo_data_free_memory ( ) ;
		return HydroStatus_BadMalloc ; 
     	   }
           for (j=0; j<MAXX; j++)
           {
              loc_cty[i][j] = (char *)malloc(MAX_COUNTY_NAME_LEN*sizeof(char));
	      if ( loc_cty[i][j] == NULL )
     	      {
     		 fprintf ( stderr , "In routine \"read_geo_data\":\n"
                           "An error was encountered while attempting to\n"  
			   "dynamically allocate memory for \"loc_cty[i][j]\"\n"
			   "data array.\n"
                           "This will likely cause problems with the rest \n"
                           "of the application, so execution is being stopped.\n");

		 read_geo_data_free_memory ( ) ;
		 return HydroStatus_BadMalloc ; 
     	      }
           }
        }

        for (i=0; i<MAXX; i++)
        for (j=0; j<MAXY; j++)
        {
           fread(loc_cty[j][i],21*sizeof(char),1,file);
        }

        fclose(file);
     }

   }

   return HydroStatus_OK ;
}

/************************************ END read_geo_data ***************/

/**********************************************************************/
/*  FUNCTION NAME:  read_mpe_coordinate_file()                        */
/*       FUNCTION:  reads the coordinate file which contains the HRAP */
/*                  dimensions of the office's MPE forecast area.     */
/***********************************************************************

Function type:
  int 

Called by function:
   get_geo_data

Functions called:
   get_apps_defaults

************************************** BEGIN read_mpe_coordinate_file *******/

HydroStatus read_mpe_coordinate_file ( int * xor, int * yor, int * maxx, 
                                       int * maxy )
{
   
   static const char * geo_data_token = "geo_data";  /* The name of the token
                                                        which contains the
                                                        geo data directory
                                                        path. */
   static const char * st3_rfc_token = "st3_rfc"; /* The name of the token
                                                     which contains the 
                                                     office id. */
   char coord_file [ 230 ];   /* Contains the coord_file name. */
   char geodata_dir [ 200 ];  /* Contains the geodata directory. */
   FILE * pFile = NULL;       /* Pointer to the file structure. */
   int len;                   /* Contains the string length */

   memset ( coord_file, '\0', 230 );
   memset ( geodata_dir, '\0', 200 );
   memset ( RFC, '\0', MAX_RFC_LEN + 1 );

   /*  create directory name for geographic data  */
   len = strlen ( geo_data_token );
   get_apps_defaults ( ( char * ) geo_data_token, &len, geodata_dir, &len );

   len = strlen ( st3_rfc_token );
   get_apps_defaults ( ( char * ) st3_rfc_token, &len, RFC, &len );

   sprintf ( coord_file, "%s/%s/ascii/coord_%s.dat", geodata_dir, RFC, RFC ); 

   pFile = fopen ( coord_file,  "r" );

   if ( pFile != NULL )
   {
      fscanf ( pFile, "%d", xor );
      fscanf ( pFile, "%d", yor );
      fscanf ( pFile, "%d", maxx );
      fscanf ( pFile, "%d", maxy );
      fclose ( pFile );
      pFile = NULL;
      return HydroStatus_OK;
   }
   else
   {
      return HydroStatus_NoCoordFile ;
   }
}

/************************************ END read_mpe_coordinate_file *********/

/**********************************************************************/
/*  FUNCTION NAME:  read_geo_data_free_memory()                       */
/*       FUNCTION:  This routine frees dynamically allocated memory   */
/*		    used by the get_geo_data routine                  */
/***********************************************************************

Function type:
   void

Called by function:
   get_geo_data

Functions called:
   freeLatLonGrid

************************************** BEGIN read_geo_data_free_memory *******/

void read_geo_data_free_memory ( ) 
{
   int i , j ;

   /* Free the latitude / longitude lookup array. */
   freeLatLonGrid ( ) ;
   
   /* free the loc_basin data array */
   if ( loc_basin != NULL ) 
   {
     for ( i = 0 ; i < MAXY ; ++ i )
     {
        if ( loc_basin [ i ] != NULL )
        {
           for ( j = 0 ; j < MAXX ; ++ j )
           {
              if ( loc_basin [ i ] [ j ] != NULL )
              {
                 free ( loc_basin [ i ] [ j ] ) ;
                 loc_basin [ i ] [ j ] = NULL ;
              }
              else
              {
                 break ;
              }
           }
        
           free ( loc_basin [ i ] ) ;
        }
     } 
     free ( loc_basin ) ;
     loc_basin = NULL ;  
   }
   
    /* free the loc_county data array */
   if ( loc_cty != NULL ) 
   {
     for ( i = 0 ; i < MAXY ; ++ i )
     {
        if ( loc_cty [ i ] != NULL )
        {
           for ( j = 0 ; j < MAXX ; ++ j )
           {
              if ( loc_cty [ i ] [ j ] != NULL )
              {
                 free ( loc_cty [ i ] [ j ] ) ;
                 loc_cty [ i ] [ j ] = NULL ;
              }
              else
              {
                 break ;
              }
           }
        
           free ( loc_cty [ i ] ) ;
        }
     } 
     free ( loc_cty ) ;
     loc_cty = NULL ;
   }
}
/*********************************** END read_geo_data_free_memory ****/
   
