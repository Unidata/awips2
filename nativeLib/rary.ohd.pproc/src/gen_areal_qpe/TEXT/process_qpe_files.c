#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>

#include "gen_areal_qpe.h"
#include "time_convert.h"
#include "time_defs.h"

/*********************************************************************
   count_qpe_files()
 
   PURPOSE
   Count the number of files in the input directory so that memory
   can be allocated to store info on the files.
 
********************************************************************/

static int count_qpe_files(const gaq_options_struct * options)
{
   DIR 			*dirp = NULL;
   struct dirent 	*dp = NULL;
   int 			num_files;


   /* initialize */
   num_files = 0;
   

   /* open directory */
   dirp = opendir(options->input_data_path);

   if (dirp == NULL)
   {
      printf("ERROR: failed to open directory= %s\n", 
              options->input_data_path);
      printf("errno = %d\n", errno);
   }
   else
   {
      /* read through files in dir to determine number of files.
         exclude certain entries */
      while ((dp = readdir(dirp)) != NULL)
      {
         if(strlen(dp->d_name) > 2) num_files++; 
      }

      /* close directory */
      closedir(dirp);
      dirp = NULL;
   }  

   /* return with the count of files */
   return(num_files);
   
}

/*********************************************************************
   load_qpe_fileinfo()
 
   PURPOSE
   Load info for the input files into local memory for use later.
     
 ********************************************************************/
static int load_qpe_fileinfo(const gaq_options_struct *options,
                             qpe_info_struct *qpe_info,
	                     int num_files)
{
   char day [ DAY_LEN + 1 ];
   char hour [ HOUR_LEN + 1];
   char * pFileName = NULL;
   char month [MONTH_LEN + 1];
   static char * process_flags [ ] = { "", "QPE01","","","","","QPE06","","",
                                       "","","","","","","","","","","","",
                                       "","","","QPE24" };
   const static rfc_mask_val_map rfc_map [ NUM_RFC_MASK_VALUES ] = 
                                                     { { "ABRFC", ABRFC_MASK },
                                                       { "APRFC", APRFC_MASK },
                                                       { "CBRFC", CBRFC_MASK },
                                                       { "CNRFC", CNRFC_MASK },
                                                       { "LMRFC", LMRFC_MASK },
                                                       { "MARFC", MARFC_MASK },
                                                       { "MBRFC", MBRFC_MASK },
                                                       { "NCRFC", NCRFC_MASK },
                                                       { "NERFC", NERFC_MASK },
                                                       { "NWRFC", NWRFC_MASK },
                                                       { "OHRFC", OHRFC_MASK },
                                                       { "SERFC", SERFC_MASK },
                                                       { "WGRFC", WGRFC_MASK }};
             
   char * pToken = NULL;
   char year [ YEAR_LEN + 1 ];

   DIR 			*dirp = NULL;
   struct dirent 	*dp = NULL;
   int 			i;
   int                  j;
   int                  length;
   int                  status;
   struct tm            endtime_tm;

   /* open directory */
   dirp = opendir(options->input_data_path);

   if (dirp == NULL)
   {
      printf("failed to open directory (second time)= %s\n",
             options->input_data_path);
      printf("errno = %d\n",errno);
      return 1;
   }

   /* load file structure with relevant inf. filter out certain files. */

   i = 0;

   dp = readdir ( dirp );

   while (dp != NULL)
   {
      if(strlen(dp->d_name) > 2 && i < num_files)
      {
         strcpy(qpe_info[i].name, dp->d_name);
         pFileName = strdup (dp->d_name ); 

         /* Parse the RFC name from the filename. */
         pToken = strtok ( pFileName, "_" );

         if ( pToken != NULL )
         {
            strcpy ( qpe_info[i].rfc, pToken );
         }
         else
         {
            printf ( "Could not parse RFC name from file %s. Skipping ...\n",
                     dp->d_name );
            dp = readdir ( dirp );
            continue;
            
         }

         /* Using the RFC name, determine what the NPVU RFC mask value
            should be. */
         for ( j = 0; j < NUM_RFC_MASK_VALUES; ++j )
         {
            status = strcmp ( qpe_info[i].rfc, rfc_map[j].rfc_name );

            if ( status == 0 )
            {
               qpe_info[i].mask_value = rfc_map[j].mask_value;
               break;
            }

         }

         if ( j == NUM_RFC_MASK_VALUES )
         {
            printf ( "Could not determine the rfc mask value for RFC %s. "
                     "Skipping ...\n", dp->d_name );
            dp = readdir ( dirp );
            continue;
         }
       
         /* Parse the QPE duration from the filename. */
         pToken = strtok ( NULL, "_" );

         if ( pToken != NULL )
         {
            qpe_info[i].dur = atoi ( pToken );
         }
         else
         {
            printf ( "Could not parse duration from file %s. Skipping ...\n",
                      dp->d_name );
            dp = readdir ( dirp );
            continue;
         }

         /* Parse the ending time of the QPE product from the filename. */
         pToken = strtok ( NULL, "_" );

         if ( pToken != NULL )
         {
            /* Year, month, day */
            memset ( year, '\0', YEAR_LEN + 1 );
            strncpy ( year, pToken, YEAR_LEN );
            memset ( month, '\0', MONTH_LEN + 1 );
            strncpy ( month, pToken + YEAR_LEN, MONTH_LEN ); 
            memset ( day, '\0', DAY_LEN + 1 );
            strncpy ( day, pToken + YEAR_LEN + MONTH_LEN, DAY_LEN );
         }
         else
         {
            printf ( "Could not parse year, month, day from file %s. "
                     "Skipping...\n", dp->d_name );
            dp = readdir ( dirp );
            continue;
         }

         /* Parse the ending hour of the QPE product from the filename. */
         pToken = strtok ( NULL, "_" );

         if ( pToken != NULL )
         {
            /* Hour */
           memset ( hour, '\0', HOUR_LEN + 1 );
           strncpy ( hour, pToken, HOUR_LEN );
         }
         else
         {
            printf ( "Could not parse hour from file %s. Skipping ...\n",
                      dp->d_name );
            dp = readdir ( dirp );
            continue;
         }

         free ( pFileName );
         pFileName = NULL;

         endtime_tm.tm_year = atoi ( year ) - 1900;
         endtime_tm.tm_mon = atoi ( month ) - 1;
         endtime_tm.tm_mday = atoi ( day );
         endtime_tm.tm_hour = atoi ( hour );
         endtime_tm.tm_min = 0;
         endtime_tm.tm_sec = 0;

         qpe_info[i].endtime = gm_mktime ( & endtime_tm );

         /* Need to increment the hour by one.  The time on a QPE file 
            is the ending time of the one hour accumulation period. GSD
            assigns dates according to the beginning of the accumulation
            period. */
         qpe_info[i].endtime += SECONDS_PER_HOUR;
	 
         length = strlen ( process_flags [qpe_info[i].dur] );
         memset ( qpe_info[i].proc_flag, ' ', PROC_FLAG_LEN );
         qpe_info[i].proc_flag[PROC_FLAG_LEN] = '\0';
         
         if ( length > 0 )
         {
            strncpy ( qpe_info[i].proc_flag, process_flags [qpe_info[i].dur],
                      length ); 
         }

	 /* increment counter */
         ++i;
      }

      dp = readdir ( dirp );
   }


   /* close directory */

   closedir(dirp);
   dirp = NULL;
   
   return 0;
}


/*********************************************************************
   process_qpe_files()
   
   PURPOSE
   Checks for qpe files and if found loads information about the file,
   then calls a separate function to process the file.
   
 ********************************************************************/

int process_qpe_files(const gaq_options_struct * options)

{ 
   char * error_string = NULL;
   char netcdf_path [ MAX_PATH_LEN + 1 ];
   float ** pXmrgGrid = NULL;
   extern int errno;
   int i;
   int num_files; 
   int status;
   geo_data_struct  coord_info;   
   qpe_info_struct    *qpe_info = NULL;     
   static int rfc_mask [ RFC_MASK_ROWS ] [ RFC_MASK_COLS ];
      
   /* count the number of input qpe files in the input directory */ 
   num_files = count_qpe_files(options); 
   printf("num qpe files=%d\n", num_files);

   /* if no files found, issue message and return */
   if (num_files == 0)
   {
      printf("No input QPE files found.\n");
      return num_files;
   }
   
   /* allocate storage for each of the qpe files */       
   qpe_info = (qpe_info_struct *) malloc(num_files * sizeof(qpe_info_struct));

   if (qpe_info == NULL)
   {
      printf("ERROR: qpe_info malloc failed\n");
      return 0;
   }
   
   /* read the input directory and store the filenames */
   load_qpe_fileinfo(options, qpe_info, num_files);   
   
   /* get the coordinate dimensions for the output grid */
   get_coord_info(options, &coord_info); 

   /* Read the RFC Mask file.  This is the mask file used by NPVU when
      producing the national mosaic of RFC QPE. */
   get_rfc_mask ( rfc_mask );
   
   /* allocate space for the output grid. 
      use an unsigned char grid for the raw netCDF data,
      use a float grid for the internal grid. */
   pXmrgGrid = ( float ** ) malloc ( coord_info.num_rows * sizeof ( float * ));

   if ( pXmrgGrid == NULL )
   {
      printf ("ERROR: xmrg output grid malloc failed.\n" );
      return 0;
   }

   for ( i = 0; i < coord_info.num_rows; ++i )
   {
      pXmrgGrid[i] = ( float * ) malloc ( coord_info.num_cols *
                                          sizeof ( float ) );
      if ( pXmrgGrid [ i ] == NULL )
      {
         printf ( "ERROR: xmrg output grid malloc failed.\n" );
         return 0;
      }
   }


   /* loop on the number of files */
   for (i = 0; i < num_files; i++)
   { 
      printf("Processing: %s\n", qpe_info[i].name);
        
      status =  process_single_qpe(options, rfc_mask, & coord_info, 
                                   & qpe_info[i], pXmrgGrid );

      if ( status == FAIL )
      {
         printf ( "File %s not processed.\n", qpe_info[i].name );
      }
      
      /* delete the input file */  
      memset ( netcdf_path, '\0', MAX_PATH_LEN + 1 );
      sprintf ( netcdf_path, "%s/%s", options->input_data_path,
                             qpe_info[i].name );
      errno = 0;
      status = remove ( netcdf_path ); 

      if ( status == 0 )
      {
         printf ( "File %s deleted.\n",  netcdf_path );
      }
      else
      {
         error_string = strerror ( errno );
         printf ( "Error deleting file %s. Errno %d.  Error %s\n", 
                  netcdf_path, errno, error_string );
      } 
   }   
       
   if ( qpe_info != NULL )
   {
      free ( qpe_info );
      qpe_info = NULL;
   }

   if ( pXmrgGrid != NULL )
   {
      for ( i = 0; i < coord_info.num_rows; ++i )
      {
         free ( pXmrgGrid[i] );
      }

      free ( pXmrgGrid );
      pXmrgGrid = NULL;
   }
        
   return num_files;  
} 
