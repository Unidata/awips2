#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

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

int init_temperature_climo ( const char * station_climo_file ,
                             struct station tstation [ ],
                             int max_tstations )
{
   char message [ LOG_MESSAGE_LEN ];
   int i,ier;
   int index;
   int record_num;
   int num_records;
   int status;
   FILE *fp = NULL;
   char *p,kbuf[200],hb5[10],parm[10];
   int m;
   long file_position;
   float f[15];
 
   memset ( message, '\0', LOG_MESSAGE_LEN);

   /* Try to open the station climo file. If the station climo
      file does not exist, then the climo setup program must
      be run to generate it. */
   fp = fopen ( station_climo_file, "r" );
       
   if ( fp == NULL )
   {
      memset ( message, '\0', LOG_MESSAGE_LEN);
      sprintf ( message,"Could not open file: %s\n",station_climo_file);
      logMessage ( message );
      return DAILYQC_FAILED;
   }
   else
   {
      memset(message,'\0',LOG_MESSAGE_LEN);
      sprintf(message, "Opened file: %s\n", station_climo_file);
      logMessage(message);

      /* Initialize the temperature station array index to 0. */
      index = 0;
      record_num = 1;
 
      /* Read the first record from the station climo file. */
      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      if ( p == NULL )
      {
         sprintf ( message, "Reached EOF while reading first record of\n"
                            "file %s.\n", station_climo_file );
        logMessage ( "%s", message );
         logMessage ( message );
         fclose ( fp );
         fp = NULL;
         return DAILYQC_FAILED;
      }
      
      /* Search for the Temperature Climo Block. */
      /* Read the number of precipitation records. */
      ier = sscanf ( kbuf, "%d", & num_records );

      if ( ier != 1 )
      {
         sprintf ( message, "Could not read the number of PPD climo records\n"
                            "%s record %d\n", station_climo_file, 
                            record_num );
         logMessage ( message );
         fclose ( fp );
         fp = NULL;
         return DAILYQC_FAILED;
      }
    
      ++ record_num;

      /* Skip the PPD climo records. */
      for ( i = 0; i < num_records; ++i )
      {

         p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

         if ( p == NULL )
         {
            sprintf ( message, "Reached EOF while attempting to read "
                               "record %d in file %s\n", record_num,
                               station_climo_file );
            logMessage ( message );
            fclose ( fp );
            fp = NULL;
            return DAILYQC_FAILED;
         }

         ++ record_num;
      }

      /* Skip the record containing the count of temperature climo
         records. */
      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      if ( p == NULL )
      {
         sprintf ( message, "Reached EOF while attempting to read "
                            "record %d in file %s\n", record_num,
                            station_climo_file );
         logMessage ( message );
         fclose ( fp );
         fp = NULL;
         return DAILYQC_FAILED;
      }

      ++ record_num;
         
      ier = sscanf ( kbuf, "%d", & num_records );

      if ( ier != 1 )
      {
         sprintf ( message, "Could not read the number of TAI climo records\n"
                            "%s record %d\n", station_climo_file, 
                            record_num );
         logMessage ( message );
         fclose ( fp );
         fp = NULL;
         return DAILYQC_FAILED;
      }

      /* Store the record position. */
      file_position = ftell ( fp );

      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
    
      while ( ( p != NULL ) &&
              ( index < max_tstations ) )
      {
         /* Split the record into its hd5 code, the physical parameter
            and the 12 monthly average temperature fields. */  
         ier = sscanf(kbuf,"%s %s %f %f %f %f %f %f %f %f %f %f %f %f",
                      hb5,parm,&f[0],&f[1],&f[2],&f[3],&f[4],&f[5],&f[6],
                      &f[7],&f[8],&f[9],&f[10],&f[11]);

         ++ record_num;
         
         /* There needs to be 14 items per record. */
         if(ier != 14)
         {
            sprintf ( message, "In file %s, record number %d is incomplete. "
                               "This record is being skipped.\n",
                               station_climo_file, record_num );
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            continue;
         }
             
         /* Compare the hd5 just read in with the the one in the station
            list array. */
         status = strcmp ( hb5, tstation [ index ].hb5 );

         if ( status < 0 )
         {
            /* Read in the next climo record. */
            p = fgets ( kbuf, 200, fp);
         }
         else if ( status > 0 )
         {
            /* Increment the temperature station array index. */
            ++ index;
         }
         else
         {
            if ( tstation[index].parm[4] == parm[4] )
            {
               /* Is the extremum code 'Maximum'? Of temperature,
                  precipitation and freezing level, temperature is
                  the only one described with an extremum code of
                  maximum. */
               if ( parm[5] == 'X' || parm[5] == 'N' )
               {
                  if ( parm[5] == 'X' )
                  {
                     /* Copy the max temperature information into 
                        the temperature structure in the temperature station
                        array. */
                     for ( m = 0; m < 12; ++m )
                     {
                        tstation[index].max[m]=f[m];
	             }

                  }
                  else if ( parm[5] == 'N' )
                  {
                     /* Does this have an extremum code of 'Minimum'? If so,
                        store its information in the temperature structure. */
                     for ( m = 0; m < 12; ++ m )
                     {
                        tstation[index].min[m] = f[m];
                     }  
                  }

                  strcpy ( tstation[index].cparm, parm ); 
               }

               /* Read in the next climo record. */
               p = fgets ( kbuf, 200, fp);
            }
            else
            {
               /* Increment the station array index. */
               ++ index;
            }
         }

      }

      /* Rewind the climo file. */
      index = 0;
      fseek ( fp, file_position, SEEK_SET ); 

      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
    
      while ( ( p != NULL ) &&
              ( index < max_tstations ) )
      {
         /* Split the record into its hd5 code, the physical parameter
            and the 12 monthly average temperature fields. */  
         ier = sscanf(kbuf,"%s %s %f %f %f %f %f %f %f %f %f %f %f %f",
                      hb5,parm,&f[0],&f[1],&f[2],&f[3],&f[4],&f[5],&f[6],
                      &f[7],&f[8],&f[9],&f[10],&f[11]);

         ++ record_num;
         
         /* There needs to be 14 items per record. */
         if ( ier != 14 )
         {
            sprintf ( message, "In file %s, record number %d is incomplete. "
                               "This record is being skipped.\n",
                               station_climo_file, record_num );
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            continue;
         }

         if(strncmp(parm,"TAIPB",5) != 0 &&
            strncmp(parm,"TAIRZ",5) != 0)
         {
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            continue;
         }
             
         /* Compare the hb5 just read in with the the one in the station
            list array. */
         status = strcmp ( hb5, tstation [ index ].hb5 );

         if ( status < 0 )
         {
            /* Read in the next climo record. */
            p = fgets ( kbuf, 200, fp);
         }
         else if ( status > 0 )
         {
            /* Increment the temperature station array index. */
            ++ index;
         }
         else
         {
            if ((parm[5]=='X' && tstation[index].max[0] < 0) ||
                (parm[5]=='N' && tstation[index].min[0] < 0) ||
                 strncmp(tstation[index].cparm,"TAIPB",5)==0)
            {
               /* Is the extremum code 'Maximum'? Of temperature,
                  precipitation and freezing level, temperature is
                  the only one described with an extremum code of
                  maximum. */
               if ( parm[5] == 'X' )
               {
                  /* Copy the max temperature information into 
                     the temperature structure in the temperature station
                     array. */
                  for ( m = 0; m < 12; ++m )
                  {
                     tstation[index].max[m]=f[m];
	          }
               }
               else if ( parm[5] == 'N' )
               {
                  /* Does this have an extremum code of 'Minimum'? If so,
                     store its information in the temperature structure. */
                  for ( m = 0; m < 12; ++ m )
                  {
                     tstation[index].min[m] = f[m];
                  }  
               }

               /* Read in the next climo record. */
               p = fgets ( kbuf, 200, fp);
				                                    
               strcpy ( tstation[index].cparm, parm ); 
            }
            else
            {
               /* Increment the station array index. */
               ++ index;
            }
         }

      }
	       
      /* Close the climo file. */
      fclose(fp);	       
      fp = NULL;
   }

   return DAILYQC_OK;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/init_temperature_climo.c,v $";
 static char rcs_id2[] = "$Id: init_temperature_climo.c,v 1.3 2007/10/23 18:02:53 lawrence Exp $";}
/*  ===================================================  */

}
