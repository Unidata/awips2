/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       March 7, 2006
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     3/7/2006     Bryon Lawrence    Original Coding
********************************************************************************
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_log_utils.h"

/*******************************************************************************
* MODULE NAME:  init_precip_climo
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
int init_precip_climo ( const char * station_climo_file,
		        struct station station [ ],
		        int max_stations )
{
   char message[LOG_MESSAGE_LEN];

   int i,ier;
   int index;
   int record_count;
   int record_num;
   int num_records;
   FILE *fp = NULL;
   char *p,kbuf[200],hb5[10],parm[10];
   int m,k;
   int kk;
   int status;
   long file_position;
   float f[15];
   int oldk;

   memset ( message, '\0', LOG_MESSAGE_LEN);

   /* Try to open the station climo file. If the station climo
    *       file does not exist, then the climo setup program must
    *             be run to generate it. */
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
   
      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      /* Read the first record from the station climo file. */
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

      index = 0;
      record_count = 0;
      record_num ++;

      file_position = ftell ( fp );

      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      while ( ( record_count < num_records ) &&
              ( index < max_stations ) )
      {
         if ( p == NULL )
         {
            sprintf ( message, "Reached EOF while attempting to read record\n"
                               "%d in file %s\n", record_num, 
                               station_climo_file );
           logMessage ( "%s", message );
            logMessage(message);
            fclose ( fp );
            fp = NULL;
            return DAILYQC_FAILED;
         }

         ier = sscanf ( kbuf, "%s %s %f %f %f %f %f %f %f %f %f %f %f %f",
                        hb5, parm, &f[0], &f[1], &f[2], &f[3], &f[4], &f[5],
                        &f[6], &f[7], &f[8], &f[9], &f[10], &f[11] );
         
         if ( ier != 14 )
         {
            sprintf ( message, "In file %s, record number %d is incomplete. "
                               "This record is being skipped.\n",
                               station_climo_file, record_num );
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            ++ record_count;
            continue;
         }

         record_num ++;

         status = strcmp ( hb5, station [ index ].hb5 );

         if ( status < 0 )
         {
            /* Read the next record from the climo file. */
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            ++ record_count;
         }
         else if ( status > 0 )
         {
            /* Increment the station array index. */
            ++ index;
         }
         else
         {
            if ( station[index].parm[4] == parm[4] )
            {
               for(m=0;m<12;m++)
               {
                  station[index].isoh[m]=f[m];
               }
                                  
               strcpy(station[index].cparm,parm); 
            }

            ++ index;
         }
         
      }

      /* Reset the file position. */
      fseek ( fp, file_position, SEEK_SET ); 
      record_count = 0;
      index = 0;

      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      while ( ( record_count < num_records ) &&
              ( index < max_stations ) )
      {
         if ( p == NULL )
         {
            sprintf ( message, "Reached EOF while attempting to read record\n"
                               "%d in file %s\n", record_num, 
                               station_climo_file );
           logMessage ( "%s", message );
            logMessage(message);
            fclose ( fp );
            fp = NULL;
            return DAILYQC_FAILED;
         }

         ier = sscanf ( kbuf, "%s %s %f %f %f %f %f %f %f %f %f %f %f %f",
                        hb5, parm, &f[0], &f[1], &f[2], &f[3], &f[4], &f[5],
                        &f[6], &f[7], &f[8], &f[9], &f[10], &f[11] );
         
         if ( ier != 14 )
         {
            sprintf ( message, "In file %s, record number %d is incomplete. "
                               "This record is being skipped.\n",
                               station_climo_file, record_num );
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            ++ record_count;
            continue;
         }

         if(strcmp(parm,"PPMPBCM") != 0 &&
            strcmp(parm,"PPMRZCM") != 0)
         {
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            ++ record_count;
            continue;
         }

         record_num ++;

         status = strcmp ( hb5, station [ index ].hb5 );

         if ( status < 0 )
         {
            /* Read the next record from the climo file. */
            p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
            ++ record_count;
         }
         else if ( status > 0 )
         {
            /* Increment the station array index. */
            ++ index;
         }
         else
         {
            if ( ( station[index].isoh[0] < 0 ) ||
                   ( strcmp(station[index].cparm,"PPMPBCM")==0) )
            {
               for(m=0;m<12;m++)
               {
                  station[index].isoh[m]=f[m];
               }
                                  
               strcpy(station[index].cparm,parm); 
            }

            ++ index;
         }
         
      }

      fclose(fp);	       
      fp = NULL;
   }

   /* finally sum up for seasonal totals */
   for ( i=0; i<max_stations; i++ )
   {

      oldk=-1;

      for(kk=0;kk<12;kk++)
      {
         if ( kk < 3)
         {
            k=9+kk;
         }
         else
         {
            k=kk-3;
         }
    
         if(oldk==-1)
         {
            station[i].isoh[k+12]=station[i].isoh[k];
         }
         else
         {
            station[i].isoh[k+12]=station[i].isoh[k] + station[i].isoh[oldk];
         }
    
         oldk=k+12;
    
      }

   }

   return DAILYQC_OK;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
