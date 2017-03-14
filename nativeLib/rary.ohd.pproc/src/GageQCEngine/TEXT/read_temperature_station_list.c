#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"

int max_tstations = 0;

struct station * tstation = NULL;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_temperature_station_list
* PURPOSE:       Reads the list of TAI stations from the station list file.
*                Dynamically allocates an array of station structures to
*                contain the read-in station data.  This array must be
*                deallocated by the caller of this routine. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Output int *       num_gages      The number of TAI stations read from the
*                                     station list file.
*   Input  char *      area_name      The name of the area to read the station
*                                     list for.  If NULL, then it defaults
*                                     to the master station list.
*   Input  int         master_flag    1 - read from the master file, skip PPH
*                                         section.
*                                     0 - not reading from master file ...
*                                         there is no PPH section.
*
* RETURNS:
*   DATA TYPE            DESCRIPTION
*   struct station *     Dynamically allocated array of temperature station
*                        information.
*
* APIs UTILIZED:
*   NAME                       HEADER FILE     DESCRIPTION
*   get_station_list_path      gageqc_types.h  Retrieves the station path.
*   logMessage                 gageqc_types.h  Writes to the log file.
*
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE     NAME                 DESCRIPTION
*   struct dval   dval
*   char *        fname                The path/name of the station list file.
*   char          hb5[]                The handbook 5 identifier of the station.*   char          parm[]               The parameter code of the station.
*   char          message[]            Message to log to log file.
*   double        dist1                Used in determining list of closest
*                                      station neighbors.
*   double        dist2                Used in determining list of closest
*                                      station neighbors.
*   double        dist                 Used in determining list of closest
*                                      station neighbors.
*   double        sorted[]             The array of gage neighbors.
*   char          station_custom_file[] The file containing station label
*                                       locations.
*   int           h                     Loop index.
*   int           i                     Loop index.
*   int           ier                   Tracks number of fields read in from
*                                       station list records.
*   int           l                     Loop index.
*   int           sflag                 Flag used in parsing name field
*                                       out of station list record.
*   int           field                 Count of number of fields in station
*                                       list record.
*   int           j                     Loop index.
*   FILE *        fp                    File pointer to station list.
*   char *        p                     Pointer to current record being
*                                       processed.
*   int           index                 The station array index.
*   int           m                     A loop indexing variable.
*   int           num_records           The number of records in the
*                                       block of the station list file.
*   int           status                Status of string comparisons.
*   int           xadd                  Station label x positions read in from
*                                       customized station list file.
*   int           yadd                  Station label y positions read in from
*                                       customized station list file.
*   float         lat                   The latitude of the station.
*   float         lon                   The longitude of the station. 
*   float         conv                  Used in the conversion of lat/lon to
*                                       pixel coordinates.
*   long          t                     Used in the conversion of lat/lon to
*                                       pixel coordinates.
*   long          u                     Used in the conversion of lat/lon to
*                                       pixel coordinates.
*   float         elev                  The elevation of the station.
*   int           record_num            The number of the current record
*
* DATA FILES AND/OR DATABASE:
* Relies on the existence of the station file.
* The path to this file is given by token mpe_station_list_dir.
* The name of the file is built as <area_name>_station_list.  If area_name
* is NULL, then the name of the file is <mpe_site_id>_station_list where
* mpe_site_id is a token.
*
* ERROR HANDLING:
*    ERROR CODE                        DESCRIPTION
*    NULL                              No station list information could be
*                                      read.
********************************************************************************
*/

struct station * read_temperature_station_list ( int * num_gages,
                                                 const char * area_id,
	                                         int master_flag )
{
   extern int mpe_dqc_max_temp_neighbors;
   char message[LOG_MESSAGE_LEN];
   double dist1,dist2,dist,sorted[mpe_dqc_max_temp_neighbors];
   extern struct dval dval;
   int h,i,ier,l,sflag,field,j;
   int index;
   FILE *fp = NULL;
   char hb5[10],parm[10]; 
   const char * fname = NULL;
   extern char tstation_list_custom_file [ ];
   char *p,kbuf[MAX_STATION_RECORD_LEN];
   int m;
   int space_test;
   int status;
   int xadd, yadd;
   float lat,lon;
   float conv=.0174;
   long t,u;
   float elev;
   int num_records;
   int record_num = 1;
   HRAP hrap_point;
 
   max_tstations=0;
   * num_gages = 0;
   memset(message,'\0',LOG_MESSAGE_LEN);

   fname = get_station_list_path ( area_id );

   if ( fname == NULL )
   {
      sprintf ( message, "No path or name defined for the station list.\n" );
      logMessage ( message );
      return NULL;
   }

   fp = fopen ( fname, "r" );

   if ( fp == NULL ) 
   {
     logMessage("could not open %s\n",fname);
      sprintf(message,"Could not open file: %s\n",fname);
      return NULL;
   }
   else
   {
      sprintf(message,"Opened file: %s\n",fname);
      logMessage(message);
   }
 
   if ( master_flag == 1 )
   {
      /* Read the number of PPH records in the file. */
      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      if ( p == NULL )
      {
         sprintf ( message, "Reached EOF while reading first record of\n"
                   "file %s\n", fname );
        logMessage ( "%s", message );
         logMessage(message);
         fclose ( fp );
         return NULL;
      }

      ier = sscanf ( kbuf, "%d", & num_records );

      if ( ier != 1 )
      {
         sprintf ( message, "Could not read the number of 1 hour gages from\n"
                            "%s record %d\n", fname, record_num );
        logMessage ( "%s", message );
         logMessage ( message );
         fclose ( fp );
         return NULL;
      }

      ++record_num;

      /* Need to skip the PPH records. */
      /* Has this already been done?  If so, then use the fseek routine. */
      for( i = 0; i < num_records; ++i )
      {
         p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

         if ( p == NULL )
         {
            sprintf ( message, "Reached EOF while attempting to read "
                               "record %d in file %s\n", record_num,
                               fname );
            logMessage ( message );
            fclose ( fp );
            return NULL;
         }

         ++ record_num;
      }
      //--
      //record_num--;
   }

   /* Read the record containing the number of PPD stations. */
   p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

   if ( p == NULL )
   {
      sprintf ( message, "Reached EOF while reading record %d of\n"
                "file %s\n", record_num, fname );
     logMessage ( "%s", message );
      logMessage(message);
      fclose ( fp );
      return NULL;
   }

   ier = sscanf ( kbuf, "%d", & num_records );

   if ( ier != 1 )
   {
      sprintf ( message, "Could not read the number of 1 hour gages from\n"
                         "%s record %d\n", fname, record_num );
     logMessage ( "%s", message );
      logMessage ( message );
      fclose ( fp );
   }

   ++ record_num;

   /* Skip the PPD records. */
   /* Has this already been done?  If so, then use the fseek routine. */
   for( i = 0; i < num_records; ++i )
   {
      p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

      if ( p == NULL )
      {
         sprintf ( message, "Reached EOF while attempting to read "
                            "record %d in file %s\n", record_num,
                            fname );
         logMessage ( message );
         fclose ( fp );
         return NULL;
      }

      ++ record_num;
   }
   //--
   //record_num--;

   /* Read the number of TAI stations. */
   /* Read the record containing the number of PPD stations. */
   p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

   if ( p == NULL )
   {
      sprintf ( message, "Reached EOF while reading record %d of\n"
                "file %s\n", record_num, fname );
     logMessage ( "%s", message );
      logMessage(message);
      fclose ( fp );
      return NULL;
   }

   ier = sscanf ( kbuf, "%d", & num_records );

   if ( ier != 1 )
   {
      sprintf ( message, "Could not read the number of temperature gages\n"
                         "from %s record %d\n", fname, record_num );
     logMessage ( "%s", message );
      logMessage ( message );
      fclose ( fp );
      return NULL;
   }

   ++ record_num;

   /* Allocate memory for the temperture stations. */
   tstation = ( struct station * ) malloc ( num_records 
                                 * sizeof ( struct station ) );

   if ( tstation == NULL )
   {
      sprintf ( message, "Could not allocate memory for the temperature "
                         "station list.\n" );
      fclose ( fp );
      return NULL;
   }
      
   for ( i = 0; i < num_records; ++i )
   {

    p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

    if ( p==NULL )
    {
       sprintf ( message, "Reached EOF while reading record %d of\n"
                 "file %s\n", record_num, fname );
      logMessage ( "%s", message );
       logMessage(message);
       break;
    }

    tstation[i].hb5=calloc(10,sizeof(char));
    tstation[i].name=calloc(50,sizeof(char));
    tstation[i].parm=calloc(10,sizeof(char));
    tstation[i].cparm=calloc(10,sizeof(char));
    tstation[i].max=calloc(12,sizeof(float));
    tstation[i].min=calloc(12,sizeof(float));
    tstation[i].index=calloc(mpe_dqc_max_temp_neighbors,sizeof(short int));
    tstation[i].xadd = 0;
    tstation[i].yadd = 0;
    
    ier=sscanf(kbuf,"%s %s %f %f %f %d ",tstation[i].hb5,
               tstation[i].parm,&tstation[i].lat,
               &tstation[i].lon,&elev,&tstation[i].tip);
	        
    /* If the elevation is 0, set it to 1.  The value of 0
       causes problems with subsequent computations in 
       DailyQC. */
    if ( elev == 0 )
    {
       elev = 1;
    }

    tstation[i].elev=(int)elev;
       

    if(ier != 6)
    {
       sprintf ( message, "Incomplete record.  File %s record %d.\n",
                          fname, record_num );
       continue;
    }
      
    ++ record_num;

    lat=tstation[i].lat;
    lon=tstation[i].lon;
    
    /* Store the station's coordinates in HRAP. */
    hrap_point = LatLongToHrapMpe ( lat, lon );

    tstation[i].hrap_x = hrap_point.x;
    tstation[i].hrap_y = hrap_point.y;

    t=dval.a * cos(lat*conv)/(1+sin(lat*conv))
             * cos((lon-dval.lo-90)*conv) + dval.xo +.5;

    tstation[i].x=t;

    u=dval.a * cos(lat*conv)/(1+sin(lat*conv))
             * sin((lon-dval.lo-90)*conv) + dval.yo + .5;
       
    tstation[i].y=u;

    sflag=1;
    field=0;

    /* Tokenize the temperature station record. */
    for ( j = 0; j < strlen(kbuf); ++j )
    {
       space_test = isspace ( kbuf [j] );

       if ( ( space_test == 0 )  && ( sflag == 1 ) )
       {
          sflag=0; 
          field++;

       }
       else if( space_test != 0 )
       {
          sflag=1;
       }

       if ( field == 7 )
       {
          kbuf[j+49]=0;

          strcpy(tstation[i].name,&kbuf[j]);

          /* Remove any trailing white space characters. */
          strip_tblanks ( tstation[i].name );
          break;
       }

    }
		
  }

  max_tstations=i;
  * num_gages = max_tstations;

  fclose ( fp );

  fp = NULL;

  for ( i=0; i < max_tstations; i++ )
  {
         
     for(m=0;m<12;m++)
     {
        tstation[i].max[m]=-99; 
        tstation[i].min[m]=-99;
     }

  }

   /* Read in the custom positioning information for the temperature
      stations. */
   record_num = 0;
   sprintf ( tstation_list_custom_file, "%s_label_position", fname );
   fp = fopen ( tstation_list_custom_file, "r" );

   if ( fp != NULL )
   {
      memset(message,'\0',150);
      sprintf(message,"Opened file: %s\n", tstation_list_custom_file);
      logMessage(message);

      index = 0;

      p = fgets ( kbuf, 80, fp );

      while ( ( p != NULL ) && ( index < max_tstations ) )
      {
          ++ record_num;

          ier = sscanf ( kbuf,"%s %s %d %d",hb5,parm,&xadd,&yadd );

          if ( ier != 4 )
          {
             sprintf ( message, "In file %s, record number %d is incomplete. "
                                "This record is being skipped.\n",
                                tstation_list_custom_file, record_num );
             p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
             continue;
          }

          status = strcmp ( hb5, tstation [ index ].hb5 );

          if ( status < 0 )
          {
             /* Read the next record from the custom climo file. */
             p = fgets ( kbuf, 80, fp );
          }
          else if ( status > 0 )
          {
             /* increment the index in the temperature station array. */
             ++ index;
          }
          else
          {
             if(strncmp(parm,tstation[index].parm,3)==0 &&
                        parm[4]==tstation[index].parm[4])
             {
                tstation[index].xadd=xadd;
                tstation[index].yadd=yadd;
             }

             ++ index;
          }
      }

      fclose(fp);
      fp = NULL;

   }
   else
   {
      memset ( message, '\0', LOG_MESSAGE_LEN);
      sprintf(message,"Could not open custom temperature station file: %s\n",
              tstation_list_custom_file);
      logMessage(message);
   }

   /* For each temperature station for the indices of the nearest 
      temperature stations. */
   for ( i=0; i < max_tstations; ++i)
   {
      for(l=0;l<mpe_dqc_max_temp_neighbors;l++)
      {
         sorted[l]=9999999;
      }

      for(m=0;m<max_tstations;m++)
      {
         if(i==m)
         {
            continue;
         }

         dist1=tstation[i].lat-tstation[m].lat;
         dist2=(tstation[i].lon-tstation[m].lon)*
                      cos((tstation[i].lat+tstation[m].lat)/2*conv);

         dist=pow(dist1,2)+pow(dist2,2);

         for(l=0;l<mpe_dqc_max_temp_neighbors;l++)
         {

            if(dist < sorted[l])
            {
               for(h=mpe_dqc_max_temp_neighbors-1; h > l; h--)
               {
                  sorted[h]=sorted[h-1];
                  tstation[i].index[h]=
                  tstation[i].index[h-1];
               }

               sorted[l]=dist;
               tstation[i].index[l]=m;
               break;
            }
         }
      }
   }

   return tstation;
}
struct station * get_temperature_station_list ( int * num_tgages )
 {
    * num_tgages = max_tstations;
    return tstation;
}

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
void free_temperature_station_list ( )
{
    int i;

    if ( tstation != NULL )
    {
       /* Loop over the number of stations. */
       for ( i = 0; i < max_tstations; ++i )
       {
          free ( tstation[i].hb5 );
          free ( tstation[i].name );
          free ( tstation[i].parm );
          free ( tstation[i].cparm );
          free ( tstation[i].max );
          free ( tstation[i].min );
          free ( tstation[i].index );
       }

       free ( tstation );
       tstation = NULL;
       max_tstations = 0;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/read_temperature_station_list.c,v $";
 static char rcs_id2[] = "$Id: read_temperature_station_list.c,v 1.7 2007/10/23 17:08:13 lawrence Exp $";}
/*  ===================================================  */

}
