#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"
#include "stage3.h"

/* The maximum number of precipitation gages. */
int max_stations = 0;

/* Contains the array of precip gages used in the 6,24 
   gage qc. */
struct station * station = NULL;

int pcp_in_use [ 500 ];

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_precip_station_list
* PURPOSE:       Reads the list of PPD stations from the station list file.
*                Dynamically allocates an array of station structures to
*                contain the read-in station data.  This array must be
*                decallocated by the caller of this routine.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Output int *       num_gages      The number of PPD stations read from the
*                                     station list file. 
*   Input  char *      area_name      The name of the area to read the station
*                                     list for.  If NULL, then it defaults
*                                     to the master station list.
*   Input  int         master_flag    1 - read from the master file, skip PPH
*                                         section.
*                                     0 - not reading from master file ...
*                                         there is no PPH section.
* RETURNS:
*   DATA TYPE                         DESCRIPTION
*   struct station *                  Dynamically allocated array of 
*                                     station information.
*
* APIs UTILIZED:
*   NAME                       HEADER FILE     DESCRIPTION
*   get_station_list_path      gageqc_types.h  Retrieves the station path.
*   logMessage                 gageqc_types.h  Writes to the log file.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE     NAME                 DESCRIPTION
*   struct dval   dval      
*   char *        fname                The path/name of the station list file.
*   char          hb5[]                The handbook 5 identifier of the station.
*   char          parm[]               The parameter code of the station.
*   char          message[]            Message to log to log file.
*   double        dist1                Used in determining list of closest
*                                      station neighbors. 
*   double        dist2                Used in determining list of closest
*                                      station neighbors.
*   double        dist                 Used in determining list of closest
*                                      station neighbors.
*   double        sorted[]             The array of gage neighbors.
*   char          station_list_custom_file[] The file containing station label
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
*                                       being read from the station list
*                                       file. 
*
* DATA FILES AND/OR DATABASE:
* Relies on the existence of the station file.
* The path to this file is given by token mpe_station_list_dir.
* The name of the file is built as <area_name>_station_list.  If area_name
* is NULL, then the name of the file is <mpe_site_id>_station_list where
* mpe_site_id is a token.
*
* ERROR HANDLING:
*    ERROR CODE                         DESCRIPTION
*    NULL                               No station list information could be
*                                       be read.
*
********************************************************************************
*/

struct station * read_precip_station_list ( int * num_gages,
                                            const char * area_name,
	                                    int master_flag )
{
   extern struct dval dval;
   extern int mpe_dqc_max_precip_neighbors;

   const char * fname = NULL;
   char hb5[10],parm[10];
   char message [ LOG_MESSAGE_LEN ];
   double dist1,dist2,dist,sorted[mpe_dqc_max_precip_neighbors];
   extern char station_list_custom_file [ ];
   int h,i,ier,l,sflag,field,j;
   FILE *fp = NULL;
   char *p = NULL,kbuf[MAX_STATION_RECORD_LEN];
   HRAP hrap_point;
   int index;
   int m;
   int num_records;
   int space_test;
   int status;
   int xadd, yadd;
   float lat,lon;
   float conv=.0174;
   long t,u;
   float elev;

   int record_num = 1;
 
   max_stations=0;
   * num_gages = 0;
  

   /* Retrieve the name and path of the station list file. */
   fname = get_station_list_path ( area_name );

   if ( fname == NULL )
   {
      sprintf ( message, "No path or name defined for the station list.\n" );
      logMessage ( message );
      return NULL;
   }

   /* Open the station list. */
   fp = fopen ( fname, "r" );

   if( fp == NULL )
   {
     logMessage("could not open %s\n",fname);
      memset(message, '\0', LOG_MESSAGE_LEN);
      sprintf(message,"Could not open file: %s\n",fname);
      logMessage(message);
      return NULL;
   }
   else
   {
      memset(message, '\0', LOG_MESSAGE_LEN);
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
         logMessage ( message );
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
      sprintf ( message, "Could not read the number of 6/24 hour gages\n" 
                            "from file %s record %d\n", fname, record_num );
     logMessage ( "%s", message );
      logMessage ( message );
   }

   if ( num_records == 0 )
   {
      sprintf ( message, "There are no PPD records to be read.\n" );
      logMessage ( message );
      fclose ( fp );
      fp = NULL; 
      return NULL;
   }

   /* Allocate space for the stations array. */
   station = ( struct station * ) malloc ( num_records * 
                                           sizeof ( struct station )) ;
                                           

   if ( station == NULL )
   {
      sprintf ( message, "Could not allocate memory for the array of "
                         "struct stations.\n" );
      logMessage ( message );
      return NULL;
   }

   /* Read the PPD stations. */
   for( i = 0; i < num_records; ++i )
   {

    p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );

    if ( p == NULL )
    {
       sprintf ( message, "Reached EOF while reading record %d of\n"
                 "file %s\n", record_num, fname );
      logMessage ( "%s", message );
       logMessage(message);
       break;
    }

    station[i].isoh=calloc(24,sizeof(float));
    station[i].hb5=calloc(10,sizeof(char));
    station[i].name=calloc(50,sizeof(char));
    station[i].parm=calloc(10,sizeof(char));
    station[i].cparm=calloc(10,sizeof(char));
    station[i].index=calloc(mpe_dqc_max_precip_neighbors,sizeof(short int));
    station[i].zindex=calloc(5,sizeof(short int));
    station[i].xadd = 0;
    station[i].yadd = 0; 

    ier=sscanf(kbuf,"%s %s %f %f %f %d ",station[i].hb5,
               station[i].parm,&station[i].lat,
               &station[i].lon,&elev,&station[i].tip);

    /* If the elevation is 0, set it to 1 foot.  The value
       of 0 creates problems with subsequent computations
       in DailyQC. */
    if ( elev == 0 )
    {
       elev = 1;
    }

    station[i].elev= ( int ) elev;
    
    if(ier != 6)
    {
       continue;
    }

    lat=station[i].lat;
    lon=station[i].lon;

    /* Store the station's coordinates in HRAP. */
    hrap_point = LatLongToHrapMpe ( lat, lon );

    station[i].hrap_x = hrap_point.x; 
    station[i].hrap_y = hrap_point.y;


    t=dval.a * cos(lat*conv)/(1+sin(lat*conv))
      * cos((lon-dval.lo-90)*conv) + dval.xo +.5;

    station[i].x=t;

    u=dval.a * cos(lat*conv)/(1+sin(lat*conv))
      * sin((lon-dval.lo-90)*conv) + dval.yo + .5;
       
    station[i].y=u;

    sflag=1;
    field=0;

    for ( j = 0; j < strlen(kbuf); j++) 
    {

       space_test = isspace ( kbuf[j] );

       if( ( space_test == 0 )  && (sflag==1) )
       {
          sflag=0; 
          ++field;
       }
       else if ( space_test != 0 )
       {
          sflag=1;
       }

       if(field==7)
       {
          kbuf[j+49]=0;

          /* Trim off any trailing white space characters. */
          strip_tblanks ( &kbuf[j] );

          strcpy(station[i].name,&kbuf[j]);
          break;

       }
    }

   }

   max_stations = i;
   * num_gages = max_stations ;

   /* Close the file. */
   fclose(fp);
   fp = NULL;

   for(i=0;i<max_stations;i++)
   {
      for(m=0;m<24;m++)
      {
         station[i].isoh[m]=-99; 
      }
   }

   /* Open the custom station position file. */
   sprintf ( station_list_custom_file, "%s_label_position", fname );

   /* Read in the custom positioning information for the temperature
      stations. */
   record_num = 0;
   fp = fopen ( station_list_custom_file, "r" );

   if ( fp != NULL )
   {
      memset(message,'\0',150);
      sprintf(message,"Opened file: %s\n", station_list_custom_file);
      logMessage(message);

      index = 0;

      p = fgets ( kbuf, 80, fp );

      while ( ( p != NULL ) && ( index < max_stations ) )
      {
          ++ record_num;

          ier = sscanf ( kbuf,"%s %s %d %d",hb5,parm,&xadd,&yadd );
          if ( ier != 4 )
          {
             sprintf ( message, "In file %s, record number %d is incomplete. "
                                "This record is being skipped.\n",
                                station_list_custom_file, record_num );
             p = fgets ( kbuf, MAX_STATION_RECORD_LEN, fp );
             continue;
          }

          status = strcmp ( hb5, station [ index ].hb5 );

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
             if(strncmp(parm,station[index].parm,3)==0 &&
                        parm[4]==station[index].parm[4])
             {
                station[index].xadd=xadd;
                station[index].yadd=yadd;
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
      sprintf(message,"Could not open custom precip station file: %s\n",
              station_list_custom_file);
      logMessage(message);
   }

   /* Compute the list of closest neighbor's.  For each station,
      this list will include a list of the indexes of the closest
      gages. */
   /* This is very inefficient.  It needs to eventually be done as 
      a preprocessor step. */
   for(i=0;i<max_stations;i++)
   {
      for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
      {
         sorted[l]=9999999;
      }

      for ( m=0; m < max_stations; ++m )
      {
         if ( i == m )
         {
            continue;
         }

         dist1=station[i].lat-station[m].lat;
         dist2=(station[i].lon-station[m].lon)*
                cos((station[i].lat+station[m].lat)/2*conv);

         dist=pow(dist1,2)+pow(dist2,2);

         for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
         {
            if(dist < sorted[l])
            {
               for (h=mpe_dqc_max_precip_neighbors-1; h > l; h--)
               {
                  sorted[h]=sorted[h-1];
                  station[i].index[h]=
                  station[i].index[h-1];
               }

               sorted[l]=dist;
               station[i].index[l]=m;

               break;

            }
         }
      }
   }

   return station;
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

struct station * get_precip_station_list ( int * num_gages )
{
   * num_gages = max_stations;
   return station;     
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
void free_precip_station_list ( )
{
    int i;

    if ( station != NULL )
    {
       for ( i = 0; i < max_stations; ++i )
       {
          free ( station[i].isoh );
          free ( station[i].hb5 );
          free ( station[i].name );
          free ( station[i].parm );
          free ( station[i].cparm );
          free ( station[i].index );
          free ( station[i].zindex );
       }

       free ( station );
       station = NULL;
       max_stations = 0;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCEngine/RCS/read_precip_station_list.c,v $";
 static char rcs_id2[] = "$Id: read_precip_station_list.c,v 1.3 2007/10/23 17:08:10 lawrence Exp $";}
/*  ===================================================  */

}
