/*******************************************************************************
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Jan 01 2008  Ram Varma         New Code
*
*********************************************************************************/

#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "GeneralUtil.h"
#include "stage3.h"
#include "disagg.h"
#include "rfcwide.h"
#include "sys/stat.h"
#include "time_convert.h"

struct station * disagg_station_1hr = NULL;
extern struct station * disagg_station_6hr;
extern struct Dist * dist_6hr_to_1hr;
struct Values_1hr * valuesReadIn = NULL;
extern FILE *disagg_log_fd;

extern time_t station_list_time_stamp_6hr;
extern FILE *disagg_1hr_neighbors_fd;
extern char neighbor_list_file[128];
extern int ret_neighbors;

extern char **obsdate;
extern date_t * obsdate_date_t;
 
int num_records = -1;
int * sorted_list_1hr = NULL;
char * temp_str = NULL;
int sorted_array_size = 0;
int duplicate_station = 0;

void compute_1hr_station_list ()
{
   extern struct isoh *isoh;
   extern struct dval dval;
   extern int smonth;
   extern int emonth;

   char hb5[10];
   double dist1,dist2,dist,sorted[mpe_dqc_max_precip_neighbors];
   int ind,h,i,ier,l,j,m,k;
   char *p = NULL,kbuf[MAX_STATION_RECORD_LEN];
   HRAP hrap_point;
   float lat,lon;
   double conv=.0174533;
   long t,u;
   float elev;
   
   int file_error = 0;
   int len = 0;
   char area_id[256] = {'\0'};
   static const char * fname = "_station_list";
   char disagg_1hr_stations_dir[256] = {'\0'};
   char disagg_1hr_stations_path[256] = {'\0'};
   int num_distinct_1hrs = 0;
   FILE * disagg_1hr_stations_fd = NULL;
   char line[LINE_MAX];

   int xor, yor, maxx, maxy;

   int timestamp_previous_neighbors;
   int num_previous_neighbors;
   int generate = 0;
   char temp_buf[128];

   struct stat buffer;
 
            double R, dLat, dLon, a, c;
   int num_days_to_qc = get_num_days_to_qc();
 
   len = strlen("mpe_station_list_dir");
   get_apps_defaults("mpe_station_list_dir",&len,disagg_1hr_stations_dir,&len);
   len = 0;
   len = strlen("mpe_site_id");
   get_apps_defaults("mpe_site_id",&len,area_id,&len);

   if(len > 0)
   {
      sprintf (disagg_1hr_stations_path, "%s/%s%s", disagg_1hr_stations_dir, area_id, fname);
      fprintf(disagg_log_fd, "mpe_gage_locations filename = %s\n", disagg_1hr_stations_path);
   }
   else
   {
      fprintf(disagg_log_fd, "token mpe_site_id not defined\n");
      return;
   }
  
   /* Open the station list. */
   disagg_1hr_stations_fd = fopen ( disagg_1hr_stations_path, "r");

   if(disagg_1hr_stations_fd == NULL)
   {
      fprintf(disagg_log_fd, "could not open mpe_gage_locations file\n");
      return;
   }

   /* Read the record containing the number of hourly stations. */

   p = fgets ( kbuf, MAX_STATION_RECORD_LEN, disagg_1hr_stations_fd );

   if ( p == NULL )
   {
      fprintf(disagg_log_fd, "could not read number of hourly stations from file\n");
      fclose ( disagg_1hr_stations_fd );
      disagg_1hr_stations_fd = NULL; 
      return;
   }

   ier = sscanf ( kbuf, "%d", & num_records );

   if ( ier != 1 )
   {
      fprintf(disagg_log_fd, "could not read number of hourly stations from file\n");
      fclose ( disagg_1hr_stations_fd );
      disagg_1hr_stations_fd = NULL; 
      return;
   }

   if ( num_records <= 0 )
   {
      fprintf(disagg_log_fd, "number of hourly stations le 0\n");
      fclose ( disagg_1hr_stations_fd );
      disagg_1hr_stations_fd = NULL; 
      return;
   }

   /* Allocate space for the stations array. */
   disagg_station_1hr = (struct station *) malloc(num_records * sizeof(struct station)) ; 

   if ( disagg_station_1hr == NULL )
   {
      fprintf(disagg_log_fd, " could not malloc space for structure containing 1hr stations\n");
      return;
   }

   temp_str = (char*) malloc(10*sizeof(char));

   read_mpe_coordinate_file ( &xor, &yor, &maxx, &maxy);

   /* Read the PPH stations. */
   i=0;
   for( ind = 0; ind < num_records; ++ind )
   {
      /*p = fgets ( kbuf, MAX_STATION_RECORD_LEN, disagg_1hr_stations_fd ); */
      p = fgets ( kbuf, sizeof(kbuf), disagg_1hr_stations_fd );

      if ( p == NULL)
      {
         fprintf(disagg_log_fd, " error reading 1hr station list -- ind = %d num_records = %d\n",
                 ind, num_records);
         break;
      }

      ier=sscanf(kbuf,"%s %s %f %f ",hb5, temp_str, &lat, &lon);
		 
      disagg_station_1hr[i].isoh=calloc(24,sizeof(float));
      disagg_station_1hr[i].hb5=calloc(10,sizeof(char));
      disagg_station_1hr[i].index=calloc(mpe_dqc_max_precip_neighbors,sizeof(short int));
      disagg_station_1hr[i].parm=calloc(10,sizeof(char));
      disagg_station_1hr[i].cparm=calloc(10,sizeof(char));

      //eliminate duplicate stations from the station list file
      for(k=0;k<num_distinct_1hrs;k++)
      {
         if(!strcmp(hb5, disagg_station_1hr[k].hb5))
	 {
	    //we found a duplicate station
	    duplicate_station = 1;
	    break;
	 }
      }	
      
      if(duplicate_station)
      {
         duplicate_station = 0;
	 continue;
      }
      else
      {
         num_distinct_1hrs++;
      }

      strcpy(disagg_station_1hr[i].hb5,hb5);
      disagg_station_1hr[i].lat = lat;
      disagg_station_1hr[i].lon = lon;

      /* Set elev to 1 ft.  The value
       of 0 creates problems with subsequent computations
       in DailyQC. */

      disagg_station_1hr[i].elev= 1;

      /*if(ier != 4)
      {
	 continue;
      }*/

      lat=disagg_station_1hr[i].lat;
      lon=disagg_station_1hr[i].lon;

      /* Store the station's coordinates in HRAP. */
      hrap_point = LatLongToHrapMpe ( lat, lon );

      disagg_station_1hr[i].hrap_x = hrap_point.x; 
      disagg_station_1hr[i].hrap_y = hrap_point.y;

      for (k = 0; k < 12; k++)
      {
	 disagg_station_1hr[i].isoh[k] = -1;
	 ier = is_good (k, smonth, emonth);
	 if (ier == -1)
	 {
            continue;
	 }
	 if(((int)hrap_point.x-xor) < maxx && ((int)hrap_point.y-yor) < maxy &&
           ((int)hrap_point.x-xor) >= 0 && ((int)hrap_point.y-yor) >= 0)
	 {
            disagg_station_1hr[i].isoh[k] = isoh->value[k][(int)hrap_point.y-yor][(int)hrap_point.x-xor];
	 }
      }

      i++;
   }
   
   num_records = num_distinct_1hrs;

   if(temp_str != NULL)
   {
      free(temp_str);
      temp_str = NULL;
   }
   
   /* Close the stations file. */
   if(disagg_1hr_stations_fd != NULL)
   {
      fclose(disagg_1hr_stations_fd);
      disagg_1hr_stations_fd = NULL;
   }

   //code to find if 1hr nearest neighbors file already exists
   //if exists, then check if either of, the number of neighbors token or
   //the last modification time of the 6hr station lists, both of
   //which are stored in the nearest neighbors file, have changed.
   //if neither of them have changed, just read the file instead of
   //generating the file, else re-generate.
   if(ret_neighbors == 0)
   {
      disagg_1hr_neighbors_fd = fopen(neighbor_list_file, "r");
      fgets(line, LINE_MAX, disagg_1hr_neighbors_fd);
      if(line != NULL)
      {
         sscanf(line,"%d", &num_previous_neighbors);
      }
      else
      {
         generate = 1;
      }
      fgets(line, LINE_MAX, disagg_1hr_neighbors_fd);
      if(line != NULL)
      {
         sscanf(line,"%d", &timestamp_previous_neighbors);
      }
      else
      {
         generate = 1;
      }
      if(mpe_dqc_max_precip_neighbors != num_previous_neighbors ||
        (time_t) timestamp_previous_neighbors < station_list_time_stamp_6hr)
      {
         generate = 1;
      }
      else
      {
         fprintf(disagg_log_fd, "Reading %d nearest neighbor 1hr stations\n", mpe_dqc_max_precip_neighbors);
	       
         for(i=0;i<num_disagg_stations;i++)
	 {
	    fgets(line, LINE_MAX, disagg_1hr_neighbors_fd);
	    for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
	    {
	       //read from neighbor list file
	       //read the 1hr station info for each 6hr station
	       fgets(line, LINE_MAX, disagg_1hr_neighbors_fd);
	       if(line != NULL)
	       {
	          sscanf(line, "%hd%s%f%f%lf", &disagg_station_6hr[i].index[l], 
		                           disagg_station_1hr[disagg_station_6hr[i].index[l]].hb5,
		                           &disagg_station_1hr[disagg_station_6hr[i].index[l]].lat,
					   &disagg_station_1hr[disagg_station_6hr[i].index[l]].lon,
					   &dist_6hr_to_1hr[i].distances_to_neighbors[l]);
	       }
	       else
	       {
	          fprintf(disagg_log_fd, "Error reading neighbor 1hr stations...file corrupted...generating 1hr neighbours\n");
		  file_error = 1;
	       }
	    }
	    if(file_error == 1)
	    {
	       break;
	    }
	 }
         generate = 0;
      }
   }
   else
   {
      generate = 1;
   }

   if(file_error == 1)
   {
      generate = 1;
   }
 
   if(generate == 1)
   {
      //Logic to find 1hr nearest neighbors for the 6hr disagg stations
      //---------------------------------------------------------------
      fprintf(disagg_log_fd, "Generating %d nearest neighbor 1hr stations\n", mpe_dqc_max_precip_neighbors);

      for(i=0;i<num_disagg_stations;i++)
      {
	 for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
	 {
            sorted[l]=9999999;
	 }

	 for ( m=0; m < num_records; ++m )
	 {

            //do not use 1hr station with same id as 6hr station     
            if(!strcmp(disagg_station_1hr[m].hb5, disagg_station_6hr[i].hb5))
            {
               continue;
            }
            dist1=disagg_station_6hr[i].lat-disagg_station_1hr[m].lat;
            dist2=(disagg_station_6hr[i].lon-disagg_station_1hr[m].lon)*
                  cos((disagg_station_6hr[i].lat+disagg_station_1hr[m].lat)*conv/2.);

            dist=pow(dist1,2)+pow(dist2,2); 

/*
            R = 6371.;
            dLat = (disagg_station_6hr[i].lat-disagg_station_1hr[m].lat) * conv;
	    dLon = (disagg_station_6hr[i].lon-disagg_station_1hr[m].lon) * conv;
	    a = sin(dLat/2.) * sin(dLat/2.) +
	        cos(disagg_station_6hr[i].lat * conv) * cos(disagg_station_1hr[m].lat * conv) *
		sin(dLon/2.) * sin(dLon/2.);
	
	    c = 2. * atan2(sqrt(a), sqrt(1.-a)); 
	    dist = R * c;
	    
*/
            /*---------------------------------------------------------*/
            /* do not use 1hr stations greater than a certain distance */
            /*---------------------------------------------------------*/
/*
	    if(dist > max_distance)
	    {
	       continue;
	    }
*/

            //In the following logic we calculate if the current distance
	    //between the 6hr and 1hr stations is shorter than one of the gages
	    //in the 'sorted' array. If it is we add it and remove the farthest
	    //distance from the array. 
	    //Note that the 'sorted' array is sorted literally!
	    for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
            {
               if(dist < sorted[l])
               {
        	  for (h=mpe_dqc_max_precip_neighbors-1; h > l; h--)
        	  {
                     sorted[h]=sorted[h-1];
                     disagg_station_6hr[i].index[h] = disagg_station_6hr[i].index[h-1];
        	  }

		  //add this distance into the array of current "num neighbors" shortest distances
        	  sorted[l]=dist; 

		  //add the 1hr station as a nearest neighbor to the 6hr station
        	  disagg_station_6hr[i].index[l]=m; 

        	  //storing the distance of this 1hr gage from the 6hr gage
		  dist_6hr_to_1hr[i].distances_to_neighbors[l] = dist; 

        	  break;
               }
            }
	 }
      }
      //----------------------------------------------------------------
   }

   if(generate == 1)
   {
      if(disagg_1hr_neighbors_fd != NULL)
      {
         fclose(disagg_1hr_neighbors_fd);
	 disagg_1hr_neighbors_fd = NULL;
      }
      
      disagg_1hr_neighbors_fd = fopen(neighbor_list_file, "w");
      fprintf(disagg_1hr_neighbors_fd, "%d\n", mpe_dqc_max_precip_neighbors);
      stat(neighbor_list_file, &buffer);
      station_list_time_stamp_6hr = buffer.st_mtime;
      fprintf(disagg_1hr_neighbors_fd, "%d\n", (int)station_list_time_stamp_6hr);
   }

   /* print out list of 1hr nearest neighbor stations */
   for(i=0;i<num_disagg_stations;i++)
   {
      fprintf(disagg_log_fd, "---%s---\n",disagg_station_6hr[i].hb5);
      if(generate == 1)
      {
         fprintf(disagg_1hr_neighbors_fd, "%s\n", disagg_station_6hr[i].hb5);
      }

      for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
      {
         memset(temp_buf, '\0', 128);
	 sprintf(temp_buf, "%d\t%s\t%5.2f\t%5.2f\t%6.2lf", 
	         disagg_station_6hr[i].index[l], 
		 disagg_station_1hr[disagg_station_6hr[i].index[l]].hb5,
	   	 disagg_station_1hr[disagg_station_6hr[i].index[l]].lat,
		 disagg_station_1hr[disagg_station_6hr[i].index[l]].lon,
		 dist_6hr_to_1hr[i].distances_to_neighbors[l]*60.);

         if(generate == 1)
	 {
            fprintf(disagg_1hr_neighbors_fd, "%s\n", temp_buf);
	 }
		 
         fprintf(disagg_log_fd, "%s\n", temp_buf);
      }
   }
   if(disagg_1hr_neighbors_fd != NULL)
   {
      fclose(disagg_1hr_neighbors_fd);
      disagg_1hr_neighbors_fd = NULL;
   }
}

void Read1hrGageVals()
{
   int pp_rec_cnt = 0;
   int pc_rec_cnt = 0;
   short int duration = 1 ; /* The duration in hours. */
   struct total_precip total_precip ;
   extern time_t endtime_disagg ;
   extern time_t starttime_disagg ;
   time_t end_time_temp ;
   int status ;
   HourlyPP * pOrigHourlyPP = NULL ;
   HourlyPP * pHourlyPP = NULL ;
   HourlyPC * pOrigHourlyPC = NULL ;
   HourlyPC * pHourlyPC = NULL ;
   double min_percent = 0.0 ;
   short int advance = 0 ;
   int day = 0;
   
   int i, j, k;
   int one_time = 0;
   int temp[num_disagg_stations * mpe_dqc_max_precip_neighbors];

   int num_days_to_qc = get_num_days_to_qc();

   fprintf(disagg_log_fd, " Reading 1hr Precip Gage Values \n");
   
   sorted_list_1hr = (int*) malloc(num_disagg_stations * mpe_dqc_max_precip_neighbors * sizeof(int));
   if(sorted_list_1hr == NULL)
   {
      fprintf(disagg_log_fd, " malloc of sorted_list_1hr array failed -- exit \n");
      exit(0);
   }
  
   /* sorted_list_1hr array is array of indexes of 1hr gages*/
   /* defined in compute_1hr_station_list function */

   for(i=0;i<num_disagg_stations * mpe_dqc_max_precip_neighbors; i++)
   {
      sorted_list_1hr[i] = -1;
   }
   for(i=0;i<num_disagg_stations; i++)
   {
      for(j=0;j<mpe_dqc_max_precip_neighbors;j++)
      {
         sorted_list_1hr[mpe_dqc_max_precip_neighbors * i + j] = disagg_station_6hr[i].index[j]; 
      }
   }

   //remove duplicates (if any) from among sets of 20 nearest-neighbor stations
   sort_without_duplicates(sorted_list_1hr, temp, num_disagg_stations * mpe_dqc_max_precip_neighbors, &sorted_array_size);
   fprintf(disagg_log_fd, " number of non-duplicate 1hr nearest-neighbor stations = %d\n", sorted_array_size);

   valuesReadIn = (struct Values_1hr *) malloc(num_days_to_qc * sorted_array_size * sizeof(struct Values_1hr));
   if(valuesReadIn == NULL)
   {
      fprintf(disagg_log_fd, " malloc of valuesReadIn array failed -- exit \n");
      exit(0);
   }

   fprintf(disagg_log_fd, "endtime_disagg = %10.0f  starttime_disagg = %10.0f\n", (float) endtime_disagg, (float) starttime_disagg);
   
   //get 1hr precip gage values using precip totalling routines
   //loop on the number of unique (non-duplicate) 1hr stations
   for(i=0; i<sorted_array_size; i++) 
   {
      day = 0;
      if(sorted_list_1hr[i] != -1)
      {
         fprintf(disagg_log_fd, "station = %s\n", disagg_station_1hr[sorted_list_1hr[i]].hb5);

         /* load precip data for all hours for this station */
	 pOrigHourlyPP = load_PP_hourly ( starttime_disagg, endtime_disagg, 
	                 disagg_station_1hr[sorted_list_1hr[i]].hb5, NULL, 0, &pp_rec_cnt ) ;
	 pOrigHourlyPC = load_PC_hourly ( starttime_disagg, endtime_disagg, 
	                 disagg_station_1hr[sorted_list_1hr[i]].hb5, NULL, 0, &pc_rec_cnt ) ;
        
         pHourlyPP = pOrigHourlyPP;
         pHourlyPC = pOrigHourlyPC;
	 if(pHourlyPP == NULL && pHourlyPC == NULL)
	 {
            fprintf(disagg_log_fd, "precip totalling routines found no data for gage %s and missing data substituted \n", disagg_station_1hr[sorted_list_1hr[i]].hb5);
	    end_time_temp = endtime_disagg;
	    
	    for(k=0;k<num_days_to_qc;k++)
	    {  
	       valuesReadIn[k*sorted_array_size + i].index_in_1hr_list = sorted_list_1hr[i];
	       strcpy(valuesReadIn[k*sorted_array_size + i].ID, disagg_station_1hr[sorted_list_1hr[i]].hb5);
	       for(j=0;j<24;j++)
	       {
	          end_time_temp = end_time_temp - SECONDS_PER_HOUR;
	          valuesReadIn[k*sorted_array_size + i].HourlyValues[j] = -9999.;
	          if(j == 23)
	          {
		     valuesReadIn[k*sorted_array_size + i].dqc_day = day++;
	          }
	       }
	    }
	 }
         else
         {
            fprintf(disagg_log_fd, "hour  day  value\n");
            end_time_temp = endtime_disagg;

	    for(k=0;k<num_days_to_qc;k++)
	    {
	       strcpy(valuesReadIn[k*sorted_array_size + i].ID, disagg_station_1hr[sorted_list_1hr[i]].hb5);	       
	       for(j=0;j<24;j++)
	       {
	          end_time_temp = end_time_temp - SECONDS_PER_HOUR;
	          total_precip =  get_total_hourly_precip (&pHourlyPC, &pHourlyPP, end_time_temp, duration ,
		                  min_percent, PRECIP_TS_RANK, advance, &pc_rec_cnt, &pp_rec_cnt ) ;

	          valuesReadIn[k*sorted_array_size + i].HourlyValues[j] = total_precip.value;
	          if(j == 23)
	          {
		     valuesReadIn[k*sorted_array_size + i].dqc_day = day++;
	          }

                  fprintf(disagg_log_fd, "   %d  %d  %6.2f\n", j, k, total_precip.value);
	       }
	    } /* end for (k=0 ... */
         } /* if(pHourlyPP == NULL)  */
      } /* end if(sorted_list_1hr ... */
   } /* end for (i=0 ... */

   fflush(disagg_log_fd);
}

void free_1hr_station_list()
{
    int i;

    if ( disagg_station_1hr != NULL )
    {
       for ( i = 0; i < num_records; ++i )
       {
          free(disagg_station_1hr[i].isoh);
          free(disagg_station_1hr[i].hb5);
          free(disagg_station_1hr[i].parm);
          free(disagg_station_1hr[i].cparm);          
	  disagg_station_1hr[i].isoh = NULL;
          disagg_station_1hr[i].hb5 = NULL;
          disagg_station_1hr[i].parm = NULL;
          disagg_station_1hr[i].cparm = NULL;
       }

       free(disagg_station_1hr);
       disagg_station_1hr = NULL;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
