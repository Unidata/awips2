/*******************************************************************************
* FILENAME:  Disagg6hr.c
*
* Purpose:
* Main Disagg routine. Also initializes 6hr station values.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Jan 07 2008  Ram Varma         New Code
*********************************************************************************/

#include <stdio.h>
#include <time.h>

#include "gageqc_types.h"
#include "gageqc_defs.h"
#include "stage3.h"
#include "disagg.h"
#include "mpe_constants.h"
#include "rfcwide.h"
#include <sys/types.h>
#include <sys/stat.h>

#define YYYYmmddHHMM_Len 12

struct station * disagg_station_6hr = NULL;
struct Values_1hr * disaggValues = NULL;
struct Values_6hr * values6hr = NULL;
   
int disagg_db_factor = 100;
struct Dist * dist_6hr_to_1hr = NULL;
FILE *disagg_log_fd = NULL;
FILE *disagg_1hr_neighbors_fd = NULL;
char neighbor_list_file[128];
int disagg_maxx, disagg_maxy;
time_t station_list_time_stamp_6hr;
struct stat buffer;
int ret_neighbors = -1;
char disagg_method[6];

FILE *disagg_stations_fd = NULL;

char **obsdate;
date_t * obsdate_date_t;
time_t tmpTime, start_time, end_time;

/*  global variables for disagg */
char qpe_files_dir[128], date_form[4];
double  *** QPEgrids;
int val6hreq0, val6hrgt0;

time_t endtime_disagg ;
time_t starttime_disagg ;
time_t end_time_temp ;

int num_days_to_qc;

extern time_t btim;

extern struct Values_1hr * valuesReadIn;
extern int * sorted_list_1hr;

void disagg_cleanup()
{
    int i,j, pos = -1;
    
    if(valuesReadIn != NULL)
    {
       free(valuesReadIn);
       valuesReadIn = NULL;
    }

    if(dist_6hr_to_1hr != NULL)
    {
       free(dist_6hr_to_1hr);
       dist_6hr_to_1hr = NULL;
    }    
    
    if(disaggValues != NULL)
    {
       free(disaggValues);
       disaggValues = NULL;
    }
    
    if(values6hr != NULL)
    {
       free(values6hr);
       values6hr = NULL;
    }
    
    if(disagg_stations_fd != NULL)
    {
       fclose(disagg_stations_fd);
       disagg_stations_fd = NULL;
    }
/*
    if(disagg_log_fd != NULL)
    {
       fclose(disagg_log_fd);
       disagg_log_fd = NULL;
    }
*/
    
    if(obsdate != NULL)
    {
       free(obsdate);
       obsdate = NULL;
    }
    if(obsdate_date_t != NULL)
    {
       free(obsdate_date_t);
       obsdate_date_t = NULL;
    }
    
    if(sorted_list_1hr != NULL)
    {
       free(sorted_list_1hr);
       sorted_list_1hr = NULL;
    }
    
    if(!strcmp(disagg_method, "GRID"))
    {
       for(i=0;i<6;i++)
       {
	  for(j=0;j<disagg_maxy;j++)
	  {
	      if(QPEgrids[i][j] != NULL)
	      {
        	 free(QPEgrids[i][j]);
		 QPEgrids[i][j] = NULL;
	      }
	  }

	  if(QPEgrids[i] != NULL)
	  {
             free(QPEgrids[i]);
             QPEgrids[i] = NULL;
	  }
       }

       if(QPEgrids != NULL)
       {
	  free(QPEgrids);
	  QPEgrids = NULL;
       }
   
       grid_cleanup();   
    }

    free_1hr_station_list();

    for(i=0;i<num_days_to_qc;i++)
    {
       for(j=0;j<num_disagg_stations;j++)
       {
          pos = i*num_disagg_stations+j;

          if(disagg_station_6hr[pos].isoh != NULL)
	  {
	     free(disagg_station_6hr[pos].isoh);
	     disagg_station_6hr[pos].isoh = NULL;
	  }
          if(disagg_station_6hr[pos].hb5 != NULL)
	  {
	     free(disagg_station_6hr[pos].hb5);
	     disagg_station_6hr[pos].hb5 = NULL;
	  }
          if(disagg_station_6hr[pos].index != NULL)
	  {
	     free(disagg_station_6hr[pos].index);
	     disagg_station_6hr[pos].index = NULL;
	  }
          if(disagg_station_6hr[pos].parm != NULL)
	  {
	     free(disagg_station_6hr[pos].parm);
	     disagg_station_6hr[pos].parm = NULL;
	  }
          if(disagg_station_6hr[pos].cparm != NULL)
	  {
	     free(disagg_station_6hr[pos].cparm);
	     disagg_station_6hr[pos].cparm = NULL;
	  }
       }
    }   
       
    if(disagg_station_6hr != NULL)
    {
       free(disagg_station_6hr);
       disagg_station_6hr = NULL;
    }
}

void Disagg6hr()
{
   char *p = NULL,kbuf[MAX_STATION_RECORD_LEN];
   static int first = 0;
   time_t start_time, end_time;
   char logdir[128], station_list_dir[128];
   char neighbor_list_dir[128], disagg_log_file[128];
   char gridmask_dir[128];
   char station_list_file[128];
   char mpe_disagg_execute[4];
   char buf[10];
   char cval6hr[2];

   time_t currentTime;
   char datestring[YYYYmmddHHMM_Len+1];

   int max_stations;
   struct station * station = NULL;

   int ier,k, j, i, ii, len=0;
   extern int emonth;
   extern int smonth;
   extern struct isoh *isoh;
   extern char * area_val_local;
   extern struct pdata pdata[MAX_GAGEQC_DAYS];

   HRAP hrap_point;
   int xor, yor, maxx, maxy; 
   int index = -1;
   
   time(&start_time);
   
   num_days_to_qc = get_num_days_to_qc();

   station = get_precip_station_list ( &max_stations );

   currentTime = (time_t) time(NULL);
   strftime (datestring, sizeof(datestring), "%Y%m%d%H%M", gmtime(&currentTime));
      
   //read disagg station list
   //this is not in the "first" block because
   //we have to "stat" this file every time to
   //see if stations have been added or deleted.
   memset(station_list_dir, '\0', 128);
   memset(gridmask_dir, '\0', 128);
   memset(station_list_file, '\0', 128);
   memset(neighbor_list_file, '\0', 128);
   len = strlen("mpe_station_list_dir");
   get_apps_defaults("mpe_station_list_dir",&len,station_list_dir,&len);        
   len = strlen("mpe_gridmask_dir");
   get_apps_defaults("mpe_gridmask_dir",&len,gridmask_dir,&len);        
   sprintf(station_list_file,"%s/%s_disagg_station_list", station_list_dir, area_val_local);
   sprintf(neighbor_list_file,"%s/%s_disagg_1hr_neighbors", gridmask_dir, area_val_local);


   /*---------------------------------------------*/
   /*   First time only:                          */
   /*   read .Apps_defaults tokens                */
   /*   open log file                             */
   /*   read disagg station list                  */
   /*   if using the "point" algorithm, then      */
   /*     generate/read list of surrounding 1hr   */
   /*     stations                                */
   /*---------------------------------------------*/
   if(first == 0)
   {
      memset(mpe_disagg_execute, '\0', 3);
      len = 0;
      len = strlen("mpe_disagg_execute");
      get_apps_defaults("mpe_disagg_execute",&len,mpe_disagg_execute,&len);
      if(!strcasecmp(mpe_disagg_execute, "off"))
      {
         printf("Disagg token is OFF\n");
	 printf("exiting from disagg routine..mpe_editor continuing...\n");
	 
	 return;
      }
     
      memset(logdir, '\0', 128);
      memset(disagg_log_file, '\0', 128);

      /*---------------------------------------------*/
      /*   define log file name                      */
      /*   open log file                             */
      /*---------------------------------------------*/
      len = 0;
      len = strlen("mpe_editor_logs_dir");
      get_apps_defaults("mpe_editor_logs_dir",&len,logdir,&len);        
      sprintf(disagg_log_file,"%s/disagg_%sz",logdir,datestring);
      if((disagg_log_fd = fopen(disagg_log_file,"w")) != NULL)
      {
         fprintf(disagg_log_fd, "\t\t-- 6hr to 1hr Disaggregation -- \n") ;
         fprintf(disagg_log_fd, "\t\tVersion OB8.3 -- January 16, 2008 \n") ;
      }
      else
      {
         printf("Warning: Could not open disagg log file...\n");
	 printf("exiting from disagg routine..mpe_editor continuing...\n");

	 return;
      }

      fprintf(disagg_log_fd, "hydrologic day = 12z - 12z\n");

      first=1;
   }

      memset(disagg_method, '\0', 6);
      memset(neighbor_list_dir, '\0', 128);
      memset(buf, '\0', 10);

      len = 0;
      len= strlen("mpe_disagg_method");
      get_apps_defaults("mpe_disagg_method",&len,disagg_method,&len);        

      fprintf(disagg_log_fd, "6hr disagg station list file name: %s\n", station_list_file);

      read_mpe_coordinate_file ( &xor, &yor, &maxx, &maxy);
      disagg_maxx = maxx;
      disagg_maxy = maxy;
      
      endtime_disagg = btim;
      starttime_disagg = endtime_disagg - num_days_to_qc * SECONDS_PER_DAY;
      end_time_temp = endtime_disagg;
      fprintf(disagg_log_fd, " endtime = %10.0f  starttime = %10.0f\n", (float) endtime_disagg, (float) starttime_disagg);
          
      disagg_stations_fd = fopen(station_list_file, "r");
      if(disagg_stations_fd == NULL)
      {
         fprintf(disagg_log_fd, "WARNING: Could not open disagg station list file\n");
	 fprintf(disagg_log_fd, "exiting from disagg routine..mpe_editor continuing...\n");
	 
	 return;
      }

      fgets(buf, sizeof(buf), disagg_stations_fd);
      num_disagg_stations = atoi(buf);
      if(num_disagg_stations <= 0)
      {
         fprintf(disagg_log_fd, "WARNING: No 6hr gages to disagg\n");
	 fprintf(disagg_log_fd, "exiting from disagg routine..mpe_editor continuing...\n");
	 
	 return;
      }

      disagg_station_6hr = (struct station *) malloc(num_days_to_qc*num_disagg_stations*sizeof(struct station));
      disaggValues = (struct Values_1hr *) malloc(num_days_to_qc*num_disagg_stations*sizeof(struct Values_1hr));
      values6hr = (struct Values_6hr *) malloc(num_days_to_qc*num_disagg_stations*sizeof(struct Values_6hr));
      if(disagg_station_6hr == NULL || disaggValues == NULL || values6hr == NULL)
      {
         fprintf(disagg_log_fd, "malloc of disagg_station_6hr/disaggValues/values6hr failed -- exiting disagg\n");
	 return; 
      }
	 
      dist_6hr_to_1hr = (struct Dist *) malloc(num_disagg_stations * sizeof(struct Dist));
      
      obsdate = (char**) malloc((num_days_to_qc+1)*sizeof(char*));
      obsdate_date_t = (date_t*) malloc((num_days_to_qc+1)*sizeof(date_t));
      if(obsdate == NULL || obsdate_date_t == NULL)
      {
         fprintf(disagg_log_fd, " malloc of obsdate array failed -- exiting disagg\n");
         return;
      }
      
      if(dist_6hr_to_1hr == NULL)
      {
         fprintf(disagg_log_fd, " malloc of dist_6hr_to_1hr array failed -- exiting disagg\n");
         return;
      }

      fprintf(disagg_log_fd, " 6hr Disagg Station List\n");

      for(j=0;j<num_days_to_qc;j++)
      { 
         for(i=0;i<num_disagg_stations;i++)
	 {
	    index = j*num_disagg_stations+i;
	    
	    disagg_station_6hr[index].isoh = NULL;
	    disagg_station_6hr[index].max = NULL;
	    disagg_station_6hr[index].min = NULL;
	    disagg_station_6hr[index].hb5 = NULL;
	    disagg_station_6hr[index].name = NULL;
	    disagg_station_6hr[index].parm = NULL;
	    disagg_station_6hr[index].cparm = NULL;
	    disagg_station_6hr[index].index = NULL;
	    disagg_station_6hr[index].zindex = NULL;
	    
	    dist_6hr_to_1hr[i].distances_to_neighbors = (double*) malloc(mpe_dqc_max_precip_neighbors * sizeof(double));
	    if(dist_6hr_to_1hr[i].distances_to_neighbors == NULL)
	    {
	       fprintf(disagg_log_fd, "malloc failed for dist_6hr_to_1hr[i].distances_to_neighbors");
               fprintf(disagg_log_fd, " -- exiting disagg\n");
	       return; 
	    }
	 
	    disagg_station_6hr[index].isoh=calloc(12,sizeof(float));
	    disagg_station_6hr[index].hb5=calloc(10,sizeof(char));
            disagg_station_6hr[index].index=calloc(mpe_dqc_max_precip_neighbors,sizeof(short int));
	    disagg_station_6hr[index].parm=calloc(10,sizeof(char));
	    disagg_station_6hr[index].cparm=calloc(10,sizeof(char));

	    if(j == 0)
	    {
	       p = fgets ( kbuf, 80, disagg_stations_fd);
	       if ( p == NULL )
	       {
	          //write to log file
	          break;
	       }
	       
	       sscanf(kbuf,"%s %f %f ",disagg_station_6hr[i].hb5, &disagg_station_6hr[i].lat,
	              &disagg_station_6hr[i].lon);

               hrap_point = LatLongToHrapMpe ( disagg_station_6hr[i].lat, disagg_station_6hr[i].lon );

               values6hr[index].hrapx_local = (int)hrap_point.x-xor;
               values6hr[index].hrapy_local = (int)hrap_point.y-yor;
               
	       fprintf(disagg_log_fd," %s   %5.2f  %5.2f;    hrapx = %d hrapy = %d\n",
                   disagg_station_6hr[i].hb5,
                   disagg_station_6hr[i].lat,
                   disagg_station_6hr[i].lon,
		   values6hr[index].hrapx_local,
		   values6hr[index].hrapy_local);

               for (k = 0; k < 12; k++)
	       {
	          disagg_station_6hr[i].isoh[k] = -1;
	          ier = is_good (k, smonth, emonth);
	          if (ier == -1)
	          {
		     continue;
	          }
		  if(((int)hrap_point.x-xor) < maxx && ((int)hrap_point.y-yor) < maxy &&
		    ((int)hrap_point.x-xor) >= 0 && ((int)hrap_point.y-yor) >= 0)
		  {
                     disagg_station_6hr[i].isoh[k] = isoh->value[k][(int)hrap_point.y-yor][(int)hrap_point.x-xor];
		  }
	       }
	    }
	    else
	    {
               values6hr[index].hrapx_local = values6hr[i].hrapx_local;
               values6hr[index].hrapy_local = values6hr[i].hrapy_local;
               
	       for(k=0;k<12;k++)
	       {
		  disagg_station_6hr[index].isoh[k] = disagg_station_6hr[i].isoh[k];
	       }
	    }
	    
	    strcpy(disagg_station_6hr[index].hb5, disagg_station_6hr[i].hb5);
	    disagg_station_6hr[index].lat = disagg_station_6hr[i].lat;
	    disagg_station_6hr[index].lon = disagg_station_6hr[i].lon;
	    
	    for(k=0;k<max_stations;k++)
	    {
	       if(!strcasecmp(disagg_station_6hr[index].hb5, station[k].hb5))
	       {
                  strcpy(values6hr[index].ID, disagg_station_6hr[index].hb5);
		  for(ii=0;ii<4;ii++) //for 4 6hr values
		  {
		     //we treat "bad" 6hr stations as missing in Disagg calculationa
		     //note that "bad" stations have a quality code of '1'
		     if(pdata[j].stn[k].frain[ii].data >= 0 && pdata[j].stn[k].frain[ii].data != 1)
		     {
			values6hr[index].value[ii] = pdata[j].stn[k].frain[ii].data;
		     }
		     else
		     {
		        values6hr[index].value[ii] = -9999.;
		     }
		  }
		  break;
	       }
	    }
	 }
      	 
	 obsdate[j] = (char*) malloc(11*sizeof(char));
	 if(obsdate[j] == NULL)
	 {
	    fprintf(disagg_log_fd, "failed to allocate memory for obsdate -- exiting disagg\n");
	    return; 
	 }
         memset(obsdate[j], '\0', 11);
	 timet_to_yearday_ansi(end_time_temp, obsdate[j]);
	 ansi_date_to_date_t (obsdate[j], &obsdate_date_t[j]);
	 fprintf(disagg_log_fd, "datestring for disagg day %d = %s\n", j, obsdate[j]);	  
	 end_time_temp = end_time_temp - SECONDS_PER_DAY;
      }

      obsdate[num_days_to_qc] = (char*) malloc(11*sizeof(char));
      if(obsdate[num_days_to_qc] == NULL)
      {
	 fprintf(disagg_log_fd, "failed to allocate memory for obsdate -- exiting disagg\n");
         return; 
      }
      memset(obsdate[num_days_to_qc], '\0', 11);
      //note that end_time_temp is not being decremented
      //as in the above loop because it will be decremented 
      //one extra time in the loop already.
      timet_to_yearday_ansi(end_time_temp, obsdate[num_days_to_qc]);
      fprintf(disagg_log_fd, "datestring for disagg day %d = %s\n", num_days_to_qc, obsdate[num_days_to_qc]);
      ansi_date_to_date_t (obsdate[num_days_to_qc], &obsdate_date_t[num_days_to_qc]);

      /* print 6hr values to log */     
      fprintf(disagg_log_fd, "\n");
      fprintf(disagg_log_fd, "6hr Values\n");
      fprintf(disagg_log_fd, "Day #\tPeriod #\tValue \n");
      
      index = -1;
      
      for(i=0;i<num_disagg_stations;i++)
      {
         for(j=0;j<num_days_to_qc;j++)
         { 
            index = j*num_disagg_stations+i;
	    
	    if(j == 0) fprintf(disagg_log_fd, "%s \n", disagg_station_6hr[index].hb5);

            for(ii=0;ii<4;ii++)
            {
               fprintf(disagg_log_fd, "  %d\t%d\t%5.2f\n", j,ii,values6hr[index].value[ii]);

            }
         }
      }

      fprintf(disagg_log_fd, "6hr-1hr disagg method: %s\n", disagg_method);

      fflush(disagg_log_fd);

      if(!strcasecmp(disagg_method, "grid"))
      {
	 len = 0;
         len = strlen("mpe_qpe_dir");
	 memset(qpe_files_dir, '\0', 128);
         get_apps_defaults("mpe_qpe_dir",&len,qpe_files_dir,&len);        
	 len = 0;
         len = strlen("st3_date_form");
	 memset(date_form, '\0', 4);
         get_apps_defaults("st3_date_form",&len,date_form,&len); 

         fprintf(disagg_log_fd, "dir containing QPE files: %s\n", qpe_files_dir);
         fprintf(disagg_log_fd, "xmrg date format: %s\n", date_form);

	 len = 0;
         len = strlen("mpe_disagg_6hreq_0");
	 memset(cval6hr, '\0', 2);
         get_apps_defaults("mpe_disagg_6hreq_0",&len,cval6hr,&len); 
         if(len > 0)
         {
            val6hreq0 = atoi(cval6hr);
            fprintf(disagg_log_fd, "val6hreq0= %d\n", val6hreq0);
         }
         else
         {
            val6hreq0 = 1;
            fprintf(disagg_log_fd, "mpe_disagg_6hreq_0 token not defined -- value = 1 used\n");
         }

	 len = 0;
         len = strlen("mpe_disagg_6hrgt_0");
	 memset(cval6hr, '\0', 2);
         get_apps_defaults("mpe_disagg_6hrgt_0",&len,cval6hr,&len); 
         if(len > 0)
         {
            val6hrgt0 = atoi(cval6hr);
            fprintf(disagg_log_fd, "val6hrgt0= %d\n", val6hrgt0);
         }
         else
         {
            val6hrgt0 = 1;
            fprintf(disagg_log_fd, "mpe_disagg_6hrgt_0 token not defined -- value = 1 used\n");
         }

           /* malloc space for 6 arrays each of size (maxy x maxx) to hold QPE grids */
           /* QPEgrids[k][i][j] array */
           /*   k = 0,1,2,3,4,5  */
           /*   i = num of columns  */
           /*   j = num of rows  */
           QPEgrids = (double ***)malloc(6 * sizeof(double **));
           if(QPEgrids == NULL)
           {
	       fprintf(disagg_log_fd, "ERROR: memory allocation failure for QPEgrids array...\n");
	       fprintf(disagg_log_fd, "Exiting from Disagg...\n");

	       return;
           }

           for(k = 0; k < 6; k++)
           {
              QPEgrids[k] = (double **)malloc(disagg_maxy * sizeof(double *));
              if(QPEgrids[k] == NULL)
              {
	         fprintf(disagg_log_fd, "ERROR: memory allocation failure for QPEgrids array...\n");
	         fprintf(disagg_log_fd, "Exiting from Disagg...\n");

	         return;
              }
              for(i = 0; i < disagg_maxy; i++)
              {
                 QPEgrids[k][i] = (double *)malloc(disagg_maxx * sizeof(double));
                 if(QPEgrids[k][i] == NULL)
                 {
	            fprintf(disagg_log_fd, "ERROR: memory allocation failure for QPEgrids array...\n");
	            fprintf(disagg_log_fd, "Exiting from Disagg...\n");

	            return;
                 }

                 for(j=0;j<disagg_maxx;j++)
                 {
                    QPEgrids[k][i][j] = MOSAIC_DEFAULT;
                 }
              }
           }
	   fflush(disagg_log_fd);
        }
        else
        {
           /*----------------------------------------------------------*/
           /*     generate and read list of surrounding stations       */
           /*----------------------------------------------------------*/
           stat(station_list_file, &buffer);
           station_list_time_stamp_6hr = buffer.st_mtime;
           ret_neighbors = stat(neighbor_list_file, &buffer);
     
           compute_1hr_station_list ();
	   Read1hrGageVals();
        }
      
   fprintf(disagg_log_fd, "---------------------\n");
      
   /*---------------------------------------------*/
   /*   disagg 6hr to 1hr values                  */
   /*---------------------------------------------*/
   if(!strcasecmp(disagg_method, "GRID"))
   {
      /*---------------------------------------------*/
      /*   grid method                               */
      /*---------------------------------------------*/
      DisaggGridMethod();
   }
   else
   {
      /*---------------------------------------------*/
      /* nearest neighbor (point) method             */
      /*---------------------------------------------*/
      DisaggPointMethod();
   }

   /*---------------------------------------------*/
   /*   write 1hr values to HourlyPP table        */
   /*---------------------------------------------*/
   Write1hrValuesFor6hrGages();
    
   time(&end_time);
   fprintf(disagg_log_fd, "Disagg exit --- elapsed time = %ld second(s)\n",(end_time - start_time));
  
   //cleanup/free all the allocated structures in
   //this file and other globals and file descriptors
   disagg_cleanup();
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
