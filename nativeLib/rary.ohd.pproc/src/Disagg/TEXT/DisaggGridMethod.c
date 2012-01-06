/*******************************************************************************
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Jan 01 2008  Paul Tilles         New Code
*
*********************************************************************************/

/*----------------------------------------------------*/
/* function to disagg 6hr values into 1hr values      */
/*  using 1hr Best Estimate QPE grids                 */
/*                                                    */
/* Inputs:                                            */
/*  1) 6hr disagg gages - identifiers and values      */
/*  2) directory containing QPE grids                 */
/*     token name = mpe_qpe_dir                       */
/*  3) QPE grids                                      */
/*  4) disagg_db_factor = 100                         */
/*               - units of db values are inches x 100*/
/*                                                    */
/* Output:                                            */
/* 1) 1hr values for each 6hr gage                    */
/*                                                    */
/* QPEgrids[k][i][j] array                            */
/*   k = 0,1,2,3,4,5 (hour index)                     */
/*   i = Num of cols                                  */
/*   j = Num of Rows                                  */
/*----------------------------------------------------*/

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "mpe_fieldgen.h"
#include "disagg.h"
#include "time_convert.h"

extern struct station * disagg_station_6hr;
extern struct Values_1hr * disaggValues;
extern double  *** QPEgrids;
extern int  val6hreq0, val6hrgt0;
extern int disagg_db_factor;
extern struct Values_6hr * values6hr;
extern FILE *disagg_log_fd;
char **obsdate;
extern date_t * obsdate_date_t;
   
double * total_6hr;
double ** totals_1hr;

void grid_cleanup()
{
   int i;

   if(total_6hr != NULL)
   {
      free(total_6hr);
      total_6hr = NULL;
   }
   
   for(i=0;i<6;i++)
   {
      if(totals_1hr[i] != NULL)
      {
         free(totals_1hr[i]);
	 totals_1hr[i] = NULL;
      }
   }

   if(totals_1hr != NULL)
   {
      free(totals_1hr);
      totals_1hr = NULL;
   }
}

void DisaggGridMethod()
{

   int i,j,k,l,index,nn,mm;
   int hour, hrapx, hrapy, num_miss;
   double total, diff_1hr;
   time_t end_time_temp;   
   extern time_t starttime_disagg, endtime_disagg;
   char date[24];

   int num_days_to_qc = get_num_days_to_qc();

   /*--------------------------------------*/
   /*  initialize other variables          */
   /*  double totals_1hr[6][num_disagg_stations]  */
   /*  double total_6hr[num_disagg_stations]      */
   /*--------------------------------------*/
   total_6hr = (double *) malloc(num_disagg_stations*sizeof(double));
   totals_1hr = (double **) malloc(6*sizeof(double));
   
   for(i = 0; i < 6; i++)
   {
      totals_1hr[i] = (double *) malloc(num_disagg_stations*sizeof(double));
   }

   end_time_temp = endtime_disagg;

   for(j=0;j<num_days_to_qc;j++)
   {
      for(k=0;k<4;k++)//loop on 4 6hr periods
      {
            /*-------------------------------------------*/
            /*   if 6hr value is missing, set all resulting 1hr values to missing  */
            /*-------------------------------------------*/

            /*-------------------------------------------*/
            /*   read QPE grids into QPEgrids array      */
            /*-------------------------------------------*/

            fprintf(disagg_log_fd, "\n   -- 6hr Period %d -- \n",k);
            fflush(disagg_log_fd);
            num_miss = GetQPEGrids (j, k);

            if(num_miss > 0)
            {
               /*---------------------------------------------------------*/
               /*  case of missing QPE grid(s)                            */
               /*  set all 1hr values to missing for all disagg stations  */
               /*     and take next 6hr period                            */
               /*---------------------------------------------------------*/
               fprintf(disagg_log_fd, "%d QPE grids missing -- all 1hr values set to missing\n",num_miss);
               fflush(disagg_log_fd);

               for (l=0; l<6; l++)
               {
        	  for(i=0;i<num_disagg_stations;i++)
        	  {
                     disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] = -9999;
        	  }
               }

               continue;
            }
            else
            {

               /*------------------------------------------------------------*/
               /*  case of all 6 QPE grids available                         */
               /*                                                            */
               /*  calculate average of 9 nearest neighbor grid values       */
               /*       to location of gage                                  */
               /*------------------------------------------------------------*/
               /*  QPEgrids[l][i][j] array                                   */
               /*    l = 0,1,2,3,4,5                                         */
               /*    i = num of cols                                   */
               /*    j = num of rows                                      */
               /*  values6hr[].hrapx_local = HRAP x coord of station      */
               /*  values6hr[].hrapy_local = HRAP y coord of station      */
               /*------------------------------------------------------------*/

        	  for(i=0;i<num_disagg_stations;i++)
        	  {
                     index = j*num_disagg_stations+i;
                     hrapx = values6hr[index].hrapx_local;
                     hrapy = values6hr[index].hrapy_local;

                     total_6hr[i] = 0.0; 

                     for (l=0; l<6; l++)
                     {

                        total = 0;

                        for(nn=-1; nn<2; nn++)
                        {
                	   for(mm=-1; mm<2; mm++)
                	   {
                              total = total + QPEgrids[l][hrapy+nn][hrapx+mm];
                	   } 
                        }

                        totals_1hr[l][i] = total/9.; 
                        total_6hr[i] = total_6hr[i] + total/9.; 
                        fprintf(disagg_log_fd," i=%d  l=%d  avg1hr=%5.2f\n",i,l,totals_1hr[l][i]);
                     }

                  }

               /*----------------------------------------------------------*/
               /*  if 6hr value is missing,                                */
               /*    then set all resulting 1hr values to missing          */
               /*  check for case of 6hr value = 0.0, sum 1hr values > 0.0 */
               /*    val6hreq0 = 1,2                                       */
               /*    1 -- ignore 1hr estimates, set all 1hr values = 0.0   */
               /*    2 -- ignore 6hr value, use 1hr values from grids      */
               /*  check for case of 6hr value > 0.0, sum 1hr values = 0.0 */
               /*    val6hrgt0 = 1,2                                       */
               /*    1 -- ignore 6hr gage value, set all 1hr values to 0.0 */
               /*    2 -- ignore 1hr estimated values, reestimate based on */
               /*               nearest neighbor 1hr gages                 */
               /*              (future enhancement)                        */
               /*----------------------------------------------------------*/

               for(i=0;i<num_disagg_stations;i++)
               {
                     index = j*num_disagg_stations+i;
                     fprintf(disagg_log_fd, "%s \n",disagg_station_6hr[i].hb5);
                     
                     if(values6hr[index].value[k] == -9999.)
                     {

                	 for (l=0; l<6; l++)
                	 {
                            disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] = -9999.;
                	 }
                     }
                     else if(values6hr[index].value[k] == 0.0 && total_6hr[i] > 0.0)
                     {
                	if(val6hreq0 == 1)
                	{
                            fprintf(disagg_log_fd, "case of 6hr value = 0.0, sum 1hr values > 0.0 -- ");
                            fprintf(disagg_log_fd, "ignore 1hr estimates, set all 1hr values = 0.0\n");
                            for (l=0; l<6; l++)
                            {
                               disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] = 0.0;
                            }
                	}
                	else
                	{
                            fprintf(disagg_log_fd, "case of 6hr value = 0.0, sum 1hr values > 0.0 -- ");
                            fprintf(disagg_log_fd, "ignore 6hr value, use 1hr values from grids\n");
                            for (l=0; l<6; l++)
                            {
                               disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] =
                                  totals_1hr[l][i] * disagg_db_factor;
                            }
                	}
                     }
                     else if (values6hr[index].value[k] > 0.0 && total_6hr[i] == 0.0)
                     {
                	if(val6hrgt0 == 1)
                	{
                            fprintf(disagg_log_fd, "case of 6hr value > 0.0, sum 1hr values = 0.0 -- ");
                            fprintf(disagg_log_fd, "ignore 6hr gage value, set all 1hr values to 0.0\n");
                            for (l=0; l<6; l++)
                            {
                               disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] = 0.0;
                            }
                	}
                	else
                	{
                            /* future enhancement - use nearest neighbor method to estimate 1hr values */

                            fprintf(disagg_log_fd, "case of 6hr value > 0.0, sum 1hr values = 0.0 -- ");
                            fprintf(disagg_log_fd, "ignore 6hr gage value, set all 1hr values to 0.0\n");
                            for (l=0; l<6; l++)
                            {
                               disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] = 0.0;
                            }
                	}
                     }
                     else
                     {
                	/*-------------------------------------------------*/
                	/* compare 6hr value against sum of six 1hr values */
                	/* apply difference equally to all hours in period */
                	/*-------------------------------------------------*/

                	diff_1hr = ( values6hr[index].value[k] - total_6hr[i] )/6.;

                	for (l=0; l<6; l++)
                	{
                           disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] =              
                        	 (totals_1hr[l][i] + diff_1hr) * disagg_db_factor;             
                           if(disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] < 0.0) 
                              disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l] = 0.0;
                	}
                     }

                     for (l=0; l<6; l++)
                     {
                           hour = (k*6) + l;
                           fprintf(disagg_log_fd, "    %d    %6.1f\n",
                              hour,disaggValues[j*num_disagg_stations+i].HourlyValues[6*k+l]);
                           fflush(disagg_log_fd);
                     }

               }  /* end for(i=0;i<num_disagg_stations;i++) */

            }  /*  end if(num_miss > 0) ... */

      }  /* end loop on for(k=0;k<4;k++) */

      /*-----------------------------*/
      /*  take next day              */
      /*-----------------------------*/

      end_time_temp = end_time_temp + SECONDS_PER_DAY ;

   }  /* end loop on for(j=0;j<num_days_to_qc;j++) */

}

/***************************************************************************/

int GetQPEGrids (int j, int k)
{

   /*------------------------------------------------*/
   /* function to read QPE grids for a 6hr period    */
   /* Input:                                         */
   /*  j = day number                                */
   /*  k = period number                             */
   /*  obsdate array - contains dates in yyyy-mm-dd format  */
   /*  disagg_maxx,disagg_maxy = max hrap x/y coord                */
   /*  qpe_files_dir = dir containing QPE grids      */
   /*  date_form = value of st3_date_form token      */
   /*            - format of date in filename        */
   /*                                                */
   /* Output:                                        */
   /* 1) QPE grid values stored in QPEgrids array    */
   /* 2) num_miss = number of missing 1hr QPE grids  */
   /*------------------------------------------------*/

   int i,l,jj,hour,len,ierr, num_miss;
   char qpe_filename[256]={'\0'};
   char os[3];
   FILE * qpe_file_fd;
   double factor=2540.;
   extern char qpe_files_dir[128], date_form[4];
   extern int disagg_maxx, disagg_maxy;

   int num_days_to_qc = get_num_days_to_qc();

   num_miss = 0;
   os[2]='\0';
   strcpy(os, "LX");
   jj = j + 1;

   /*----------------------------------------------*/
   /*  loop on 6 hours in each period              */
   /*----------------------------------------------*/

   for (l=0; l<6; l++)
   {

      /*----------------------------------------------*/
      /*  create file name  */
      /*  filenames are of the form: */
      /*     xmrgmmddyyyyhhz for st3_date_form = mdY  */
      /*     xmrgyyyymmddhhz for st3_date_form = Ymd  */
      /*----------------------------------------------*/

      hour = 12 + (k*6) +l + 1;
      if(hour == 24)
      {
         hour = 0;
         jj = j;
      }
      else if (hour > 23)
      {
         hour = ((k-2)*6) + l +1;
         jj = j;
      }

      if(date_form[0] == 'm')
      {
	 sprintf(qpe_filename, "%s/xmrg%c%c%c%c%c%c%c%c%02dz",
                           qpe_files_dir,
                           obsdate[jj][5],obsdate[jj][6],obsdate[jj][8],obsdate[jj][9],
                           obsdate[jj][0],obsdate[jj][1],obsdate[jj][2],obsdate[jj][3],
                           hour);
      }
      else
      {
	 sprintf(qpe_filename, "%s/xmrg%c%c%c%c%c%c%c%c%02dz",
                           qpe_files_dir,
                           obsdate[jj][0],obsdate[jj][1],obsdate[jj][2],obsdate[jj][3],
                           obsdate[jj][5],obsdate[jj][6],obsdate[jj][8],obsdate[jj][9],
                           hour);
      }
      len = strlen("qpe_filename");

      /*----------------------------*/
      /*  attempt to open the file  */
      /*----------------------------*/

      fprintf(disagg_log_fd, "attempting to open file %s\n",qpe_filename);
      fflush(disagg_log_fd);

      qpe_file_fd = fopen(qpe_filename, "r");
      if(qpe_file_fd == NULL)
      {
	 fprintf(disagg_log_fd, "      could not open file\n");
	 fflush(disagg_log_fd);
	 num_miss++;
	 continue;
      }

      /*--------------------------------------*/
      /*  read QPE file   */
      /*  format of file is xmrg  */
      /*  store values in QPEgrids array      */
      /*  readxmrg routine transforms values to float */
      /*--------------------------------------*/

      readxmrg ( os, disagg_maxy , disagg_maxx , qpe_filename , len , factor, QPEgrids[l] , &ierr) ; 

      if (ierr != 0)
      {
	 fprintf(disagg_log_fd, "   error reading file\n");
	 num_miss++;
	 continue;
      }

   }

   return num_miss;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
