/*******************************************************************************
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Jan 01 2008  Ram Varma         New Code
*
*********************************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "gageqc_defs.h"
#include "gageqc_types.h"
#include "disagg.h"

//global disagg structures
extern struct station * disagg_station_6hr;
extern struct Values_1hr * disaggValues;
extern struct Values_6hr * values6hr;
extern struct Dist * dist_6hr_to_1hr;
extern struct station * disagg_station_1hr;
extern struct Values_1hr * valuesReadIn;

extern FILE *disagg_log_fd;
extern int sorted_array_size;
extern int * sorted_list_1hr;
extern int disagg_db_factor;

void DisaggPointMethod()
{
   int i,j,k,l,m,n,p;

   float fdist = 0.;
   float fdata = 0.;
   float fvalue[6];
   float val_1hr = -9999.;
   short prism_1hr = -9999.;
   short prism_6hr = -9999.;
   float dist; //6hr to 1hr
   float padj;
   
   float scale = 1.;
   float stotal = 0.;
   int num_missing_periods = 0;

   int num_1hrs_reported = 0;
   int go_to_next_neighbor = 0;
   int next_6hr_station = 0;
   
   int index = -1;

   int num_days_to_qc = get_num_days_to_qc();

   for(i=0;i<num_disagg_stations;i++)
   {
      for(j=0;j<num_days_to_qc;j++)
      {
	 index = j*num_disagg_stations+i;
	 
	 for(k=0;k<4;k++)//4 6hr periods
	 {
	    //If the quality code is the following don't disagg
	    //if()
	    //{
	    //}
	    
	    for(n=0;n<6;n++)
	    {
	       fvalue[n] = -9999.;

	       //block that checks if the six hr station has a missing report
	       //if it is a missing report, don't bother disagging the station
	       if(values6hr[index].value[k] < 0.)
	       {
		  disaggValues[index].dqc_day = j;
		  for(l=0;l<6;l++)
		  {
	             disaggValues[index].HourlyValues[6*k+l] = -9999.;
		  }
		  next_6hr_station = 1;
		  break;
	       }
	       //block that handles a six hr station with a val = 0
	       //all 1hr periods get a value of '0'
	       else if(values6hr[index].value[k] == 0.)
	       {
		  disaggValues[index].dqc_day = j;
		  for(l=0;l<6;l++)
		  {
	             disaggValues[index].HourlyValues[6*k+l] = 0.;
		  }
		  next_6hr_station = 1;
		  break;
	       }
	       //block that handles a six hr station with a non missing, non zero report
	       else
	       {
	          for(p=0;p<12;p++)
		  {
		     if(disagg_station_6hr[index].isoh[p] != -1)
		     {
		        //at the moment we are using the prism value for the
			//first month dqc run time spans into. in some cases
			//it could span into 2 months
		        prism_6hr = disagg_station_6hr[index].isoh[p];
			break;
		     }
		  }
		  for(l=0;l<mpe_dqc_max_precip_neighbors;l++)
		  {    
	             dist = dist_6hr_to_1hr[i].distances_to_neighbors[l];
		     if(dist == 0)
		     {
		        dist = 0.000001;   
		     }
		     
		     for(m=0;m<sorted_array_size;m++)
		     {
	        	if(disagg_station_6hr[index].index[l] == sorted_list_1hr[m])
			{
		           val_1hr = valuesReadIn[j*sorted_array_size + m].HourlyValues[6*k+n];
		           if(val_1hr < 0.0)
		           {
		              go_to_next_neighbor = 1;
			      break;
		           }
			   for(p=0;p<12;p++)
			   {
			      if(disagg_station_1hr[sorted_list_1hr[m]].isoh[p] != -1)
			      {
			         //at the moment we are using the prism value for the
			         //first month dqc run time spans into. in some cases
			         //it could span into 2 months
			         prism_1hr = disagg_station_1hr[sorted_list_1hr[m]].isoh[p]; 
			         break;
			      }
			   }
			   break;
	        	}
		     }  
		     if(go_to_next_neighbor)
		     {
			go_to_next_neighbor = 0;
			continue;
		     }
		     num_1hrs_reported++;
		     padj = val_1hr*(prism_6hr/prism_1hr);
		     fdist = fdist+(1/pow(dist,2));
		     fdata = fdata+(padj/pow(dist,2));		     
		  }
		  //Quit if number of reported 1hrs is less
		  //than desired. This piece is being commented
		  //at the moment.
		  /*if(num_1hrs_reported <= 15)
		  {
		      num_1hrs_reported = 0;
		      continue;
		  }*/
		  fvalue[n] = fdata/fdist;
	       }
            }
	    
	    stotal = 0.0;
	    num_missing_periods = 0;
	    for(l=0;l<6;l++)
	    {
	       if(fvalue[l] >= 0)
	       {
		  stotal = stotal + fvalue[l];
	       }
	       else
	       {
		  num_missing_periods++;
	       }
	    } 

	    disaggValues[index].dqc_day = j;
	    if(num_missing_periods > 0)
	    {
	       //write to log file
	       //set all 1hr values to missing
	       for(l=0;l<6;l++)
	       {
		  disaggValues[index].HourlyValues[6*k+l] = -9999.;
	       }
	    }
	    else
	    {
	       scale = values6hr[index].value[k]/stotal;
	       for(l=0;l<6;l++)
	       {
		  if(fvalue[l] != -9999.)
		  {
		     disaggValues[index].HourlyValues[6*k+l] = fvalue[l] * scale * disagg_db_factor;
		  }
	       }
	    }
	    if(next_6hr_station)
	    {
	       next_6hr_station = 0;
	       continue;
	    }   
	 }	 
      }  
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
