/*
	File:		floodrept_interp.c
	Date:		May 1997
	Author:		Paul Taylor
	
	Purpose:	Provide support for the Flood Report DS.
*/


#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <Xm/Xm.h>
#include "Xtools.h"
#include "FloodTs.h"
#include "floodrept_interp.h"
#include "time_series.h"
#include "time_convert.h"

FloodTs	floodrept_interp(FloodTs pt1, FloodTs pt2, double fs)
{
   FloodTs	newPt;
   time_t	rounding_factor = 60*1;  /* for rounding to nearest minute */

   time_t	pt1_time_t,
		pt2_time_t,
		newPt_time_t;
   
   double	time_diff;
   double	value_diff;
   double	slope;
   
   int		found_pt = False;
   
   
   /*
   	Init newPt to have a value of flood stage.
   */
   newPt = pt1;
   newPt.value = fs;
   
   
   /*
   	If one of the points is exactly at flood stage,
   	set the returned structure to it.
   */
   if (pt1.value == fs)
   {   
      newPt = pt1;
      found_pt = True;
   }
   else if (pt2.value == fs)
   {
      newPt = pt2;
      found_pt = True;
   }   
   
   
   if ( ! found_pt )
   { 
      /*
      		Convert to time_t variables.
      */
      yearsec_dt_to_timet(pt1.obstime, &pt1_time_t);
      yearsec_dt_to_timet(pt2.obstime, &pt2_time_t);
      
      
      /*
      		Set the point-slope variables.
      */
      time_diff = pt2_time_t - pt1_time_t;
      
      value_diff = pt2.value - pt1.value;
      
      slope = value_diff/time_diff;
      
      
      /*
      		Calc time of flood stage crossing.
      */
      newPt_time_t = pt1_time_t + ((newPt.value - pt1.value)/slope) ;
      
   }

   else  /* still need to round it, even if exactly */
   {
      yearsec_dt_to_timet(newPt.obstime, &newPt_time_t);
   }	

   
   
   /*
   	Round time according to the rounding_factor.
   */
   newPt_time_t /= rounding_factor;
   newPt_time_t *= rounding_factor;
   
   timet_to_yearsec_dt(newPt_time_t, &newPt.obstime);


   
   return(newPt);
}





