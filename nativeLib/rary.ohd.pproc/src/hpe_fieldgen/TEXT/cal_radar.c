/*******************************************************************************
* FILENAME:            cal_radar.c
*
* Purpose:
*
* This file has the actual gist of the p3 local bias calculation. 
* this has routines that use some functions from the p3_util library.
*
* calling function: p3_lmosaic
* functions called: functions from file mathstuf.c
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2005   Ram Varma         ABRFC P3 mosaic algorithm 
**
********************************************************************************
*/


/* This file has the actual gist of the p3 local bias calculation. 
 * this has routines that use some functions from the p3_util library.
*/


#include "p3.h"
#include "empe_fieldgen.h"


/* Global Variables */
extern p3_gage_struct gages[];
extern long int numofgages;
extern long int numofgagetri;
extern long int numofradtri;
extern TRIANGLE *tg;
extern TRIANGLE *tr;

extern p3_gage_struct*  radarpts;

/***********************************************************************
* Purpose:
* High level logic of p3_lmosaic calculation
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*
***********************************************************************/



//actual high level logic of the p3_lmosaic calculation. 
//the helper functions used here are all defined in the work.c file.
int
calibrate_radar ()
{
   double ref_lat, ref_lon;
   int i;
   long *rain_sortx;
   long *radar_sortx;
   P3_CALIBRATE_DATA *rain_cal;
   P3_CALIBRATE_DATA *radar_cal;
   TRIPNT *rain_xref;
   TRIPNT *radar_xref;
   long beg_time, end_time;
   double dtemp;
   DPOINTL dpnt;

   rain_sortx = NULL;
   radar_sortx = NULL;
   rain_cal = NULL;
   radar_cal = NULL;
   rain_xref = NULL;
   radar_xref = NULL;
   ref_lon = gages[0].lon;
   ref_lat = gages[0].lat;

   ll_to_mi (radarpts, numofradarpts, ref_lon, ref_lat);
   ll_to_mi (gages, numofgages, ref_lon, ref_lat);

   /* set up the initial arrays that sort the contour data on x  */
   rain_sortx = (long *) malloc (numofgages * sizeof (long));
   if (!rain_sortx)
   {
      sprintf (message, 
      "\n***Fatal error.  Memory alloc error doing rain_sortx***");
      hpe_fieldgen_printMessage( message );
      return (-1);
   }
   radar_sortx = (long *) malloc (numofradarpts * sizeof (long));
   if (!radar_sortx)
   {
      sprintf (message, 
      "\n***Fatal error.  Memory alloc error doing radar_sortx***");
      hpe_fieldgen_printMessage( message );
      return (-1);
   }
   rain_cal = (P3_CALIBRATE_DATA *) malloc (numofgages *
   sizeof(P3_CALIBRATE_DATA));
   if (!rain_cal)
   {
      sprintf (message,
	       "\n***Fatal error.  Memory alloc error doing rain_cal***");
      hpe_fieldgen_printMessage( message );
      return (-1);
   }
   radar_cal = (P3_CALIBRATE_DATA *) malloc (numofradarpts *
   sizeof(P3_CALIBRATE_DATA));
   if (!radar_cal)
   {
      sprintf (message,
	       "\n***Fatal error.  Memory alloc error doing radar_cal***");
      hpe_fieldgen_printMessage( message );
      return (-1);
   }
   contour_data_sort_x (gages, numofgages, rain_sortx);
   contour_data_sort_x (radarpts, numofradarpts, radar_sortx);
   time (&beg_time);
   rain_xref = cross_reference_triangles (gages, numofgages,
   tg, numofgagetri);
   if (!rain_xref)
   {
      sprintf (message, "\nmemory allocation error during rain xref...");
      hpe_fieldgen_printMessage( message );
      return (-1);
   }
   radar_xref = cross_reference_triangles (radarpts, numofradarpts,
   tr, numofradtri);
   if (!radar_xref)
   {
      sprintf (message, "\nmemory allocation error during radar xref...");
      hpe_fieldgen_printMessage( message );
      return (-1);
   }
   time (&end_time);
   time (&beg_time);
   for (i = 0; i < numofgages; i++)
   {
      if (gages[i].value < 0.0)
      {
	 rain_cal[i].other_dataset_zval = MISSING_VALUE;
	 rain_cal[i].other_dataset_tri_idx = -1;
	 continue;
      }
      dpnt.lon = gages[i].lon;
      dpnt.lat = gages[i].lat;

      rain_cal[i].other_dataset_tri_idx = determine_dataset_triangle (&dpnt,
								      radarpts,
								      radar_xref,
								      radar_sortx,
								      numofradarpts,
								      tr,
								      numofradtri);
      if (rain_cal[i].other_dataset_tri_idx >= 0)
      {
	 calc_z_value_with_known_triangle (radarpts, tr,
					   rain_cal[i].other_dataset_tri_idx,
					   &dpnt,
					   &rain_cal[i].other_dataset_zval);
      }
      else
      {
	 rain_cal[i].other_dataset_zval = MISSING_VALUE;
	 rain_cal[i].other_dataset_tri_idx = -1;
      }
   }
   time (&end_time);
   time (&beg_time);
   for (i = 0; i < numofradarpts; i++)
   {
      if (radarpts[i].value < 0.0)
      {
	 radar_cal[i].other_dataset_zval = MISSING_VALUE;
	 radar_cal[i].other_dataset_tri_idx = -1;
      }
      dpnt.lon = radarpts[i].lon;
      dpnt.lat = radarpts[i].lat;
//printf("lat = %lf lon = %lf\n",dpnt.lat,dpnt.lon);    
      radar_cal[i].other_dataset_tri_idx = determine_dataset_triangle (&dpnt,
								       gages,
								       rain_xref,
								       rain_sortx,
								       numofgages,
								       tg,
								       numofgagetri);
      if (radar_cal[i].other_dataset_tri_idx >= 0)
      {
	 calc_z_value_with_known_triangle (gages, tg,
					   radar_cal[i].other_dataset_tri_idx,
					   &dpnt,
					   &radar_cal[i].other_dataset_zval);
      }
      else
      {
	 radar_cal[i].other_dataset_zval = MISSING_VALUE;
	 radar_cal[i].other_dataset_tri_idx = -1;
      }
   }
   time (&end_time);
   for (i = 0; i < numofradarpts; i++)
   {
      if (radar_cal[i].other_dataset_tri_idx < 0)
      {
	 continue;
      }
      dtemp = 0.0;
      compute_adjusted_zval (gages, rain_cal, tg,
			     radar_cal[i].other_dataset_tri_idx,
			     &radarpts[i].lon, &radarpts[i].lat,
			     &radarpts[i].value, &dtemp);
      radarpts[i].value = dtemp;
   }
   for (i = 0; i < numofradarpts; i++)
   {
      if (radarpts[i].value < 0.005)
      {
	 radarpts[i].value = 0.00;
      }
   }
   //printf("done adjusting z values...\n");     
   mi_to_ll (radarpts, numofradarpts, ref_lon, ref_lat);
   mi_to_ll (gages, numofgages, ref_lon, ref_lat);

   //  free up memory                            
   if (rain_sortx != NULL)
   {
      free (rain_sortx);
      rain_sortx = NULL;
   }
   if (radar_sortx != NULL)
   {
      free (radar_sortx);
      radar_sortx = NULL;
   }
   if (radar_cal != NULL)
   {
      free (radar_cal);
      radar_cal = NULL;
   }
   if (rain_cal != NULL)
   {
      free (rain_cal);
      rain_cal = NULL;
   }

   for (i = 0; i < numofgages; ++i)
   {
      if (rain_xref[i].tri_pnt != NULL)
      {
	 free (rain_xref[i].tri_pnt);
	 rain_xref[i].tri_pnt = NULL;
      }
   }

   if (rain_xref != NULL)
   {
      free (rain_xref);
      rain_xref = NULL;
   }

   for (i = 0; i < numofradarpts; ++i)
   {
      if (radar_xref[i].tri_pnt != NULL)
      {
	 free (radar_xref[i].tri_pnt);
	 radar_xref[i].tri_pnt = NULL;
      }
   }

   if (radar_xref != NULL)
   {
      free (radar_xref);
      radar_xref = NULL;
   }

   return (0);
}

/**************************************************************************************/
