/*******************************************************************************
* FILENAME:            mathstuf.c
*
* Purpose:
*
* This file has routines that are helper functions to the ones in the cal_radar.c 
* file. thes files do the end math work in the calculation of the p3 local bias. 
* the actual function calls to these routines is done in the cal_radar.c file.
*
* calling function: calibrate_radar
* functions called: functions from file mathstuf.c
*
* input variables
* 
* output variables
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2005   Ram Varma         ABRFC P3 mosaic algorithm 
**
********************************************************************************
*/


/* This file has routines that are helper functions to the ones in the cal_radar.c 
 * file. thes files do the end math work in the calculation of the p3 local bias. 
 * the actual function calls to these routines is done in the cal_radar.c file.
*/


#include "p3.h"
#define PA c[t[triidx].a]
#define PB c[t[triidx].b]
#define PC c[t[triidx].c]

#define POINT_A_DEFINED (0x01)
#define POINT_B_DEFINED (0x02)
#define POINT_C_DEFINED (0x04)

double MPEFieldGen_Max_ratio = 3.00000;

/*/////////////////////////////////////////////////////////////////////////////*/
//gets the intersection point of two lines.

/***********************************************************************
* Purpose:
* gets the intersection point of two lines
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


short
MPEFieldGen_get_intersection_point (double *a1x, double *a1y, double *a2x, double *a2y,
			double *b1x, double *b1y, double *b2x, double *b2y,
			double *x, double *y)
{
   /*this routine returns the intersection of two lines.  They better intersect!  */
   double slope_a, slope_b, intercept_a, intercept_b;
   /* if either line is zero length, assume it to be the intersection point  */
   if (*a1x == *a2x && *a1y == *a2y)
   {
      *x = *a1x;
      *y = *a1y;
      return (-1);
   }
   if (*b1x == *b2x && *b1y == *b2y)
   {
      *x = *b1x;
      *y = *b1y;
      return (-2);
   }
   if (*a1x == *a2x)
   {
      /* first line is vertical  */
      slope_b = (*b2y - *b1y) / (*b2x - *b1x);
      intercept_b = *b2y - slope_b * *b2x;
      *x = *a1x;
      *y = slope_b * (*x) + intercept_b;
      return (1);
   }
   if (*b1x == *b2x)
   {
      /* second line is vertical  */
      slope_a = (*a2y - *a1y) / (*a2x - *a1x);
      intercept_a = *a2y - slope_a * *a2x;
      *x = *b1x;
      *y = slope_a * (*x) + intercept_a;
      return (2);
   }
   /* neither line is vertical  */
   slope_a = (*a2y - *a1y) / (*a2x - *a1x);
   intercept_a = *a2y - slope_a * *a2x;
   slope_b = (*b2y - *b1y) / (*b2x - *b1x);
   intercept_b = *b2y - slope_b * *b2x;
   if (slope_a == slope_b)
   {
      return (0);		/*  lines are parallel  */
   }
   *x = (intercept_b - intercept_a) / (slope_a - slope_b);
   *y = slope_a * (*x) + intercept_a;
   return (3);
} /* end MPEFieldGen_get_intersection_point */


//final computation where all the grid bins inside the gage triangles are 
// adjusted according to the gage to radar ratio.

/***********************************************************************
* Purpose: final computation where all the grid bins inside the gage 
* triangles are adjusted according to the gage to radar ratio. 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


short
MPEFieldGen_compute_adjusted_zval (p3_gage_struct * c, P3_CALIBRATE_DATA * cal,
		       TRIANGLE * t,
		       long triidx,
		       double *x, double *y, double *z, double *adj_z)
{
   /*
      Given a contour, calibration and triangle array and a pointer to 
      which triangle a particular point is in, this routine computes an 
      adjusted z value (adj_z) for the point (x,y,z)
    */

   double weight_a = 0.0, weight_b = 0.0, weight_c = 0.0;
   double ratio_a = 0.0, ratio_b = 0.0, ratio_c = 0.0;
   double diff_a = 0.0, diff_b = 0.0, diff_c = 0.0;
   double x_b_to_c = 0.0, y_b_to_c = 0.0;
   unsigned short defined_points = 0;
   short point_def_count = 0;

   /* we will compute the adjusted z value differently depending on how many
      triangle corner points have a calibrate factor.  e.g. if a point in 'c'
      is not contained in the convex hull of the other dataset, then a 
      calibrate factor cannot be computed  */

   if (cal[t[triidx].a].other_dataset_zval >= 0.0)
   {
      defined_points |= POINT_A_DEFINED;
      point_def_count++;
   }
   if (cal[t[triidx].b].other_dataset_zval >= 0.0)
   {
      defined_points |= POINT_B_DEFINED;
      point_def_count++;
   }
   if (cal[t[triidx].c].other_dataset_zval >= 0.0)
   {
      defined_points |= POINT_C_DEFINED;
      point_def_count++;
   }
   if (point_def_count == 3)
   {
      /*  use a geometric interpolation to determine weighting of 
      the calibrate factors */
      if (PA.lon == *x && PA.lat == *y)
      {
	 /*  gage is at point a, dont attempt an interpolation */
	 *adj_z = PA.value;
	 return (0);
      }
      MPEFieldGen_get_intersection_point (&PB.lon, &PB.lat, &PC.lon, &PC.lat,
			      &PA.lon, &PA.lat, &*x, &*y,
			      &x_b_to_c, &y_b_to_c);
      /* determine the weight of the 3 corner points of the triangle
      This test is necessary because of the problems horiz and vert 
      lines cause */
      if (fabs (PB.lon - PC.lon) > fabs (PB.lat - PC.lat))
      {
	 weight_b = (x_b_to_c - PC.lon) / (PB.lon - PC.lon);
      }
      else
      {
	 weight_b = (y_b_to_c - PC.lat) / (PB.lat - PC.lat);
      }
      weight_c = 1 - weight_b;
      /*/ now do the interpolated point along the B-C line to A  */
      if (fabs (x_b_to_c - PA.lon) > fabs (y_b_to_c - PA.lat))
      {
	 weight_a = (*x - x_b_to_c) / (PA.lon - x_b_to_c);
      }
      else
      {
	 weight_a = (*y - y_b_to_c) / (PA.lat - y_b_to_c);
      }
      weight_b *= (1 - weight_a);
      weight_c *= (1 - weight_a);
      MPEFieldGen_compute_ratio_and_diff_at_point (PA.value,
				       cal[t[triidx].a].other_dataset_zval,
				       MPEFieldGen_Max_ratio, &ratio_a, &diff_a);
      MPEFieldGen_compute_ratio_and_diff_at_point (PB.value,
				       cal[t[triidx].b].other_dataset_zval,
				       MPEFieldGen_Max_ratio, &ratio_b, &diff_b);
      MPEFieldGen_compute_ratio_and_diff_at_point (PC.value,
				       cal[t[triidx].c].other_dataset_zval,
				       MPEFieldGen_Max_ratio, &ratio_c, &diff_c);
      *adj_z =
	 (*z *
	  (ratio_a * weight_a + ratio_b * weight_b + ratio_c * weight_c)) +
	 diff_a * weight_a + diff_b * weight_b + diff_c * weight_c;

      return (3);		/*/ 3 points used for weighting */
   }
   if (point_def_count == 2)
   {
      /* use a weighting based on the two defined points to determine the 
      calibrate factors.  I though about combining methods 1,2 and 3 into 
      a single block but thought this read better. */
      if (defined_points & POINT_A_DEFINED
	  && defined_points & POINT_B_DEFINED)
      {
	 /*/ compute the distance between point a and (x,y) */
	 MPEFieldGen_get_intersection_point (&PA.lon, &PA.lat, &PB.lon, &PB.lat,
				 &PC.lon, &PC.lat, &*x, &*y,
				 &x_b_to_c, &y_b_to_c);
	 if (fabs (PA.lon - PB.lon) > fabs (PA.lat - PB.lat))
	 {
	    weight_a = (x_b_to_c - PB.lon) / (PA.lon - PB.lon);
	 }
      }
      else
      {
	 weight_a = (y_b_to_c - PB.lat) / (PA.lat - PB.lat);
      }
      weight_b = 1 - weight_a;
      MPEFieldGen_compute_ratio_and_diff_at_point (PA.value,
				       cal[t[triidx].a].other_dataset_zval,
				       MPEFieldGen_Max_ratio, &ratio_a, &diff_a);
      MPEFieldGen_compute_ratio_and_diff_at_point (PB.value,
				       cal[t[triidx].b].other_dataset_zval,
				       MPEFieldGen_Max_ratio, &ratio_b, &diff_b);
      *adj_z =
	 (*z * (ratio_a * weight_a + ratio_b * weight_b)) +
	 diff_a * weight_a + diff_b * weight_b;
      /*/printf("\nloc2a %15.5lf %15.5lf", weight_a, weight_b); */
      if (defined_points & POINT_B_DEFINED
	  && defined_points & POINT_C_DEFINED)
      {
	 /*/ compute the distance between point a and (x,y) */
	 MPEFieldGen_get_intersection_point (&PB.lon, &PB.lat, &PC.lon, &PC.lat,
				 &PA.lon, &PA.lat, &*x, &*y,
				 &x_b_to_c, &y_b_to_c);
	 if (fabs (PB.lon - PC.lon) > fabs (PB.lat - PC.lat))
	 {
	    weight_b = (x_b_to_c - PC.lon) / (PB.lon - PC.lon);
	 }
	 else
	 {
	    weight_b = (y_b_to_c - PC.lat) / (PB.lat - PC.lat);
	 }
	 weight_c = 1 - weight_b;
	 MPEFieldGen_compute_ratio_and_diff_at_point (PB.value,
					  cal[t[triidx].b].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_b, &diff_b);
	 MPEFieldGen_compute_ratio_and_diff_at_point (PC.value,
					  cal[t[triidx].c].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_c, &diff_c);
	 *adj_z =
	    (*z * (ratio_b * weight_b + ratio_c * weight_c)) +
	    diff_b * weight_b + diff_c * weight_c;
	 /*/printf("\nloc2c %15.5lf %15.5lf", weight_b, weight_c); */
      }
      if (defined_points & POINT_C_DEFINED
	  && defined_points & POINT_A_DEFINED)
      {
	 /*/ compute the distance between point a and (x,y) */
	 MPEFieldGen_get_intersection_point (&PC.lon, &PC.lat, &PA.lon, &PA.lat,
				 &PB.lon, &PB.lat, &*x, &*y,
				 &x_b_to_c, &y_b_to_c);
	 if (fabs (PC.lon - PA.lon) > fabs (PC.lat - PA.lat))
	 {
	    weight_c = (x_b_to_c - PA.lon) / (PC.lon - PA.lon);
	 }
	 else
	 {
	    weight_c = (y_b_to_c - PA.lat) / (PC.lat - PA.lat);
	 }
	 weight_a = 1 - weight_c;
	 MPEFieldGen_compute_ratio_and_diff_at_point (PA.value,
					  cal[t[triidx].a].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_a, &diff_a);
	 MPEFieldGen_compute_ratio_and_diff_at_point (PC.value,
					  cal[t[triidx].c].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_c, &diff_c);
	 *adj_z =
	    (*z * (ratio_a * weight_a + ratio_c * weight_c)) +
	    diff_a * weight_a + diff_c * weight_c;
	 /* printf("\nloc2b %15.5lf %15.5lf", weight_a, weight_c);  */
      }
      return (2);		/*  2 points used for weighting */
   }
   if (point_def_count == 1)
   {
      /* base weighting on the single defined point to determine the 
      calibrate factors */
      if (defined_points & POINT_A_DEFINED)
      {
	 MPEFieldGen_compute_ratio_and_diff_at_point (PA.value,
					  cal[t[triidx].a].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_a, &diff_a);
	 *adj_z = *z * ratio_a + diff_a;
      }
      if (defined_points & POINT_B_DEFINED)
      {
	 MPEFieldGen_compute_ratio_and_diff_at_point (PB.value,
					  cal[t[triidx].b].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_b, &diff_b);
	 *adj_z = *z * ratio_b + diff_b;
      }
      if (defined_points & POINT_C_DEFINED)
      {
	 MPEFieldGen_compute_ratio_and_diff_at_point (PC.value,
					  cal[t[triidx].c].other_dataset_zval,
					  MPEFieldGen_Max_ratio, &ratio_c, &diff_c);
	 *adj_z = *z * ratio_c + diff_c;
      }
      /*/printf("\nloc3"); */
      return (1);		/* 1 point used for weighting */
   }
   if (point_def_count == 0)
   {
      /* no weighting can be performed, this point lays outside the 
      convex hull of the data */
      *adj_z = *z;
      return (0); // *z returned, no computations could be performed
   }
   return (0);
} /* end MPEFieldGen_compute_adjusted_zval */

/*///////////////////////////////////////////////////////////////////////////// */


/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


short
MPEFieldGen_compute_ratio_and_diff_at_point (double rain_z, double radar_z,
				 double max_ratio, double *ratio,
				 double *diff)
{
   *ratio = 10000000;
   if (radar_z > .00001)
   {
      *ratio = rain_z / radar_z;
   }
   if (*ratio > max_ratio)
   {
      *ratio = 1.0;
      *diff = rain_z - radar_z;
   }
   else
   {
      *diff = 0.0;
   }
   return (0);
} /* end MPEFieldGen_compute_ratio_and_diff_at_point */

/*************************************************************************************/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


short
MPEFieldGen_contour_data_sort_x (p3_gage_struct * c, long numpnt, long *sortx)
{
   /* This routine shell sorts a contour array in a table by x  */
   long temp;
   long off_ptr;
   long counter;
   long base_ptr;
   double x;

   /* move the contour elements into the sortx struct */
   for (counter = 0; counter < numpnt; counter++)
   {
      sortx[counter] = counter;
   }
   for (off_ptr = 1; off_ptr < numpnt; off_ptr = off_ptr * 3 + 1);
   while (off_ptr /= 3)
   {
      for (counter = off_ptr; counter < numpnt; counter++)
      {
	 x = c[sortx[counter]].lon;
	 temp = sortx[counter];
	 base_ptr = counter;
	 while (c[sortx[base_ptr - off_ptr]].lon > x)
	 {
	    sortx[base_ptr] = sortx[base_ptr - off_ptr];
	    base_ptr -= off_ptr;
	    if (base_ptr < off_ptr)
	    {
	       break;
	    }
	 }
	 sortx[base_ptr] = temp;
      }
   }

   return (0);
} /* end MPEFieldGen_contour_data_sort_x */

/*/////////////////////////////////////////////////////////////////////////*/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/

short
MPEFieldGen_closest_pnt_from_array (p3_gage_struct * c, long numpnt, long *sortx,
			double *x, double *y, long *closest_point,
			short options)
{
   /* this routine returns the closest point in the dataset that has
   c[].n_contig >= 0 which signifies it is a non deleted member of the dataset
   the return value is -1 if the closest point has the exact same x and
   y coordinate as (x, y).  Otherwise it is 0.  */

   long cur = 0, lo, hi;
   long i;
   double dist_test, x_dist_test, closest_dist;
   lo = 0;
   hi = numpnt - 1;
   if (*x <= c[sortx[lo]].lon)
   {
      cur = lo;
   }
   else
   {
      if (*x >= c[sortx[hi]].lon)
      {
	 cur = hi;
      }
      else
      {
	 /* do a binary search for a starting point */
	 while (lo < hi - 1)
	 {
	    cur = (lo + hi) / 2;
	    if (*x < c[sortx[cur]].lon)
	    {
	       hi = cur;
	    }
	    else
	       lo = cur;
	 }
      }
   }
   /* make sure we don't use a deleted point as the closest point  */
   if (!(options & INCLUDE_DELETED_POINTS))
   {
      for (i = cur; i < numpnt && c[sortx[i]].n_contig < 0; i++);
      cur = i;
      for (i = cur; i >= 0 && c[sortx[i]].n_contig < 0; i--);
      cur = i;
   }
   /* now that we know the closest point in the x direction, find the closest
   point overall  */
   *closest_point = sortx[cur];
   closest_dist = (c[sortx[cur]].lon - *x) * (c[sortx[cur]].lon - *x) +
      (c[sortx[cur]].lat - *y) * (c[sortx[cur]].lat - *y);
   if (options & CLOSEST_X_ONLY)
   {
      if (closest_dist == 0.0)
      {
	 return (-1);
      }
      else
      {
	 return (0);
      }
   }

   /* search up  */
   for (i = cur + 1; i < numpnt; i++)
   {
      x_dist_test = (c[sortx[i]].lon - *x) * (c[sortx[i]].lon - *x);
      if (x_dist_test > closest_dist)
      {
	 break; //x dist to point > dist to closest point so far
      }
      /* if the point has been deleted, don't use if  */
      if (!(options & INCLUDE_DELETED_POINTS) && c[sortx[i]].n_contig < 0)
      {
	 continue;
      }
      dist_test =
	 x_dist_test + (c[sortx[i]].lat - *y) * (c[sortx[i]].lat - *y);
      if (dist_test < closest_dist)
      {
	 closest_dist = dist_test;
	 *closest_point = sortx[i];
      }
   }

   /* search down  */
   for (i = cur - 1; i > -1; i--)
   {
      x_dist_test = (c[sortx[i]].lon - *x) * (c[sortx[i]].lon - *x);
      if (x_dist_test > closest_dist)
      {
	 break;	//x dist to point > dist to closest point so far
      }
      /* if the point has been deleted, don't use if */
      if (!(options & INCLUDE_DELETED_POINTS) && c[sortx[i]].n_contig < 0)
      {
	 continue;
      }
      dist_test =
	 x_dist_test + (c[sortx[i]].lat - *y) * (c[sortx[i]].lat - *y);
      if (dist_test < closest_dist)
      {
	 closest_dist = dist_test;
	 *closest_point = sortx[i];
      }
   }
   if (closest_dist == 0.0)
   {
      return (-1);		/* exact matches are flagged  */
   }
   return (0);
} /* end MPEFieldGen_closest_pnt_from_array */

/*/////////////////////////////////////////////////////////////////////////////
*/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


short
MPEFieldGen_point_below_line (double *x, double *y,
		  double *x1, double *y1, double *x2, double *y2)
{
   double slope, intercept;
   /* make sure the x coordinate of the point is within the line segment range
      // It is very important that point 1 of the line segment is > or < and
      // point 2 of the line segment is >= or <=.  This keeps any line segment
      // from being counted twice  */
   if ((*x1 < *x && *x <= *x2) | /* x1 is less than x2 and x is between them */
       (*x2 < *x && *x <= *x1))	/* x2 is less than x1 and x is between them */
   {
      if (*y1 >= *y && *y2 >= *y)
      {
	 return (1); //return true		/* point below (or on) line */
      }
      if (*y1 < *y && *y2 < *y)
      {
	 return (0); //return false	/* point above line  */
      }
      /* damn!, slope computation time!  */
      slope = (*y2 - *y1) / (*x2 - *x1);
      intercept = *y1 - slope * *x1;
      if (slope * *x + intercept >= *y)
      {
	 return (1); // return true
      }
   }
   return (0); // return false
} /* end MPEFieldGen_point_below_line */ 

/*//////////////////////////////////////////////////////////////////////////////*/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


short
MPEFieldGen_pnt_in_tri (p3_gage_struct * con, TRIANGLE * tri, long triidx, DPOINTL * dptl)
{
	/***********************/
   short count = 0;
   short temp1, temp2, temp3;
   /* this routine determines if point dptl is internal to tri[idx].
      // test if x is between link a to b  */
   if (MPEFieldGen_point_below_line (&dptl->lon,
			 &dptl->lat,
			 &con[tri[triidx].a].lon,
			 &con[tri[triidx].a].lat,
			 &con[tri[triidx].b].lon, &con[tri[triidx].b].lat))
      count = (count + 1) % 2;
   /* test if x is between link b to c */
   if (MPEFieldGen_point_below_line (&dptl->lon,
			 &dptl->lat,
			 &con[tri[triidx].b].lon,
			 &con[tri[triidx].b].lat,
			 &con[tri[triidx].c].lon, &con[tri[triidx].c].lat))
      count = (count + 1) % 2;
   /* test if x is between link c to a  */
   if (MPEFieldGen_point_below_line (&dptl->lon,
			 &dptl->lat,
			 &con[tri[triidx].c].lon,
			 &con[tri[triidx].c].lat,
			 &con[tri[triidx].a].lon, &con[tri[triidx].a].lat))
      count = (count + 1) % 2;
   /* if count is no zero, the point is in (or on) the triangle  */
   return (count);
   /* the theory used here is that if the point is clockwise (-1) of all three lines
      // or zero (on the line) then the point is in the triangle.  The same holds
      // true if all three test counterclockwise (1).  We will consider a point
      // on the line (0) to be in the triangle.  A value of -2 or 2 returned
      // indicates that the point is collinear with the line but not on it and
      // therefore out of the triangle  */
   temp1 = ccwise (&con[tri[triidx].a].lon,
		   &con[tri[triidx].a].lat,
		   &con[tri[triidx].b].lon,
		   &con[tri[triidx].b].lat, &dptl->lon, &dptl->lat);
   if (temp1 == -2 || temp1 == 2)
   {
      return (0);
   }
   temp2 = ccwise (&con[tri[triidx].b].lon,
		   &con[tri[triidx].b].lat,
		   &con[tri[triidx].c].lon,
		   &con[tri[triidx].c].lat, &dptl->lon, &dptl->lat);
   if (temp2 == -2 || temp2 == 2)
   {
      return (0);
   }
   /* if first lines test out bad, dont bother with the 3rd  */
   if ((temp1 < 0 && temp2 > 0) || (temp1 > 0 && temp2 < 0))
   {
      return (0);
   }
   temp3 = ccwise (&con[tri[triidx].c].lon,
		   &con[tri[triidx].c].lat,
		   &con[tri[triidx].a].lon,
		   &con[tri[triidx].a].lat, &dptl->lon, &dptl->lat);
   if (temp3 == -2 || temp3 == 2)
   {
      return (0);
   }
   /* we already know that temp1 and temp2 are compatible, so just use 1 of them
      // in testing temp3   */
   if ((temp1 < 0 && temp3 > 0) || (temp1 > 0 && temp3 < 0))
   {
      return (0);
   }
   /* if it gets to here then point is in or on the triangle  */
   return (1);
} /* end MPEFieldGen_pnt_in_tri */


/**************************************************************************************/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


TRIPNT *
MPEFieldGen_cross_reference_triangles (p3_gage_struct * c, long numpnt, TRIANGLE * t,
			   long numtri)
{
   long i;
   TRIPNT *tripnt = NULL;
   tripnt = (TRIPNT *) malloc (numpnt * sizeof (TRIPNT));
   if (!tripnt)
   {
      return (NULL);
   }
   for (i = 0; i < numpnt; i++)
   {
      tripnt[i].n_triangles = 0;
      tripnt[i].tri_pnt = NULL;
   }
   for (i = 0; i < numtri; i++)
   {
      add_tripnt (tripnt, t[i].a, t[i].b, t[i].c, i);
   }
   return (tripnt);
} /* end MPEFieldGen_cross_reference_triangles */

/***************************************************************************************/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


void
MPEFieldGen_calc_z_value_with_known_triangle (p3_gage_struct * c,
				  TRIANGLE * t,
				  long triidx, DPOINTL * pnt, double *z)
{

   /* this procedure computes a z value for a point given the triangle
      // that the datapoint occurs in.  Other routines such as calc_z_value
      // will determine the triangle that the point is in (given the seed point)  */
   double interp_rain;
   double d1, d2;
   double x, y;
   *z = -1.0;
   /* calculate the rainfall at the given point  */
   if (PA.lon == pnt->lon && PA.lat == pnt->lat)
   {
      /* gage is at point a, dont attemp an interpolation */
      *z = PA.value;
      return;
   }
   MPEFieldGen_get_intersection_point (&PB.lon, &PB.lat, &PC.lon, &PC.lat,
			   &PA.lon, &PA.lat, &pnt->lon, &pnt->lat, &x, &y);
   /* interpolate a rainfall at point (x,y)  */
   d1 =
      sqrt ((PC.lon - PB.lon) * (PC.lon - PB.lon) +
	    (PC.lat - PB.lat) * (PC.lat - PB.lat));
   d2 = sqrt ((x - PB.lon) * (x - PB.lon) + (y - PB.lat) * (y - PB.lat));
   interp_rain = PB.value * (1.0 - (d2 / d1)) + PC.value * d2 / d1;
   d1 = sqrt ((x - PA.lon) * (x - PA.lon) + (y - PA.lat) * (y - PA.lat));
   d2 = sqrt ((pnt->lon - PA.lon) * (pnt->lon - PA.lon) +
	      (pnt->lat - PA.lat) * (pnt->lat - PA.lat));
   *z = PA.value * (1.0 - (d2 / d1)) + interp_rain * d2 / d1;

   return;
} /* end MPEFieldGen_calc_z_value_with_known_triangle */

/************************************************************************************/

/***********************************************************************
* Purpose: 
* 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma          
*
***********************************************************************/


long
MPEFieldGen_determine_dataset_triangle (DPOINTL * pnt,
			    p3_gage_struct * c, TRIPNT * tripnt,
			    long *sortx, long numpnt,
			    TRIANGLE * tri, long numtri)
{
   /*
      given a datapoint (x, y).  this procedure determines which triangle
      fron the tri array that the data point is in.  It returns -1 if the
      datapoint is not in the dataset (external to the convex hull of the data)

      IMPORTANT NOTE   IMPORTANT NOTE   IMPORTANT NOTE   IMPORTANT NOTE
      the contour array "c" MUST be dimensioned to hold numpnt+1 !!!!!!!!
      IMPORTANT NOTE   IMPORTANT NOTE   IMPORTANT NOTE   IMPORTANT NOTE
    */

   /* by definition, a given point (x,y) must be in a triangle that uses 
   what would be the seed point for (x,y) if (x,y) was in the dataset.  
   In other words, if we find the seed point for (x,y), then this point 
   has to be one of the vertexes for the triangle containing the point.  
   If it is not then the point is external to the dataset and a -1 is returned.  
   If an error is encountered, a number < -1 is returned  */

   long closest_point;
   long *recursion_array = NULL;
   long i;
   static long last_triangle = 0;

   /* before we search the entire dataset for a triangle, lets check the last 
   one we found to see if the point we are interested in is in the same 
   triangle--common for radar grid cell points in rain triangles
    */
   if (last_triangle > numtri - 1)
   {
      last_triangle = 0;
   }
   if (MPEFieldGen_pnt_in_tri (c, tri, last_triangle, pnt))
   {
      /*if one of the points making up the corner points of the triangle is 
      missing, then the point cannot be computed and is actually outside of the 
      dataset */
      if (c[tri[last_triangle].a].value == MISSING_VALUE ||
	  c[tri[last_triangle].b].value == MISSING_VALUE ||
	  c[tri[last_triangle].c].value == MISSING_VALUE)
      {
	 //printf("return 1\n");
	 return (-1);
      }
      //printf("return 2\n");
      return (last_triangle);
   }
   /*  first find the closest point */
   MPEFieldGen_closest_pnt_from_array (c, numpnt, sortx, &pnt->lon, &pnt->lat,
			   &closest_point, NORMAL_SEARCH);
   /* determine the seed point using closest_point as the beginning point.
      // load minimum amount of data into the contour array */
   c[numpnt].lon = pnt->lon;
   c[numpnt].lat = pnt->lat;
   /* get a starting seed point
 */
   closest_point =
      get_seed_point (c, numpnt, closest_point, &recursion_array);
   /* free the work array */
   if (recursion_array)
   {
      free (recursion_array);
      recursion_array = NULL;
   }
   if (closest_point < 0)
   {
      //printf("return 3\n");
      return (-2);
   }
   /*  now that we have the closest point, determine which triangle it is in */
   /*  loop for every triangle that contains closest_point as a vertex */
   for (i = 0; i < tripnt[closest_point].n_triangles; i++)
   {
      if (MPEFieldGen_pnt_in_tri (c, tri, tripnt[closest_point].tri_pnt[i], pnt))
      {
	 /*if one of the points making up the corner points of the triangle 
	 is missing,then the point cannot be computed and is actually outside 
	 of the dataset  */
	 if (c[tri[tripnt[closest_point].tri_pnt[i]].a].value == MISSING_VALUE
	     || c[tri[tripnt[closest_point].tri_pnt[i]].b].value ==
	     MISSING_VALUE
	     || c[tri[tripnt[closest_point].tri_pnt[i]].c].value ==
	     MISSING_VALUE)
	 {
	    //printf("return 4\n");
	    return (-1);
	 }
	 last_triangle = tripnt[closest_point].tri_pnt[i]; //save the last good
	 //printf("found a good one....returning %ld  i = %ld\n", 
	 //tripnt[closest_point].tri_pnt[i], i);
	 //printf("return 5\n");
	 return (tripnt[closest_point].tri_pnt[i]);
      }
   } /* for i */

   //printf("return 6\n");
   return (-1);			/* point outside of convex hull */

} /* end MPEFieldGen_determine_dataset_triangle */
