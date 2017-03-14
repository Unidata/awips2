#include <stdio.h>
#include <math.h>

/* File: scaleMaxMin.c
 *
 * Given the highest and lowest values to be plotted, and
 *  the minimum range to be allowed, this function returns
 *  min and max scale values and the increment assuming
 *  ten spaces (11 tick marks).
 *
 * Written by George Smith, HRL, November 1991.
 */

int scale_max_min(min_in_orig, max_in_orig, minimum_range,
		  scale_min, scale_max, increment)

 float  min_in_orig, max_in_orig, minimum_range,
	*scale_min, *scale_max, *increment;
{

 float  min_in;  /* minimum scale value input */
 float  max_in;  /* maximum scale value input */
 float  test;    /* range test value */
 float  power;   /* power of base 10 used for the range check */
 float  range;   /* data range value */
 float  range_check;
 float  range_limit;
 double integral_part; /* multiplication factor used for min scale */
/*
 * Check range from min to max, make the range a reasonable
 *  number, and adjust the max or min to give even increments.
 */
 if(max_in_orig < min_in_orig)
   {
    printf("In scale_max_min, max value to be plotted ");
    printf("is less than minimum value allowed on axis.\n");
   }
 min_in = (float)floor((double) min_in_orig);
 max_in = (float)ceil((double) max_in_orig);

 range = fabs(max_in - min_in);

 if(range <= 2.0 && minimum_range <= 2.0)
   {
    *scale_max = max_in;
    *scale_min = min_in;
    if (*scale_max == *scale_min) *scale_max = (*scale_min) + 1;
    *increment = (*scale_max - *scale_min) / 10.0;
   }

 else /* range > 2.0 or minimum_range > 2.0 */
   {
    if(range > minimum_range)
      {
       test = range;
       power = 0.0;
       while (test > 1.0)
	 {
	  test = test/10.0;
	  power++;
	 }
       range_check = pow(10, power);
       if(range <= 0.5 * range_check)
	 {
	  range_check = range_check * 0.5;
	  if(range <= 0.4 * range_check)
	     range_check = 0.4 * range_check;
	 }
      }
    else  /* range <= minimum_range */
       range_check = minimum_range;

    *increment = range_check / 10.0;

    if(min_in == 0.0)
      {
       *scale_min = min_in;
       *scale_max = *scale_min + range_check;
      }
    else  /* min_in != 0.0 */
      {
       if(min_in > 0.0)
	 {
	  if(max_in - range_check <= 0.0)
	     *scale_min = 0.0;
	  else
	    {
	     for(range_limit=100; range_limit < 10000000; range_limit *= 10)
		{
		 if(range_check <= range_limit)
		   {
		    modf(min_in/(range_limit/10), &integral_part);
		    *scale_min = integral_part * (range_limit/10);

		    modf((max_in - 1)/(range_limit/10), &integral_part);
		    *scale_max = (integral_part + 1) * (range_limit/10);

		    if(*scale_max - *scale_min == (range_limit/10)   ||
		       *scale_max - *scale_min == (range_limit/10)*2 ||
		       *scale_max - *scale_min == (range_limit/10)*5 ||
		       *scale_max - *scale_min == (range_limit))
		      {
		       *increment = (*scale_max - *scale_min) / 10.0;
		       return;
		      }
		    else if(*scale_max - *scale_min < (range_limit/10)*5)
			   {
			    *scale_max = *scale_min + (range_limit/10)*5;
			    *increment = (*scale_max - *scale_min) / 10.0;
			    return;
			   }
		    else if(*scale_max - *scale_min < range_limit)
			   {
			    *scale_max = *scale_min + range_limit;
			    *increment = (*scale_max - *scale_min) / 10.0;
			    return;
			   }
		   }  /* end if(range_check <= range_limit) */
		}     /* end for(range_limit = 10 ...       */
	    }         /* end else ...                       */
	 }
       else  /* min_in < 0.0 */
	 {
/*
 * Note that for min (or max) input values less than zero
 *  the algorithm will not provide the even starting and
 *  ending scale values that it will for min and max => zero.
 * The approach to take here is to look at the other two cases:
 *       1) min < zero && max > zero
 *            and
 *       2) min and max < zero
 *  and round up or down properly to give even start and ends of
 *  the ranges.  The difficulty is that rounding with negative
 *  numbers goes the wrong way.  Need to handle the proper rounding
 *  in the above two cases.
 * The following code will produce valid but not necessarily
 *  pretty scale ranges.
 */
	  *scale_min = min_in -
	      (float) fmod((double) min_in, (double) *increment) -
	      *increment;
	 }
       *scale_max = *scale_min + range_check;
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/scaleMaxMin.c,v $";
 static char rcs_id2[] = "$Id: scaleMaxMin.c,v 1.1 1995/09/08 15:01:28 page Exp $";}
/*  ===================================================  */

}
