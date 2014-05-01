/*
	File:		hybase_utils.c
	Date:		April 4, 1997
	Author:		Paul Taylor
	
	Purpose:	Utility functions for Hydrobase.		
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "hybase_utils.h"
#include "cvt_latlon.h"


int	hb_check_lat_bounds(char *lat)
{
   double	x;
   int		rc;
   
   /* Check for an empty string here. */
   if ( lat [ 0 ] == '\0' )
   {
      return ( False ) ;
   }

   x = cvt_spaced_format(lat, 0);
   
   /*
   if ((x > 90.0) || (x < 0.0))
      rc = False;
   else
      rc = True;
   */
   
   if ((x > 90.0) || (x < -90.0))
      rc = False;	/* failure */
   else
      rc = True;	/* success */

   return(rc);
}


int	hb_check_lon_bounds(char *lon)
{
   double	x;
   int		rc;

   /* Check for an empty string here. */
   if ( lon [ 0 ] == '\0' )
   {
      return ( False ) ;
   }
   
   x = cvt_spaced_format(lon, 0);
   
   /*
   if ((x > 180.0) || (x < 0.0))
      rc = False;	
   else
      rc = True;
   */
   if ((x > 180.0) || (x < -180.0))
      rc = False;	/* failure */
   else
      rc = True;	/* success */
   
   return(rc);
}


/*****************************************************************************
   hb_cvt_latlon_from_string:
   
  NOTE: This function is perfectly useable, however, if the database field
        is stored as a double, you SHOULD use the xtools convenience
        function "cvt_latlon_from_double" which returns a char* in
        degrees, minutes, seconds.
*****************************************************************************/

char*	hb_cvt_latlon_from_string(char *string)
{
   static char	buf[10];
   double	x;
   
   x = cvt_spaced_format(string, 0);

   memset(&buf, '\0', sizeof(buf));
   strcpy(buf, cvt_latlon_from_double(x));
   
   return(buf);
}


