/*
	File:		cvt_to_latlon.h
	Date:		August 1994
	Authors:	Dale Shelton, Chip Gobs, Paul Taylor (4/1/97)
	
	Purpose:	Provide support for converting lat/lon floating values
			to & from regular string variables.
*/


#ifndef cvt_latlon_h
#define cvt_latlon_h

#define MAX_DBL ( ( float ) 1.7976931348623157e+308 )


double	cvt_to_latlon(char *str, int negate);


int	isPackedLatLon(char *str);


double	cvt_spaced_format(char *str, int negate);

double	cvt_packed_format(char *str, int negate);


double	cvt_to_lat(char *str);
double	cvt_to_lon(char *str);


double	bcvt_to_lat(char *str);
double	ncvt_to_latlon(char *s);


char*	cvt_latlon_from_double(double x);


#endif


