/*
** LatLonToHRAP - convert from lat/lon coordinates to HRAP coordinates
**
** degrad	.... factor to convert degrees to radians
** earthrad	.... the Earth's radius in km
** lat		.... latitude in degrees N
** latrad	.... "lat" in radians
** lon		.... longitude in degrees W (always positive)
** lonrad	.... "lon" in radians
** meshlen	.... length of an HRAP mesh division at/along standard latitude
** re		.... ?
*/

#include "ResJ.h"

int LatLonToHRAP ( float lat, float lon, float *x, float *y )
{	float	degrad = 0.01745, earthrad = 6371.2, latrad, lonrad,
		meshlen = 4.7625, r, re, stdlat = 60.0, stdlon = 105.0,
		xp = 401.0, yp = 1601.0;

	if ( lon < 0.0 )	lon *= -1.0;
	re	= (earthrad*(1.0 + sin(stdlat*degrad)))/meshlen;
	latrad	= lat*degrad;
	lonrad	= (lon + 180.0 - stdlon)*degrad;
	r	= re*cos(latrad)/(1.0 + sin(latrad));
	*x	= r*sin(lonrad) + xp;	/* transfer origin from N pole */
	*y	= r*cos(lonrad) + yp;
	return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/LatLonToHRAP.c,v $";
 static char rcs_id2[] = "$Id: LatLonToHRAP.c,v 1.1 1999/02/18 15:16:59 dws Exp $";}
/*  ===================================================  */

}
