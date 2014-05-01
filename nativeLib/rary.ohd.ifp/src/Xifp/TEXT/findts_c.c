
/* function findts_c - returns the location of the start of data
		       in the d array for a specified time series         */

#include <stdio.h>

int     findts_c (id, datatype, delta_t, ts_float, ts_char)

char    *id, *datatype, ts_char[][4];
float   ts_float[];
int     delta_t;

{
	int     locts, locd, loc_id, loc_datatype, loc_delta_t;

		locts = 1;

		while (locts > 0)
		 {
		  if(ts_float[locts-1] >= 1.0)
		    {                              /*  There are remaining t.s.      */
		     locd = ts_float[locts-1 + 7]; /*  Location of data in d array   */

		     loc_id       = locts-1 + 2;
		     loc_datatype = locts-1 + 4;
		     loc_delta_t  = locts-1 + 5;

		     if(strncmp(ts_char[loc_id], id, 8) == 0              &&
			strncmp(ts_char[loc_datatype], datatype, 4) == 0  &&
			(int)ts_float[loc_delta_t] == delta_t)

		     return (locd);     /*  Have a match, return location in d array */

		    }                                  /*  end if(ts_float..  */
		  locts = ts_float[locts-1 + 1];
		 }                                     /*  end while          */

		return (0);     /*  No match for time series, return zero            */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/findts_c.c,v $";
 static char rcs_id2[] = "$Id: findts_c.c,v 1.1 1995/09/08 15:00:20 page Exp $";}
/*  ===================================================  */

}
