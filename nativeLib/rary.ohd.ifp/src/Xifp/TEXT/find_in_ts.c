
/* function find_in_ts - returns the location of the start of information
			 in the ts array for a specified time series         */

#include <stdio.h>

int     find_in_ts (id, datatype, delta_t, ts_float, ts_char)

char    *id;           /* time series id */
char    *datatype;     /* time series data type */
char    ts_char[][4];  /* time series character data */
float   ts_float[];    /* time series floating point data */
int     delta_t;       /* sample time interval */

{
	int     locts, loc_id, loc_datatype, loc_delta_t;

		locts = 1;

		while (locts > 0)
		 {
		  if(ts_float[locts-1] >= 1.0)
		    {                              /*  There are remaining t.s.      */

		     loc_id       = locts-1 + 2;
		     loc_datatype = locts-1 + 4;
		     loc_delta_t  = locts-1 + 5;

		     if(strncmp(ts_char[loc_id], id, 8) == 0              &&
			strncmp(ts_char[loc_datatype], datatype, 4) == 0  &&
			(int)ts_float[loc_delta_t] == delta_t)
		       {
			return (locts); /*  Have a match, return location in ts array */
		       }
		     locts = ts_float[locts-1 + 1];
		    }                                  /*  end if(ts_float..  */
		  else
		     locts = 0;                        /*  no more t.s.       */
		 }                                     /*  end while          */

		return (0);     /*  No match for time series, return zero            */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/find_in_ts.c,v $";
 static char rcs_id2[] = "$Id: find_in_ts.c,v 1.1 1995/09/08 15:00:13 page Exp $";}
/*  ===================================================  */

}
