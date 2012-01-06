
/* File: pr_ts_names.c - prints the id, datatype, delta_t, and type for
			  each time series in the current segment         */

#include <stdio.h>

int     print_ts_names(ts_float, ts_char)

char    ts_char[][4];
float   ts_float[];
{
	int     locts;         /* location of the time series array */
	int     loc_id;        /* location of the time series id */
	int     loc_datatype;  /* location of the time series data type */
	int     loc_delta_t;   /* location of the time series sample time interval */
	int     i, j;          /* counters */

		locts = 1;

		while (locts > 0)
		 {
		  if(ts_float[locts-1] >= 1.0)
		    {                              /*  There are remaining t.s.      */

		     loc_id       = locts-1 + 2;
		     loc_datatype = locts-1 + 4;
		     loc_delta_t  = locts-1 + 5;

		   for(j = 0; j < 2; j++)
		    for(i = 0; i < 4; i++)
		     printf("%c", ts_char[loc_id+j][i]);
		   printf(", ");
		   for(i = 0; i < 4; i++)
		     printf("%c", ts_char[loc_datatype][i]);
		   printf(", %2d, type = %d\n",
				 (int)ts_float[loc_delta_t],
				 (int)ts_float[locts-1]);
		    }                                  /*  end if(ts_float..  */
		  locts = ts_float[locts-1 + 1];
		 }                                     /*  end while          */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/pr_ts_names.c,v $";
 static char rcs_id2[] = "$Id: pr_ts_names.c,v 1.1 1995/09/08 15:01:09 page Exp $";}
/*  ===================================================  */

}
