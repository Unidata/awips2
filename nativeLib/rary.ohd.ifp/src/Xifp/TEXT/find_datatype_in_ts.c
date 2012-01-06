/*
 * File: find_datatype_in_ts.c
 *
 *      returns the identifiers of time series of a given datatype
 *      in the ts array
 *  originally written by George Smith, HRL, September 1992
 *  changed by D. Page, HRL, Oct. 1997 to search for a secondary
 *      datatype if it is provided
 */
#include <stdio.h>

int     find_datatype_in_ts (char **ids, int *nids, char *datatype, 
                          float *ts_float, char (*ts_char)[4], char *datatype2)
{
int     locts; /* location of time series array */
int     loc_id; /* location of time series id */
int     loc_datatype; /* location of time series data type */

  locts = 1;

  while (locts > 0)
   {
    if(ts_float[locts-1] >= 1.0)
      {                              /*  There are remaining t.s.      */
       loc_id       = locts-1 + 2;
       loc_datatype = locts-1 + 4;

       if(strncmp(ts_char[loc_datatype], datatype, 4) == 0)
       {
	  /*  Have a match, save time series id  */
	  strncpy(ids[(*nids)++], ts_char[loc_id], 8);
       }
       /* else if(strcmp(datatype2, (char *) NULL) != 0)--Changed by AV--*/
       else if(datatype2 != NULL) 
       {
          if(strncmp(ts_char[loc_datatype], datatype2, 4) == 0)
          {
             /* found match with secondary datatype */
             strncpy(ids[(*nids)++], ts_char[loc_id], 8);
          }
       }
       locts = ts_float[locts-1 + 1];
      }                                  /*  end if(ts_float..  */
    else
       locts = 0;                        /*  no more t.s.       */
   }                                     /*  end while          */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/find_datatype_in_ts.c,v $";
 static char rcs_id2[] = "$Id: find_datatype_in_ts.c,v 1.3 2002/02/11 19:06:56 dws Exp $";}
/*  ===================================================  */

}




