/* File: mods_keywds.c
 *
 *  Modified to use menu_structure to pass active mods flag
 *      George Smith, HRL, December 1990.
 *
 */

/*  #include <stdio.h>  */
#include "libXifp.h"
#include "libXs.h"
#include "mods_info.h"

void mods_keywords(ts_id, datatype, delta_t,
		  ts_float, ts_char,
		  menu_array,
		  size_of_menu)

char    *ts_id;    /* time series id pointer */
char    *datatype;
char    **ts_char; /* address of time series character array pointer */
int     delta_t;   /* sample time interval */
float   *ts_float; /* time series floating data pointer */
xs_menu_struct  menu_array[];
int     size_of_menu;

{
char    ts_id8[9];    /* time series id array */
char    datatype4[5];
int     locts;        /* location of the time series array */
int     ts_type;      /* time series type */
int     i;            /* counter */

memset(ts_id8, ' ', 8);
ts_id8[8] = '\0';
memset(datatype4, ' ', 4);
datatype4[4] = '\0';

strncpy(ts_id8, ts_id, strlen(ts_id));
strncpy(datatype4, datatype, strlen(datatype));

/*
 *  Set all active flags in the menu_ptr array to FALSE.
 */

for (i = 0; i < size_of_menu; i++)
     menu_array[i].active = FALSE;

locts = find_in_ts(ts_id8, datatype4, delta_t, ts_float, ts_char);

if(locts > 0)
  {              /*  locts is start of info in ts array         */
   ts_type = ts_float[locts-1];
 /*
  * ts_type is the type of time series.
  * The choices are
  *     = 1, input time series (reads data from external data files),
  *     = 2, update time series (data initially read from external
  *            files and written to external files at end of
  *            operations table processing),
  *     = 3, output time series (data values are written to
  *            external files at end of operations table processing),
  *     = 4, internal time series (data are only used to pass
  *            information between operations, not read or written
  *            to/from external files).
  *
  * Valid Mod time series keywords are
  *     for input time series
  *         FIRST or optype are valid.
  *         FIRST is assumed if no entry is made.
  *     for update time series
  *         FIRST, LAST, or optype are required.
  *     for output time series
  *         LAST or optype are valid.
  *         LAST is assumed if no entry is made.
  *     for internal time series
  *         optype is required.
  */
   switch (ts_type)
    {
     case 1:                                  /* input t.s.    */
	{
	 menu_array[0].active = TRUE;           /* FIRST  */
	 menu_array[2].active = TRUE;           /* optype */
	 break;
	}

     case 2:                                  /* update t.s.   */
	{
	 menu_array[0].active = TRUE;           /* FIRST  */
	 menu_array[1].active = TRUE;           /* LAST   */
	 menu_array[2].active = TRUE;           /* optype */
	 break;
	}

     case 3:                                  /* output t.s.   */
	{
	 menu_array[1].active = TRUE;           /* LAST   */
	 menu_array[2].active = TRUE;           /* optype */
	 break;
	}

     case 4:                                  /* internal t.s. */
	{
	 menu_array[2].active = TRUE;           /* optype */
	 break;
	}

     default:
	{   /*  shouldn't get here - invalid ts_type   */
	 break;
	}
    }                /*    end switch         */
  }                  /*    end if             */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/mods_keywds.c,v $";
 static char rcs_id2[] = "$Id: mods_keywds.c,v 1.1 1995/09/08 15:01:00 page Exp $";}
/*  ===================================================  */

}                    /*    end mods_keywords  */
