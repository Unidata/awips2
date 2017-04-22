/* File: find_num_rrm.c
 *
 * Finds the number of rainfall runoff models.
 *
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include "c_call_f/fserch.h"/* -- added by AV --*/


find_num_rrm(p_float, p_char, ts_float, ts_char, mp,
	       num_io_ts)

 float  p_float[];       /* parameter data */
 float  ts_float[];      /* Time series floating point data */
 char   p_char[][4];     /* Parameter character data   */
 char   ts_char[][4];    /* Time series character data */
 int    mp;              /* maximum parameter array size */
 int    *num_io_ts;      /* number of input/output time series to the runoff
			    model */
{
#define SAC_SMA   1       /* internal rainfall runoff model values */
#define API_CONT 24
#define API_MKC  29
#define API_CIN  33
#define API_SLC  34
#define API_HAR  35
#define XIN_SMA  36
#define API_HAR2 41
#define API_HFD  43

 static int     rrm_operations[] = {SAC_SMA,
				    API_CONT,
				    API_MKC,
				    API_CIN,
				    API_SLC,
				    API_HAR,
				    XIN_SMA,
				    API_HAR2,
				    API_HFD};

 int    i;                   /* counter */
 int    locp;                /* pointer to the location of the beginning of
				parameter array. */
 char   nameout[8];          /* rainfall runoff model operations */


 *num_io_ts = 0;             /* number of input/output time series used */

 for(i = 0; i < XtNumber(rrm_operations); i++)
    {
     locp = 1;

     FSERCH(&rrm_operations[i], nameout, &locp, p_float, p_char, &mp);

     while(locp > 0)
	  {
	   switch (rrm_operations[i])
	   {
/* ================================================================== */
	    case SAC_SMA:

	    (*num_io_ts)++;

	    break;

/* ================================================================== */
            case API_CONT:
            
	    (*num_io_ts)++;

	    break;
            
            
/* ================================================================== */
	    case API_MKC:
	    case API_CIN:

	    (*num_io_ts)++;

	    break;

/* ================================================================== */

	    case API_SLC:
	    
	    (*num_io_ts)++;

	    break;

/* ================================================================== */

	    case API_HAR:

	    (*num_io_ts)++;

	    break;

/* ================================================================== */

	    case XIN_SMA:

	    (*num_io_ts)++;

	    break;

/* ================================================================== */

	    case API_HAR2:

	    (*num_io_ts)++;

	    break;

/* ================================================================== */

	    case API_HFD:

	    (*num_io_ts)++;

	    break;

/* ================================================================== */
	    default:

	    printf("Match for rrm operation %d not found - ", i);
	    printf("in function find_rrm_io_ts\n");
	   }  /* end switch */
	 /*
	  *  Now search for other rrm operations of the current type
	  *   in the segment.
	  */
	   FSERCH(&rrm_operations[i], nameout, &locp, p_float, p_char, &mp);

	  }   /* end while  */
	 /*
	  *  Cycle through all rrm operation types - do not expect to
	  *   find more than one type in a given segment, but
	  *   check anyway.
	  */
    }         /* end for    */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/find_num_rrm.c,v $";
 static char rcs_id2[] = "$Id: find_num_rrm.c,v 1.3 2002/02/11 19:29:37 dws Exp $";}
/*  ===================================================  */

}
