/* File: find_rrm_io_ts.c
 *
 * Finds the time series of the rainfall runoff models.
 *
 */

#include <stdio.h>
#include <X11/Intrinsic.h>

#include "c_call_f/fserch.h"/* -- added by AV --*/


find_rrm_io_ts(p_float, p_char, ts_float, ts_char, mp,
	       num_io_ts, loc_in_ts, loc_out_ts)

 float  p_float[];       /* parameter data */
 float  ts_float[];      /* Time series floating point data */
 char   p_char[][4];     /* Parameter character data   */
 char   ts_char[][4];    /* Time series character data */
 int    mp;              /* maximum parameter array size */
 int    *num_io_ts;      /* number of input/output time series to the runoff
			    model */
 int    loc_in_ts[];     /* location of input time series */
 int    loc_out_ts[];    /* location of output time series */
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
 int    in_locts;            /* location of input time series array */
 int    out_locts;           /* location of output time series array */
 int    loc_id;              /* location of runoff time series info in p array */
 int    loc_xin_ts_info;     /* location of XIN input time series array */
 char   nameout[8];          /* rainfall runoff model operations */
 char   in_ts_id[8];         /* input time series id */
 char   in_ts_datatype[4];   /* input time series data type array */
 int    in_ts_delta_t;       /* input time series sample time interval */
 char   out_ts_id[8];        /* output time series id */
 char   out_ts_datatype[4];  /* output time series data type array */
 int    out_ts_delta_t;      /* output time series sample time interval */

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

	    strncpy(in_ts_id, p_char[locp-1 + 7], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 9], 4);
	    in_ts_delta_t = p_float[locp-1 + 1];

	    strncpy(out_ts_id, p_char[locp-1 + 10], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + 12], 4);
	    out_ts_delta_t = p_float[locp-1 + 1];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;

/* ================================================================== */
            case API_CONT:
            
            strncpy(in_ts_id, p_char[locp-1 + 7], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 9], 4);
	    in_ts_delta_t = p_float[locp-1 + 6];

	    strncpy(out_ts_id, p_char[locp-1 + 10], 8);
	    /* Check to see if there is a total runoff time series.
	       If not, use the surface runoff time series.
	    */
	    if(strncmp(out_ts_id, "        ",8) != 0)
	       strncpy(out_ts_datatype, p_char[locp-1 + 12], 4);
	    else
	    {
	       loc_id = (int)p_float[locp-1 + 18];
	       strncpy(out_ts_id, p_char[locp-1 + loc_id-1], 8);
	       strncpy(out_ts_datatype, p_char[locp-1 + loc_id-1 + 2], 4);
	    }   
	    out_ts_delta_t = p_float[locp-1 + 6];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;
            
            
/* ================================================================== */
	    case API_MKC:
	    case API_CIN:

	    strncpy(in_ts_id, p_char[locp-1 + 13], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 15], 4);
	    in_ts_delta_t = p_float[locp-1 + 12];

	    strncpy(out_ts_id, p_char[locp-1 + 16], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + 18], 4);
	    out_ts_delta_t = p_float[locp-1 + 12];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;

/* ================================================================== */

	    case API_SLC:

	    strncpy(in_ts_id, p_char[locp-1 + 11], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 13], 4);
	    in_ts_delta_t = p_float[locp-1 + 10];

	    strncpy(out_ts_id, p_char[locp-1 + 15], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + 17], 4);
	    out_ts_delta_t = p_float[locp-1 + 14];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;

/* ================================================================== */

	    case API_HAR:

	    strncpy(in_ts_id, p_char[locp-1 + 25], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 27], 4);
	    in_ts_delta_t = p_float[locp-1 + 19];

	    strncpy(out_ts_id, p_char[locp-1 + 31], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + 33], 4);
	    out_ts_delta_t = p_float[locp-1 + 19];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;

/* ================================================================== */

	    case XIN_SMA:

	    loc_xin_ts_info = p_float[locp-1 + 14] - 1;

	    strncpy(in_ts_id, p_char[locp-1 + loc_xin_ts_info], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + loc_xin_ts_info + 2], 4);
	    in_ts_delta_t = p_float[locp-1 + 1];

	    strncpy(out_ts_id, p_char[locp-1 + loc_xin_ts_info + 3], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + loc_xin_ts_info + 5], 4);
	    out_ts_delta_t = p_float[locp-1 + 1];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;

/* ================================================================== */

	    case API_HAR2:

	    strncpy(in_ts_id, p_char[locp-1 + 31], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 33], 4);
	    in_ts_delta_t = p_float[locp-1 + 25];

	    strncpy(out_ts_id, p_char[locp-1 + 37], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + 39], 4);
	    out_ts_delta_t = p_float[locp-1 + 25];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

	    break;

/* ================================================================== */

	    case API_HFD:

	    strncpy(in_ts_id, p_char[locp-1 + 36], 8);
	    strncpy(in_ts_datatype, p_char[locp-1 + 38], 4);
	    in_ts_delta_t = p_float[locp-1 + 23];

	    strncpy(out_ts_id, p_char[locp-1 + 39], 8);
	    strncpy(out_ts_datatype, p_char[locp-1 + 41], 4);
	    out_ts_delta_t = p_float[locp-1 + 23];

	    in_locts = find_in_ts(in_ts_id, in_ts_datatype, in_ts_delta_t,
				  ts_float, ts_char);

	    if(in_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			in_ts_id, in_ts_datatype, in_ts_delta_t);
	       break;
	      }

	    loc_in_ts[*num_io_ts] = in_locts;

	    out_locts = find_in_ts(out_ts_id, out_ts_datatype, out_ts_delta_t,
				  ts_float, ts_char);

	    if(out_locts == 0)
	      {
	       printf("Problem finding time series %s, %s, %d\n",
			out_ts_id, out_ts_datatype, out_ts_delta_t);
	       break;
	      }

	    loc_out_ts[(*num_io_ts)++] = out_locts;

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/find_rrm_io_ts.c,v $";
 static char rcs_id2[] = "$Id: find_rrm_io_ts.c,v 1.3 2002/02/11 19:29:52 dws Exp $";}
/*  ===================================================  */

}
