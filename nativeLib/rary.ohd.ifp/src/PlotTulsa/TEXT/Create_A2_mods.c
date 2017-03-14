/* File: Create_A2_mods.c
 *
 *  Create_mods function will compare the original time series array (orig_ts_array)
 *  with the ts_array in existance when the rerun button is clicked on
 *  for the time series passed to it.  It then fills a mod structure for
 *  each separate group of changed data.  This creates mod structures for 
 *  TSCHNG mods only!
 *
 *  modified from Create_mods.c - dp - 15 Oct. 1995
 */

#include "libXifp.h"
#include "libXs.h"
#define  ChooseTStypestruct
#include "Mods_initStruct.h"
#include "Mods_opTSDataStruct.h"
#include "ifp_globals.h"
#include "Mods_everythingStruct.h"

void  Create_A2_mods(float    **orig_ts_array,     /* original time series array */
                     float    **ts_array,          /* time series array */
                     int      num_ts,              /* number of time series */
                     int      num_pts,             /* number of time series data points */
                     int      *start_run,          /* start of run pointer */
                     int      *valid_date,         /* end of observed period pointer */
                     char     *currentSegment,     /* current segment pointer */
                     TS_INFO  *ts_info,            /* time series info data structure */
                     int      ts_index,            /* time series array index */
                     char     *keyword,            /* modification array keyword pointer */
                     char     *optype,             /* type of operation */
                     char     *opname,             /* operation name */
                     Mods_everythingStruct *data   /* big mods structure */
                    )
{
   int      i,j;                 /* counters */
   int      start_change;        /* beginning of time series changes */
   int      end_change;          /* end of time series changes */
   int      start_display;       /* start of displayed data */
   int      start_switch=0;      /* flag to determine if at start */
   char     *tsid_str;           /* time series id string pointer */
   char     *command_name;       /* modification command name */

   command_name = "TSCHNG";

/* check that there is still room to make another mod */
if(data->ModIndex > MAX_MODS)
{
   printf("ERROR: data->ModIndex > MAX_MODS\n");
   return;
}

      for (j=0; j<num_pts; j++)
      {
	   if (ts_array[ts_index][j] != orig_ts_array[ts_index][j])
	   {
	      if (start_switch == 0)
	      {
		 start_change = j;           /* beginning of ts changes */
		 start_switch = 1;           /* flag to determine if at start of changes (0=yes) */
	      }

	      if (ts_array[ts_index][j+1] == orig_ts_array[ts_index][j+1])
	      {
		 /*printf("\nts[%d][%d] = %f  orig_ts[%d][%d] = %f\n", ts_index, j,
		 ts_array[ts_index][j], ts_index, j, orig_ts_array[ts_index][j]);
		 printf("start_mod = %d\n", *start_display+j*6);*/
		 data->ModArray[data->ModIndex] = (ModInfo *)malloc(sizeof(ModInfo));

		 end_change = j;
		 start_switch = 0;

		 data->ModArray[data->ModIndex]->type          = Mod_format_A2;
		 data->ModArray[data->ModIndex]->a2.type       = Mod_format_A2;
		 data->ModArray[data->ModIndex]->a2.info       = &ts_info[ts_index];
		 strcpy (data->ModArray[data->ModIndex]->a2.command, command_name);
		 start_display = *start_run + data->ModArray[data->ModIndex]->a2.info->delta_t;
		 data->ModArray[data->ModIndex]->a2.start_date =
						start_display +
						start_change*data->ModArray[data->ModIndex]->a2.info->delta_t;
		 data->ModArray[data->ModIndex]->a2.num_values = (end_change - start_change) + 1;
	    /* Comment out the setting of end_date for new Mods format ! - dp 15 Oct. 1995
	     *   if (start_change != end_change)
	     *       data->ModArray[data->ModIndex]->a2.end_date =
	     *				data->ModArray[data->ModIndex]->a2.start_date +
	     *				data->ModArray[data->ModIndex]->a2.num_values *
	     *				data->ModArray[data->ModIndex]->a2.info->delta_t;
	     *   else
	    */	 
		    data->ModArray[data->ModIndex]->a2.end_date = 0;
		 data->ModArray[data->ModIndex]->a2.valid_date = *valid_date; 
		 strcpy (data->ModArray[data->ModIndex]->a2.segment_id, currentSegment);
		 data->ModArray[data->ModIndex]->a2.values     = &ts_array[ts_index][start_change];
		 strcpy (data->ModArray[data->ModIndex]->a2.keyword, keyword);
		 strcpy (data->ModArray[data->ModIndex]->a2.optype, optype);
		 strcpy (data->ModArray[data->ModIndex]->a2.opname, opname);
	     /*
	      *   printf("type = %d mod.typ = %d num_val = %d start = %d command = %s seg = %s\n",
	      *          data->ModArray[data->ModIndex]->type,
	      *          data->ModArray[data->ModIndex]->a2.type,
	      *          data->ModArray[data->ModIndex]->a2.num_values,
	      *          data->ModArray[data->ModIndex]->a2.start_date,
	      *          data->ModArray[data->ModIndex]->a2.command,
	      *          data->ModArray[data->ModIndex]->a2.segment_id);
	      *   printf("ts_id = %s datatype = %s  ts_type = %d  delta_t = %d\n",
	      *          data->ModArray[data->ModIndex]->a2.info->ts_id,
	      *          data->ModArray[data->ModIndex]->a2.info->data_type,
	      *          data->ModArray[data->ModIndex]->a2.info->ts_type,
	      *          data->ModArray[data->ModIndex]->a2.info->delta_t);
	      *   printf("keyword = %s optype = %s  opname = %s\n",
	      *          data->ModArray[data->ModIndex]->a2.keyword,
	      *          data->ModArray[data->ModIndex]->a2.optype,
	      *          data->ModArray[data->ModIndex]->a2.opname);
	      */
		 /* check that there is still room to make another mod */
		 if(data->ModIndex > MAX_MODS)
		 {
		    printf("creating new mod will make data->ModIndex > MAX_MODS\n");
		    return;
		 }
		 else
		    data->ModIndex++;
	      }
	      else
	      {
		 /*printf("ts = %f orig_ts = %f ", ts_array[ts_index][j],
			  orig_ts_array[ts_index][j]); */
	      }

	   } /* end of if */
      }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/Create_A2_mods.c,v $";
 static char rcs_id2[] = "$Id: Create_A2_mods.c,v 1.1 1995/11/14 15:49:33 page Exp $";}
/*  ===================================================  */

}  /* end of Create_mods */

