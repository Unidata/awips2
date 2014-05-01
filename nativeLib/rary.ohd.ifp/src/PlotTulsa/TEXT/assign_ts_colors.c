#define  ChooseTStypestruct
/*#include "mods_info.h"*/
#include <X11/Intrinsic.h>
#include "ifp_struct.h"
#include "mods_info.h"

#define PLOT   200

/* File: assign_ts_colors.c
 *
 *  Assign the colors for plotting the time series data.
 *
 *
 */

void assign_ts_colors(data, colors, ts_info)
   plot_cb_struct  *data;        /* Call back data structure pointer */
   char            *colors[];    /* Color pointer */
   TS_INFO         *ts_info;     /* Time series information structure pointer */
{
   int             i;            /* counter */

   for(i=0; i<*data->num_plotted_ts; i++)
   {
      if(data->obs_mask[data->plot_index[i]] == PLOT)
	 data->ts_color[i] = colors[0];
      else if(strcmp(ts_info[data->plot_index[i]].data_type, "PELE") == 0 ||
	      strcmp(ts_info[data->plot_index[i]].data_type, "QINE") == 0 ||
	      strcmp(ts_info[data->plot_index[i]].data_type, "RQIE") == 0 ||
	      strcmp(ts_info[data->plot_index[i]].data_type, "DQIE") == 0)
	   data->ts_color[i] = colors[1];
      else if(strcmp(ts_info[data->plot_index[i]].data_type, "SPEL") == 0 ||
	      strcmp(ts_info[data->plot_index[i]].data_type, "SQIN") == 0 ||
	      strcmp(ts_info[data->plot_index[i]].data_type, "SDQI") == 0 ||
	      strcmp(ts_info[data->plot_index[i]].data_type, "SQME") == 0)
	 data->ts_color[i] = colors[2];
      else if(2+i < 7)
	 data->ts_color[i] = colors[2+i];
	 else
	    data->ts_color[i] = colors[4];
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/assign_ts_colors.c,v $";
 static char rcs_id2[] = "$Id: assign_ts_colors.c,v 1.2 1996/12/06 21:06:50 dws Exp $";}
/*  ===================================================  */

}
