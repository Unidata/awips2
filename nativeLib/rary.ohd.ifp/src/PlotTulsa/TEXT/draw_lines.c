/* File: draw_lines.c
 *
 * Line drawing routine .
 */

#include "plot.h"
#include "ifp_struct.h"

void draw_lines(w, data, cb)
  Widget          w;                  /* widget data structure */
  combined_struct *data;              /* tables and plot data structure pointer */
  XmDrawingAreaCallbackStruct   *cb;  /* pointer to window data structure */

  {
     int       ts_index;

     for(ts_index = 0; ts_index < *data->plot->num_plotted_ts; ts_index++)
     {
	if(data->plot->obs_mask[data->plot->plot_index[ts_index]] != PLOT)
	    draw_graph_line(w, data, ts_index);
     }
     for(ts_index = 0; ts_index < *data->plot->num_plotted_ts; ts_index++)
     {
	if(data->plot->obs_mask[data->plot->plot_index[ts_index]] == PLOT)
	    draw_graph_line(w, data, ts_index);
     }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/draw_lines.c,v $";
 static char rcs_id2[] = "$Id: draw_lines.c,v 1.1 1995/09/08 14:57:14 page Exp $";}
/*  ===================================================  */

  }

