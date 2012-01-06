/* File: update_plot.c
 *
 * Updates hydrograph plot.
 *
 */


#include "plot.h"
#include "menus.h"
#include "ifp_struct.h"

void update_plot(current_column, data)
   int                          current_column;
   combined_struct              *data;    /* tables and plot data structure pointer */
{
   Arg       wargs[10];
   int       i;                           /* counter */
   XGCValues line_gcv;
   Pixel     foreground, background;      /* foreground, background colors */

   for(i=0; i<*data->plot->num_plotted_ts; i++)
   {
      if(strncmp(data->plot->ts_name[i],
		data->tables->ts_menu_names[current_column-1], 8) == 0)
      {
	/* printf("found match for %s\n", data->plot->ts_name[i]); */
	 resize_hydrograph(data->plot->drawing_area_widget[5], data, NULL);
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/update_plot.c,v $";
 static char rcs_id2[] = "$Id: update_plot.c,v 1.1 1995/09/08 14:58:41 page Exp $";}
/*  ===================================================  */

}

