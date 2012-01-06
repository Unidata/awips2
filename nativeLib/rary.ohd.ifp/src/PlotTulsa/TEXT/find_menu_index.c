#include "plot.h"
#include "menus.h"
#include "ifp_struct.h"

/* File: find_menu_index.c
 *
 * Function called by the change_ts function. Compares the selected
 * ts_name (minus the info for the plotting symbol) with the
 * ts_menu_names.
 * If a match is found the menu_index is set to the subscript of the
 * matching ts_menu_names.
 */

void find_menu_index(data)
   combined_struct              *data;  /*  tables and plot data structure pointer */
{
   int       i;                         /* counter */
   int       ts_name_length;            /* time series name length */


   ts_name_length = strlen(data->plot->ts_name[data->plot->ts_index]) - 4; 
   data->tables->menu_index = -1;
   for(i=0; i<data->tables->num_ts_menus; i++)
   {
/*      if(strncmp(data->tables->ts_menu_names[i],
                  data->plot->ts_name[data->plot->ts_index], ts_name_length) == 0)
*/
      if((strncmp(data->tables->ts_menu_names[i],
                  data->plot->ts_name[data->plot->ts_index], ts_name_length) == 0)
          &&      
                  (data->tables->ts_info[data->tables->list_ts_index[i]].delta_t == 
                   data->tables->ts_info[data->plot->plot_index[data->plot->ts_index]].delta_t))

      {
         data->tables->menu_index = i;
      } 
      
	       /*printf("found match for %s\n",
	               	 data->plot->ts_name[data->plot->ts_index]);*/
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/find_menu_index.c,v $";
 static char rcs_id2[] = "$Id: find_menu_index.c,v 1.4 1999/04/26 12:08:45 page Exp $";}
/*  ===================================================  */

}
