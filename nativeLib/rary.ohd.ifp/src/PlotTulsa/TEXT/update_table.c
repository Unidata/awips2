/* File: update_table.
 *
 * Updates data tables.
 *
 */

#include "plot.h"
#include "menus.h"
#include "ifp_struct.h"

void update_table(data)
   combined_struct              *data;     /* tables and plot data structure pointer */
{
   Arg       wargs[10];
   int       i;

   if(data->tables->menu_index > -1)
   {
      change_bb_data(data);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/update_table.c,v $";
 static char rcs_id2[] = "$Id: update_table.c,v 1.2 1997/04/04 15:34:57 page Exp $";}
/*  ===================================================  */

}
