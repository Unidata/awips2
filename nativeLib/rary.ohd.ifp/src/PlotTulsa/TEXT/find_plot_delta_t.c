#include "plot.h"
#include "ifp_struct.h"

/* ---------------------------------------------------------- 
     find_plot_delta_t(plot_cb_struct *, TS_INFO *)
     
     Finds the minimum delta_t of the time series that are
     plotted on the hydrograph and fills the variable in the
     plot_cb_struct.
     
     Written:  13 Jan. 1998 - D. Page
     
----------------------------------------------------------*/      

#define MAX_DELTA_T  24

void find_plot_delta_t(plot_cb_struct *p_data, TS_INFO *ts_info)
{
   int      i;
   
   p_data->plot_delta_t = MAX_DELTA_T;

   for(i=0; i<*p_data->num_plotted_ts; i++)
      if(ts_info[p_data->plot_index[i]].delta_t < p_data->plot_delta_t)
         p_data->plot_delta_t = ts_info[p_data->plot_index[i]].delta_t;
   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/find_plot_delta_t.c,v $";
 static char rcs_id2[] = "$Id: find_plot_delta_t.c,v 1.1 1998/04/08 11:14:49 page Exp $";}
/*  ===================================================  */

}        
