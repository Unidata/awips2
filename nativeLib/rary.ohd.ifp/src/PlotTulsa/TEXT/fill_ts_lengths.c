#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include "ifp_atoms.h"
#include "cex25.h"
#include "plot.h"
#include "menus.h"
#include "ifp_struct.h"

/* File: fill_ts_lengths.c
 *
 * Determine the actual number of data points for each time series 
 * 
 *
 */
void fill_ts_lengths(start_run, end_run, p_float, locp, ts_info, plot_data)

   int *start_run;
   int *end_run;                 
   float p_float[];              /* parameter array */
   int *locp;                    /* startting location of Tul Plot in parameter array */
   TS_INFO  *ts_info;            /* Time series information structure  */
   plot_cb_struct *plot_data;      
{
   int      novrsn;         /* Tul Plot version number */
   int      dt_ts;          /* time series time increment */
   int      tul_locp;       /* overhead information in parameter array */
   int      i=0;            /* counter        */
   int      locts=1;        /* location of the time series array  */
   int      num_ts;         /* number of time series to be plotted or listed or both */

   novrsn = p_float[*locp-1];
   num_ts = p_float[*locp-1 + 8];
/* printf(" in fill_ts_lengths.c novrsn num_ts = %d %d\n",novrsn,num_ts); */
   for(i=0; i<num_ts; i++)
   { 
      if((p_float[*locp-1 + 3] > 60.0) ||
         (p_float[*locp-1 + 3] < 60.0 && p_float[*locp-1 + 9] < 4))
      {
         tul_locp = 30;
      }
      else
      {
         tul_locp = 43;
      }
      dt_ts = p_float[*locp-1+7];
      if (novrsn == 2)
      {
         dt_ts = p_float[*locp-1+tul_locp+12];
         if (i>0)
         {
            dt_ts = p_float[*locp-1+tul_locp+12+(i*18)];
         }
      }
      plot_data->end[i] = ((*end_run - *start_run)/ts_info[i].delta_t + 0.01) + 1;  
/*    plot_data->end[i] = ((*end_run - *start_run)/dt_ts + 0.01) + 1; */
/*    printf(" in fill_ts_lengths end_run start_run ts_info[i].delta_t dt_ts %d %d %d %d\n",
               *end_run, *start_run, ts_info[i].delta_t,dt_ts); */
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/fill_ts_lengths.c,v $";
 static char rcs_id2[] = "$Id: fill_ts_lengths.c,v 1.1 1996/12/10 20:16:22 dws Exp $";}
/*  ===================================================  */

}  /* end of fill_ts_lengths */

