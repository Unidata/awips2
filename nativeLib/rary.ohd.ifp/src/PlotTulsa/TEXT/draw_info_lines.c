/* File: draw_info_lines.c
 *
 *  Gets the width and height of the drawing area.
 *
 *  Gets foreground and background colors to make graphics context
 *  or lines to draw into pixmap.
 *
 *  Creates graphics context for the end of observation line.
 *
 *  Converts stage from English to metric then calculates metric
 *  discharge rate and converts it to English units if needed.
 *
 */

#include "plot.h"
#include "ifp_struct.h"
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h" 
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fcshft.h"

float         stage_to_q();
float         q_to_stage();

void draw_info_lines(Widget w, combined_struct *data)
{
   int   i, n;                                /* counters */
   Arg   wargs[10];                           /* window resource data structure array */
   Dimension     width, height;               /* widget dimensions */
   Dimension     pix_width, pix_height;       /* drawing area dimensions */
   GC            info_gc;                     /* graphics context data structure */
   Pixel         foreground, background;      /* foreground, backround colors */
   XGCValues     info_gcv;                    /* graphics context data structure */
   float         val;                         /* end of obsevations value   */
   float         discharge;                   /* discharge rate  */
   float         stage_metric;                /* metric river depth */
   int           x1, y1, x2, y2;              /* plot points */
   char          line_label[3];               /* label for lines */

  /* printf("in draw_info_lines\n"); */

/*
 * Get the width and height of the drawing area.
 * Also, get foreground and background colors to make
 *  graphics context for lines to draw into pixmap.
 */
   n=0;
   XtSetArg(wargs[n], XmNwidth, &width); n++;
   XtSetArg(wargs[n], XmNheight, &height); n++;
   XtSetArg(wargs[n], XmNforeground, &foreground); n++;
   XtSetArg(wargs[n], XmNbackground, &background); n++;
   XtGetValues(w, wargs, n);

   pix_width = data->plot->width_scale * width;
   pix_height = data->plot->height_scale * height;

/* Create graphics context for end_obs line.  Can be changed for
   flood and alert lines if needed (if rating curve exists).
*/
   info_gcv.foreground = get_pixel_by_name(w, "white");
   info_gcv.line_style = LineOnOffDash;
   info_gc = XCreateGC(XtDisplay(w), data->plot->pix[5],
		       GCForeground | GCLineStyle, &info_gcv);

   val = data->plot->end_obs;
   x1 = x2 = val_to_pixel(&val, data->plot->min_x, data->plot->max_x,
			  &data->plot->origin_x, &data->plot->end_x);
   y1 = pix_height;
   y2 = 0;

   if(x1 > 0.0 && x1 <= pix_width)
      XDrawLine(XtDisplay(w), data->plot->pix[5], info_gc, x1, y1, x2, y2);
      
/* Add zero line if negative values are plotted - dp - 6 March 1998 */
   if(*data->plot->min_y < 0.0)
   {
      val = 0.0;
      y1 = y2 = val_to_pixel(&val, data->plot->min_y, data->plot->discharge_axis_max,
			     &data->plot->origin_y, &data->plot->end_y);
      x1 = 0;
      x2 = pix_width;
      if(y1 > 0.0 && y1 <= pix_height)
         XDrawLine(XtDisplay(w), data->plot->pix[5], info_gc, x1, y1, x2, y2);
   }
   			            
   if(data->plot->rc_data->RC == TRUE)
   {
      x1=10;/*10 or any int are fine.*/
      FCSHFT(&x1,data->plot->rc_data->rc_id);/*kwz.r25-34.Added so q_to_stage
	  would work correctly.  x1 is a dummy variable.*/
/*  Draw top of rating curve line */
      info_gcv.foreground = get_pixel_by_name(w, "tan");
      XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
                info_gc, GCForeground, &info_gcv);

      discharge =  data->plot->rc_data->rating_curve_q[data->plot->rc_data->rc_num-1];

      x1 = 0;
      x2 = pix_width;
      val = discharge;
      y1 = y2 = val_to_pixel(&val, data->plot->min_y,
                             data->plot->discharge_axis_max,
                             &data->plot->origin_y, &data->plot->end_y);

      if(y1 > 0.0 && y1 <= pix_height)
      {
         XDrawLine(XtDisplay(w), data->plot->pix[5], info_gc, x1, y1, x2, y2);
         memset(line_label, '\0', 3);
         strcpy(line_label, "RC");         
         XDrawString(XtDisplay(w), data->plot->pix[5], info_gc, x2-60, y1, 
                     line_label, strlen(line_label));
      }

/*  Draw max of record line */
      info_gcv.foreground = get_pixel_by_name(w, "MediumPurple1");
      /*info_gcv.foreground = get_pixel_by_name(w, "medium slate blue");*/
      XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
                info_gc, GCForeground, &info_gcv);

      if (data->plot->rc_data->max_record_q > 0.0)
         discharge = data->plot->rc_data->max_record_q;
      else if(data->plot->rc_data->max_record_stg > 0.0)
         if(NWSRFS_general_units == 0)
         {
            /* convert stage from English to metric then calculate
               discharge (metric) and convert back to English
            */
            stage_metric = (data->plot->rc_data->max_record_stg
                            - data->plot->rc_data->stg_add_constant)
                            / data->plot->rc_data->stg_mult_conver_factor;

            discharge = stage_to_q(stage_metric);

            discharge = discharge * data->plot->rc_data->q_mult_conver_factor +
                        data->plot->rc_data->q_add_constant;
         }
         else  /* already in metric */
            discharge = stage_to_q(data->plot->rc_data->max_record_stg);

      x1 = 0;
      x2 = pix_width;
      val = discharge;
      y1 = y2 = val_to_pixel(&val, data->plot->min_y,
                             data->plot->discharge_axis_max,
                             &data->plot->origin_y, &data->plot->end_y);

      if(y1 > 0.0 && y1 <= pix_height)
      {
         XDrawLine(XtDisplay(w), data->plot->pix[5], info_gc, x1, y1, x2, y2);
         memset(line_label, '\0', 3);
         strcpy(line_label, "MF");         
         XDrawString(XtDisplay(w), data->plot->pix[5], info_gc, x2-45, y1, 
                     line_label, strlen(line_label));         
      }
      
/*  Draw alert line */      
      if(data->plot->rc_data->warning_stg > 0.0 &&
         data->plot->rc_data->warning_stg != data->plot->rc_data->flood_stg)
      {
         info_gcv.foreground = get_pixel_by_name(w, "yellow");
         XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
                   info_gc, GCForeground, &info_gcv);

         if(NWSRFS_general_units == 0)
         {
            /* convert stage from English to metric then calculate
               discharge (metric) and convert back to English */
            stage_metric = (data->plot->rc_data->warning_stg
                            - data->plot->rc_data->stg_add_constant)
                            / data->plot->rc_data->stg_mult_conver_factor;

            discharge = stage_to_q(stage_metric);

            discharge = discharge * data->plot->rc_data->q_mult_conver_factor +
                        data->plot->rc_data->q_add_constant;
         }
         else
            discharge = stage_to_q(data->plot->rc_data->warning_stg);

         data->plot->rc_data->warning_flow = discharge;

         x1 = 0;
         x2 = pix_width;
         val = discharge;
         y1 = y2 = val_to_pixel(&val, data->plot->min_y,
                                data->plot->discharge_axis_max,
                                &data->plot->origin_y, &data->plot->end_y);

         if(y1 > 0.0 && y1 <= pix_height)
         {
            XDrawLine(XtDisplay(w), data->plot->pix[5], info_gc, x1, y1, x2, y2);
            memset(line_label, '\0', 3);
            strcpy(line_label, "A");         
            XDrawString(XtDisplay(w), data->plot->pix[5], info_gc, x2-30, y1, 
                        line_label, strlen(line_label));         
         }
            
      }
      else /* warning stage missing or equal to flood stage */
      {
       data->plot->rc_data->warning_flow = data->plot->rc_data->flood_flow;
      }
      
/*  Draw flood line */   
      if(data->plot->rc_data->flood_flow > 0.0 ||
	 data->plot->rc_data->flood_stg > 0.0)
      {
	 info_gcv.foreground = get_pixel_by_name(w, "red");
	 XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
		   info_gc, GCForeground, &info_gcv);

/*kwz if (data->plot->rc_data->flood_flow > 0.0)
	    discharge = data->plot->rc_data->flood_flow;
	 else if(data->plot->rc_data->flood_stg > 0.0)
*//*Changed because flood_flow is not reliable if there's a qcshift mod.r25-34*/
     if(data->plot->rc_data->flood_stg > 0.0)
	 {
	    if(NWSRFS_general_units == 0)
	    {
	       /* convert stage from English to metric then calculate
		  discharge (metric) and convert back to English
	       */
	       stage_metric = (data->plot->rc_data->flood_stg
			       - data->plot->rc_data->stg_add_constant)
			       / data->plot->rc_data->stg_mult_conver_factor;

	       discharge = stage_to_q(stage_metric);

	       discharge = discharge * data->plot->rc_data->q_mult_conver_factor +
			   data->plot->rc_data->q_add_constant;
	    }
	    else  /* already in metric */
	       discharge = stage_to_q(data->plot->rc_data->flood_stg);
	       
	    data->plot->rc_data->flood_flow = discharge;          
     }
	 x1 = 0;
	 x2 = pix_width;
	 val = discharge;
	 y1 = y2 = val_to_pixel(&val, data->plot->min_y,
				data->plot->discharge_axis_max,
				&data->plot->origin_y, &data->plot->end_y);

	 if(y1 > 0.0 && y1 <= pix_height)
	 {
	    XDrawLine(XtDisplay(w), data->plot->pix[5], info_gc, x1, y1, x2, y2);
            memset(line_label, '\0', 3);
            strcpy(line_label, "F");         
            XDrawString(XtDisplay(w), data->plot->pix[5], info_gc, x2-20, y1, 
                        line_label, strlen(line_label));         
         }
      }

   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/draw_info_lines.c,v $";
 static char rcs_id2[] = "$Id: draw_info_lines.c,v 1.6 2005/03/17 16:44:34 wkwock Exp $";}
/*  ===================================================  */

}
