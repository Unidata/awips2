#include "plot.h"
#include "ifp_struct.h"

void draw_graph_line(w, data, index)
  Widget          w;            /* widget data structure */
  combined_struct *data;        /* tables and plot data structure pointer */
  int             index;        /* array member value  */
  {
     float     val;             /* x axis data value */
     int       i;               /* counter           */
     int       increment;       /* increment for different time series interval */
     int       dir;             /* direction text is drawn */
     int       ascent;          /* position above the baseline */
     int       desc;            /* position below the baseline */
     int       x_offset;        /* x position from the label border */
     int       y_offset;        /* y position from the label border */
     Font      font_id;
     XCharStruct  char_info;    /* width, left bearing, right bearing string
				   information structure */
     XFontStruct  *label_font;  /* label font pointer    */
     XPoint    *points;         /* points structure pointer */

     points = (XPoint *)malloc(*data->plot->num_pts * sizeof(XPoint)); 

     increment = ((*data->plot->num_pts - 1)/(data->plot->end[data->plot->plot_index[index]] -1));

     for(i=1; i<data->plot->end[data->plot->plot_index[index]]; i++)
     {
/*      val = data->plot->x[i]; */

/* fixed to plot time series with different time interval - ddt 10/17/96 */

        val = ((i-1)*increment) + (1*increment);

	points[i-1].x = val_to_pixel(&val, data->plot->min_x, data->plot->max_x,
				   &data->plot->origin_x, &data->plot->end_x);
	val = data->plot->y[index][i-1];
	points[i-1].y = val_to_pixel(&val, data->plot->min_y,
				   data->plot->discharge_axis_max,
				   &data->plot->origin_y, &data->plot->end_y);
     }

     /* Check the observed mask to see if the plotted time series
	is observed.  If not observed, draw lines and symbols.  If
	observed, draw only symbols.
     */
     if(data->plot->obs_mask[data->plot->plot_index[index]] != PLOT)
     {
	XDrawLines(XtDisplay(w), data->plot->pix[5], data->plot->line_gc[index],
		   points, data->plot->end[data->plot->plot_index[index]]-1, CoordModeOrigin);
     }

     /*  Draw symbols */
     /*font_id = XLoadFont(XtDisplay(w), "serifb10");*/
     /* There is no serifb10 font on linux --AV--*/
     font_id = XLoadFont(XtDisplay(w), "*-adobe-times-bold-r-normal--12-120-75-75-p-67-*");
     XSetFont(XtDisplay(w), data->plot->line_gc[index], font_id);
     label_font = XQueryFont(XtDisplay(w), font_id);
     XTextExtents(label_font, data->plot->ts_symbol[index], 1, &dir, &ascent,
		  &desc, &char_info);
     x_offset = (int)((char_info.lbearing + char_info.rbearing)/2);
     y_offset = (int)(ascent/2);
     for(i = 0; i < data->plot->end[data->plot->plot_index[index]]-1; i++)
	 XDrawString(XtDisplay(w), data->plot->pix[5],
		     data->plot->line_gc[index], points[i].x - x_offset,
		     points[i].y + y_offset, data->plot->ts_symbol[index], 1);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/draw_graph_line.c,v $";
 static char rcs_id2[] = "$Id: draw_graph_line.c,v 1.4 2002/02/11 19:26:58 dws Exp $";}
/*  ===================================================  */

  }
