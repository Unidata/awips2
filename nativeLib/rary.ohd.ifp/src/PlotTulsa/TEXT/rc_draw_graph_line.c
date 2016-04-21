/* File: rc_draw_graph_line.c
 *
 * Draws the rating curve graph
 *
 */

#include "rating_curve.h"

void rc_draw_graph_line(w, data)
  Widget          w;            /* widget data structure */
  rc_struct       *data;        /* rating curve sturctured data pointer */
  {
     Arg       wargs[5];        /* window resource data structure array */
     float     val;             /* rating curve discharge rate */
     int       i;               /* counter */
     int       dir;             /* FontLeftToRight or FontRightToLeft */
     int       ascent;          /* font ascent */
     int       desc;            /* font descent */
     int       x_offset;        /* text horizontal midpoint position */
     int       y_offset;        /* text vertical midpoint position with reference
				   to the baseline */
     Font      font_id;         /* font resource id */
     XCharStruct  char_info;    /* overall character string dimemsions */
     XFontStruct  *label_font;  /* pointer to font structure */
     XPoint    *points;         /* pointer to data points structure */
     char      *symbol="*";     /* character symbol pointer */
     Pixel     foreground;      /* foreground color */
     XGCValues gcv;             /* graphics context data structure */
     GC        line_gc=NULL;    /* graphics context data structure */

    /* printf("in rc_draw_graph_line\n"); */

     if(line_gc == NULL)
     {
	gcv.foreground = get_pixel_by_name(w, "yellow");
	line_gc = XCreateGC(XtDisplay(w), data->pix[4],
			    GCForeground, &gcv);
     }

     points = (XPoint *)malloc(data->rc_num * sizeof(XPoint));

     for(i=0; i<data->rc_num; i++)
     {
	val = data->rating_curve_q[i];
	points[i].x = val_to_pixel(&val, &data->min_q,
				   &data->q_axis_max,
				   &data->origin_x, &data->end_x);
	val = data->rating_curve_stg[i];
	points[i].y = val_to_pixel(&val, &data->min_stg,
				   &data->stg_axis_max,
				   &data->origin_y, &data->end_y);
     }

     XDrawLines(XtDisplay(w), data->pix[4], line_gc,
		points, data->rc_num, CoordModeOrigin);

     /*  Draw symbols */
     /*font_id = XLoadFont(XtDisplay(w), "serifb10");--AV serifb10 is not availabel on linux --*/
     font_id = XLoadFont(XtDisplay(w), "*-b&h-lucida-medium-r-normal-sans-12-120-*");
     XSetFont(XtDisplay(w), line_gc, font_id);
     label_font = XQueryFont(XtDisplay(w), font_id);
     XTextExtents(label_font, symbol, 1, &dir, &ascent,
		  &desc, &char_info);
     x_offset = (int)((char_info.lbearing + char_info.rbearing)/2);
     y_offset = (int)(ascent/2);
     for(i = 0; i < data->rc_num; i++)
	 XDrawString(XtDisplay(w), data->pix[4],
		     line_gc, points[i].x - x_offset,
		     points[i].y + y_offset, symbol, 1);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_draw_graph_line.c,v $";
 static char rcs_id2[] = "$Id: rc_draw_graph_line.c,v 1.2 2002/02/12 15:17:57 dws Exp $";}
/*  ===================================================  */

  }
