#include <Xm/Xm.h>

#include "gageqc_types.h"
#include "gageqc_defs.h"
#include "gageqc_gui.h"

#include "map.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_resource.h"

static struct legend temperature_legend;


const struct legend * get_temperature_legend ( )
{
   return & temperature_legend;
}

static void reset_temperature_legend ( )
{
   const char * dqc_nodata_color = NULL;
   char ** temperature_colormap = NULL;
   int i;
   extern int dqc_temp_numcol;

   /* Get the DQC No Data Color. */
   
   dqc_nodata_color = get_dqc_nodata_color ( );

   /* Restore the temperature colormap. */
   
   temperature_colormap = get_temperature_base_colormap ( );

   for ( i = 0; i < dqc_temp_numcol; ++i )
   {
      temperature_legend.colors[i] = temperature_colormap [i];
   }

   /* Check if either the filter up or filter down options are
      active.  If they are, then modify the color legend accordingly. */
      
   if ( temperature_legend.legend_filter_up_state == M_ON )
   {
      for ( i = temperature_legend.legend_filter_index + 1;
            i < dqc_temp_numcol; ++i )
      {
          temperature_legend.colors[i] =
                 temperature_colormap[temperature_legend.legend_filter_index];
      }
   }
   else if ( temperature_legend.legend_filter_down_state == M_ON )
   {
      for ( i = 0; i <= temperature_legend.legend_filter_index; ++i )
      {
         temperature_legend.colors[i] = ( char * ) dqc_nodata_color;
      }
   }
}

char ** get_temperature_colors ( )
{
   reset_temperature_legend ();

   return temperature_legend.colors;
}

void temperature_legend_select_cb ( Widget w, XtPointer client_data, 
                                    XEvent * event )
{
   int i;
   extern int tscale;
   int selected_colorbox;
   extern int dqc_temp_numcol;
   
   /* Check if this is a mouse button release event.  If it is then do
      not process this event. */
   if ( ( event->type != ButtonRelease ) && ( event->xbutton.button == 1 ) )
   {
      /* Check to determine if the mouse click is in the correct range. */
      if ( ( event->xbutton.y >= temperature_legend.filter_up_y ) &&
         ( event->xbutton.y <= temperature_legend.filter_up_y + 
                               LEGEND_BOX_HEIGHT ) )
      {
         selected_colorbox = ( event->xbutton.x - INITIAL_LEGEND_OFFSET ) /
                             LEGEND_BOX_WIDTH;

         if ( ( selected_colorbox >= 0 ) &&
              ( selected_colorbox < dqc_temp_numcol ) )
         {
            temperature_legend.legend_filter_index  = selected_colorbox;
            send_expose ( );
            return;
         }
         else if ( selected_colorbox == dqc_temp_numcol )
         {
            /* the filter down button was selected. */
            temperature_legend.legend_filter_down_state = M_ON;
            temperature_legend.legend_filter_up_state = M_OFF;
            reset_temperature_legend ( );
            send_expose ( );
            send_legend_expose ( );
         }
         else if ( selected_colorbox == (dqc_temp_numcol + 1 ) )
         {
            /* the filter up button was selected. */
            temperature_legend.legend_filter_up_state = M_ON;
            temperature_legend.legend_filter_down_state = M_OFF;
            reset_temperature_legend ( );
            send_expose ( );
            send_legend_expose ( );
         }
         else if ( selected_colorbox == ( dqc_temp_numcol + 2 ) )
         {
           /* the filter off button was selected. */
           if ( temperature_legend.legend_filter_up_state == M_ON ||
                temperature_legend.legend_filter_down_state == M_ON )

           {
              temperature_legend.legend_filter_up_state = M_OFF;
              temperature_legend.legend_filter_down_state = M_OFF;
              reset_temperature_legend ( );
              send_expose ( );
              send_legend_expose ( );
           }
         }
         else
         {
            for ( i = 0; i < NUM_TEMP_RANGES; ++i )
            {
               if ( selected_colorbox == ( dqc_temp_numcol + 3 + i ) )
               {
                   /* This color range has been selected. */
                   tscale = i;
                   send_expose ( );
                   send_legend_expose ( );
               }
            }
         }
      }
   } 
}

void draw_temperature_legend ( int map_number, int yoffset )
{
   char ** temperature_colormap = NULL;
   Display * display = NULL;
   extern char tbuf [ ];
   //extern Display *display;
   GC gc;   
   extern int tscale;
   extern Pixmap logo[4];
   char buf[50];
   static int first = 1;
   int k;
   Dimension width, height;
   int legendystart;
   /*int i;*/
   int xpos;
   int selection_xpos = -1;
   Pixmap pixmap;
   extern double ** dqc_temp_delim;
   extern int      dqc_temp_numcol;
   
   /* If this is the first call, then initialize the legend structure. */
   if ( first == 1 )
   {
      temperature_legend.legend_state = M_ON;
      temperature_legend.legend_filter_up_state = M_OFF;
      temperature_legend.legend_filter_down_state = M_OFF;
      temperature_legend.legend_filter_index = 0;
      temperature_legend.filter_up_x = 0;
      temperature_legend.filter_up_y = 0;
      temperature_legend.filter_down_x = 0;
      temperature_legend.filter_down_y = 0;
      temperature_legend.filter_off_x = 0;
      temperature_legend.filter_off_y = 0;
      temperature_legend.legend_range = 0;

      first = 0;
      reset_temperature_legend ( );
   }

   /* Retrieve the temperature colormap. */
   
   temperature_colormap = get_temperature_colors ( );   
   
   /* Get the color limits */
   
   set_dqc_colordelimit();
      
   /* Retrieve the legend graphics context. */
   
   legendystart = yoffset + 15; 

   /* Draw the DailyQC temperature product legend.  This was created
      in the plotGageQCData routine. */
      
   xpos = 10;
   mSetColor ( "White" );
   mDrawText ( M_LEGEND, map_number, xpos, legendystart, tbuf );

   /* XSetFont (display, gc, font[3]);
   strcpy (buf, "Temp (degF)");
   XSetForeground (display, gc, qmap[15]);
   text_width = XTextWidth (info_font[3], buf, strlen (buf));
   XDrawString (display, pixm, gc, width - 5 - text_width, legendystart + 5,
		buf, strlen (buf));
   XSetFont (display, gc, font[1]); */

   /* Retrieve the legend dimensions. */
   height = _get_legend_height ( );
   width = _get_legend_width ( );

   /* Retrieve the Map Library Legend Pixmap. */
   pixmap = _get_legend_pixmap ( map_number );

   /* Draw the colored GageQC rectangles indicating the data levels. */
   legendystart += 5;

   for (k = 0; k < dqc_temp_numcol; k++)
   {
      mSetColor ( temperature_colormap [ k ] );

      _area_draw_fill_box ( pixmap, xpos, legendystart, 
                            LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );
      

      /*  Test if this is the selected color. */
      
      if (temperature_legend.legend_filter_index == k)
      {
         selection_xpos = xpos;
      } 


      /* Create the legend associated with the colorbar. */
     /* if (k == 0)
      {
	 sprintf (buf, "%4.1f", dqc_temp_delim[tscale][0]);
      }
      else if (k == (dqc_temp_numcol - 1))
      {
	 sprintf (buf, "%4.1f", dqc_temp_delim[tscale][k]);
      }
      else
      {
	 sprintf (buf, "%4.1f", dqc_temp_delim[tscale][k+1] );
      }
      */
      sprintf (buf, "%4.1f", dqc_temp_delim[tscale][k]);
      
      mSetColor ( "White" );
   
      mDrawText ( M_LEGEND, map_number, xpos, legendystart +
                  LEGEND_BOX_HEIGHT + 15, buf );
     xpos += LEGEND_BOX_WIDTH;
   }

   if ( selection_xpos >= 0 )
   {
      mSetLineWidth ( SELECTION_LINE_WIDTH );
      mSetColor ( "White" );
      _area_draw_box ( pixmap, selection_xpos, legendystart, 
                       LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );
      mSetLineWidth ( MAPLIB_DEFAULT_LINE_WIDTH );
   } 

   /* Draw the filter buttons. */
   /* Get the map legend Graphics Context. */
   gc = _get_map_gc ( );
   display = _get_map_display ( );

   mSetColor( "Grey" );

   XSetFillStyle (display, gc, FillStippled);
   XSetStipple (display, gc, logo[0]);
   _area_draw_fill_box ( pixmap, xpos, legendystart,
                         LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

   temperature_legend.filter_down_x = xpos;
   temperature_legend.filter_down_y = legendystart;

   strcpy (buf, "down");
   XSetFillStyle (display, gc, FillSolid);
   mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
               buf );

   xpos += LEGEND_BOX_WIDTH;

   XSetStipple (display, gc, logo[2]);
   XSetFillStyle (display, gc, FillStippled);
   _area_draw_fill_box ( pixmap, xpos, legendystart,
                         LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

   temperature_legend.filter_up_x = xpos;
   temperature_legend.filter_up_y = legendystart;

   strcpy (buf, "up");
   XSetFillStyle (display, gc, FillSolid);
   mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
               buf );

   xpos += LEGEND_BOX_WIDTH;

   XSetStipple (display, gc, logo[0]);
   XSetFillStyle (display, gc, FillStippled);
   _area_draw_fill_box ( pixmap, xpos, legendystart,
                         LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

   temperature_legend.filter_off_x = xpos;
   temperature_legend.filter_off_y = legendystart;

   strcpy (buf, "off");
   XSetFillStyle (display, gc, FillSolid);
   mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
               buf );

  /* for ( i = 0; i < NUM_TEMP_RANGES; ++i)
   {
      xpos += LEGEND_BOX_WIDTH;

      if ((i / 2) * 2 == i)
      {
         XSetStipple (display, gc, logo[2]);
      }
      else
      {
         XSetStipple (display, gc, logo[0]);
      }

      XSetFillStyle (display, gc, FillStippled);
      _area_draw_fill_box ( pixmap, xpos, legendystart,
                            LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

      sprintf (buf, "%-2.0f-%-2.0f", dqc_temp_delim[i][0], 
                    dqc_temp_delim[i][dqc_temp_numcol - 1]);

      XSetFillStyle (display, gc, FillSolid);
      mDrawText ( M_LEGEND, map_number, xpos,
                  legendystart+LEGEND_BOX_HEIGHT+15,
                  buf );
   } */ 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
