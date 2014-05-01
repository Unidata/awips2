#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "map.h"
#include "map_defines.h"
#include "map_draw.h"
#include "map_library.h"
#include "map_resource.h"


static struct legend precip_legend;


static void reset_precip_legend ( )
{
   const char * dqc_nodata_color = NULL;
   char ** precip_colormap = NULL; 
   int i;
   extern int dqc_precip_numcol;

   /* Get the DQC No Data Color. */
   
   dqc_nodata_color = get_dqc_nodata_color ( );

   /* Restore the precipitation colormap. */
   
   precip_colormap = get_precip_base_colormap ( );

   for ( i = 0; i < dqc_precip_numcol; ++i )
   {
      precip_legend.colors[i] = precip_colormap [i];
   }

   /* Check if either the filter up or filter down options are  
      active.  If they are, then modify the color legend accordingly. */
      
   if ( precip_legend.legend_filter_up_state == M_ON )
   {
      for ( i = precip_legend.legend_filter_index + 1; 
            i < dqc_precip_numcol; ++i )
      {
          precip_legend.colors[i] = 
                      precip_colormap[precip_legend.legend_filter_index];
      }
   }
   else if ( precip_legend.legend_filter_down_state == M_ON )
   {
      for ( i = 0; i <= precip_legend.legend_filter_index; ++i )
      {
         precip_legend.colors[i] = ( char * ) dqc_nodata_color;         
      }
   }
}

char ** get_precip_colors ( )
{
   reset_precip_legend ();

   return precip_legend.colors; 
}
void precip_legend_select_cb ( Widget w, XtPointer client_data, XEvent * event )
{
   int i;
   extern int kscale;
   int selected_colorbox;
   extern int dqc_precip_numcol;

   /* Check if this is a mouse button release event.  If it is then do
      not process this event. */
   if ( ( event->type != ButtonRelease ) && ( event->xbutton.button == 1 ) )
   {
      /* Check to determine if the mouse click is in the correct range. */
      if ( ( event->xbutton.y >= precip_legend.filter_up_y ) &&
         ( event->xbutton.y <= precip_legend.filter_up_y +
                               LEGEND_BOX_HEIGHT ) )
      {
         selected_colorbox = ( event->xbutton.x - INITIAL_LEGEND_OFFSET ) /
                             LEGEND_BOX_WIDTH;

         if ( ( selected_colorbox >= 0 ) &&
              ( selected_colorbox < dqc_precip_numcol ) )
         {
            precip_legend.legend_filter_index  = selected_colorbox;
            send_expose ( );
            return;
         }
         else if ( selected_colorbox == dqc_precip_numcol )
         {
            /* the filter down button was selected. */
            precip_legend.legend_filter_down_state = M_ON;
            precip_legend.legend_filter_up_state = M_OFF;
            reset_precip_legend ( );
            send_expose ( );
            send_legend_expose ( );
         }
         else if ( selected_colorbox == (dqc_precip_numcol + 1 ) )
         {
            /* the filter up button was selected. */
            precip_legend.legend_filter_up_state = M_ON;
            precip_legend.legend_filter_down_state = M_OFF;
            reset_precip_legend ( );
            send_expose ( );
            send_legend_expose ( );
         }
         else if ( selected_colorbox == ( dqc_precip_numcol + 2 ) )
         {
           /* the filter off button was selected. */
           if ( precip_legend.legend_filter_up_state == M_ON ||
                precip_legend.legend_filter_down_state == M_ON )

           {
              precip_legend.legend_filter_up_state = M_OFF;
              precip_legend.legend_filter_down_state = M_OFF;
              reset_precip_legend ( );
              send_expose ( );
              send_legend_expose ( );
           }
         }
         else
         {
            for ( i = 0; i < NUM_PRECIP_RANGES; ++i )
            {
               if ( selected_colorbox == ( dqc_precip_numcol + 3 + i ) )
               {
                   /* This color range has been selected. */
                   kscale = i;
                   send_expose ( );
                   send_legend_expose ( );
               }
            }
         }
      }
   }
}

void dqc_legend_select_cb ( Widget w, XtPointer client_data, XEvent * event, Boolean * flag )
{
   enum MapState                dqc_freezing_flag;
   enum MapState                dqc_precip_flag;
   enum MapState                dqc_temp_flag;

   dqc_temp_flag = isThereDQCtempData ( );
   dqc_precip_flag = isThereDQCprecipData ( );
   dqc_freezing_flag = isThereDQCfreezingData ( );

   /* Retrieve the GageQC edit states. */
   if ( dqc_precip_flag == M_ON)
   {
      precip_legend_select_cb (  w, client_data, event );
   }
   else if ( dqc_temp_flag == M_ON )
   {
      temperature_legend_select_cb ( w, client_data, event );
   }
   else if ( dqc_freezing_flag == M_ON )
   {
      freezing_legend_select_cb ( w, client_data, event );
   }

}

void draw_precip_legend (int map_number, int yoffset)
{
   char ** precip_colormap = NULL;
   Display * display = NULL;
   extern char tbuf [ ];
   GC gc;   
   extern int kscale;
   int xpos;
   extern Pixmap logo[4];
   char buf[50];
   int k;
   int selection_xpos = -1;
   Dimension width, height;
   static int first = 1;
   int legendystart;
  /* int i;*/
   Pixmap pixmap;
   extern double ** dqc_precip_delim;   
   extern int      dqc_precip_numcol;
   
   
   /* If this is the first call, then initialize the legend structure. */
   if ( first == 1 )
   {
      precip_legend.legend_state = M_ON;
      precip_legend.legend_filter_up_state = M_OFF;
      precip_legend.legend_filter_down_state = M_OFF;
      precip_legend.legend_filter_index = 0;
      precip_legend.filter_up_x = 0;
      precip_legend.filter_up_y = 0;
      precip_legend.filter_down_x = 0;
      precip_legend.filter_down_y = 0;
      precip_legend.filter_off_x = 0;
      precip_legend.filter_off_y = 0;
      precip_legend.legend_range = 0;
    
      first = 0;
      reset_precip_legend ( );
   }
                     
   /* Retrieve the precipitation colormap. */
   
   precip_colormap = get_precip_colors( );

   /* Get the color limits */
   
   set_dqc_colordelimit();
   
   /* Retrieve the legend graphics context. */
   
   legendystart = yoffset + 15;

   /* Draw the DailyQC product legend. This was created in the 
      plotGageQCData routine. */ 
   xpos = INITIAL_LEGEND_OFFSET;
   mSetColor ( "White" );
   mDrawText ( M_LEGEND, map_number, xpos, legendystart, tbuf );

   /* Retrieve the legend dimensions. */
   height = _get_legend_height( );
   width = _get_legend_width ( );

   pixmap = _get_legend_pixmap ( map_number );

   /* Draw the colored GageQC rectangles. */
   legendystart += 5;

   for (k = 0; k < dqc_precip_numcol; k++)
   {
      /* Fill with the precipitation color. */
      mSetColor ( precip_colormap [ k ] );

      _area_draw_fill_box ( pixmap, xpos, legendystart, 
                            LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

      /* Check if this is the currently highlighted color. */ 
      
      if ( precip_legend.legend_filter_index == k )
      {
         selection_xpos = xpos;
      }

      /* Print out the legend. */
      
      if (k == 0)
      {
	 sprintf (buf, "%3.2f", dqc_precip_delim[kscale][k]);
	 
      }
      else if (k == (dqc_precip_numcol - 1 ))
      {
	 sprintf (buf, "%03.1f", dqc_precip_delim[kscale][k]);

      }
      else
      {
         sprintf (buf, "%03.1f", dqc_precip_delim[kscale][k]);
        
      }

      mSetColor ( "White" );

      mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
                  buf ); 
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

   precip_legend.filter_down_x = xpos;
   precip_legend.filter_down_y = legendystart;

   strcpy (buf, "down");
   XSetFillStyle (display, gc, FillSolid);
   mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
               buf ); 

   xpos += LEGEND_BOX_WIDTH; 

   XSetStipple (display, gc, logo[2]);
   XSetFillStyle (display, gc, FillStippled);
   _area_draw_fill_box ( pixmap, xpos, legendystart, 
                         LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

   precip_legend.filter_up_x = xpos;
   precip_legend.filter_up_y = legendystart;

   strcpy (buf, "up");
   XSetFillStyle (display, gc, FillSolid);
   mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
               buf ); 

   xpos += LEGEND_BOX_WIDTH; 

   XSetStipple (display, gc, logo[0]);
   XSetFillStyle (display, gc, FillStippled);
   _area_draw_fill_box ( pixmap, xpos, legendystart, 
                         LEGEND_BOX_WIDTH, LEGEND_BOX_HEIGHT );

   precip_legend.filter_off_x = xpos;
   precip_legend.filter_off_y = legendystart;

   strcpy (buf, "off");
   XSetFillStyle (display, gc, FillSolid);
   mDrawText ( M_LEGEND, map_number, xpos, legendystart+LEGEND_BOX_HEIGHT+15,
               buf ); 

   /*for (i = 0; i < NUM_PRECIP_RANGES; ++i)
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

      sprintf (buf, "%-1.0f-%-4.1f", dqc_precip_delim[i][0], dqc_precip_delim[i][dqc_precip_numcol-1]);

      XSetFillStyle (display, gc, FillSolid);
      mDrawText ( M_LEGEND, map_number, xpos, 
                  legendystart+LEGEND_BOX_HEIGHT+15,
                  buf ); 
   } */
   


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/GageQCGui/RCS/draw_precip_legend.c,v $";
 static char rcs_id2[] = "$Id: draw_precip_legend.c,v 1.3 2007/05/23 21:51:11 whfs Exp $";}
/*  ===================================================  */

}
