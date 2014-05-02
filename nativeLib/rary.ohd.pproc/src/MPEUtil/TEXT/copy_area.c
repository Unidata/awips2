/*=========================================================================*/
/*                    FILE PATH/NAME:  STAGE3_SOURCE/copy_area.c           */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   copy_area()                        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <Xm/Xm.h>

#include "drawa.h"
#include "map_resource.h"
#include "post_functions.h"
#include "stage3_globals.h"
#include "stage3_interface.h"
#include "stage3.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:  STAGE3_SOURCE/copy_area.c                              */
/*  FUNCTION NAME:   copy_area()                                           */
/*       FUNCTION:   copy portion of pixmap back to screen as a result of  */
/*                    an expose event                                      */
/***************************************************************************

Function type:
   void

Called by function:
   fill_pixmap

Functions called:
   none

Local variables:
   w - Widget structure; DrawingArea widget
   data - deref draw_struct structure;
   call_data - deref XmDrawingAreaCallbackStruct structure;
   event - deref XExposeEvent structure; Expose Event detailing area exposed

******************************************** BEGIN copy_area ***************/

int ss_window_changed = 1 ;

draw_struct * ds_array [ 4 ] = { NULL , NULL , NULL , NULL } ;

void MPEUtil_copy_area ( Widget w , XtPointer client_data , 
	         XEvent * calldata , Boolean * flag ) 
{
   Arg wargs [ 5 ] ;
   Dimension height ;
   Dimension width ;
   Display * dpy = NULL ;
   int i ;
   int n ;
   int ssnum = ( int ) client_data ; 

   XExposeEvent * event = ( XExposeEvent * ) calldata ; 

   dpy = _get_map_display ( ) ;

   if ( ss_window_changed == 1 )
   {
      /* Add overlays for all of the windows. */
      add_overlays ( ssnum ) ;
      ss_window_changed = 0 ;

      label_radcov  ( ds_array [ MsData ] [ ssnum ].w , 
                      & ds_array [ MsData ] [ ssnum ] , NULL ) ;
      label_radclim ( ds_array [ GgData ] [ ssnum ].w , 
                      & ds_array [ GgData ] [ ssnum ] , NULL ) ;
      label_rawrad  ( ds_array [ St1iData ] [ ssnum ].w , 
                      & ds_array [ St1iData ] [ ssnum ] , NULL ) ;
      label_unbrad  ( ds_array [ St1iiData ] [ ssnum ].w , 
                      & ds_array [ St1iiData ] [ ssnum ] , NULL ) ;

      for ( i = MsData ; i <= St1iiData ; ++ i )
      {
           n=0 ;
           XtSetArg ( wargs[n], XmNwidth, & width ) ; n++ ;
           XtSetArg ( wargs[n], XmNheight, & height ) ; n++ ;
           XtGetValues ( ds_array [ i ] [ ssnum ].w , wargs , n ) ;

           XCopyArea ( dpy , ds_array [ i ] [ ssnum ].pix, 
                       XtWindow ( ds_array [ i ] [ ssnum ].w ) , 
                       ds_array [ i ] [ ssnum ].gc[0],  0 ,  0 , width, 
                       height, 0 , 0 ) ;
      }

   }
   else
   {
      /* Determine the correct pixmap to copy to the window. */
      for ( i = MsData ; i <= St1iiData ; ++ i )
      {
         if ( w == ds_array [ i ] [ ssnum ].w )
         {
            /* Extract the expose area from the event and
               redraw the window. */
            XCopyArea(XtDisplay(w), ds_array [ i ] [ ssnum ].pix, XtWindow(w), 
                      ds_array [ i ] [ ssnum ].gc[0],  event->x ,  event->y , 
                      event->width, event->height, event->x, event->y);
            break ;
         }
      }
   }
}

/********************************************* END copy_area ***************/

/***************************************************************************/
/*  FUNCTION NAME:   label_rawrad()                                        */
/*       FUNCTION:   writes "Raw SP Radar" label                              */
/***************************************************************************

Function type:
   void

Called by function:
   create_ss_interface_rfcwide
   callback from overlay options

Functions called:
   get_pixel_by_name


******************************************** BEGIN label_rawrad ************/

void label_rawrad ( Widget w , XtPointer clientdata , XtPointer calldata )

{
   char        *string="Raw SP Radar";
   char        *fontname="*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*";
   XFontStruct *font_info = NULL ;
   Display     *dpy = NULL ;
   GC           textgc;
   XGCValues    gcv;
   int          mask = GCForeground ;
   draw_struct *data = ( draw_struct * ) clientdata ;

   gcv.foreground = get_pixel_by_name(data->w,"gold2");
   dpy = XtDisplay(data->w);
   textgc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

   font_info = XLoadQueryFont(dpy, fontname);
  
   if ( font_info != NULL )
   {
      XSetFont(dpy, textgc, font_info->fid);
   }

   XDrawString(dpy, data->pix, textgc, 35, 25, string, strlen(string));
   XFreeGC ( dpy , textgc ) ;
   if ( font_info != NULL ) XFreeFont ( dpy , font_info ) ;

}

/********************************************* END label_rawrad ************/


/***************************************************************************/
/*  FUNCTION NAME:   label_unbrad()                                       */
/*       FUNCTION:   writes "Stage I Adjusted Radar" label                 */
/***************************************************************************

Function type:
   void

Called by function:
   create_ss_interface_rfcwide

Functions called:
   get_pixel_by_name

******************************************** BEGIN label_unbrad ************/

void label_unbrad ( Widget w , XtPointer clientdata , XtPointer calldata )

{
   char        *string="Mean Field Bias Corrected SP Radar";
   char        *fontname="*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*";
   XFontStruct *font_info = NULL ;
   Display     *dpy = NULL ;
   GC           textgc;
   XGCValues    gcv;
   int          mask = GCForeground ;

   draw_struct *data = ( draw_struct * ) clientdata ;

  /* XmDrawingAreaCallbackStruct *call_data =
                        ( XmDrawingAreaCallbackStruct * ) calldata ;*/

   gcv.foreground = get_pixel_by_name(data->w,"gold2");
   dpy = XtDisplay(data->w);
   textgc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

   font_info = XLoadQueryFont(dpy, fontname);

   if ( font_info != NULL ) XSetFont(dpy, textgc, font_info->fid);

   XDrawString(dpy, data->pix, textgc, 25, 25, string, strlen(string));
   XFreeGC ( dpy , textgc ) ;
   if ( font_info != NULL ) XFreeFont ( dpy , font_info ) ;
}

/********************************************* END label_unbrad ************/

/***************************************************************************/
/*  FUNCTION NAME:   label_radclim()                                       */
/*       FUNCTION:   writes "Raw DP Radar" label                      */
/***************************************************************************

Function type:
   void

Called by function:
   create_ss_interface_rfcwide

Functions called:
   get_pixel_by_name


******************************************** BEGIN label_radclim ************/

void label_radclim ( Widget w , XtPointer clientdata , XtPointer calldata )

{
   char        *string="Raw DP Radar";
   char        *fontname="*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*";
   XFontStruct *font_info = NULL ;
   Display     *dpy = NULL ;
   GC           textgc;
   XGCValues    gcv;
   int          mask = GCForeground ;

   draw_struct *data = ( draw_struct * ) clientdata ;

   gcv.foreground = get_pixel_by_name(data->w,"gold2");
   dpy = XtDisplay(data->w);
   textgc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

   font_info = XLoadQueryFont(dpy, fontname);
   if ( font_info != NULL ) XSetFont(dpy, textgc, font_info->fid);

   XDrawString(dpy, data->pix, textgc, 55, 25, string, strlen(string));
   XFreeGC ( dpy , textgc ) ;
   if ( font_info != NULL ) XFreeFont ( dpy , font_info ) ;

}

/********************************************* END label_radclim ************/

/***************************************************************************/
/*  FUNCTION NAME:   label_radcov()                                        */
/*       FUNCTION:   writes "Radar Coverage Map" label                     */
/***************************************************************************

Function type:
   void

Called by function:
   create_ss_interface_rfcwide

Functions called:
   get_pixel_by_name


******************************************** BEGIN label_radcov ************/

void label_radcov ( Widget w , XtPointer clientdata , XtPointer calldata )
 
{
   char        *string="Radar Coverage Map";
   char        *fontname="*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*";
   XFontStruct *font_info = NULL ;
   Display     *dpy = NULL ;
   GC           textgc;
   XGCValues    gcv;
   int          mask = GCForeground ;

   draw_struct *data = ( draw_struct * ) clientdata ;

   gcv.foreground = get_pixel_by_name(data->w,"gold2");
   dpy = XtDisplay(data->w);
   textgc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

   font_info = XLoadQueryFont(dpy, fontname);
   if ( font_info != NULL ) XSetFont(dpy, textgc, font_info->fid);

   XDrawString(dpy, data->pix, textgc, 40, 25, string, strlen(string));
   XFreeGC ( dpy , textgc ) ;
   if ( font_info != NULL ) XFreeFont ( dpy , font_info ) ;

}

/********************************************* END label_radcov ************/
