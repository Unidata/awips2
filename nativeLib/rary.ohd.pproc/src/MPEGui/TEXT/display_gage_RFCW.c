/*=========================================================================*/
/*                         FILE NAME:   display_gage_RFCW.c                */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_gage_RFCW                  */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <Xm/Scale.h>
#include <Xm/Xm.h>

#include "display7x7.h"
#include "display7x7_show.h"
#include "display_field_data_RFCW.h"
#include "drawa.h"
#include "map.h"
#include "map_library.h"
#include "map_resource.h"
#include "post_functions.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*  FUNCTION NAME:   display_gage_RFCW                                     */
/*       FUNCTION:   create display shell to display 7x7 field centered    */
/*                     on gage closest to chosen point                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) close_gage
   (callback) zoom
   (callback) gageid

Functions called:
   (callback) close_gage
   get_pixel_by_name
   get_vip_level
   get_reverse_color

Local variables:
   num - integer; gage number in array
   gageid - Widget structure; widget containing 5-character gage id
   gagevalue - Widget structure; widget containing gage accumulation
   gage_quit - Widget structure; button widget to close shell
   ix - integer; location of hrap bin in field data array
   iy - integer;
   level - integer;  data level
   fgcolor - integer; foreground color

******************************************** BEGIN display_gage_RFCW ************/

/* Widgets that must be remembered during subsequent calls to the
   display_gage_RFCW routine.  These variables are only visible to the
   routines below. */
static Widget shell = 0 ;

void display_gage_RFCW ( int num , draw_struct * data )
{
   Arg          wargs[10] ;
   char         mv [ 6 ] , gval [ 12 ] ;
   enum DisplayFieldData display_field_type ;
   float        factor,x ;
   int          i, j, n, ix, iy, level ;
   int          color,  fgcolor ;
   Widget       label ;
   XmString	xm_str;
   
 /*--------------------------------------------------------------*/
 /*     create shell and form widget to display data             */
 /*--------------------------------------------------------------*/
  
 /* Check to determine if the display 7X7 GUI is already visible. */

 display_field_type = rad_data[ 0 ].field_type;

 if ( shell == 0 )
 {
    /* Create the gui shell. The call to this routine also adds the
       appropriate callbacks to the display 7x7 widgets. */
    show_display7x7DS ( toplevel ) ;

    /* Set the keyboard focus. */
    XtSetKeyboardFocus ( display7x7FO , closePB ) ;
 }

 set_display7x7_gage_number ( num ) ;

 /* Define the label displaying the gage's identifier. */
 xm_str = XmStringCreate(gage[num].id,XmSTRING_DEFAULT_CHARSET);
 XtVaSetValues ( sitelidLB , XmNlabelString , xm_str , NULL ) ;
 XmStringFree ( xm_str ) ;

 if(strstr(gage[num].id, "PSEUDO") != NULL)
 {
    XtSetSensitive(badPB, FALSE);   
 }

   if(gage[num].is_bad == false)
   {
      xm_str = XmStringCreate ( "Set Bad" , XmSTRING_DEFAULT_CHARSET ) ;
      XtVaSetValues ( badPB , XmNlabelString , xm_str , NULL ) ;
      XmStringFree ( xm_str ) ;
   }
   else if(gage[num].is_bad == true)
   {
      xm_str = XmStringCreate ( "Set Not Bad" , XmSTRING_DEFAULT_CHARSET ) ;
      XtVaSetValues ( badPB , XmNlabelString , xm_str , NULL ) ;
      XmStringFree ( xm_str ) ;
   }


 if(gage[num].gval == -999.)
 {
   sprintf(gval , "missing" ) ;
   XmScaleSetValue ( editprecipSC , 0 ) ;
 }
 else if(gage[num].is_bad == true)
 {
    sprintf(gval , "bad" ) ;
    XmScaleSetValue ( editprecipSC , 0 ) ;
 }
 else
 {

   switch ( gage[num].edit [ 0 ] )
   {
      case '\0' :

         sprintf ( gval , "%.2f in." , gage[num].gval ) ;
         XmScaleSetValue ( editprecipSC , 100 * ( gage [ num ].gval ) ) ;
         break ;

      case 'M' :

         strcpy ( gval , "missing" ) ;
         break ;

      case 'B':

         strcpy( gval, "bad");
         break;

      default :

         sprintf ( gval , "%s" , gage [ num ].edit ) ;
         XmScaleSetValue ( editprecipSC , 
                           100 * ( atof ( gage [ num ].edit ) ) ) ;
         break ;
   }
      
 }

 /* Define the label displaying the gage's value. */
 xm_str = XmStringCreate(gval,XmSTRING_DEFAULT_CHARSET);
 XtVaSetValues ( precipamountLB , XmNlabelString , xm_str , NULL ) ;
 XmStringFree ( xm_str ) ;

 /*--------------------------------------------------------------*/
 /*     fill each point in row-column                            */
 /*     background of label will be in color which is same as    */
 /*        shown in legend                                       */
 /*     text will be in a complementary color (reverse color)    */
 /*     if any 7x7 HRAP bins are beyond rectangle defined in     */
 /*       coord_XXXXX.dat, then they are set to missing          */
 /*--------------------------------------------------------------*/

    factor = units_factor*scale_factor;

  switch ( display_field_type )
 {
    case display_Index :
    case display_Height :
    case display_Locspan :
    case display_Locbias :
    case display_Prism :  
    case display_maxtempPrism :
    case display_mintempPrism :
                            
       factor = scale_factor;
       
    default:

       /* Do Nothing. */
       break ;
 }
 
 for (i=0;i<7;i++)
 for (j=0;j<7;j++)
 {
    ix = ( int ) gage[num].hrap.x - XOR + i - 3;
    iy = ( int ) gage[num].hrap.y - YOR - j + 3;
 
    if (ix >= MAXX || iy >= MAXY || ix < 0 || iy < 0)
    {
      x=-1.0;
      level = get_vip_level(data->num_levels, data->levels, x);
    }
    else
    {
      x = (float)rad_data[0].data_array[ix][iy]/factor;
      level = get_vip_level(data->num_levels, data->levels, 
                            rad_data[0].data_array[ix][iy]);
    }

    color = get_pixel_by_name(data->w, color_list_levels[level]);
    fgcolor = get_reverse_color(data->w, color);

    if (x >= 0)
    {
       if(strcmp(cv_use,"HEIGHT") == 0 || strcmp(cv_use,"INDEX") == 0)
          sprintf(mv,"%.0f",x);
       else  
          sprintf(mv,"%.2f",x);
    }
    else
       strcpy(mv,"M");

    n=0;
    xm_str = XmStringCreate ( mv, XmSTRING_DEFAULT_CHARSET ) ;
    XtSetArg(wargs[n], XmNlabelString, xm_str ) ; n++ ;
    XtSetArg(wargs[n], XmNbackground, color ) ; n++ ;
    XtSetArg(wargs[n], XmNforeground, fgcolor ) ; n++ ;

    label = XtNameToWidget ( hrapRC , hrap_cells [ j ] [ i ] ) ;
    XtVaSetValues ( label , XmNlabelString , xm_str ,
                    XmNbackground, color , XmNforeground , fgcolor , 
                    NULL ) ;
  

    XmStringFree ( xm_str ) ;
 }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

/********************************************* END display_gage_RFCW ************/

