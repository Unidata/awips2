
/* ********************************************************************************************

	scale.c

		Contains functions for handling drawing to scale

		Coded by:       Tom Adams
		Affiliation:    DOC/NOAA/NWS/Office of Hydrology/Hydrologic Research Lab.
		Date:           12/21/92


	scale.c contains functions:

				void       check_scale_value()
				void       do_scale_change()
				void       draw_to_scale()


   ******************************************************************************************** */

/* ------------------------- INCLUDE FILES ------------------------- */

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"

/* ----------------------------------------------------------------- */


static void draw_to_scale();




/* ****************************************************************************

	check_scale_value()

   **************************************************************************** */

void check_scale_value(w, widget_struct, call_data)
	Widget                          w;
	the_widget_struct               *widget_struct;
	XmTextVerifyCallbackStruct      *call_data;
{

/*------------------------------------------------------*/
/*      Paste Operation...                              */
/*------------------------------------------------------*/
/*
 if(call_data->text->length > 1)
	{       * Implies a paste operation which we're not allowing...        *
	call_data->doit = FALSE;
	return;
	}
*/

/*------------------------------------------------------*/
/*      Allow single digit entries only...              */
/*------------------------------------------------------*/
 if(call_data->text->ptr != NULL)
	{
	if(!isdigit(call_data->text->ptr[0]))
	       {
	       call_data->doit = FALSE;
	       return;
	       }
	}

}




/* ****************************************************************************

	do_scale_change()

   **************************************************************************** */

void do_scale_change(w, widget_struct, call_data)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XmAnyCallbackStruct     *call_data;
{

	char    *string;
	int     value;



 string = XmTextFieldGetString(w);
 value = atoi(string);

 XtFree(string);

/*------------------------------------------------------*/
/*      Do not allow zooming-in below a specified       */
/*      threshold...                                    */
/*------------------------------------------------------*/
 if(value < 24000)
	{
	XBell(XtDisplay(w), 50);
	return;
	}
 else   {
	printf("The entered scale value is: 1:%d\n", value);
	draw_to_scale(value, widget_struct);
	}

}




/* ****************************************************************************

	draw_to_scale()

   **************************************************************************** */

void draw_to_scale(scale, widget_struct)
	int                     scale;
	the_widget_struct       *widget_struct;
{

	float   x_pixels_per_bin;
	float   y_pixels_per_bin;
	float   PixelsPerBin;
	float   delta_x;
	float   delta_y;

	float   OnePlus_Sin60 = 1.8660254;
	float   BinWidth;               /* Distance in kilometers...                            */
	float   factor = 4.7625;        /* HRAP bin width at 60 N Lat & 102 W Long...           */
	double  phi;                    /* Latitude (radians)...                                */
	HRAP    LatLong;

	Dimension       sw_width, sw_height;    /* Width & Height of the ScrolledWindow widget,         */
						/* which is the parent of the DrawingArea widget...     */


 XtVaGetValues(widget_struct->drawArea_SWindow,
	      XmNwidth,               &sw_width,
	      XmNheight,              &sw_height,
	      NULL);

 LatLong  = HrapToLatLong(widget_struct->overlays->center);
 phi      = (double)(PI*(double)LatLong.y/(double)180.);       /* Convert to radians...         */
 BinWidth = factor/(OnePlus_Sin60/(1. + sin(phi)));

 PixelsPerBin = (BinWidth * (float)CMS_PER_KM * (float)Screen_Resolution)/scale;

 /*-----------------------------------------------------------------------------*/
 /*     determine size of selected zoom area...                                 */
 /*-----------------------------------------------------------------------------*/

 x_pixels_per_bin = (float)widget_struct->overlays->width/(float) widget_struct->overlays->maximum_columns;
 y_pixels_per_bin = (float)widget_struct->overlays->height/(float) widget_struct->overlays->maximum_rows;

 delta_x = (float)sw_width * x_pixels_per_bin/PixelsPerBin;
 delta_y = (float)sw_height * y_pixels_per_bin/PixelsPerBin;

 printf("delta_x = %f, delta_y = %f\n", delta_x, delta_y);

 rbdata.start_x = (float)widget_struct->overlays->center.x - delta_x/2;
 rbdata.start_y = (float)widget_struct->overlays->center.y - delta_y/2;
 rbdata.last_x  = (float)widget_struct->overlays->center.x + delta_x/2;
 rbdata.last_y  = (float)widget_struct->overlays->center.y + delta_y/2;

 zoom(&widget_struct->overlays->center, widget_struct, widget_struct->overlays);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/scale.c,v $";
 static char rcs_id2[] = "$Id: scale.c,v 1.1 1995/09/08 14:55:48 page Exp $";}
/*  ===================================================  */

}
