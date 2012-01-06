
/* ********************************************************************************

	fcstGroup_zoom.c

	Purpose:        contains functions used to zoom into the Forecast Group
			map in a way such that the scrolled window of the DrawingArea
			widget contains the complete Forecast Group at its maximum
			possible size

	Coded by:       Tom Adams
	Affiliation:    NOAA/NWS/Office of Hydrology/HRL
	Date:           10/21/1992
	Major change:   10/21/1992

   ******************************************************************************** */


/* ---------- Headers --------- */

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"

/* -----------------------------*/


static point get_ForecastGroup_center();


/* **************************************************************************

	void zoom_forecastGroup()

		finds the center of a bounding rectangle for the Forecast
		Group, in HRAP coordinates, and calls 'zoom_in()' to show
		an enlarged view...

		'rad_data' is a global data structure


   ************************************************************************** */

void zoom_forecastGroup(widget_struct)
	the_widget_struct       *widget_struct;
{

	point   center;         /* (x,y) center of the zoom area in HRAP units...       */


 center = get_ForecastGroup_center(widget_struct, rad_data);

 zoom_in(&center, widget_struct, rad_data);


}


/* **************************************************************************

	point get_ForecastGroup_center()


   ************************************************************************** */

point get_ForecastGroup_center(widget_struct, data)
	the_widget_struct       *widget_struct;
	draw_struct             *data;
{

	int             i;
	int             j;
	int             k;
	int             numbasin;
	int             x;
	int             y;
	int             MAPBasinFound;

	char            *blank;
	char            temp1[9];
	char            temp2[9];

	Dimension       width;
	Dimension       height;

	point           center;
	point           upperLeft;
	point           lowerRight;

	overlay_struct  **basin;



 memset(temp1, '\0', 9);
 memset(temp2, '\0', 9);

 upperLeft.x  =  10000;
 upperLeft.y  = -10000;

 lowerRight.x = -10000;
 lowerRight.y =  10000;

 basin = mapbasin;
 numbasin = nummap;


 /*-------------------------------------------------------------*/
 /*     Find the upper-left and lower-right verticies of the    */
 /*     bounding rectangle...                                   */
 /*-------------------------------------------------------------*/

 for (k = 0; k < NumBasinsInCurrFcstGroup; k++)
	{
	/* It's not certain that there will be a match in the data between      */
	/* FGBasin_ID[] and basin[] in all cases, so let's check...             */
	MAPBasinFound = FALSE;

	/* Make sure there are no trailing blanks...                            */
	strcpy(temp1, FGBasin_ID[k]);
	if((blank = strchr(temp1, ' ')) != (char *)NULL) *blank = '\0';


	for (i = 0; i < numbasin; i++)
	       {
	       /* Make sure there are no trailing blanks...                     */
	       strcpy(temp2, basin[i]->id);
	       if((blank = strchr(temp2, ' ')) != (char *)NULL) *blank = '\0';


	       if(strcmp(temp1, temp2) == 0)
			{
			/* printf("%s, %s.\n", FGBasin_ID[k], basin[i]->id);     */
			MAPBasinFound = TRUE;
			break;
			}
	       }

	if(MAPBasinFound)
		{
		for (j = 0; j < basin[i]->npts; j++)
			{
			if(basin[i]->hrap[j].x < upperLeft.x) upperLeft.x = basin[i]->hrap[j].x;
			if(basin[i]->hrap[j].y > upperLeft.y) upperLeft.y = basin[i]->hrap[j].y;

			if(basin[i]->hrap[j].x > lowerRight.x) lowerRight.x = basin[i]->hrap[j].x;
			if(basin[i]->hrap[j].y < lowerRight.y) lowerRight.y = basin[i]->hrap[j].y;
			}
		}
	}

 center.x = upperLeft.x + abs(lowerRight.x - upperLeft.x)/2;
 center.y = upperLeft.y - abs(lowerRight.y - upperLeft.y)/2;


 /*--------------------------------------------------------------*/
 /*     determine number of pixels per hrap bin                  */
 /*--------------------------------------------------------------*/

 XtVaGetValues(data->w, XmNwidth, &width, XmNheight, &height, NULL);

 x = (float)width/(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;

 if (x > y) x = y;
 else if (y > x) y = x;


 /*-------------------------------------------------------------*/
 /*     Needed for subsequent functions; convert HRAP           */
 /*     coordinates to DrawingArea widget pixel units...        */
 /*-------------------------------------------------------------*/

 rbdata.start_x = (upperLeft.x - data->origin.x) * x;
 rbdata.start_y = (data->maximum_rows - (upperLeft.y - data->origin.y))*y;
 rbdata.last_x  = (lowerRight.x - data->origin.x) * x;
 rbdata.last_y  = (data->maximum_rows - (lowerRight.y - data->origin.y))*y;


 /*--------------------------------------------------------------*/


 return(center);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/fcstGroup_zoom.c,v $";
 static char rcs_id2[] = "$Id: fcstGroup_zoom.c,v 1.1 1995/09/08 14:55:22 page Exp $";}
/*  ===================================================  */

}
