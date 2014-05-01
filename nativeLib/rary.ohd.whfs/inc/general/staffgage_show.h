

#ifndef staff_gage_show_h
#define staff_gage_show_h


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "MotifWidgets.h"
#include "Location.h"
#include "Crest.h"
#include "Riverstat.h"
#include "Floodcat.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"

#include "staffgage.h"
#include "Xtools.h"
#include "hv_util.h"


#define DEFAULT_MAX -9999
#define DEFAULT_MIN 9999
#define SCALE_ROUNDING 5
/*
   main structure typedef
*/

typedef struct _StaffGageDialog
{
   /*
       graphics
   */
   GC gc;
   Display *display;
   Window window;
   Screen *screen;
   Pixmap pixmap;
   
   
   /*
   Drawing area dimensions.
   */
   Dimension	upperBorder;
   Dimension	lowerBorder;
   Dimension	centerHeight;
   Dimension	centerWidth;
   
   
   /*
   Display attributes.
   */
   XFontStruct	*finfo;
   
   Widget drawingArea;
   Dimension daHeight;
   Dimension daWidth;
   
   
   
   /*  
   reference
   */
   char lid[LOC_ID_LEN+1];
   char name[LOC_NAME_LEN+1];
   char basin[RIV_BAS_LEN+1];
   char stream[STREAM_NAME_LEN+1];
   
   char county[COUNTY_LEN+1];
   char state[STATE_LEN+1];
   
   double lat;
   double lon;
   
   double elevation;
   
   char tidalEffects[TIDAL_EFF_LEN+1];
   
   double riverMile;
   
   
   /*
   stages
   */    
   
   double minStage;
   double maxStage;
   
   double floodStage;
   double floodFlow;
   double bankfullStage;
   double actionStage;
   double actionFlow;
   
   double zeroDatum;
   
   double minorStage;
   double moderateStage;
   double majorStage;
   double minorFlow;
   double moderateFlow;
   double majorFlow;

   double recordStage;
   double recordFlow;
   char recordCrestDate[20];
  
     
} StaffGageDialog;




StaffGageDialog *  getStaffGageDialog(void);
void loadStaffGage(StaffGageDialog *dialog, char *lid);
void loadStaffGageWidgets(StaffGageDialog *dialog);
void drawStaffGage(StaffGageDialog *dialog);
void showStaffGage(Widget parent, char *lid);



void staffGageAddCallbacks(StaffGageDialog *dialog);
void staffGageCloseCallback(Widget w, XtPointer ptr, XtPointer cbs );
void staffGageRedrawCallback(Widget w, XtPointer ptr, XtPointer cbs );


void	draw_mainarea(StaffGageDialog *dialog);
void	river_draw(Widget widget, XtPointer ptr, XtPointer cbs);




/*
	Drawing prototypes.
*/
void	draw_background(StaffGageDialog *dialog);
void	draw_mainarea(StaffGageDialog *dialog);
void	draw_missing(StaffGageDialog *dialog);
void	draw_brackets(StaffGageDialog *dialog);
void	draw_labels(StaffGageDialog *dialog);
void	draw_stagelabels(StaffGageDialog *dialog,
			 float stage,
			 char *stagetype,
			 char leftright);
void	draw_staff(StaffGageDialog *dialog);
void	draw_stage(StaffGageDialog *dialog, float stage);
void	draw_elev(StaffGageDialog *dialog, float stage);


/*
	Utility prototypes.	
*/
float	calc_max_stage(StaffGageDialog *dialog);
float	calc_min_stage(StaffGageDialog *dialog);


#endif
