/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/colors.c              */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_colors()                   */
/*                                      read_color_list()                  */
/*                                      save_user_colors()                 */
/*                                      save_system_colors()               */
/*                                      cancel_colors()                    */
/*                                      change_color()                     */
/*                                      select_color_to_change()           */
/*                                      color_select()                     */
/*                                      end_colors()                       */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <sys/stat.h>
#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"
#include "drawa.h"

/*~~~GLOBAL VARIABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

static Widget   list;
static int      color_change;
static Widget   widchg;
static char    *oldcolors[26];

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   display_colors()                                      */
/*       FUNCTION:   create color preferences shell                        */
/***************************************************************************

Function type:
   void

Called by function:


Functions called:
   get_pixel_by_name
   (callback) save_user_colors
   (callback) end_colors
   (callback) fill_pixmap
   (callback) cancel_colors
   (callback) color_select
   (callback) change_color
   (callback) select_color_to_change

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   shell - Widget structure;
   form - Widget structure;
   missingb - Widget structure;
   level0b - Widget structure;
   level1b - Widget structure;
   level2b - Widget structure;
   level3b - Widget structure;
   level4b - Widget structure;
   level5b - Widget structure;
   level6b - Widget structure;
   level7b - Widget structure;
   level8b - Widget structure;
   level9b - Widget structure;
   level10b - Widget structure;
   level11b - Widget structure;
   level12b - Widget structure;
   level13b - Widget structure;
   level14b - Widget structure;
   level15b - Widget structure;
   missinga - Widget structure;
   level0a - Widget structure;
   level1a - Widget structure;
   level2a - Widget structure;
   level3a - Widget structure;
   level4a - Widget structure;
   level5a - Widget structure;
   level6a - Widget structure;
   level7a - Widget structure;
   level8a - Widget structure;
   level9a - Widget structure;
   level10a - Widget structure;
   level11a - Widget structure;
   level12a - Widget structure;
   level13a - Widget structure;
   level14a - Widget structure;
   level15a - Widget structure;
   stateb - Widget structure;
   countyb - Widget structure;
   cityb - Widget structure;
   riverb - Widget structure;
   basinb - Widget structure;
   rfcb - Widget structure;
   gageb - Widget structure;
   ringb - Widget structure;
   statea - Widget structure;
   countya - Widget structure;
   citya - Widget structure;
   rivera - Widget structure;
   basina - Widget structure;
   rfca - Widget structure;
   gagea - Widget structure;
   ringa - Widget structure;
   saveu - Widget structure;
   saved - Widget structure;
   cancel - Widget structure;
   i - integer;
   ncolors - integer;
   fg - integer;
   bg - integer;
   n - integer; index (incrementor) for wargs array
   wargs - stack deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned 15
   width - Dimension structure;
   cname - stack double-deref (array) character; dimensioned 600 by 20;
   xmstr - deref XmString structure;
   colorfile - deref FILE structure;
   colorfile_name - stack deref (array) character; dimensioned 120;

******************************************** BEGIN display_colors **********/

void display_colors(widget_struct)
	the_widget_struct    *widget_struct;
{

	Widget       shell, form, level_rc, overlay_rc, rc;
	Widget       missingb, level0b, level1b, level2b, level3b, level4b;
	Widget       level5b, level6b, level7b, level8b, level9b, level10b;
	Widget       level11b, level12b, level13b, level14b, level15b ;
	Widget       missinga, level0a, level1a, level2a, level3a, level4a;
	Widget       level5a, level6a, level7a, level8a, level9a, level10a;
	Widget       level11a, level12a, level13a, level14a, level15a;
	Widget       stateb, countyb, cityb, riverb, basinb, rfcb, gageb, ringb;
	Widget       statea, countya, citya, rivera, basina, rfca, gagea, ringa;
	Widget       saveu, saved, cancel;
	int          i, n, ncolors;
	int          fg, bg;
	Arg          wargs[15];
	Dimension    width;
	char         cname[600][20];
	XmString    *xmstr;
	FILE        *colorfile;
	char         colorfile_name[80];
        int          len, len2;


 /*--------------------------------------------------------------*/
 /*     save current color settings                              */
 /*--------------------------------------------------------------*/

 for(i=0; i<25; i++)
    {
    oldcolors[i] = (char *) malloc(15*sizeof(char));
    strcpy(oldcolors[i], color_list[i]);
    }
 widchg = NULL;

 /*--------------------------------------------------------------*/
 /*     create shell for color preferences display               */
 /*--------------------------------------------------------------*/

 shell = XtCreatePopupShell("color_shell",
	 transientShellWidgetClass, global_toplevel, NULL, 0);

 form       = XtVaCreateManagedWidget("color_form", xmBulletinBoardWidgetClass, shell, NULL);

 rc         = XtVaCreateManagedWidget("color_level_rc", xmRowColumnWidgetClass, form,
				     XmNorientation, XmVERTICAL,
				     XmNpacking,     XmPACK_COLUMN,
				     XmNnumColumns,  3,
				     NULL);

 level_rc   = XtVaCreateManagedWidget("color_level_rc", xmRowColumnWidgetClass, rc,
				     XmNorientation, XmVERTICAL,
				     XmNpacking,     XmPACK_COLUMN,
				     XmNnumColumns,  2,
				     NULL);

 overlay_rc = XtVaCreateManagedWidget("color_overlay_rc", xmRowColumnWidgetClass, rc,
				     XmNorientation, XmVERTICAL,
				     XmNpacking,     XmPACK_COLUMN,
				     XmNnumColumns,  2,
				     NULL);


 /*--------------------------------------------------------------*/
 /*     set background color as color level 1                    */
 /*--------------------------------------------------------------*/

 bg = get_pixel_by_name(shell, color_list[1]);

 /*--------------------------------------------------------------*/
 /*     create list of push button widgets for data levels       */
 /*--------------------------------------------------------------*/

 missingb = XtVaCreateManagedWidget("Missing",  xmPushButtonWidgetClass, level_rc, NULL);
 level0b  = XtVaCreateManagedWidget("Level 0",  xmPushButtonWidgetClass, level_rc, NULL);
 level1b  = XtVaCreateManagedWidget("Level 1",  xmPushButtonWidgetClass, level_rc, NULL);
 level2b  = XtVaCreateManagedWidget("Level 2",  xmPushButtonWidgetClass, level_rc, NULL);
 level3b  = XtVaCreateManagedWidget("Level 3",  xmPushButtonWidgetClass, level_rc, NULL);
 level4b  = XtVaCreateManagedWidget("Level 4",  xmPushButtonWidgetClass, level_rc, NULL);
 level5b  = XtVaCreateManagedWidget("Level 5",  xmPushButtonWidgetClass, level_rc, NULL);
 level6b  = XtVaCreateManagedWidget("Level 6",  xmPushButtonWidgetClass, level_rc, NULL);
 level7b  = XtVaCreateManagedWidget("Level 7",  xmPushButtonWidgetClass, level_rc, NULL);
 level8b  = XtVaCreateManagedWidget("Level 8",  xmPushButtonWidgetClass, level_rc, NULL);
 level9b  = XtVaCreateManagedWidget("Level 9",  xmPushButtonWidgetClass, level_rc, NULL);
 level10b = XtVaCreateManagedWidget("Level 10", xmPushButtonWidgetClass, level_rc, NULL);
 level11b = XtVaCreateManagedWidget("Level 11", xmPushButtonWidgetClass, level_rc, NULL);
 level12b = XtVaCreateManagedWidget("Level 12", xmPushButtonWidgetClass, level_rc, NULL);
 level13b = XtVaCreateManagedWidget("Level 13", xmPushButtonWidgetClass, level_rc, NULL);
 level14b = XtVaCreateManagedWidget("Level 14", xmPushButtonWidgetClass, level_rc, NULL);
 level15b = XtVaCreateManagedWidget("Level 15", xmPushButtonWidgetClass, level_rc, NULL);

 /*--------------------------------------------------------------*/
 /*     create label widgets to display current color names      */
 /*--------------------------------------------------------------*/

  fg = get_pixel_by_name(form, color_list[0]);
  missinga = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[0],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[1]);
  level0a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[1],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[2]);
  level1a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[2],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[3]);
  level2a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[3],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[4]);
  level3a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[4],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[5]);
  level4a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[5],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[6]);
  level5a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[6],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[7]);
  level6a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[7],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[8]);
  level7a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[8],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[9]);
  level8a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[9],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[10]);
  level9a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[10],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[11]);
  level10a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[11],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[12]);
  level11a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[12],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[13]);
  level12a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[13],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[14]);
  level13a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[14],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[15]);
  level14a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[15],XmSTRING_DEFAULT_CHARSET),
				    NULL);

  fg = get_pixel_by_name(form, color_list[16]);
  level15a = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, level_rc,
				    XmNbackground, bg,
				    XmNforeground, fg,
				    XmNlabelString, XmStringCreate(color_list[16],XmSTRING_DEFAULT_CHARSET),
				    NULL);


 /*--------------------------------------------------------------*/
 /*     create push buttons for overlay colors                   */
 /*--------------------------------------------------------------*/

  stateb  = XtVaCreateManagedWidget("States",          xmPushButtonWidgetClass, overlay_rc, NULL);
  countyb = XtVaCreateManagedWidget("Counties",        xmPushButtonWidgetClass, overlay_rc, NULL);
  cityb   = XtVaCreateManagedWidget("Cities",          xmPushButtonWidgetClass, overlay_rc, NULL);
  riverb  = XtVaCreateManagedWidget("Rivers",          xmPushButtonWidgetClass, overlay_rc, NULL);
  basinb  = XtVaCreateManagedWidget("Basins",          xmPushButtonWidgetClass, overlay_rc, NULL);
  rfcb    = XtVaCreateManagedWidget("RFC border",      xmPushButtonWidgetClass, overlay_rc, NULL);
  gageb   = XtVaCreateManagedWidget("Forecast Points", xmPushButtonWidgetClass, overlay_rc, NULL);
  ringb   = XtVaCreateManagedWidget("Forecast Group",  xmPushButtonWidgetClass, overlay_rc, NULL);

 /*--------------------------------------------------------------*/
 /*     create label widgets to display current overlay colors   */
 /*--------------------------------------------------------------*/

  fg = get_pixel_by_name(form, color_list[17]);
  statea = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				  XmNbackground, bg,
				  XmNforeground, fg,
				  XmNlabelString, XmStringCreate(color_list[17],XmSTRING_DEFAULT_CHARSET),
				  NULL);

  fg = get_pixel_by_name(form, color_list[24]);
  countya = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				   XmNbackground, bg,
				   XmNforeground, fg,
				   XmNlabelString, XmStringCreate(color_list[24],XmSTRING_DEFAULT_CHARSET),
				   NULL);

  fg = get_pixel_by_name(form, color_list[21]);
  citya = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				 XmNbackground, bg,
				 XmNforeground, fg,
				 XmNlabelString, XmStringCreate(color_list[21],XmSTRING_DEFAULT_CHARSET),
				 NULL);

  fg = get_pixel_by_name(form, color_list[19]);
  rivera = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				  XmNbackground, bg,
				  XmNforeground, fg,
				  XmNlabelString, XmStringCreate(color_list[19],XmSTRING_DEFAULT_CHARSET),
				  NULL);

  fg = get_pixel_by_name(form, color_list[18]);
  basina = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				  XmNbackground, bg,
				  XmNforeground, fg,
				  XmNlabelString, XmStringCreate(color_list[18],XmSTRING_DEFAULT_CHARSET),
				  NULL);

  fg = get_pixel_by_name(form, color_list[23]);
  rfca = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				XmNbackground, bg,
				XmNforeground, fg,
				XmNlabelString, XmStringCreate(color_list[23],XmSTRING_DEFAULT_CHARSET),
				NULL);

  fg = get_pixel_by_name(form, color_list[22]);
  gagea = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				 XmNbackground, bg,
				 XmNforeground, fg,
				 XmNlabelString, XmStringCreate(color_list[22],XmSTRING_DEFAULT_CHARSET),
				 NULL);

  fg = get_pixel_by_name(form, color_list[20]);
  ringa = XtVaCreateManagedWidget("colorarea", xmLabelWidgetClass, overlay_rc,
				 XmNbackground, bg,
				 XmNforeground, fg,
				 XmNlabelString, XmStringCreate(color_list[20],XmSTRING_DEFAULT_CHARSET),
				 NULL);


 /*--------------------------------------------------------------*/
 /*     create button to save as user defined colors             */
 /*--------------------------------------------------------------*/

  saveu = XtVaCreateManagedWidget("User", xmPushButtonWidgetClass, form, 
                                  XmNlabelString, XmStringCreate("User",XmSTRING_DEFAULT_CHARSET),
                                  NULL);
  XtAddCallback(saveu, XmNactivateCallback, save_user_colors, NULL);
  XtAddCallback(saveu, XmNactivateCallback, end_colors, shell);
  XtAddCallback(saveu, XmNactivateCallback, fill_pixmap, widget_struct);

 /*--------------------------------------------------------------*/
 /*     create button to save colors as new system defaults      */
 /*--------------------------------------------------------------*/

  saved = XtVaCreateManagedWidget("System", xmPushButtonWidgetClass, form, 
                                  XmNlabelString, XmStringCreate("System",XmSTRING_DEFAULT_CHARSET),
                                  NULL);
  XtAddCallback(saved, XmNactivateCallback, save_system_colors, NULL);
  XtAddCallback(saved, XmNactivateCallback, end_colors, shell);
  XtAddCallback(saved, XmNactivateCallback, fill_pixmap, widget_struct);

 /*--------------------------------------------------------------*/
 /*     create button to cancel color change process             */
 /*     without making any changes                               */
 /*--------------------------------------------------------------*/

  cancel = XtVaCreateManagedWidget("Cancel", xmPushButtonWidgetClass, form, 
                                   XmNlabelString, XmStringCreate("Cancel",XmSTRING_DEFAULT_CHARSET),
                                   NULL);
  XtAddCallback(cancel, XmNactivateCallback, cancel_colors, oldcolors);
  XtAddCallback(cancel, XmNactivateCallback, end_colors, shell);

 /*--------------------------------------------------------------*/
 /*     get map_overlays directory name  then                    */
 /*     get list of allowable color names                        */
 /*--------------------------------------------------------------*/

 memset(colorfile_name, '\0', 80);

 /* call routine to get the colors directory path */
 len = strlen("ifp_colors_dir");
 get_apps_defaults("ifp_colors_dir", &len, colorfile_name, &len2);

 strcat(colorfile_name, "/color_names");

 if( (colorfile = fopen(colorfile_name, "r") ) != NULL )
 {
    i=0;
    for (;;)
       {
       n = fscanf(colorfile,"%s",cname[i]);
       if (n==EOF) break;
       i++;
       }
    ncolors = i;
    fclose(colorfile);
 }
 else 
    printf("Problem opening %s \n", colorfile_name);
 
 /*--------------------------------------------------------------*/
 /*     create scrolloed window to display color names list      */
 /*--------------------------------------------------------------*/

 xmstr = (XmString *) XtMalloc(sizeof(XmString) * ncolors - 1);
 for(i = 0; i < ncolors; i++)
    xmstr[i] = XmStringCreate(cname[i],XmSTRING_DEFAULT_CHARSET);

 /*--------------------------------------------------------------*/
 /*     create list widget to display color names                */
 /*--------------------------------------------------------------*/


 n=0;
 XtSetArg(wargs[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
 XtSetArg(wargs[n], XmNheight, 300 ); n++;
 XtSetArg(wargs[n], XmNwidth, 200 ); n++;
 XtSetArg(wargs[n], XmNitems, xmstr); n++;
 XtSetArg(wargs[n], XmNitemCount, ncolors ); n++;
 XtSetArg(wargs[n], XmNvisibleItemCount, ncolors ); n++;
 XtSetArg(wargs[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
 list = XmCreateScrolledList(rc,"colorlist", wargs, n);
 XtAddCallback(list, XmNsingleSelectionCallback, color_select, NULL);

 /*--------------------------------------------------------------*/
 /*     create callbacks for each push button widget             */
 /*--------------------------------------------------------------*/

 XtAddCallback(missingb, XmNactivateCallback, change_color, missinga);
 XtAddCallback(level0b,  XmNactivateCallback, change_color, level0a);
 XtAddCallback(level1b,  XmNactivateCallback, change_color, level1a);
 XtAddCallback(level2b,  XmNactivateCallback, change_color, level2a);
 XtAddCallback(level3b,  XmNactivateCallback, change_color, level3a);
 XtAddCallback(level4b,  XmNactivateCallback, change_color, level4a);
 XtAddCallback(level5b,  XmNactivateCallback, change_color, level5a);
 XtAddCallback(level6b,  XmNactivateCallback, change_color, level6a);
 XtAddCallback(level7b,  XmNactivateCallback, change_color, level7a);
 XtAddCallback(level8b,  XmNactivateCallback, change_color, level8a);
 XtAddCallback(level9b,  XmNactivateCallback, change_color, level9a);
 XtAddCallback(level10b, XmNactivateCallback, change_color, level10a);
 XtAddCallback(level11b, XmNactivateCallback, change_color, level11a);
 XtAddCallback(level12b, XmNactivateCallback, change_color, level12a);
 XtAddCallback(level13b, XmNactivateCallback, change_color, level13a);
 XtAddCallback(level14b, XmNactivateCallback, change_color, level14a);
 XtAddCallback(level15b, XmNactivateCallback, change_color, level15a);

 XtAddCallback(stateb,  XmNactivateCallback, change_color, statea);
 XtAddCallback(countyb, XmNactivateCallback, change_color, countya);
 XtAddCallback(cityb,   XmNactivateCallback, change_color, citya);
 XtAddCallback(riverb,  XmNactivateCallback, change_color, rivera);
 XtAddCallback(basinb,  XmNactivateCallback, change_color, basina);
 XtAddCallback(rfcb,    XmNactivateCallback, change_color, rfca);
 XtAddCallback(gageb,   XmNactivateCallback, change_color, gagea);
 XtAddCallback(ringb,   XmNactivateCallback, change_color, ringa);

 XtAddCallback(missingb, XmNactivateCallback, select_color_to_change, (XtPointer) 0);
 XtAddCallback(level0b,  XmNactivateCallback, select_color_to_change, (XtPointer) 1);
 XtAddCallback(level1b,  XmNactivateCallback, select_color_to_change, (XtPointer)2);
 XtAddCallback(level2b,  XmNactivateCallback, select_color_to_change, (XtPointer)3);
 XtAddCallback(level3b,  XmNactivateCallback, select_color_to_change, (XtPointer)4);
 XtAddCallback(level4b,  XmNactivateCallback, select_color_to_change, (XtPointer)5);
 XtAddCallback(level5b,  XmNactivateCallback, select_color_to_change, (XtPointer)6);
 XtAddCallback(level6b,  XmNactivateCallback, select_color_to_change, (XtPointer)7);
 XtAddCallback(level7b,  XmNactivateCallback, select_color_to_change, (XtPointer)8);
 XtAddCallback(level8b,  XmNactivateCallback, select_color_to_change, (XtPointer)9);
 XtAddCallback(level9b,  XmNactivateCallback, select_color_to_change, (XtPointer)10);
 XtAddCallback(level10b, XmNactivateCallback, select_color_to_change, (XtPointer)11);
 XtAddCallback(level11b, XmNactivateCallback, select_color_to_change, (XtPointer)12);
 XtAddCallback(level12b, XmNactivateCallback, select_color_to_change, (XtPointer)13);
 XtAddCallback(level13b, XmNactivateCallback, select_color_to_change, (XtPointer)14);
 XtAddCallback(level14b, XmNactivateCallback, select_color_to_change, (XtPointer)15);
 XtAddCallback(level15b, XmNactivateCallback, select_color_to_change, (XtPointer)16);

 XtAddCallback(stateb,  XmNactivateCallback, select_color_to_change, (XtPointer)17);
 XtAddCallback(countyb, XmNactivateCallback, select_color_to_change, (XtPointer)24);
 XtAddCallback(cityb,   XmNactivateCallback, select_color_to_change, (XtPointer)21);
 XtAddCallback(riverb,  XmNactivateCallback, select_color_to_change, (XtPointer)19);
 XtAddCallback(basinb,  XmNactivateCallback, select_color_to_change, (XtPointer)18);
 XtAddCallback(rfcb,    XmNactivateCallback, select_color_to_change, (XtPointer)23);
 XtAddCallback(gageb,   XmNactivateCallback, select_color_to_change, (XtPointer)22);
 XtAddCallback(ringb,   XmNactivateCallback, select_color_to_change, (XtPointer)20);


 /*--------------------------------------------------------------*/
 /*     popup color window                                       */
 /*--------------------------------------------------------------*/

 XtPopup(shell,XtGrabNone);

}

/********************************************* END display_colors **********/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   read_color_list()                                     */
/*       FUNCTION:   read list of current color names                      */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   none

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   i - integer;
   k - integer;
   colorfile - deref FILE structure;
   buf - stat structure;
   filename - stack deref (array) character; dimensioned 80;

******************************************** BEGIN read_color_list *********/

void read_color_list()
{
   int          i, k, len, len2;
   FILE        *colorfile;
   struct stat  buf;
   char         filename[120];

 /*--------------------------------------------------------------*/
 /*     check to see if user has defined personal color values   */
 /*     if not use default set of colors                         */
 /*--------------------------------------------------------------*/

 memset(filename, '\0', 120);

 strcpy(filename, (char *)getenv("HOME"));
 strcat(filename, "/.ifp_files/ifp_map_user_colors");
 i = stat(filename,&buf);
 if (i == -1)
 {
    memset(filename, '\0', 120);

    /* call routine to get the colors directory path */
    len = strlen("ifp_colors_dir");
    get_apps_defaults("ifp_colors_dir", &len, filename, &len2);

    strcat(filename, "/ifp_map_user_colors");
  }

 /*--------------------------------------------------------------*/
 /*     read colors                                              */
 /*--------------------------------------------------------------*/

 if( (colorfile = fopen(filename,"r") ) != NULL)
 {
    for (i=0;i<25;i++)
    {
       color_list[i] = (char *) malloc(15*sizeof(char));
       k = fscanf(colorfile,"%s",color_list[i]);
       if (k==EOF) break;
    }
    fclose(colorfile);
 }
 else
    printf("Problem opening %s \n", filename );

}

/********************************************* END read_color_list *********/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   save_user_colors()                                    */
/*       FUNCTION:   save list of current color names to user's home dir   */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   get_pixel_by_name

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   w - Widget structure;
   data - deref caddr_t structure;
   call_data - deref caddr_t structure;
   filename - stack deref (array) character; dimensioned 40;
   i - integer;
   file - deref FILE structure;
   mask - integer
   gcv - XGCValues structure;

******************************************** BEGIN save_user_colors ********/

void save_user_colors(w, data, call_data)
   Widget       w;
   caddr_t     *data;
   caddr_t     *call_data;
{
   char         filename[40];
   int          i;
   FILE        *file;
   int          mask = GCForeground;
   XGCValues    gcv;

 /*--------------------------------------------------------------*/
 /*     create file in home directory                            */
 /*--------------------------------------------------------------*/

 strcpy(filename, (char *)getenv("HOME"));
 strcat(filename, "/.ifp_files/ifp_map_user_colors");
 file = fopen(filename, "w");
 for (i=0; i<25; i++)
    fprintf(file, "%s\n", color_list[i]);
 fclose(file);

 /*--------------------------------------------------------------*/
 /*     re-create graphics context based on color changes        */
 /*--------------------------------------------------------------*/

 for (i=0; i<rad_data->num_levels; i++)
    {
    gcv.foreground = get_pixel_by_name(rad_data->w, color_list[i]);
    rad_data->gc[i] = XCreateGC(XtDisplay(rad_data->w),
		     DefaultRootWindow(XtDisplay(rad_data->w)), mask, &gcv);
    }
}

/********************************************* END save_user_colors ********/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   save_system_colors()                                  */
/*       FUNCTION:   save list of current color names to default directory */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   get_pixel_by_name

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   w - Widget structure;
   data - deref caddr_t structure;
   call_data - deref caddr_t structure;
   filename - stack deref (array) character; dimensioned 120;
   i - integer;
   file - deref FILE structure;
   mask - integer
   gcv - XGCValues structure;

******************************************** BEGIN save_system_colors ******/

void save_system_colors(w, data, call_data)
   Widget       w;
   caddr_t     *data;
   caddr_t     *call_data;
{
   char         filename[120];
   int          i;
   FILE        *file;
   int          mask = GCForeground;
   XGCValues    gcv;
   int          len, len2;

 memset(filename, '\0', 120);

 /* call routine to get the colors directory path */
 len = strlen("ifp_colors_dir");
 get_apps_defaults("ifp_colors_dir", &len, filename, &len2);

 strcat(filename, "/ifp_map_user_colors");

 file = fopen(filename,"w");
 for (i=0; i<25; i++)
    fprintf(file, "%s\n", color_list[i]);

 fclose(file);
 for (i=0; i<rad_data->num_levels; i++)
    {
    gcv.foreground = get_pixel_by_name(rad_data->w, color_list[i]);
    rad_data->gc[i] = XCreateGC(XtDisplay(rad_data->w),
		     DefaultRootWindow(XtDisplay(rad_data->w)), mask, &gcv);
    }
}

/********************************************* END save_system_colors ******/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   cancel_colors()                                       */
/*       FUNCTION:   cancel color change process and reset colors to those */
/*                      originally set                                     */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   none

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   w - Widget structure;
   oldcolors - stack double-deref (array) character; dimensioned 26 (stack);
   call_data - caddr_t structure;
   i - integer;

******************************************** BEGIN cancel_colors ***********/

void cancel_colors(w, oldcolors, call_data)
   Widget       w;
   char        *oldcolors[26];
   caddr_t      call_data;
{
   int          i;

 for (i=0; i<25; i++)
    strcpy(color_list[i], oldcolors[i]);
}

/********************************************* END cancel_colors ***********/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   change_color()                                        */
/*       FUNCTION:   select widget to change and highlight selected button */
/*                      and unhighlight previously selected button         */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   none

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Static variables:
   previous - Widget structure;

Local variables:
   w - Widget structure;
   w2 - Widget structure;
   call_data - deref caddr_t structure;
   n - integer; index (incrementor) for wargs array
   fg - integer;
   bg - integer;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned 10

******************************************** BEGIN change_color ************/

void change_color(w, w2, call_data)
   Widget               w, w2;
   caddr_t             *call_data;
{
   int                  n, fg, bg;
   Arg                  wargs[10];
   static Widget        previous;

 /*--------------------------------------------------------------*/
 /*     if this is first button selected, the list widget with   */
 /*     allowable color names needs to be displayed              */
 /*--------------------------------------------------------------*/

 if (widchg == NULL)
    {
    XtManageChild(list);
    }

 /*--------------------------------------------------------------*/
 /*     reset colors of previously selected button               */
 /*--------------------------------------------------------------*/

 else
    {
    n=0;
    XtSetArg(wargs[n], XtNbackground, &bg); n++;
    XtSetArg(wargs[n], XtNforeground, &fg); n++;
    XtGetValues(previous, wargs, n);
    n=0;
    XtSetArg(wargs[n], XtNforeground, bg); n++;
    XtSetArg(wargs[n], XtNbackground, fg); n++;
    XtSetValues(previous, wargs, n);
    }

 /*--------------------------------------------------------------*/
 /*     higlight currently selected button                       */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XtNbackground, &bg); n++;
 XtSetArg(wargs[n], XtNforeground, &fg); n++;
 XtGetValues(w, wargs, n);

 n=0;
 XtSetArg(wargs[n], XtNforeground, bg); n++;
 XtSetArg(wargs[n], XtNbackground, fg); n++;
 XtSetValues(w, wargs, n);

 /*--------------------------------------------------------------*/
 /*     save info on button selected                             */
 /*--------------------------------------------------------------*/

 previous = w;
 widchg = w2;
}

/********************************************* END change_color ************/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   select_color_to_change()                              */
/*       FUNCTION:   set number of color in color list to change based     */
/*                      upon button that has been selected                 */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   none

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   w - Widget structure;
   num - integer;
   call_data - deref caddr_t structure;

******************************************** BEGIN select_color_to_change **/

void select_color_to_change(w, num, call_data)
   Widget       w;
   int          num;
   caddr_t     *call_data;
{
 color_change = num;
}

/********************************************* END select_color_to_change **/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   color_select()                                        */
/*       FUNCTION:   determine which color has been selected to replace    */
/*                      the current color setting and change colors        */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   get_pixel_by_name

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   w - Widget structure;
   client_data - deref integer;
   call_data - deref XmListCallbackStruct structure;
   text - deref character;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned 10
   n - integer; index (incrementor) for wargs array
   fg - integer;

******************************************** BEGIN color_select ************/

void color_select(w, client_data, call_data)
   Widget                w;
   int                  *client_data;
   XmListCallbackStruct *call_data;
{
   char                 *text;
   Arg                   wargs[10];
   int                   n, fg;

 /*--------------------------------------------------------------*/
 /*     determine color name selected from list                  */
 /*--------------------------------------------------------------*/

 XmStringGetLtoR(call_data->item, XmSTRING_DEFAULT_CHARSET, &text);

 /*--------------------------------------------------------------*/
 /*     modify color in user color list                         */
 /*--------------------------------------------------------------*/

 strcpy(color_list[color_change], text);

 /*--------------------------------------------------------------*/
 /*     modify color in label widget to account for change       */
 /*--------------------------------------------------------------*/

 fg = get_pixel_by_name(w, color_list[color_change]);
 n=0;
 XtSetArg(wargs[n], XtNforeground, fg); n++;
 XtSetArg(wargs[n],XmNlabelString,
       XmStringCreate(color_list[color_change],XmSTRING_DEFAULT_CHARSET));
 n++;
 XtSetValues(widchg, wargs, n);
}

/********************************************* END color_select ************/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/colors.c                                 */
/*  FUNCTION NAME:   end_colors()                                          */
/*       FUNCTION:   end color editing process by destroying shell         */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) display_colors

Functions called:
   none

Global variables:
   list - static Widget structure
   color_change - static integer
   widchg - static Widget structure
   oldcolors - static stack double-deref (array) character; dimensioned 26;

Local variables:
   w - Widget structure;
   shell - Widget structure;
   call_data - deref caddr_t structure;

******************************************** BEGIN end_colors **************/

void end_colors(w, shell, call_data)
   Widget       w, shell;
   caddr_t     *call_data;
{
 XtDestroyWidget(shell);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/colors.c,v $";
 static char rcs_id2[] = "$Id: colors.c,v 1.3 2006/04/07 13:29:31 aivo Exp $";}
/*  ===================================================  */

}

/********************************************* END end_colors **************/
