/*=========================================================================*/
/*                         FILE NAME:   post_callbacks.c                   */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   quit()                             */
/*                                      quit_post_from_WM                  */
/*                                      merge_data()                       */
/*                                      zoom()                             */
/*                                      show_gage_table()                  */
/*                                      remove_ap()                        */
/*                                      close_shell()                      */
/*                                      quit_and_save()                    */
/*                                      redo_gageonly()                    */
/*                                      show_gages()                       */
/*                                      show_values()                      */
/*                                      display_single_gage()              */
/*                                      switch_sensitive                   */
/*                                      zoom_sensitive                     */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <X11/Intrinsic.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/DrawingA.h>
#include <Xm/Separator.h>
#include <Xm/MessageB.h>
#include <Xm/Scale.h>
#include <sys/utsname.h>

#include "fill_pixmap.h"
#include "postX.h"
#include "post_stage3.h"
#include "overlay.h"
#include "postanalysis_functions.h"
#include "post_stage3_globals.h"
#include <stdlib.h>
#include "post_stage3_interface.h"
#include "zoom_data_struct.h"
#include "write_xmrg.h"
#include "CurPP.h"
#include "DbmsDefs.h"
#include "read_xmrg_file.h"
#include "Swap2Bytes.h"
#include "TestByteOrder.h"
#include "TestXmrg.h"
#include "GetOS.h"
#include "version.h"

//extern char *version_number;
extern char *color_list_overlays[];


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/*  FUNCTION NAME:   quit()                                                */
/*       FUNCTION:   quit the post analysis process                        */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Quit button

Functions called:
   none

******************************************** BEGIN quit ********************/

void quit(w, data, call_data)
   Widget       w;
   caddr_t     *data;
   caddr_t     *call_data;
{
 exit(0);
}

/********************************************* END quit ********************/



/***************************************************************************/
/*  FUNCTION NAME:   quit_post_from_WM                                     */
/*       FUNCTION:   callback for exiting from post analysis from window   */
/*                   manager                                               */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) window manager button

******************************************** BEGIN quit_post_from_WM ****/

void quit_post_from_WM()
{

  exit(0);

}

/********************************************* END quit_post_from_WM ****/

/*********************************************************************/
/*  FUNCTION NAME:   merge_data()                                    */
/*       FUNCTION:   Merge the gage only field with the summed hourly*/
/*                   MPE field to create new field             */
/**********************************************************************

Function type:
   void

Called by function:
   (callback) Merge Data button

Functions called:
   MergeData
   display_merge

******************************************** BEGIN merge_data **************/

void merge_data(w, data, call_data)
   Widget           w;
   draw_struct     *data;
   caddr_t         *call_data;

{

if (dbg) printf("In merge_data\n");

 MergeData(ratio);

 display_merge();

}

/********************************************* END merge_data **************/



/***************************************************************************/
/*  FUNCTION NAME:   zoom()                                                */
/*       FUNCTION:   zoom into section of fields specified by user         */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) zoom option - called twice each time selected

Functions called:
   (callback) fill_pixmap
   (callback) copy_area
   (callback) show_states
   (callback) show_county
   (callback) show_rivers
   (callback) show_cities_and_towns
   (callback) show_basin_boundaries
   (callback) show_gages
   (callback) show_values
   set_colorvalues
   get_pixel_by_name
   fill_pixmap

******************************************** BEGIN zoom ********************/

void zoom(w, data, call_data)
   Widget         w;
   zoom_data_struct    *data;
   caddr_t        *call_data;
{
   int                  n, x_pixels_per_bin, y_pixels_per_bin;
   int                  i, j, x, y;
   Dimension           width, height;
   Display             *dpy;
   Arg                 wargs[5];
   XGCValues           gcv;
   int                  mask = GCForeground;
   static int           zoom_on=0;
   draw_struct         *draw_data;


 if (dbg)
   printf("In zoom\n");

 draw_data = (draw_struct *)malloc(sizeof(draw_struct));

 if ( draw_data == NULL )
 {
    fprintf(stderr, "malloc failed for draw_data in zoom in Post Aanalysis\n"
                    "-- program stopping\n" ) ;
    exit(0);
 }

 draw_data = data->ddata;



 /*-------------------------------------------------------------------------*/
 /*     if zoom is currently set and has been selected again                */
 /*     remove callbacks and reset display for main view                    */
 /*-------------------------------------------------------------------------*/

 if (zoom_on > 1)
 {
    XtRemoveCallback(draw_data->w, XmNresizeCallback, fill_postanalysis_pixmap, &pa_zoom_data[zoom_on-2]);
    XtRemoveCallback(draw_data->w, XmNexposeCallback, copy_area,   &pa_zoom_data[zoom_on-2]);

    XtAddCallback(draw_data->w, XmNresizeCallback, fill_postanalysis_pixmap,               draw_data);
    XtAddCallback(draw_data->w, XmNexposeCallback, copy_area,                 draw_data);
    XtAddCallback(data->statew,  XmNactivateCallback, show_states,            draw_data);
    XtAddCallback(data->countyw, XmNactivateCallback, show_county,            draw_data);
    XtAddCallback(data->riverw,  XmNactivateCallback, show_rivers,            draw_data);
    XtAddCallback(data->cityw,   XmNactivateCallback, show_cities_and_towns,  draw_data);
    XtAddCallback(data->basinw,  XmNactivateCallback, show_basin_boundaries,  draw_data);

    XtAddEventHandler(draw_data->w, ButtonPressMask,   FALSE, start_rubber_band, &rbdata);
    XtAddEventHandler(draw_data->w, ButtonMotionMask,  FALSE, track_rubber_band, &rbdata);
    XtAddEventHandler(draw_data->w, ButtonReleaseMask, FALSE, end_rubber_band,   &rbdata);
    XtAddEventHandler(draw_data->w, ButtonPressMask,   FALSE, locate_main,       draw_data);

    XtAddEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
                      zoom_widget);
    XtAddEventHandler(gageonly_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
                      zoom_widget);

    XtAddEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
                      remove_apw);

    XtRemoveEventHandler(draw_data->w, ButtonReleaseMask, FALSE, display_single_gage,
                                                       &pa_zoom_data[zoom_on-2]);

    XtSetSensitive(data->gagew,FALSE);
    XtSetSensitive(data->gagvw,FALSE);

    XClearArea(XtDisplay(w),XtWindow(draw_data->w),0,0,0,0,TRUE);

    zoom_on++;
    if (zoom_on == 4)
    {
       zoom_on = 0;
       rbdata.last_x = rbdata.start_x = 0;
       rbdata.last_y = rbdata.start_y = 0;

       XtSetSensitive(zoom_widget,FALSE);
       XtSetSensitive(remove_apw,FALSE);



       if(overlay_avail.fgbasin == 0)
       {
         XtSetSensitive(data->basinw,FALSE);
       }
       else
       {
         XtSetSensitive(data->basinw,TRUE);
       }
    }

    return;

 }

 /*-------------------------------------------------------------------------*/
 /*     remove callbacks not used by zoom                                   */
 /*-------------------------------------------------------------------------*/

 XtRemoveCallback(draw_data->w, XmNresizeCallback,   fill_postanalysis_pixmap,  draw_data);
 XtRemoveCallback(draw_data->w, XmNexposeCallback,   copy_area,    draw_data);
 XtRemoveCallback(data->statew,  XmNactivateCallback, show_states, draw_data);
 XtRemoveCallback(data->countyw, XmNactivateCallback, show_county, draw_data);
 XtRemoveCallback(data->riverw,  XmNactivateCallback, show_rivers, draw_data);
 XtRemoveCallback(data->basinw,  XmNactivateCallback, show_basin_boundaries, draw_data);
 XtRemoveCallback(data->cityw,   XmNactivateCallback, show_cities_and_towns, draw_data);
 XtRemoveCallback(data->gagew,   XmNactivateCallback, show_gages,   draw_data);
 XtRemoveCallback(data->gagvw,   XmNactivateCallback, show_values,  draw_data);

 XtRemoveEventHandler(draw_data->w, ButtonPressMask,   FALSE, start_rubber_band, &rbdata);
 XtRemoveEventHandler(draw_data->w, ButtonMotionMask,  FALSE, track_rubber_band, &rbdata);
 XtRemoveEventHandler(draw_data->w, ButtonReleaseMask, FALSE, end_rubber_band,   &rbdata);
 XtRemoveEventHandler(draw_data->w, ButtonPressMask,   FALSE, locate_main, draw_data);
 XtRemoveEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
                      zoom_widget);
 XtRemoveEventHandler(gageonly_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
                      zoom_widget);

 XtRemoveEventHandler(multi_sensor_canvas, ButtonReleaseMask, FALSE, zoom_sensitive,
                      remove_apw);

 /*-------------------------------------------------------------------------*/
 /*     get size of pixels in original display                              */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(draw_data->w, wargs, n);

 x_pixels_per_bin = (float)width/(float)draw_data->maximum_columns;
 y_pixels_per_bin = (float)height/(float)draw_data->maximum_rows;

 if (x_pixels_per_bin > y_pixels_per_bin)
	x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin)
	y_pixels_per_bin = x_pixels_per_bin;

 /*-------------------------------------------------------------------------*/
 /*     determine size of selected zoom area and malloc necessary space     */
 /*-------------------------------------------------------------------------*/

 pa_zoom_data[zoom_on].maximum_columns =
	 abs(rbdata.last_x-rbdata.start_x)/x_pixels_per_bin;
 pa_zoom_data[zoom_on].maximum_rows =
	 abs(rbdata.last_y-rbdata.start_y)/y_pixels_per_bin;

 if (dbg) printf("In zoom, columns = %d rows = %d\n",
	     pa_zoom_data[zoom_on].maximum_columns,pa_zoom_data[zoom_on].maximum_rows);

 pa_zoom_data[zoom_on].data_array =
     (short int **)malloc((pa_zoom_data[zoom_on].maximum_columns)*sizeof(short int *));
 for (i=0; i<pa_zoom_data[zoom_on].maximum_columns; i++)
     pa_zoom_data[zoom_on].data_array[i] =
       (short int *)malloc((pa_zoom_data[zoom_on].maximum_rows)*sizeof(short int));

 /*-------------------------------------------------------------------------*/
 /*     initialize zoom data structure                                      */
 /*-------------------------------------------------------------------------*/

 pa_zoom_data[zoom_on].levels = (int *)malloc(17*sizeof(int));
 pa_zoom_data[zoom_on].gc = (GC *)malloc(17*sizeof(GC));
 pa_zoom_data[zoom_on].pix = (Pixmap) NULL;
 pa_zoom_data[zoom_on].pixbase = (Pixmap) NULL;
 pa_zoom_data[zoom_on].w = draw_data->w;

 set_colorvalues(&pa_zoom_data[zoom_on]);

 x = rbdata.last_x;
 if (rbdata.start_x < x) x = rbdata.start_x;

 y = rbdata.last_y;
 if (rbdata.start_y > y) y = rbdata.start_y;

 pa_zoom_data[zoom_on].origin.x = x/x_pixels_per_bin;
 pa_zoom_data[zoom_on].origin.y = draw_data->maximum_rows - y/y_pixels_per_bin;

 /*-------------------------------------------------------------------------*/
 /*     read subset of mosaic into zoom data structure                      */
 /*-------------------------------------------------------------------------*/

 for (i=0;i<pa_zoom_data[zoom_on].maximum_columns;i++)
 {
   for (j=0;j<pa_zoom_data[zoom_on].maximum_rows;j++)
   {
     if(i+pa_zoom_data[zoom_on].origin.x >= MAXX || j+pa_zoom_data[zoom_on].origin.y >= MAXY)
     {
       pa_zoom_data[zoom_on].data_array[i][j] = -1;
     }
     else
    {
       pa_zoom_data[zoom_on].data_array[i][j] =
           draw_data->data_array[i+pa_zoom_data[zoom_on].origin.x][j+pa_zoom_data[zoom_on].origin.y];

    }
  }
 }
 if (dbg) printf("after setting pa_zoom_data[zoom_on]\n");

 /*-------------------------------------------------------------------------*/
 /*     set zoom origin                                                     */
 /*-------------------------------------------------------------------------*/

 pa_zoom_data[zoom_on].origin.x = pa_zoom_data[zoom_on].origin.x + XOR;
 pa_zoom_data[zoom_on].origin.y = pa_zoom_data[zoom_on].origin.y + YOR;

 /*-------------------------------------------------------------------------*/
 /*     set graphics context for zoom data                                  */
 /*-------------------------------------------------------------------------*/

 dpy = XtDisplay(draw_data->w);
 for (i=0; i<pa_zoom_data[zoom_on].num_levels; i++)
 {
    gcv.foreground = get_pixel_by_name(w, color_list_levels[i]);
    pa_zoom_data[zoom_on].gc[i] = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);
 }

 /*-------------------------------------------------------------------------*/
 /*     add appropriate callbacks to display using zoom data                */
 /*-------------------------------------------------------------------------*/

 XtAddCallback(draw_data->w, XmNresizeCallback,    fill_postanalysis_pixmap,           &pa_zoom_data[zoom_on]);
 XtAddCallback(draw_data->w, XmNexposeCallback,    copy_area,             &pa_zoom_data[zoom_on]);
 XtAddCallback(data->statew,  XmNactivateCallback, show_states,           &pa_zoom_data[zoom_on]);
 XtAddCallback(data->countyw, XmNactivateCallback, show_county,           &pa_zoom_data[zoom_on]);
 XtAddCallback(data->riverw,  XmNactivateCallback, show_rivers,           &pa_zoom_data[zoom_on]);
 XtAddCallback(data->cityw,   XmNactivateCallback, show_cities_and_towns, &pa_zoom_data[zoom_on]);
 XtAddCallback(data->basinw,  XmNactivateCallback, show_basin_boundaries, &pa_zoom_data[zoom_on]);

 if(ngages > 0)
 {
    XtSetSensitive(data->gagew,TRUE);
    XtSetSensitive(data->gagvw,TRUE);
 }
 XtAddCallback(data->gagew, XmNactivateCallback, show_gages,          &pa_zoom_data[zoom_on]);
 XtAddCallback(data->gagvw, XmNactivateCallback, show_values,         &pa_zoom_data[zoom_on]);
 XtAddEventHandler(draw_data->w, ButtonPressMask, FALSE, locate_main, &pa_zoom_data[zoom_on]);

/*----------------------------------------------------------------*/
/*   turn off sensitivity on remove ap option                     */
/*----------------------------------------------------------------*/

   XtSetSensitive(remove_apw,FALSE);

/*--------------------------------------------------------------------*/
/*   set sensitivity on basin overlay option                          */
/*--------------------------------------------------------------------*/

 if(overlay_avail.mapbasin == 0)
 {
   XtSetSensitive(data->basinw,FALSE);
 }
 else
 {
   XtSetSensitive(data->basinw,TRUE);
 }

 /*-------------------------------------------------------------------------*/
 /*     create pixmap with zoom data                                        */
 /*-------------------------------------------------------------------------*/

 pa_zoom_data[zoom_on].states_on = draw_data->states_on;
 pa_zoom_data[zoom_on].county_on = draw_data->county_on;
 pa_zoom_data[zoom_on].cities_on = draw_data->cities_on;
 pa_zoom_data[zoom_on].rivers_on = draw_data->rivers_on;
 pa_zoom_data[zoom_on].basins_on = draw_data->basins_on;
 fill_postanalysis_pixmap(draw_data->w, &pa_zoom_data[zoom_on], NULL);
 zoom_on++;

}

/********************************************* END zoom ********************/


/***************************************************************************/
/*  FUNCTION NAME:   show_gage_table()                                     */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Gage table option from main and merged displays

Functions called:
   create_gage_table

******************************************** BEGIN show_gage_table *********/

void show_gage_table(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
  create_gage_table(data);
}

/********************************************* END show_gage_table *********/



/***************************************************************************/
/*  FUNCTION NAME:   remove_ap()                                           */
/*       FUNCTION:   removes selected bins by setting them to missing      */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Remove AP button

Functions called:
   fill_pixmap

******************************************** BEGIN remove_ap ***************/

void remove_ap(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          i, j, n, temp, x1, x2, y1, y2;
   int          x_pixels_per_bin, y_pixels_per_bin;
   Dimension    width, height;
   Arg          wargs[10];
   static Cursor watch_cursor = (Cursor) NULL;

 if (dbg) printf("In remove ap\n");

 if (watch_cursor == (Cursor) NULL)
    watch_cursor = XCreateFontCursor(XtDisplay(data->w),XC_watch);
 XDefineCursor(XtDisplay(data->w), XtWindow(XtParent(XtParent(data->w))), watch_cursor);

 /*-------------------------------------------------------------------------*/
 /*     get size of pixels in original display                              */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 x_pixels_per_bin = (float)width/(float)data->maximum_columns;
 y_pixels_per_bin = (float)height/(float)data->maximum_rows;
 if (x_pixels_per_bin > y_pixels_per_bin)
	x_pixels_per_bin = y_pixels_per_bin;
 else if (y_pixels_per_bin > x_pixels_per_bin)
	y_pixels_per_bin = x_pixels_per_bin;

 x1 = rbdata.start_x/x_pixels_per_bin;
 x2 = rbdata.last_x /x_pixels_per_bin;
 y1 = data->maximum_rows - (rbdata.start_y/y_pixels_per_bin);
 y2 = data->maximum_rows - (rbdata.last_y/y_pixels_per_bin);

 if (dbg) printf("x1=%d x2=%d y1=%d y2=%d\n",x1,x2,y1,y2);

 if (x2 < x1)
    {
    temp=x1;
    x1 = x2;
    x2 = temp;
    }
 if (y2 < y1)
    {
    temp=y1;
    y1 = y2;
    y2 = temp;
    }

 for (i=x1; i<=x2; i++)
 for (j=y1; j<=y2; j++)
    {
      if(i < MAXX && i >= 0 && j < MAXY && j >= 0)
      {
        data->data_array[i][j] = -1;
        misbin[j][i] = 0;
      }
    }

 fill_postanalysis_pixmap(data->w, data, NULL);

 rbdata.start_x = 0;
 rbdata.last_x = 0;
 XtSetSensitive(remove_apw, FALSE);
 XtSetSensitive(zoom_widget, FALSE);

}

/********************************************* END remove_ap ***************/


/***************************************************************************/
/*  FUNCTION NAME:   close_shell()                                         */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

******************************************** BEGIN close_shell *************/

void close_shell(w, w2, call_data)
    Widget w, w2;
    caddr_t *call_data;
{
 XtDestroyWidget(w2);
}

/********************************************* END close_shell *************/



/***************************************************************************/
/*  FUNCTION NAME:   quit_and_save()                                       */
/*       FUNCTION:   save output and exit Post Analysis                    */
/***************************************************************************

Function type:
   void

Called by function:
   Save/separate (callback)
   Save/overwrite (callback)

Functions called:
   readmosaic
   writemosaic

Local variables:
   istat = status of read of previously saved xmrg file
         = 0 -- read successful
         = 1 -- error occurred attempting to read xmrg file

******************************************** BEGIN quit_and_save ***********/

void quit_and_save(w, w2, call_data)
   Widget       w;
   Widget       w2;
   caddr_t     *call_data;
{
   int           mx, my, i, j, k, l, maxxmrg, retcode = 2, len,len2,lenus,irc, lenpf;
   float        vernumf;
   char         vernumc[10];
   char         *button_name,user[11],datet[20],datetimev[20],proc_flag[9];
   static       Cursor watch_cursor = (Cursor) NULL;
   short int    **precip;
   struct tm    *t_local;
   time_t         tnow;
   char          system[6]="";
   struct utsname uts_struct;
   int           len_fname, istat, idate, first = 1, datenum;
   char          os[3] = "";
   enum TestByteResult  result = DontFlipBytes;
   OperSys        oper = OS_UNIX;
   char          *dirname;
   char          cdate[11]="", fname[30]="";

 printf("In quit and save\n");
 fprintf(stderr, "In quit and save \n");

 /*malloc space*/

 dirname = (char *)malloc(128*sizeof(char));
 infile1 = (char **)malloc(durcode*sizeof(char *));

 for (l=0; l< durcode; l++)
 {
   infile1[l] = (char *)malloc(200*sizeof(char));
 }

/*--------------------------------------------------------------*/
/*   display watch cursor                                       */
/*--------------------------------------------------------------*/

 /*if (watch_cursor == (Cursor) NULL)
    watch_cursor = XCreateFontCursor(XtDisplay(w2), XC_watch);
 XDefineCursor(XtDisplay(w2), XtWindow(w2), watch_cursor);
 XFlush(XtDisplay(w2));
*/
/*---------------------------------------------------------*/
/*   malloc space for precip array                         */
/*   array is mosaic previously saved by hourly MPE   */
/*---------------------------------------------------------*/

 precip = (short **)malloc(MAXY*sizeof(short *));
 for (i=0; i<MAXY; i++)
 {
    precip[i] = (short *)malloc(MAXX*sizeof(short));
 }

 /*get the system*/

 oper = GetOS();

/* hourly precip1 has been calculated from sum24() in st3post.c and it
is defined as global variable*/

/*--------------------------------------------------------*/
/*   check for previously saved array                     */
/*   if found, then read into precip1 array               */
/*   multiply by ratio                                    */
/*--------------------------------------------------------*/

 time(&tnow);
 t_local = gmtime(&tnow);
 strftime(datet,20,"%Y-%m-%d %H:%M:%S",t_local);

 proc_flag[8]='\0';
 strcpy(proc_flag,"MPM01   ");
 lenpf = strlen(proc_flag);

 /*-------------------------------------*/
 /*  define user field                  */
 /*  HP or LX will be in first 2 chars  */
 /*-------------------------------------*/

 uname(&uts_struct);
 memset(user, '\0', 11);
 system[5]='\0';

 strcpy(system, (char *)uts_struct.sysname);
 if(strcmp(system,"HP-UX") == 0)
    sprintf(user,"HP%s",LOGNAME);
 else
 {
    sprintf(user,"LX%s",LOGNAME);
    strcpy(os, "LX");
 }

 lenus = strlen(user);
 mx = MAXX;
 my = MAXY;
 button_name = XtName(w);

 strcpy(vernumc,post_analysis_version_number);
 vernumf = atof(vernumc);

 for (i=0; i<durcode; i++)
 {
    maxxmrg = -9999;

    datenum = i+iselect - 1;
    strcpy(datetimev,dates[datenum].dttm); /*dates is global var defined in post_dates.c*/

    len_fname = strlen(infile[i]);

   /* Test to determine the system that this file was created on. */

    TestXmrgByteOrder_ (infile[i] , & XOR , & result ) ;

    if ( result == FlipTestFailed )
    {
        printf( "\nIn function quit_and_save, the call to \"TestXmrgByteOrder_\" failed for file %s\n",infile[i]);

    }

    if (( result != FlipTestFailed) && (oper != OS_ERROR))
    {
       for (j=0; j<MAXY; j++)
       {
	  read_xmrg_file(&MAXX, &MAXY, &j, infile[i], &len_fname, &istat, precip1[j]);

	  if (istat != 0)
	  {
            printf("ERROR reading xmrg file from %s -- no file saved, program exits.\n",infile[i]);
	    exit(1);
	   /* return;*/
	  }

	   /* the file read is assumed Big Endian if Linux, then swap bytes
            misbin and prism files are delivered to sites in Big Endian format
           if running on Linux, bytes in misbin and prism files are swapped */

          if (result == FlipBytes )
          {
	       Swap2Bytes_(precip1[j], (size_t *) & MAXX);
          }


	  for (k=0; k<MAXX; k++)
	  {
            if(precip1[j][k] > maxxmrg)
	      maxxmrg = precip1[j][k];

	    if (ratio[j][k] < 0)
	      precip[j][k] = (short)(-ratio[j][k]/(float)durcode);
	    else
	      precip[j][k] = precip1[j][k] * ratio[j][k];
	  }

       }/*end of loop on j*/

       maxxmrg = (float) maxxmrg/100.; /*since the value from xmrg file is 100mm in unit*/
    }
/*--------------------------------------------------------*/
/*   determine whether save and overwrite or save         */
/*     separately option chosen                           */
/*   if save and overwrite, then overwrite files          */
/*     with new data                                      */
/*   if save separately, then save new files into         */
/*     directory defined by "mpe_post_output" token           */
/*     with same filenames                                */
/*--------------------------------------------------------*/
     if (istat == 0)
     {
       if(strcmp(button_name,"Save/separate") == 0)
       {
          /*read token*/

	 if (first == 1)
	 {
	    len2 = strlen("mpe_post_output");
            get_apps_defaults("mpe_post_output",&len2,dirname,&len2);

	    if (len2 == 0)
            {
              printf("Invalid token value for $(mpe_post_output), program exits.\n");
              exit(1);
            }

	     first = 0;
	 }

	 /*dates is global var in stage3.h and defined as date_struct *dates*/
         /*date_form is global var form token st3_date_form*/

         if ( strcmp( date_form, "mdY") == 0)
         {
            idate = dates[datenum].month * 1000000 + dates[datenum].day*10000 +
                       dates[datenum].year ;
            sprintf(cdate,"%08d%02d",idate,dates[datenum].hour);
         }
         else
            strcpy(cdate,dates[datenum].cdate);

         sprintf(fname, "xmrg%sz",cdate);
	 sprintf(infile1[i],"%s/%s",dirname,fname);
	 len=strlen(infile1[i]);

         for (j=0; j<MAXY; j++)
         {
	    WRITE_XMRG(&XOR, &YOR,&mx,&my,&j,infile1[i],&len,user,&lenus,datet,
	              proc_flag, &lenpf, precip[j],datetimev,&maxxmrg,&vernumf,&irc);

            if(irc != 0)
            {
              printf("Error writing xmrg file %s, data can't be saved\n",infile1[i]);
              retcode=1;
              break;
            }
	    else
	      printf("Write xmrg file %s\n", infile1[i]);
         }
         retcode=2;
       }
       else
       {
         len=strlen(infile[i]);

         for (j=0; j<MAXY; j++)
         {
	    WRITE_XMRG(&XOR, &YOR,&mx,&my,&j,infile[i],&len,user,&lenus,datet,
	              proc_flag, &lenpf, precip[j],datetimev,&maxxmrg,&vernumf,&irc);

	    if(irc != 0)
            {
              printf("Error writing file %s, data can't be saved\n",infile[i]);
              retcode=1;
              break;
            }
         }
         retcode=2;
       }
     }

    }  /* end for(i=0; i<durcode; i++)  */

   exit(retcode);
}

/********************************************* END quit_and_save ***********/


/***************************************************************************/
/*  FUNCTION NAME:   redo_gageonly()                                       */
/*       FUNCTION:   recalculate gage only field                           */
/*                   if any gage values have been edited, then update      */
/*                    S2GageRadarVal table                                 */
/***************************************************************************

Function type:
   void

Called by function:
   post_display
   (callback) remove_ap

Functions called:
   GageOnly
   fill_pixmap
   update_gval

******************************************** BEGIN redo_gageonly ***********/

void redo_gageonly(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int      i, j,len;
   double    newvalue;
   float newval;
   char     gidtmp[LOC_ID_LEN + 1];
   CurPP    *curcpp = NULL;
   char     where[300]="";
   char     select_date_ansi[ANSI_TIME_LEN + 1]= "";
   int      status;

 if (dbg) printf("In redo gageonly\n");

 /*get the selected date in ansi form. decide the query begin time and end time*/

 sprintf(select_date_ansi,"%d-%02d-%02d %02d:00:00",date_time.year,date_time.month,
                  date_time.day,date_time.hour);

 for(i=0;i<ngages;i++)
 {
    if (strcmp(gage[i].edit,"") != 0 && IsNull(CHAR, gage[i].edit) == NOTNULL)
    {
       if (strcmp(gage[i].edit,"M")==0 || strcmp(gage[i].edit,"m")==0)
         strcpy(gage[i].edit,"-9999.0");

       newvalue = atof(gage[i].edit); /*the edit is in unit of inch, and the
                                                                value in curpp table is in inch unit*/

       if (newvalue < 0.)
         newvalue = -9999.;


       newval = (float)newvalue;

       memset(gidtmp,'\0',LOC_ID_LEN + 1);
       len=strlen(gage[i].id);
       strncpy(gidtmp,gage[i].id,len);

       /*get the existing info from the curpp table*/

       if (durcode == 24)
          sprintf(where, " WHERE lid = '%s' AND obstime = '%s' AND (dur= 2001 or dur = 5004)",
	                  gidtmp, select_date_ansi);
       else if (durcode == 6 )
          sprintf(where, " WHERE lid = '%s' AND obstime = '%s' AND dur = 1006 ",
	                  gidtmp, select_date_ansi);

       curcpp = GetCurPP(where);

       /*if the record has been found, update the information in the CurPP table*/

       if (curcpp != NULL)
       {
	 curcpp->value = newval;
	 status = UpdateCurPP(curcpp, where);

	 if (status < 0)
	 {
	   printf("Postgres error %d in updating %s data in CurPP table\n", status, where);
	   fprintf(stderr, "Postgres error %d updating %s data in CurPP table\n", status, where);
	 }
       }

       if (curcpp != NULL)
           FreeCurPP(curcpp);

       if (newval != -9999.)
         gage[i].gval = newval* 25.4; /*in mm unit*/
       else
         gage[i].gval = -9999.;

       gage[i].manedit = 1;
       strcpy(gage[i].edit,"");
     }
 }

 GageOnly();

 for (i=0; i<data->maximum_columns; i++)
 {
   for (j=0; j<data->maximum_rows; j++)
     data->data_array[i][j] = gageonly2[j][i];
 }

 fill_postanalysis_pixmap(data->w, data, NULL);

/*-----------------------------------------------*/
/*   undefine watch cursor defined in remove_ap  */
/*   if redo_gageonly called directly, this      */
/*     statement does nothing                    */
/*-----------------------------------------------*/
 XUndefineCursor(XtDisplay(data->w), XtWindow(XtParent(XtParent(data->w))));

}

/********************************************* END redo_gageonly ***********/


/***************************************************************************/
/*  FUNCTION NAME:   show_gages()                                          */
/*       FUNCTION:   callback to display gage locations on zoom display    */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Gage Identifiers button

Functions called:
   get_pixel_by_name

******************************************** BEGIN show_gages **************/

void show_gages(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          i, j, n, xloc, yloc;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y, done;
   int          mask = GCForeground;
   XGCValues    gcv;

 if (dbg) printf("in show_gages\n");

 dpy = XtDisplay(data->w);

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 x = (float)width /(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 gcv.foreground = get_pixel_by_name(data->w, color_list_overlays[5]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 for(i=0;i<ngages;i++)
    {
    done=0;
    for (j=0; j<i; j++)
       if (strcmp(gage[i].id, gage[j].id) == 0)
	  {
	  done = 1;
	  break;
	  }
    if (done == 0)
       {
       xloc = (gage[i].hrap.x + XOR - data->origin.x)*x;
       yloc = (data->maximum_rows - (gage[i].hrap.y + YOR - data->origin.y))*y;
       XDrawString(XtDisplay(data->w), data->pix,gc, xloc, yloc, gage[i].id, strlen(gage[i].id));
       }
    }

 XtAddEventHandler(data->w, ButtonReleaseMask, FALSE, display_single_gage, data);

 if (XtIsRealized(data->w))
    XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
}

/********************************************* END show_gages **************/


/***************************************************************************/
/*  FUNCTION NAME:   show_values()                                         */
/*       FUNCTION:   callback to display gage values on zoom display       */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Gage Values button

Functions called:
   get_pixel_by_name

******************************************** BEGIN show_values **************/

void show_values(w, data, call_data)
   Widget       w;
   draw_struct *data;
   caddr_t     *call_data;
{
   int          i, j, n, xloc, yloc;
   char        *cgval;
   GC           gc;
   Display     *dpy;
   Dimension    width, height;
   Arg          wargs[5];
   int          x, y, done;
   int          mask = GCForeground;
   XGCValues    gcv;

 if (dbg) printf("in show_gages\n");

 cgval = (char *)malloc(9*sizeof(char));
 dpy = XtDisplay(data->w);

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);

 x = (float)width /(float)data->maximum_columns;
 y = (float)height/(float)data->maximum_rows;
 if (x > y)
	x = y;
 else if (y > x)
	y = x;

 gcv.foreground = get_pixel_by_name(data->w, color_list_overlays[5]);
 gc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);

 for(i=0;i<ngages;i++)
    {
    done=0;
    for (j=0; j<i; j++)
       if (strcmp(gage[i].id, gage[j].id) == 0)
	  {
	  done = 1;
	  break;
	  }
    if (done == 0)
       {
       xloc = (gage[i].hrap.x + XOR - data->origin.x)*x;
       yloc = (data->maximum_rows - (gage[i].hrap.y + YOR - data->origin.y))*y;

       if(gage[i].manedit == 0)
         sprintf(cgval,"%.2f",gage[i].gval/25.4);
       else
       {
         if(gage[i].gval == -9999.0)
           sprintf(cgval,"%.2fE",gage[i].gval);
         else
           sprintf(cgval,"%.2fE",gage[i].gval/25.4);
       }
       XDrawString(XtDisplay(data->w), data->pix,gc, xloc, yloc, cgval, strlen(cgval));
       }
    }

    XtAddEventHandler(data->w, ButtonReleaseMask, FALSE, display_single_gage, data);

 if (XtIsRealized(data->w))
    XClearArea(dpy, XtWindow(data->w), 0, 0, 0, 0, TRUE);
}

/********************************************* END show_values **************/



/***************************************************************************/
/*  FUNCTION NAME:   display_single_gage()                                 */
/*       FUNCTION:   callback to display 7x7 field popup when button       */
/*                   press occurs in zoom display                          */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) left mouse button press on zoom canvas

Functions called:
   display_gage

******************************************** BEGIN display_single_gage *****/

void display_single_gage(w, data, event)
   Widget       w;
   draw_struct *data;
   XEvent      *event;
{
   int          xloc, yloc, n, xpix, ypix, i, imin = 0;
   Dimension    width, height;
   HRAP         hrap;
   double       xdist, ydist, dist, mindist;
   Arg          wargs[5];

 if (event->xbutton.button != 1) return;
 if (dbg) printf("In display single gage\n");

 /*-------------------------------------------------------------------------*/
 /*     check location of cursor in window coordinates                      */
 /*-------------------------------------------------------------------------*/

 xloc  = event->xbutton.x;
 yloc  = event->xbutton.y;
 if (dbg) printf("In display_single_gage, cursor location:xloc = %d yloc = %d\n", xloc, yloc);

 /*-------------------------------------------------------------------------*/
 /*     check size of pixels in display                                     */
 /*-------------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNwidth, &width); n++;
 XtSetArg(wargs[n], XmNheight, &height); n++;
 XtGetValues(data->w, wargs, n);
 xpix = (float)width/(float)data->maximum_columns;
 ypix = (float)height/(float)data->maximum_rows;
 if (xpix > ypix)
	xpix = ypix;
 else if (ypix > xpix)
	ypix = xpix;

 /*-------------------------------------------------------------------------*/
 /*     determine HRAP bin of cursor location                               */
 /*-------------------------------------------------------------------------*/

 hrap.x = data->origin.x + xloc/xpix;
 hrap.y = data->origin.y + (data->maximum_rows - yloc/ypix) ;
 if (dbg) printf("In display_single_gage, HRAP bin for cursor location:hrap x = %.2f y = %.2f\n", hrap.x, hrap.y);

 /*----------------------------------------------------------*/
 /*     loop through all gages to locate nearest gage        */
 /*        to cursor location                                */
 /*----------------------------------------------------------*/

 mindist = 999.;
 for (i=0;i<ngages;i++)
    {
    xdist = hrap.x - (gage[i].hrap.x + XOR);
    ydist = hrap.y - (gage[i].hrap.y + YOR);
    dist = sqrt(xdist*xdist + ydist*ydist);
    if (dist < mindist)
       {
       imin = i;
       mindist = dist;
       }
    }

 /*-------------------------------------------------------------------------*/
 /*     if nearest gage is less than threshold distance                     */
 /*     display 7x7 field popup                                             */
 /*-------------------------------------------------------------------------*/

 if (mindist < 900. && dbg ) printf("nearest gage = %s\n", gage[imin].id);
 if (mindist < 900.) display_gage(imin, data);
}

/********************************************* END display_single_gage *****/


/********************************************************************/
/*  FUNCTION NAME:   switch_sensitive()                             */
/*       FUNCTION:   switch the sensitivity of the show precip gages*/
/*                    option in conjunction with the zoom option    */
/*                   if no gages are available, sensitivity remains */
/*                    off                                           */
/********************************************************************

Function type:
   void

Called by function:
   (callback) zoom button

Functions called:
   none

************************************* BEGIN switch_sensitive ********/

void switch_sensitive(w, w2, call_data)
   Widget       w, w2;
   caddr_t     *call_data;
{
   static int   iset = 1;

 if (iset == 0)
    {
    XtSetSensitive(w,FALSE);
    iset = 1;
    }
 else
    {
    if(ngages > 0) XtSetSensitive(w,TRUE);
    iset = 0;
    }
}

/********************************************* END switch_sensitive ********/


/********************************************************************/
/*  FUNCTION NAME:   zoom_sensitive()                               */
/*       FUNCTION:   turn the sensitivity of the button to on       */
/*                   used by zoom option and remove Ap option       */
/********************************************************************

Function type:
   void

Called by function:
   (callback)

Functions called:
   none

************************************ BEGIN zoom_sensitive **********/

void zoom_sensitive(w, w2, event)
   Widget       w, w2;
   XEvent      *event;
{
 if (rbdata.last_x == rbdata.start_x) return;

 XtSetSensitive(w2, TRUE);

}

/********************************************* END zoom_sensitive **********/
