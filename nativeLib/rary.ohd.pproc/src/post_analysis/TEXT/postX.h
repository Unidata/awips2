#include <Xm/Xm.h>

#include "post_stage3.h"
#include "post_drawa.h"

Widget  multi_sensor_canvas;
Widget  gageonly_canvas;
Widget  working_shell;
Widget  toplevel;
Widget  zoom_widget;
Widget  remove_apw;
Display *display;
short int   durcode;
int       iselect;
short int  **precip1, **merge, **gageonly2;//new gageonly in stage3.h are different
int      **precip24, **misbin; //new
int      icounty;
float     **ratio, **XMPTIM; //new
char     **infile, **infile1;
float     *weight;
double    *dist;
int      dbg; /*debugger specification from the command line*/
int      date_given; /*date time specification from the command line*/
char     *LOGNAME; /*user id*/
int      numarg; /*number of arguments*/
float     MINVAL;
int      MINDIST;
int      IWIND;
float     SC;
float     rhat;
int      NUMHRS;
int      ngages;	       
int      cv_duration;
char     date_form[4]; /*the date/time format for xmrg file name*/
date_struct  date_time;
gage_struct * gage;

	       
void  GetPrism();
