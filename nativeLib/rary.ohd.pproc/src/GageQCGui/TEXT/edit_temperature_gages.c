/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Separator.h>
#include <Xm/ToggleB.h>
#include <Xm/Xm.h>
#include <stdlib.h>

#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "Xtools.h"

extern int load_gage_data_once;
extern char* area_val_local;
extern struct _dqc_run_date dqc_run_date;

int dflag [ MAX_GAGEQC_DAYS ];
extern int edit_stations_flag;
extern int tsmax;
extern struct ts ts [ 20 ];
extern time_t btim;
Widget scwidget [ NUM_CONSISTENCY_LEVELS ];

int change_frz_flag = -1;
int change_maxmin_flag = -1;
int change_pcpn_flag = -1;
int change_rpcpn_flag = -1;
int change_topo_flag = -1;

int elevation_filter_value = 0;
int temperature_filter_value = -50;
int temperature_reverse_filter_value = 110;

int func[]={8,0,3,1};
extern int isom;

extern int pcp_flag;
extern int pcp_in_use [ 500 ];
extern int pcpn_day;
extern int pcpn_time;
extern int pcpn_time_step;

extern int funct[];
extern int flf_on;
extern int maxmin_on;
extern int qpf_on;

int plot_view;
int qflag [ MAX_GAGEQC_DAYS ];

extern int contour_flag;
extern int points_flag;
extern int grids_flag;
extern int map_flag;

extern struct tdata tdata [ ];

extern struct pdata pdata [ MAX_GAGEQC_DAYS ];
extern unsigned long cmap [ NUM_COLORMAP_LEVELS ];
extern Widget pcpn_widget;

Widget diswidget [ 8 ];
Widget maxmin_widget = NULL;
Widget rowcol1 = NULL;
Widget rowcol10 = NULL;
Widget rpbutton = NULL;
extern Widget z_widget;
extern int mpe_show_missing_gage_set;

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void change_maxmin_edit_mode ( Widget w, 
                               XtPointer client_data, 
		               XtPointer call_data)
{
   XtDestroyWidget(maxmin_widget);

   maxmin_widget=NULL;
   edit_stations_flag=-1;
   maxmin_on=-1;

   send_expose();
   return;
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static Widget arrow1;
static Widget arrow2;

static void set_temperature_arrow_sensitivity ( )
{
   int num_qc_days;
   extern int pcp_flag;
   extern int pcpn_time_step;

   /* Retrieve the number of days to qc. */
   num_qc_days = get_num_days_to_qc ( );

   /* 6 or 24 hour mode? */
   if ( pcpn_time_step == 0 )
   {
      if (pcp_flag + 1 >= num_qc_days * 4 )
      {
         /* Grey out the down arrow. */
         XtSetSensitive ( arrow2, False );
      }
      else
      {
         /* Make sure that the down arrow is available. */
        XtSetSensitive ( arrow2, True );
      }

      if (pcp_flag - 1  < 0)
      {
         /* Grey out the up arrow. */
         XtSetSensitive ( arrow1, False );
      }
      else
      {
        /* Make sure the up arrow is available. */
        XtSetSensitive ( arrow1, True );
      }
   }
   else
   {
      if (pcp_flag + 4 >= num_qc_days * 4)
      {
         /* Grey out the down arrow. */
         XtSetSensitive ( arrow2, False );
      }
      else
      {
         /* Make sure that the down arrow is available. */
        XtSetSensitive ( arrow2, True );
      }

      if ( pcp_flag - 4 < 0 )
      {
          /* Grey out the up arrow. */
          XtSetSensitive ( arrow1, False );
      }
      else
      {
          XtSetSensitive ( arrow1, True );
      }
   }
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
static void change_maxmin_time ( Widget w,
                                 XtPointer client_data,
                                 XtPointer call_data)
{
   int num_qc_days;
   int time_pos,i;
   Arg args[10];
   time_t tget;
   struct tm *gm;
   XmDrawingAreaCallbackStruct * pData = NULL;

   pData = ( XmDrawingAreaCallbackStruct * ) call_data;

   /* Retrieve the number of days to QC. */
   num_qc_days = get_num_days_to_qc ( );

   /* 24 hour or 6 hour time step */

   if(pcpn_time_step==0)
      time_pos=pcpn_time;

   else
      time_pos=4;

   if((int)client_data==2 && pcpn_time_step==0)
      return;

   else if((int)client_data==2 && pcpn_time_step!=0) {

      pcpn_time_step=0;
      pcp_flag=3+pcpn_day*4;


   }

   else if((int)client_data==3 && pcpn_time_step==1)
      return;

   else if((int)client_data==3 && pcpn_time_step!=1) {

      pcpn_time_step=1;


   }

   else if((int)client_data==4 && pcpn_time_step==2)
      return;

   else if((int)client_data==4 && pcpn_time_step!=2) {

      pcpn_time_step=2;


   }

   /* backward or forward */

   if((int)client_data==0) {

      if(pcpn_time_step==0)
         pcp_flag--;

      else
         pcp_flag=pcp_flag-4;

   }

   else if((int)client_data==1)  {

      if(pcpn_time_step==0)
         pcp_flag++;

      else
         pcp_flag=pcp_flag+4;

   }

   if(pcp_flag < 0)
      pcp_flag=0;

   if(pcp_flag >= num_qc_days * 4)
      pcp_flag = num_qc_days * 4-1;

   pcpn_day=pcp_flag/4;

   pcpn_time=3-(pcp_flag-pcpn_day*4);

   if(pcpn_time_step==0)
      time_pos=150+pcp_flag;

   else if(pcpn_time_step==1)
      time_pos=190+pcpn_day;

   else if(pcpn_time_step==2)
      time_pos=200+pcpn_day;

   for(i=1;i<7;i++)
      XtSetSensitive(diswidget[i],True);

   if(pcp_in_use[time_pos]==-1) {

      for(i=1;i<7;i++)
         XtSetSensitive(diswidget[i],False);

   }

   if(points_flag==1 && pcp_in_use[time_pos]==-1)
      i=0;

   else if(points_flag==1 && grids_flag==-1 && map_flag==-1 && contour_flag==-1)
      i=0;

   else if(points_flag==-1 && grids_flag==1 && map_flag==-1)

      i=1;

   else if(points_flag==-1 && grids_flag==-1 && map_flag==1)
      i=2;

   else if(points_flag==1 && grids_flag==1 && map_flag==-1)
      i=3;

   else if(points_flag==1 && grids_flag==-1 && map_flag==1)
      i=4;

   else if(points_flag==-1 && contour_flag == 1)
      i=5;
    
   else if(points_flag == 1 && contour_flag == 1)
      i=6;

   else if(points_flag==-1 && grids_flag==-1 && map_flag==-1)
      i=7;

   XtSetArg(args[0],XmNmenuHistory,diswidget[i]);
   XtSetValues(rowcol1,args,1);

   if(pdata[pcpn_day].stddev==5.0)
      i=0;

   else if(pdata[pcpn_day].stddev==3.0)
      i=1;

   else
      i=2;

   XtSetArg(args[0],XmNmenuHistory,scwidget[i]);
   XtSetValues(rowcol10,args,1);

   if(((pcp_in_use[time_pos]==-1) &&
       ((pcpn_time_step==1) && (tdata[pcpn_day].used[4]!=0))) ||
      ((pcpn_time_step==2) && (tdata[pcpn_day].used[5]!=0)) ||
      ((pcpn_time_step==0) && (tdata[pcpn_day].used[pcpn_time] != 0)))
      XtSetSensitive(rpbutton,True);

   else
      XtSetSensitive(rpbutton,False);

   tget=btim-pcpn_day*86400;

   gm=gmtime(&tget);

   isom=gm->tm_mon;

   set_temperature_arrow_sensitivity ( ); 

   send_expose();
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/


static void edit_temperature_gages ( Widget w,
                                     XtPointer client_data,
                                     XtPointer call_data )
{
   Arg args[20];
   Cardinal argcount;
   char *s[10];
   Display * pMapDisplay = NULL;
   XmString t;
   Widget rowcol,pbutton,rowcol2,rowcol3,rowcol4;
   Widget rowcol5,rowcol7,rowcol8,scale,sep;
   int i,time_pos=0;
   int m;
   int j;
   extern Widget awidget [ ];
   extern Widget bwidget [ ];
   Widget widg,spbutton=NULL;
   Atom WM_DELETE_WINDOW;
extern Widget gpbutton;
   Widget shell = NULL;
   Widget top_shell = NULL;
   Widget tpbutton[3];
   static XmButtonType otypes[]={XmTITLE};
   XmString olabels[1];

/* Retrieve the map display. */
   pMapDisplay = _get_map_display ( );
   top_shell = GetTopShell ( w );

   olabels[0]=XmStringCreateLocalized("  ");
   
   if(maxmin_widget != NULL)
      return;
   
   maxmin_on=1;
   
   if(z_widget != NULL) {
      
      XtDestroyWidget(z_widget);
      z_widget=NULL;  
      flf_on=-1;
      
   }
   
   if(pcpn_widget != NULL) {
      
      XtDestroyWidget(pcpn_widget);
      pcpn_widget=NULL;  
      qpf_on=-1;
      
   }
   
   edit_stations_flag=1;
   
   
   argcount=0;
   t = XmStringCreateLocalized ( "QC Temp Options" );
   XtSetArg(args[argcount],XmNdialogTitle,t);argcount++;

   /* Set the dialog title here. */
   maxmin_widget=XmCreateMessageDialog(top_shell,"QC_stations",args,argcount);

   XmStringFree ( t );
   t = NULL;
   
   XtUnmanageChild(XmMessageBoxGetChild(maxmin_widget,XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(maxmin_widget,XmDIALOG_HELP_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(maxmin_widget,XmDIALOG_OK_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(maxmin_widget,XmDIALOG_SEPARATOR));

   
   shell=XtParent(maxmin_widget);
   WM_DELETE_WINDOW=XmInternAtom(pMapDisplay,"WM_DELETE_WINDOW",False);
   XmAddWMProtocolCallback(shell,WM_DELETE_WINDOW,change_maxmin_edit_mode,NULL);
   
   if(pcpn_time_step==-1)
      pcpn_time_step=1;
   
   argcount=0;
   XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
   rowcol=XmCreateRowColumn(maxmin_widget,"Edit Precipitation",args,argcount);
   
   t=XmStringCreateLocalized("Data options");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
   XtManageChild(pbutton);
   XmStringFree(t);
   
   
   argcount=0;
   XtSetArg(args[argcount],XmNpacking,XmPACK_TIGHT); argcount++;
   XtSetArg(args[argcount],XmNnumColumns,4);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   rowcol2=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   argcount=0;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol3=XmCreateSimpleOptionMenu(rowcol2,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol3,args,1);        
   
   s[0]="6 hourly";
   s[1]="Maximum";
   s[2]="Minimum";
   
   for(i=0;i<3;i++) {
      
      tpbutton[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(tpbutton[i],XmNactivateCallback,change_maxmin_time,
		    (XtPointer)(i+2));
   }
   
   
   XtSetArg(args[0],XmNmenuHistory,tpbutton[pcpn_time_step]);
   XtSetValues(rowcol3,args,1);
   
   argcount=0;
   XtSetArg(args[argcount],XmNarrowDirection,XmARROW_UP);argcount++;
   arrow1=XmCreateArrowButton(rowcol2,"up",args,argcount);
   XtAddCallback(arrow1,XmNactivateCallback,change_maxmin_time,(XtPointer)0);
   
   argcount=0;
   XtSetArg(args[argcount],XmNarrowDirection,XmARROW_DOWN);argcount++;
   arrow2=XmCreateArrowButton(rowcol2,"down",args,argcount);
   XtAddCallback(arrow2,XmNactivateCallback,change_maxmin_time,(XtPointer)1);
   
   XtManageChild(arrow1);
   XtManageChild(arrow2);
   
   argcount=0;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol1=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   s[0]="Points";
   s[1]="Grids";
   s[2]="MATs";
   s[3]="Points+Grids";
   s[4]="Points+MATs";
   s[5]="Contours";
   s[6]="Points+Contours";
   s[7]="None";
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol1,args,1);
   
   for(i=0;i<8;i++) {
      
      diswidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(diswidget[i],XmNactivateCallback,display_pcpn_options,
		    (XtPointer)i);
      
      if(pcpn_time_step==0)
	 time_pos=150+pcp_flag;
      
      else if(pcpn_time_step==1)
	 time_pos=190+pcpn_day;
      
      else if(pcpn_time_step==2)
	 time_pos=190+pcpn_day;
      
      if((i != 0 && i !=7) && pcp_in_use[time_pos]==-1)
	 XtSetSensitive(diswidget[i],False);
      
  }
   
   
   
   
   if(points_flag==1 && pcp_in_use[time_pos]==-1)
      i=0;
   
   else if(points_flag==1 && grids_flag==-1 && map_flag==-1)
      i=0; 
   
   else if(points_flag==-1 && grids_flag==1 && map_flag==-1)
      i=1;   
   
   else if(points_flag==-1 && grids_flag==-1 && map_flag==1)
      i=2;   
   
   else if(points_flag==1 && grids_flag==1 && map_flag==-1)
      i=3;   
   
   else if(points_flag==1 && grids_flag==-1 && map_flag==1)
      i=4;   
   
   else if(points_flag==-1 && grids_flag==-1 && map_flag==-1)
      i=5;   
   
   XtSetArg(args[0],XmNmenuHistory,diswidget[i]);
   XtSetValues(rowcol1,args,1);
   
   argcount=0;
   XtSetArg(args[argcount],XmNpacking,XmPACK_TIGHT); argcount++;
   XtSetArg(args[argcount],XmNnumColumns,3);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   rowcol4=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   argcount=0;
   
   rpbutton=XmCreatePushButton(rowcol4,"Render Grids+MATs",args,argcount);  
   XtAddCallback(rpbutton,XmNactivateCallback,other_pcpn_options,
		 (XtPointer)3);
   
   
   XtManageChild(rpbutton);
   
   if(pcp_in_use[time_pos]==-1 && pdata[i].used[4]!=0)
      XtSetSensitive(rpbutton,True);
   else
      XtSetSensitive(rpbutton,False);
   
   argcount=0;
   
   gpbutton=XmCreatePushButton(rowcol4,"Group Edit",args,argcount);  
   XtAddCallback(gpbutton,XmNactivateCallback,change_pcpn_zoom_mode,
		 (XtPointer)1);
   
   XtManageChild(gpbutton);
   
   sep=XmCreateSeparator(rowcol,"swp",NULL,0);
   XtManageChild(sep);
   
   t=XmStringCreateLocalized("Point type");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
   XtManageChild(pbutton);
   XmStringFree(t);
   
   argcount=0;
   XtSetArg(args[argcount],XmNpacking,XmPACK_COLUMN); argcount++;
   XtSetArg(args[argcount],XmNnumColumns,3);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
   rowcol7=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   s[0]="NEXRAD";
   
   for(m=0;m<tsmax;m++) 
      s[m+1]=ts[m].name;
   
   s[m+1]="ALL";
   
   for(i=0;i<tsmax+2;i++) {
      
      if(i==0)
         dflag[i]=-1;
      else
         dflag[i]=1;
      
      if(i==0)
      {
         /* Do not show the NEXRAD option with temperature gages. */
	 continue;
      }
      
      argcount=0;
      XtSetArg(args[argcount],XmNselectColor,cmap[4]);argcount++;
      
      if(dflag[i]==1)
	 XtSetArg(args[argcount],XmNset,True);
      
      else
	 XtSetArg(args[argcount],XmNset,False);
      
      argcount++;
      
      bwidget[i]=XmCreateToggleButton(rowcol7,s[i],args,argcount);
      XtAddCallback(bwidget[i],XmNvalueChangedCallback,source_select,
		    (XtPointer)i);
      XtManageChild(bwidget[i]);
      
   }
   
   t=XmStringCreateLocalized("Point quality");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
   XtManageChild(pbutton);
   XmStringFree(t);
   
   argcount=0;
   XtSetArg(args[argcount],XmNpacking,XmPACK_COLUMN); argcount++;
   XtSetArg(args[argcount],XmNnumColumns,2);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
   rowcol8=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   s[0]="Verified";
   s[1]="Screened";
   s[2]="Time Dist";
   s[3]="Manual";
   s[4]="Questionable";
   s[5]="Partial";
   s[6]="Estimated";
   s[7]="Bad";
   s[8]="Missing";
   s[9]="All";
   
   for(i=0;i<10;i++) 
      qflag[i]=1;
   
   /* qflag[5]=-1; */

   /* display missing data or not */

   if ( mpe_show_missing_gage_set == 1)
      qflag[7] = 1;
   else
      qflag[7] = -1;
   
   for(i=0;i<10;i++) {
      
      j=i;

      /* Partial does not apply to temperatures. */
      if ( i == 5 )
      {
         continue;
      }
      
      argcount=0;
      
      XtSetArg(args[argcount],XmNselectColor,cmap[4]);argcount++;
      
      if(qflag[funct[i]]==1)
	 XtSetArg(args[argcount],XmNset,True);
      
      else
	 XtSetArg(args[argcount],XmNset,False);
      
      argcount++;
      awidget[i]=XmCreateToggleButton(rowcol8,s[i],args,argcount);
      XtAddCallback(awidget[i],XmNvalueChangedCallback,quality_select_temperature,
		    (XtPointer)j);
      
      XtManageChild(awidget[i]);
      
   }
   
   
   t=XmStringCreateLocalized("Point display");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol5=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol5,args,1);
   
   s[0]="Handbook 5";
   s[1]="PC";
   s[2]="Name";
   s[3]="Data";
   s[4]="RFS";
   s[5]="Elevation";
   
   plot_view=4;
   
   for(i=0;i<6;i++) {
      
      pbutton=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(pbutton,XmNactivateCallback,change_plot,
		    (XtPointer)(i+1));
      
      if(i==3)
	 spbutton=pbutton;             
   }
   
   XtSetArg(args[0],XmNmenuHistory,spbutton);
   XtSetValues(rowcol5,args,1);
   
   t=XmStringCreateLocalized("Point screening");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol10=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol10,args,1);
   
   s[0]="Coarse";
   s[1]="Medium";
   s[2]="Fine";
   
   for(i=0;i<3;i++) {
      
      scwidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(scwidget[i],XmNactivateCallback,screening_options,
		    (XtPointer)i);
   }
   
   if(tdata[pcpn_day].stddev==15.0) 
      i=0;
   
   else if(tdata[pcpn_day].stddev==10.0) 
      i=1;
   
   else 
      i=2;
   
   XtSetArg(args[0],XmNmenuHistory,scwidget[i]);
   XtSetValues(rowcol10,args,1);
   XmStringFree(t);

   /* Added the temperature value point filter, October 11, 2007. */
   t=XmStringCreateLocalized("Point filter (degrees F)");
   scale = XtVaCreateManagedWidget ( "value_scale",
                                     xmScaleWidgetClass,
                                     rowcol,
                                     XmNminimum, -50, 
                                     XmNmaximum, 110,
                                     XmNscaleMultiple, 1,
                                     XmNdecimalPoints, 0,
                                     XmNshowValue, True,
                                     XmNtitleString, t,
                                     XmNorientation, XmHORIZONTAL,
                                     XmNvalue, -50, NULL );
   XtAddCallback ( scale, XmNvalueChangedCallback, temperature_value_filter, NULL);
   XmStringFree(t);
    
   /* Added the temperature value reverse point filter, October 11, 2007. */
   t=XmStringCreateLocalized("Point reverse filter (degrees F)");
   scale = XtVaCreateManagedWidget ( "reverse_value_scale",
                                     xmScaleWidgetClass,
                                     rowcol,
                                     XmNminimum, -50, 
                                     XmNmaximum, 110,
                                     XmNscaleMultiple, 1,
                                     XmNdecimalPoints, 0,
                                     XmNshowValue, True,
                                     XmNtitleString, t,
                                     XmNprocessingDirection, XmMAX_ON_LEFT,
                                     XmNorientation, XmHORIZONTAL,
                                     XmNvalue, 110, NULL );
   XtAddCallback ( scale, XmNvalueChangedCallback, temperature_reverse_value_filter, NULL);
   XmStringFree(t);
    
   elevation_filter_value=0;
   t=XmStringCreateLocalized("Point elevation (feet)");
   argcount=0;
   XtSetArg(args[argcount],XmNminimum,0);argcount++;
   XtSetArg(args[argcount],XmNmaximum,15000);argcount++;
   XtSetArg(args[argcount],XmNshowValue,True);argcount++;
   XtSetArg(args[argcount],XmNtitleString,t);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   scale=XmCreateScale(rowcol,"scale",args,argcount);
   XtManageChild(scale);
   XtAddCallback(scale,XmNvalueChangedCallback,new_elevation_filter,NULL);
   XmStringFree(t);
   
   XtManageChild(rowcol);
   XtManageChild(rowcol1);
   XtManageChild(rowcol2);
   XtManageChild(rowcol3);
   XtManageChild(rowcol4);
   XtManageChild(rowcol7);
   XtManageChild(rowcol8);
   XtManageChild(rowcol5);
   XtManageChild(rowcol10);
   XtManageChild(maxmin_widget);
   change_pcpn_flag=-1;
   change_rpcpn_flag=-1;
   change_topo_flag=-1;
   change_frz_flag=-1;
   change_maxmin_flag=1;
   get_legend();
   
   send_expose();

   set_temperature_arrow_sensitivity ( );
   
   return;
   
}

void edit_temp_gages_callback ( Widget w, XtPointer ptr, XtPointer cbs )
{
   Display * pMapDisplay = NULL;
   int return_value;

   pMapDisplay = _get_map_display ( );

   /* Set the text box widget. */
   set_text_box_widget ( (Widget *) ptr);
   init_dqc_local_date_and_area_values();

   if(dqc_run_date_changed())
   {
      load_gage_data_once = 0;
   }
   if(dqc_run_area_changed())
   {
      load_gage_data_once = 0;
   }

   /* Test to determine if the precip window is already displayed. */
   if ( ( ( maxmin_on == -1 ) &&
        ( maxmin_widget == NULL ) ) || ( load_gage_data_once == 0 ) )
   {
        mSetCursor ( M_WATCH );
        XFlush ( pMapDisplay );

    	if(load_gage_data_once == 0)
        {
           return_value = load_new_data_set ( w,
                                              ( const char * ) area_val_local,
                                              is_area_master ( ),
                                              dqc_run_date.dqc_data_year,
                                              dqc_run_date.dqc_data_month,
                                              dqc_run_date.dqc_data_day,
                                              edit_temperature_gages );

           if ( return_value == DAILYQC_OK )
           {
              load_gage_data_once = 1;
           }
	}
        else
        {

           /* Launch the edit preciptation gages window. */
           edit_temperature_gages ( w, ptr, cbs );
        }

        mSetCursor ( M_NORMAL );
   }
   else
   {
      set_temperature_arrow_sensitivity ( ); 
   }

   return;
}

enum MapState isThereDQCprecipData ( )
{
   return qpf_on == 1 ? M_ON : M_OFF;
}   

enum MapState isThereDQCtempData ( )
{   
   return maxmin_on == 1 ? M_ON : M_OFF;
} 

enum MapState isThereDQCfreezingData ( )
{ 
   return flf_on == 1 ? M_ON : M_OFF;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/edit_temperature_gages.c,v $";
 static char rcs_id2[] = "$Id: edit_temperature_gages.c,v 1.9 2007/10/18 20:27:50 lawrence Exp $";}
/*  ===================================================  */

}
