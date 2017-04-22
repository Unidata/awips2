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
#include <Xm/Xm.h>
#include <stdlib.h>
#include <time.h>

#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_resource.h"
#include "mpe_log_utils.h"
#include "Xtools.h"

int dflag [ MAX_GAGEQC_DAYS ];
int edit_stations_flag = -1;
int find_station_flag = -1;
extern int tsmax;
extern struct ts ts [ 20 ];
Widget rowcol15;
Widget rswidget[3];
Widget scwidget [ NUM_CONSISTENCY_LEVELS ];

Widget pcpn_widget = NULL;

Widget gpbutton = NULL;   
extern int elevation_filter_value;
extern int flf_on;
extern int funct[];
extern int func[];
extern int gage_char[];
extern int isom;
extern int maxmin_on;
extern int qpf_on;

/* TEMPORARY!!! */
extern float pxtemp;
//extern int tsmax;
//extern struct ts ts [ 20 ];
//extern int isom;
extern time_t btim;

extern struct pdata pdata [ ];
unsigned long cmap [ NUM_COLORMAP_LEVELS ];

int        contour_flag;
extern int points_flag;
extern int grids_flag;
extern int map_flag;



int pcp_flag=0;
int pcp_in_use [ 500 ];
int pcpn_day=0;
int pcpn_time=0;
int pcpn_time_step = -1;

extern int plot_view;
extern int qflag [ ];
extern Widget diswidget [ ];
extern Widget maxmin_widget;
extern Widget rowcol1;
extern Widget rowcol10;
extern Widget rpbutton;
extern Widget z_widget;

extern int change_frz_flag;
extern int change_maxmin_flag;
extern int change_pcpn_flag;
extern int change_rpcpn_flag;
extern int change_topo_flag;

float filter_value = 0;
float reverse_filter_value=20;
extern float pxtemp;

int load_gage_data_once = 0;
extern struct _dqc_run_date dqc_run_date;
extern char *area_val_local;

Widget awidget [ MAX_GAGEQC_DAYS ];
Widget bwidget [ MAX_GAGEQC_DAYS ];

static Widget arrow1;
static Widget arrow2;

int mpe_show_missing_gage_set;

static void edit_precip_gages ( Widget w, 
                                XtPointer client_data, 
                                XtPointer call_data )
{
   
   Arg args[20];
   Cardinal argcount;
   char *s[10];
   Display * pMapDisplay = NULL;
   XmString t;
   Widget rowcol,pbutton,rowcol2,rowcol3,rowcol4;
   Widget rowcol5,rowcol7,rowcol8,rowcol9,scale,sep;

   int i,time_pos;
   int m;
   int j;
   extern int rsmode;

   extern int dcmode;
   extern int tcmode;

   Widget widg,spbutton=NULL;
   Widget tcwidget [ 3 ];
   int ival;
   Atom WM_DELETE_WINDOW;
   Widget shell;
   static XmButtonType otypes[]={XmTITLE};
   XmString olabels[1];

   Widget dcwidget [ 3 ];
   Widget top_shell=NULL;
   Widget rowcol17;
   Widget rowcol18;
   Widget tpbutton[3];

   static const char * mpe_show_missing_gage_tok =
                       "mpe_show_missing_gage";
   int reply_len;
   static int  first = 1;
   static char mpe_show_missing_gage_val[20]={'\0'};
   int length;

   /* Retrieve the map display. */
   pMapDisplay = _get_map_display ( );
   top_shell = GetTopShell ( w );

   olabels[0]=XmStringCreateLocalized("  ");

   if(pcpn_widget != NULL)
   {
      return;
   }
   
   qpf_on=1;
   
   /* If the freezing level window is displayed, destroy it. */
   if(z_widget != NULL) {
      
      XtDestroyWidget(z_widget);
      z_widget=NULL;  
      flf_on=-1;
      
   }
   
   /* If the temperature window is displayed, then destroy it. */
   if(maxmin_widget != NULL) {
      
      XtDestroyWidget(maxmin_widget);
      maxmin_widget=NULL;  
      maxmin_on=-1;
      
   }
   
   /* Set the flag to 1 indicating that precipitation stations are
      being edited. */
   edit_stations_flag=1;
   
   argcount=0;

   /* Set the title! */
   t=XmStringCreateLocalized("QC Precipitation Options");
   XtSetArg(args[argcount],XmNdialogTitle,t);argcount++;
   XtSetArg(args[argcount],XmNx,750);argcount++;
   XtSetArg(args[argcount],XmNy,750);argcount++;

   /* Retrieve the top level widget. */
   pcpn_widget=XmCreateMessageDialog(top_shell,"QC Precipitation",args,
                                     argcount);

   /* Free the String. */
   XmStringFree ( t );

   XtUnmanageChild(XmMessageBoxGetChild(pcpn_widget,XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(pcpn_widget,XmDIALOG_HELP_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(pcpn_widget,XmDIALOG_OK_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(pcpn_widget,XmDIALOG_SEPARATOR));
   
   shell=XtParent(pcpn_widget);
   WM_DELETE_WINDOW=XmInternAtom(pMapDisplay,"WM_DELETE_WINDOW",False);
   XmAddWMProtocolCallback(shell,WM_DELETE_WINDOW,change_pcpn_edit_mode,NULL);
   
   if(pcpn_time_step != 0)
      pcpn_time_step=1;
   
   argcount=0;
   XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
   rowcol=XmCreateRowColumn(pcpn_widget,"Edit Precipitation",args,argcount);
   
   t=XmStringCreateLocalized("Data options");

   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
   XtManageChild(pbutton);
   XmStringFree(t);
   
   argcount=0;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   rowcol2=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   argcount=0;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol3=XmCreateSimpleOptionMenu(rowcol2,"Edit Stations",args,argcount);
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol3,args,1);
   s[0]="6 hour";
   s[1]="24 hour";
   for(i=0;i<2;i++) {
      
      argcount=0;
      tpbutton[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,args,argcount,NULL);
      XtAddCallback(tpbutton[i],XmNactivateCallback,change_pcpn_time,
		    (XtPointer)(i+2));
   }
   XtSetArg(args[0],XmNmenuHistory,tpbutton[pcpn_time_step]);
   XtSetValues(rowcol3,args,1);
   XtManageChild(rowcol3);
   
   argcount=0;
   XtSetArg(args[argcount],XmNarrowDirection,XmARROW_UP);argcount++;
   arrow1=XmCreateArrowButton(rowcol2,"up",args,argcount);
   XtAddCallback(arrow1,XmNactivateCallback,change_pcpn_time,(XtPointer)0);
   
   argcount=0;
   XtSetArg(args[argcount],XmNarrowDirection,XmARROW_DOWN);argcount++;
   arrow2=XmCreateArrowButton(rowcol2,"down",args,argcount);
   XtAddCallback(arrow2,XmNactivateCallback,change_pcpn_time,(XtPointer)1);
   
   XtManageChild(arrow1);
   XtManageChild(arrow2);
   
   argcount=0;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol1=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   s[0]="Points";
   s[1]="Grids";
   s[2]="MAPs";
   s[3]="Points+Grids";
   s[4]="Points+MAPs";
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
	 time_pos=pcp_flag;
      
      else
	 time_pos=40+pcpn_day;
      
      if((i != 0 && i !=7) && pcp_in_use[time_pos]==-1)
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
   
   argcount=0;
   XtSetArg(args[argcount],XmNpacking,XmPACK_TIGHT); argcount++;
   XtSetArg(args[argcount],XmNnumColumns,3);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   rowcol4=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   argcount=0;
   
   rpbutton=XmCreatePushButton(rowcol4,"Render Grids+MAPs",args,argcount);  
   XtAddCallback(rpbutton,XmNactivateCallback,other_pcpn_options,
		 (XtPointer)0);
   
   XtManageChild(rpbutton);
   
   if(pcp_in_use[time_pos]==-1 && pdata[i].used[4]!=0)
      XtSetSensitive(rpbutton,True);
   else
      XtSetSensitive(rpbutton,False);
   
   argcount=0;
   
   gpbutton=XmCreatePushButton(rowcol4,"Group Edit",args,argcount);  
   XtAddCallback(gpbutton,XmNactivateCallback,change_pcpn_zoom_mode,
		 (XtPointer)0);
   
   XtManageChild(gpbutton);
   
   t=XmStringCreateLocalized("Precip type");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol15=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol15,args,1);
   
   s[0]="Rain/Snow";
   s[1]="All";
   
   for(i=0;i<2;i++) {
      
      rswidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(rswidget[i],XmNactivateCallback,change_rsmode,
		    (XtPointer)i);
   }

   /* Grey-out the Rain/Snow option.  It will be implemented in a later 
      AWIPS build */
   XtSetSensitive ( rswidget[0], False );  
   if(rsmode==0) 
      i=0;
   
   else if(rsmode==1) 
      i=1;
   
   XtSetArg(args[0],XmNmenuHistory,rswidget[i]);
   XtSetValues(rowcol15,args,1);
   
   XmStringFree(t);
   
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
   {
      s[m+1]=ts[m].name;
   }
   
   s[m+1]="ALL";
   
   for(i=0;i<tsmax+2;i++) {
      
      if(i==0)
      {
         /* Do not show the NEXRAD option.  Nexrad data are now displayed
            through the MPE portion of MPE Editor. */
         dflag[i]=-1;
         continue;
      }
      else
      {
         dflag[i]=1;
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
   
   qflag[5]=-1;

   /* get the value for token mpe_show_missing_gage */

   if (first == 1)
   {
      length = strlen (mpe_show_missing_gage_tok);
      get_apps_defaults((char *) mpe_show_missing_gage_tok, &length,
                                 mpe_show_missing_gage_val, &reply_len);

      if (reply_len > 0)
      {
         if ((strcmp(mpe_show_missing_gage_val, "All") == 0) ||
             (strcmp(mpe_show_missing_gage_val, "Reported") == 0))
            mpe_show_missing_gage_set = 1;
         else
            mpe_show_missing_gage_set = 0;

      }
      else /* set default value as not showing missing gage */
         mpe_show_missing_gage_set = 0;

      first = 1;
   }


   if ( mpe_show_missing_gage_set == 1)
      qflag[7] = 1;
   else
      qflag[7] = -1;
   
   for(i=0;i<10;i++) {
      
      j=i;
      
      argcount=0;
      
      XtSetArg(args[argcount],XmNselectColor,cmap[4]);argcount++;
      
      if(qflag[funct[i]]==1)
	 XtSetArg(args[argcount],XmNset,True);
      
      else
	 XtSetArg(args[argcount],XmNset,False);
      
      argcount++;
      awidget[i]=XmCreateToggleButton(rowcol8,s[i],args,argcount);
      XtAddCallback(awidget[i],XmNvalueChangedCallback,quality_select,
		    (XtPointer)j);
      
      XtManageChild(awidget[i]);
      
   }
   
   t=XmStringCreateLocalized("Point character");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol9=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol9,args,1);
   
   s[0]="Tip";
   s[1]="Weigh";
   s[2]="Tip+Weigh";
   
   gage_char[0]=1;
   gage_char[1]=1;
   
   for(i=0;i<3;i++) {
      
      pbutton=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(pbutton,XmNactivateCallback,change_character,
		    (XtPointer)i);
   }
   
   XtSetArg(args[0],XmNmenuHistory,pbutton);
   XtSetValues(rowcol9,args,1);
   XmStringFree(t);
   
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
   s[4]="Dev";
   
   plot_view=4;
   
   for(i=0;i<5;i++) {
      
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
   
   if(pdata[pcpn_day].stddev==5.0) 
      i=0;
   
   else if(pdata[pcpn_day].stddev==3.0) 
      i=1;
   
   else 
      i=2;
   
   XtSetArg(args[0],XmNmenuHistory,scwidget[i]);
   XtSetValues(rowcol10,args,1);
   
   XmStringFree(t);
   
   t=XmStringCreateLocalized("Point Tconsistency");
   
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol17=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol17,args,1);
   
   s[0]="Consistent";
   s[1]="Inconsistent";
   s[2]="All";
   
   for(i=0;i<3;i++) {
      
      tcwidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(tcwidget[i],XmNactivateCallback,change_tcmode,
		    (XtPointer)i);
   }
   
   if(tcmode==0) 
      i=0;
   
   else if(tcmode==1) 
      i=1;
   
   else 
      i=2;
   
   XtSetArg(args[0],XmNmenuHistory,tcwidget[i]);
   XtSetValues(rowcol17,args,1);
   
   XmStringFree(t);
   
   t=XmStringCreateLocalized("Point Sconsistency");
   
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol18=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol18,args,1);
   
   s[0]="Consistent";
   s[1]="Inconsistent";
   s[2]="All";
   
   for(i=0;i<3;i++) {
      
      dcwidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(dcwidget[i],XmNactivateCallback,change_dcmode,
		    (XtPointer)i);
   }
   
   if(dcmode==0) 
      i=0;
   
   else if(dcmode==1) 
      i=1;
   
   else 
      i=2;
   
   XtSetArg(args[0],XmNmenuHistory,dcwidget[i]);
   XtSetValues(rowcol18,args,1);
   
   XmStringFree(t);
   
   filter_value=0;
   t=XmStringCreateLocalized("Point filter (inches)");
   argcount=0;
   XtSetArg(args[argcount],XmNminimum,0);argcount++;
   XtSetArg(args[argcount],XmNmaximum,500);argcount++;
   XtSetArg(args[argcount],XmNscaleMultiple,1);argcount++;
   XtSetArg(args[argcount],XmNdecimalPoints,2);argcount++;
   XtSetArg(args[argcount],XmNshowValue,True);argcount++;
   XtSetArg(args[argcount],XmNtitleString,t);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   scale=XmCreateScale(rowcol,"scale",args,argcount);
   XtManageChild(scale);
   XtAddCallback(scale,XmNvalueChangedCallback,new_filter,NULL);
   XmStringFree(t);
   
   reverse_filter_value=20;
   t=XmStringCreateLocalized("Point reverse filter (inches)");
   argcount=0;
   XtSetArg(args[argcount],XmNminimum,0);argcount++;
   XtSetArg(args[argcount],XmNmaximum,2000);argcount++;
   XtSetArg(args[argcount],XmNscaleMultiple,1);argcount++;
   XtSetArg(args[argcount],XmNdecimalPoints,2);argcount++;
   XtSetArg(args[argcount],XmNshowValue,True);argcount++;
   XtSetArg(args[argcount],XmNtitleString,t);argcount++;
   XtSetArg(args[argcount],XmNprocessingDirection,XmMAX_ON_LEFT);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   ival=2000;
   XtSetArg(args[argcount],XmNvalue,ival);argcount++;
   scale=XmCreateScale(rowcol,"scale",args,argcount);
   XtManageChild(scale);
   XtAddCallback(scale,XmNvalueChangedCallback,new_reverse_filter,NULL);
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
   
   t=XmStringCreateLocalized("Pxtemp (deg C)");
   argcount=0;
   XtSetArg(args[argcount],XmNminimum,-100);argcount++;
   XtSetArg(args[argcount],XmNmaximum,300);argcount++;
   XtSetArg(args[argcount],XmNdecimalPoints,2);argcount++;
   XtSetArg(args[argcount],XmNshowValue,True);argcount++;
   XtSetArg(args[argcount],XmNtitleString,t);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   ival=(int)(pxtemp*100);
   XtSetArg(args[argcount],XmNvalue,ival);argcount++;
   scale=XmCreateScale(rowcol,"scale",args,argcount);
   XtManageChild(scale);
   XtAddCallback(scale,XmNvalueChangedCallback,new_dm,NULL);
   XmStringFree(t);
   
   XtManageChild(rowcol);
   XtManageChild(rowcol1);
   XtManageChild(rowcol2);
   XtManageChild(rowcol4);
   XtManageChild(rowcol5);
   XtManageChild(rowcol7);
   XtManageChild(rowcol8);
   XtManageChild(rowcol9);
   XtManageChild(rowcol10);
   XtManageChild(rowcol15);
   XtManageChild(rowcol17);
   XtManageChild(rowcol18);
   XtManageChild(pcpn_widget);
   change_pcpn_flag=1;
   change_rpcpn_flag=-1;
   change_topo_flag=-1;
   change_frz_flag=-1;
   change_maxmin_flag=-1;
   get_legend();
   
   send_expose();

   /* At this point make sure that the precip arrows are correctly
      sensitized. */
   set_precip_arrow_sensitivity ( );
   
   return;
}

void edit_precip_gages_callback ( Widget w, XtPointer ptr, XtPointer cbs )
{
   Display * pMapDisplay = NULL;
   int return_value;

   /* Flag indicating whether the user is in edit precipitation gages
    * mode. */
   extern int qpf_on;

   /* Set the text box widget. */
   set_text_box_widget ( (Widget *) ptr );

   /* Retrieve the display of the map. */
   pMapDisplay = _get_map_display ( );

   /* Load the gage data if necessary. This will in turn launch the 
      edit precipitation data. */
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
   if ( ( ( qpf_on == -1 ) &&
        ( pcpn_widget == NULL ) ) || ( load_gage_data_once == 0 )  )
   {
        mSetCursor ( M_WATCH );
        XFlush ( pMapDisplay );

	if(load_gage_data_once == 0)
	{

            /* Load the GageQC datasets.  This routine will check if there
               was a previously edited dataset.  If there was, then
               it will ask the user if he wants to QC and/or save it. */
            return_value = load_new_data_set ( w,
                                              (const char *)area_val_local,
                                              is_area_master(),
                                              dqc_run_date.dqc_data_year,
                                              dqc_run_date.dqc_data_month,
                                              dqc_run_date.dqc_data_day,
                                              edit_precip_gages);

            if ( return_value == DAILYQC_OK )
            {
	       load_gage_data_once = 1;
            }
	}
        else
        {
           /* Launch the edit preciptation gages window without
              reloading gage data. */
           edit_precip_gages ( w, ptr, cbs );
        }

        mSetCursor ( M_NORMAL );
   }
   else
   {
      /* The precip gui is already displayed and new data have not been
         requested. */
      /* Make sure the up and down time step arrows are correctly sensitized
         based on time limits. */
      set_precip_arrow_sensitivity ( );
   }

   return;
}

void get_precip_time_step_arrows ( Widget * up_arrow, Widget * down_arrow )
{
   * up_arrow =  arrow1;
   * down_arrow = arrow2;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/edit_precip_gages.c,v $";
 static char rcs_id2[] = "$Id: edit_precip_gages.c,v 1.8 2007/10/18 16:08:54 lawrence Exp $";}
/*  ===================================================  */

}
