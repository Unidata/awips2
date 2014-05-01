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

#include "map.h"
#include "map_resource.h"
#include "mpe_log_utils.h"

#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "Xtools.h"


/* Variables containing the values of the freezing level point
   and reverse point value filter slider scales. */
float freezing_filter_value = 0.00;
float freezing_reverse_filter_value = 20.00;

int frzlvl_flag=1;

extern int load_gage_data_once;
extern char* area_val_local;
extern struct _dqc_run_date dqc_run_date;

extern int flf_on;
extern int funct[];
extern int func[];
extern int gage_char[];
extern int isom;
extern int maxmin_on;
extern int qpf_on;
extern int pcp_in_use [ ];

extern int contour_flag;
extern int points_flag;
extern int grids_flag;
extern int map_flag;

extern int plot_view;
extern int qflag [ ];
extern Widget diswidget [ 6 ];
extern Widget maxmin_widget;
extern Widget rowcol1;
extern Widget rowcol10;
extern Widget rpbutton;
Widget z_widget;
extern Widget pcpn_widget;

extern int abmode;
extern int change_frz_flag;
extern int change_maxmin_flag;
extern int change_pcpn_flag;
extern int change_rpcpn_flag;
extern int change_topo_flag;

extern int pcp_flag;
extern int pcp_in_use [ ];
extern int pcpn_day;
extern int pcpn_time;
extern int pcpn_time_step;

extern float pxtemp;

static int edit_stations_flag = -1;

extern struct zdata zdata [ ];

Widget zarrow1;
Widget zarrow2;

static void edit_freezing_level_gages ( Widget w,
                                        XtPointer client_data,
                                        XtPointer call_data )
{
   Arg args[20];
   Cardinal argcount;
   char *s[10];
   Display * pMapDisplay = NULL;
   XmString t;
   Widget
      rowcol,pbutton,rowcol2,rowcol3,rowcol4,rowcol5,rowcol16,scale,sep;
   int i,time_pos;
   Widget widg,spbutton=NULL;
   Widget abwidget [ MAX_GAGEQC_DAYS ];
   Widget top_shell = NULL;
   int ival;
   Atom WM_DELETE_WINDOW;
   Widget shell;
   static XmButtonType otypes[]={XmTITLE};
   XmString olabels[1];
   olabels[0]=XmStringCreateLocalized("  ");

   /* Retrieve the map display. */
   pMapDisplay = _get_map_display ( );
   top_shell = GetTopShell ( w );
   
   if(z_widget != NULL)
      return;
   
   if(pcpn_widget != NULL) {
      
      XtDestroyWidget(pcpn_widget);  
      pcpn_widget=NULL;
      qpf_on=-1;
      
   }
   
   if(maxmin_widget != NULL) {
      
      XtDestroyWidget(maxmin_widget);  
      maxmin_widget=NULL;
      maxmin_on=-1;
      
   }
   
   flf_on=1;  
   edit_stations_flag=1;
   
   argcount=0;
 
   /* Create the title of the dialog. */
   t = XmStringCreateLocalized ( "QC Freeze Options" );
   
   XtSetArg(args[argcount],XmNdialogTitle, t);argcount++;
   z_widget=XmCreateMessageDialog(top_shell,"QC_zstations",args,argcount);

   XmStringFree ( t );
   
   XtUnmanageChild(XmMessageBoxGetChild(z_widget,XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(z_widget,XmDIALOG_HELP_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(z_widget,XmDIALOG_OK_BUTTON));
   XtUnmanageChild(XmMessageBoxGetChild(z_widget,XmDIALOG_SEPARATOR));
   
   shell=XtParent(z_widget);
   WM_DELETE_WINDOW=XmInternAtom(pMapDisplay,"WM_DELETE_WINDOW",False);
   XmAddWMProtocolCallback(shell,WM_DELETE_WINDOW,change_z_edit_mode,NULL);
   
   if(pcpn_time_step != 0) {
      
      pcpn_time_step=0;
      pcpn_time=0;
      pcp_flag=3+pcpn_day*4;
      
   }
   
   argcount=0;
   XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
   rowcol=XmCreateRowColumn(z_widget,"Edit Precipitation",args,argcount);
   
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
   
   s[0]="6 hour";
   
   for(i=0;i<1;i++) {
      
      pbutton=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      
   }
   
   
   XtSetArg(args[0],XmNmenuHistory,pbutton);
   XtSetValues(rowcol3,args,1);
   
   argcount=0;
   XtSetArg(args[argcount],XmNarrowDirection,XmARROW_UP);argcount++;
   zarrow1=XmCreateArrowButton(rowcol2,"up",args,argcount);
   XtAddCallback(zarrow1,XmNactivateCallback,change_z_time,(XtPointer)0);
   
   argcount=0;
   XtSetArg(args[argcount],XmNarrowDirection,XmARROW_DOWN);argcount++;
   zarrow2=XmCreateArrowButton(rowcol2,"down",args,argcount);
   XtAddCallback(zarrow2,XmNactivateCallback,change_z_time,(XtPointer)1);
   
   XtManageChild(zarrow1);
   XtManageChild(zarrow2);
   
   argcount=0;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol1=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   s[0]="Points";
   s[1]="Grids";
   s[2]="MAZs";
   s[3]="Points+Grids";
   s[4]="Points+MAZs";
   s[5]="Contours";
   s[6]="Points+Contours";
   s[7]="None";
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol1,args,1);
   
   for(i=0;i<8;i++) {
      
      diswidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(diswidget[i],XmNactivateCallback,display_pcpn_options,
		    (XtPointer)i);
      
      
      time_pos=100+pcp_flag;
      
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
   
   i=0;
   
   XtSetArg(args[0],XmNmenuHistory,diswidget[i]);
   XtSetValues(rowcol1,args,1);
   
   argcount=0;
   XtSetArg(args[argcount],XmNpacking,XmPACK_TIGHT); argcount++;
   XtSetArg(args[argcount],XmNnumColumns,3);argcount++;
   XtSetArg(args[argcount],XmNorientation,XmHORIZONTAL);argcount++;
   rowcol4=XmCreateRowColumn(rowcol,"Edit Stations",args,argcount);
   
   argcount=0;
   
   rpbutton=XmCreatePushButton(rowcol4,"Render Grids+MAZs",args,argcount);  
   XtAddCallback(rpbutton,XmNactivateCallback,other_pcpn_options,
		 (XtPointer)2);
   
   XtManageChild(rpbutton);
   
  logMessage("time pos %d %d %d\n",time_pos,pcp_in_use[time_pos],zdata[i].used[pcpn_time]);
   
   if(pcp_in_use[time_pos]==-1 && zdata[i].used[pcpn_time]!=0)
      XtSetSensitive(rpbutton,True);
   else
      XtSetSensitive(rpbutton,False);
   
   t=XmStringCreateLocalized("Filter Z");
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   XtSetArg(args[argcount], XmNbuttonCount, 1);argcount++;
   XtSetArg(args[argcount], XmNbuttons, olabels); argcount++;
   XtSetArg(args[argcount], XmNbuttonType, otypes); argcount++;
   rowcol16=XmCreateSimpleOptionMenu(rowcol,"Edit Stations",args,argcount);
   
   XtSetArg(args[0],XmNsubMenuId,&widg);
   XtGetValues(rowcol16,args,1);
   
   s[0]="Above";
   s[1]="Below";
   s[2]="All";
   
   for(i=0;i<3;i++) {
      
      abwidget[i]=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(abwidget[i],XmNactivateCallback,change_abmode,
		    (XtPointer)i);
   }
   
   if(abmode==0) 
      i=0;
   
   else if(abmode==1) 
      i=1;
   
   else 
      i=2;
   
   XtSetArg(args[0],XmNmenuHistory,abwidget[i]);
   XtSetValues(rowcol16,args,1);
   
   XmStringFree(t);
   
   sep=XmCreateSeparator(rowcol,"swp",NULL,0);
   XtManageChild(sep);
   
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
   
   plot_view=4;
   
   for(i=0;i<4;i++) {
      
      pbutton=XtVaCreateManagedWidget(s[i],xmPushButtonGadgetClass,widg,NULL);
      XtAddCallback(pbutton,XmNactivateCallback,change_plot,
		    (XtPointer)(i+1));
      
      if(i==3)
	 spbutton=pbutton;             
   }
   
   XtSetArg(args[0],XmNmenuHistory,spbutton);
   XtSetValues(rowcol5,args,1);

   /* Added the creation of the point value and point value reverse
      filters - 10/12/2007. */
   t=XmStringCreateLocalized("Point filter (100s ft)");
   scale = XtVaCreateManagedWidget ( "value_scale",
                                     xmScaleWidgetClass,
                                     rowcol,
                                     XmNminimum, 0,
                                     XmNmaximum, 200,
                                     XmNscaleMultiple, 1,
                                     XmNdecimalPoints, 1,
                                     XmNshowValue, True,
                                     XmNtitleString, t,
                                     XmNorientation, XmHORIZONTAL,
                                     XmNvalue, 0, NULL );
   XtAddCallback ( scale, XmNvalueChangedCallback, freezing_value_filter, NULL);
   XmStringFree(t);

   /* Added the temperature value reverse point filter, October 12, 2007. */
   t=XmStringCreateLocalized("Point reverse filter (100s ft)");
   scale = XtVaCreateManagedWidget ( "reverse_value_scale",
                                     xmScaleWidgetClass,
                                     rowcol,
                                     XmNminimum, 0,
                                     XmNmaximum, 200,
                                     XmNscaleMultiple, 1,
                                     XmNdecimalPoints, 1,
                                     XmNshowValue, True,
                                     XmNtitleString, t,
                                     XmNprocessingDirection, XmMAX_ON_LEFT,
                                     XmNorientation, XmHORIZONTAL,
                                     XmNvalue, 200, NULL );
   XtAddCallback ( scale, XmNvalueChangedCallback,freezing_reverse_value_filter, NULL);
   XmStringFree(t);
   
   t=XmStringCreateLocalized("Pxtemp (Deg C)");
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
   XtManageChild(rowcol3);
   XtManageChild(rowcol4);
   XtManageChild(rowcol5);
   XtManageChild(rowcol16);
   
   XtManageChild(z_widget);
   change_pcpn_flag=-1;
   change_rpcpn_flag=-1;
   change_topo_flag=-1;
   change_frz_flag=1;
   change_maxmin_flag=-1;
   get_legend();
   
   set_freezing_arrow_sensitivity ( );

   send_expose();
   
   return;
}

void edit_freezing_gages_callback ( Widget w, XtPointer ptr, XtPointer cbs )
{
   Display * pMapDisplay = NULL;
   int return_value;

   pMapDisplay = _get_map_display ( );

   /* Set the text box widget. */
   set_text_box_widget ( (Widget *)ptr );

   /* Flag indicating whether the user is in edit precipitation gages
    * mode. */
   extern int flf_on;

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
   if ( ( ( flf_on == -1 ) &&
        ( z_widget == NULL ) ) || ( load_gage_data_once == 0 ) )
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
                                              edit_freezing_level_gages); 

           if ( return_value == DAILYQC_OK )
           {
	      load_gage_data_once = 1;
           }
	}
        else
        {
           /* Launch the edit preciptation gages window. */
           edit_freezing_level_gages ( w, ptr, cbs );
        }

        mSetCursor ( M_NORMAL );
   }
   else
   {
      set_freezing_arrow_sensitivity ( );
   } 


   return;
}

void get_freezing_time_step_arrows ( Widget * up_arrow,
                                     Widget * down_arrow )
{
   * up_arrow = zarrow1;
   * down_arrow = zarrow2;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/GageQCGui/RCS/edit_freezing_level_gages.c,v $";
 static char rcs_id2[] = "$Id: edit_freezing_level_gages.c,v 1.7 2007/10/26 12:55:06 lawrence Exp $";}
/*  ===================================================  */

}
