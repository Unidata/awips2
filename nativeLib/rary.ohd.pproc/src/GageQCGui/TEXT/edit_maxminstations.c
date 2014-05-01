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
*                   5/21/2008    B. Lawerence      Based dqcBaseIndex on / 6
*                                                  instead of % 6
********************************************************************************
*/

#include <stdlib.h>
#include <Xm/Label.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "Xtools.h"
#include "display_pdc_tsl_jni.h"

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
Widget textz, snotelwidget;
static Widget textk[6];
static int temperatureValues[6];
Widget cons_dialog;
static Widget edit_dialog = NULL;

int isave;
static Position xposition = 100;
static Position yposition = 100;
static Position xoffset;
static Position yoffset;

int reset_value;
int new_qual;
int initial_qual, initial_pos;
extern int func[];

extern struct station *tstation;
extern int max_tstations;

void time_series ();
void read_text ();

extern char message[150];

static void textFieldListner ( Widget w,
                               XtPointer clientData,
                               XtPointer callData )
{
   char * textString = NULL;

   int timePeriod = (int) clientData;

   textString = XmTextGetString ( w );

   if ( textString != NULL )
   {
      XmTextSetString ( textk[timePeriod], textString );
   }

   XtFree ( textString );
   textString = NULL;
}

void edit_maxminstations (int win_x, int win_y, unsigned int map_num)
{
   Atom atom;
   const char * pClimateSource = NULL;
   XEvent event;
   extern struct ts ts[20];
   extern char * ttimefile[][6];
   extern int tsmax;
   extern int isom;
   extern int qflag[10];
   extern struct tdata tdata[10];
   int dqcBasetime;
   int dqcBaseIndex;
   //extern unsigned long cmap[16];

   static int first = 1;

   extern void kill_widget ();

   //void tgraph_file ();
   extern int dflag[10];
   void change_tstation_location ();
   void change_tcustom_file ();
   void change_tstation_quality ();
   void reset_tstation_quality ();
   void cancel_tedit ();
   void set_snotel ();
   extern int pcpn_day, pcpn_time;
   extern int pcpn_time_step;
   Widget rowcol, rowcol3, rowcol4, pbutton, rowcol1, sep, sep2;
   Widget rowcol5; 

   Widget applyButton;
   Widget closeButton;
   Widget graphButton;

   Position xpos;
   Position ypos;

   Arg args[20];
   Cardinal argcount;
   int i;
   double testdist, maxdist;
   int x, y;
   int x1;
   int y1;
   extern int elevation_filter_value;
   extern int temperature_filter_value;
   extern int temperature_reverse_filter_value;
   XmString t;
   int display_flag, h;
   float lat, lon;
   char *st[10], buf[100], muf[10];
   int time_pos = 0, naflag, m;
   Widget graph_button;
   Widget parent_widget;
   XmString help_string;
   XmString apply_string;
   XmString close_string;
   event.xbutton.type = ButtonRelease;
   event.xbutton.x_root = 0;
   event.xbutton.y_root = 0;

   dqcBasetime = getDqcBasetime ( );
   dqcBaseIndex = dqcBasetime / 6;

   if ( edit_dialog != NULL )
   {
      XtVaGetValues ( edit_dialog, XmNx, & xposition, NULL );
      XtVaGetValues ( edit_dialog, XmNy, & yposition, NULL );
      XtPopdown ( edit_dialog );
      XtDestroyWidget (edit_dialog);
      edit_dialog = NULL;
   }

   xposition -= xoffset;
   yposition -= yoffset;

parent_widget = _get_map_shell ( );

if(pcpn_time_step==0)
                  time_pos=pcpn_time;

else if(pcpn_time_step==1)
                  time_pos=4;

else if(pcpn_time_step==2)
                  time_pos=5;


   // need to set flag to plot 

   // need to set resource in widget also 
   display_flag = 0;
   h = 0;
   x = win_x;
   y = win_y;

   isave = -1;
   maxdist = 9999;

   for (i = 0; i < max_tstations; i++)
   {

    if(tdata[pcpn_day].stn[i].tlevel2[time_pos].data ==-999 )
    {
       continue;
    }

    /* 10/11/2007 - Added logic to skip over stations which have been filtered out
       by the point filter and the point reverse filter. */
    if ( ( tdata[pcpn_day].stn[i].tlevel2[time_pos].data > temperature_reverse_filter_value ) &&
         ( tdata[pcpn_day].stn[i].tlevel2[time_pos].data < 110.0 )) 
    {
       continue;
    }

    if ((tstation[i].elev > 0) && (tstation[i].elev < elevation_filter_value ))
    {
       continue;
    }
    if (tdata[pcpn_day].stn[i].tlevel2[time_pos].data < temperature_filter_value )
    {
       continue;
    }

    lat = tstation[i].lat;
    lon = tstation[i].lon;

    for(m=0;m<tsmax;m++) {
     
           if(strncmp(&tstation[i].parm[4],&ts[m].abr[1],1)==0 && dflag[m+1] == 1)
                                   break;

                           }
                            
                           
     if(m==tsmax)
                continue;

           
     for(m=0;m<9;m++) {

             if(m==tdata[pcpn_day].stn[i].tlevel2[time_pos].qual &&
                 qflag[m]==1) 
                            break;

		       }

     if(m==9) 
              continue;


      mConvertLatLon2XY ( lat, -1 * lon, &x1, &y1 );
      testdist = pow ( (double) (win_x - (float)x1), 2) + 
                 pow ( (double) (win_y - (float)y1), 2);
      testdist = pow (testdist, .5);

      if (testdist < maxdist)
      {
	 isave = i;
	 maxdist = testdist;
      }
   }

   if (isave == -1)
   {
      return;
   }

   reset_value = 0;
initial_qual=tdata[pcpn_day].stn[isave].tlevel2[time_pos].qual;

new_qual=initial_qual;


   parent_widget = _get_map_shell();


   if (initial_qual == 6)
   {

      ErrorDialog ( parent_widget,
	            "You cannot quality control a time distributed station");
      return;

   }


   argcount = 0;
   XtSetArg (args[argcount], XmNdeleteResponse, XmDO_NOTHING);
   argcount++;
   XtSetArg (args[argcount], XmNallowShellResize, True);
   argcount++;
   XtSetArg (args[argcount], XmNx, xposition);
   argcount++;
   XtSetArg (args[argcount], XmNy, yposition);
   argcount++;

edit_dialog = XtCreatePopupShell ( "Edit Temperature Stations",
                                    xmDialogShellWidgetClass,
                                    parent_widget, args, argcount );

/* This graph button will bring up time series of the max/min/hourly temperature
   for the specified location. */
graph_button = XmMessageBoxGetChild ( edit_dialog, XmDIALOG_HELP_BUTTON );

argcount=0;
XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
rowcol=XmCreateRowColumn(edit_dialog,"Edit tstations",args,argcount);

strcpy(buf,tstation[isave].hb5);
strcat(buf,"  ");

strcat(buf,tstation[isave].parm);
t=XmStringCreateLocalized(buf);

argcount=0;
XtSetArg(args[argcount],XmNlabelString,t);argcount++;
pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
XtManageChild(pbutton);
XmStringFree(t);

strcpy(buf,tstation[isave].name);
t=XmStringCreateLocalized(buf);
argcount=0;
XtSetArg(args[argcount],XmNlabelString,t);argcount++;
pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
XtManageChild(pbutton);
XmStringFree(t);

sprintf(buf,"%d",tstation[isave].elev);
strcat(buf," ft");

t=XmStringCreateLocalized(buf);
argcount=0;
XtSetArg(args[argcount],XmNlabelString,t);argcount++;
pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
XtManageChild(pbutton);
XmStringFree(t);

if(tstation[isave].max[isom] > -99) {

   /* Get the source of the temperature climatic data */
   pClimateSource = getClimateSource ( tstation[isave].cparm );
   sprintf(buf,"monthly average high %5.1f low %5.1f source: %s",
   tstation[isave].max[isom],tstation[isave].min[isom], pClimateSource);
   t=XmStringCreateLocalized(buf);
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
   XtManageChild(pbutton);
   XmStringFree(t);

 }

if(tdata[pcpn_day].stn[isave].tlevel2[time_pos].data > -50) {

   sprintf(buf,"estimate %d ", 
       tdata[pcpn_day].stn[isave].tlevel2[time_pos].estimate);
       
   t=XmStringCreateLocalized(buf);
   argcount=0;
   XtSetArg(args[argcount],XmNlabelString,t);argcount++;
   pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
   XtManageChild(pbutton);
   XmStringFree(t);

 }

if(tdata[pcpn_day].stn[isave].tlevel2[time_pos].data < -50)
   strcpy(muf,"M");
else
   sprintf(muf,"%d",tdata[pcpn_day].stn[isave].tlevel2[time_pos].data);

argcount=0;
XtSetArg(args[argcount],XmNvalue,muf);argcount++;
textz=XmCreateTextField(rowcol,"Point QPF",args,argcount);
XtAddCallback ( textz, XmNvalueChangedCallback, textFieldListner,
                (XtPointer) time_pos);
XtManageChild(textz);

if(initial_qual != 5)      {

t=XmStringCreateLocalized("Station quality");

argcount=0;
XtSetArg(args[argcount],XmNlabelString,t);argcount++;
pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
XtManageChild(pbutton);
XmStringFree(t);

}

if(initial_qual < 0 || tdata[pcpn_day].stn[isave].tlevel2[time_pos].data < -500)
         naflag=1;

else
         naflag=0;

argcount=0;
XtSetArg(args[argcount],XmNpacking,XmPACK_COLUMN); argcount++;
XtSetArg(args[argcount],XmNnumColumns,2);argcount++;
XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
rowcol1=XmCreateRadioBox(rowcol,"Edit tstations",args,argcount);

if(initial_qual==2) {

     st[0]="Manual";
     st[1]="Reset to Original";

   for(i=0;i<2;i++) {

      argcount=0;

      if(i==0)
          XtSetArg(args[argcount],XmNset,True);

      else
          XtSetArg(args[argcount],XmNset,False);

      argcount++;

      pbutton=XmCreateToggleButton(rowcol1,st[i],args,argcount);
      XtAddCallback(pbutton,XmNvalueChangedCallback,reset_tstation_quality,
                   (XtPointer)i);

      XtManageChild(pbutton);


    }


}


else if(initial_qual != 5)      {

st[0]="Verified";
st[1]="Screened (Force)";
st[2]="Questionable";
st[3]="Bad";

for(i=0;i<4;i++) {

      argcount=0;

      if(func[i]==initial_qual && naflag != 1)
          XtSetArg(args[argcount],XmNset,True);

      else
          XtSetArg(args[argcount],XmNset,False);

      argcount++;

      pbutton=XmCreateToggleButton(rowcol1,st[i],args,argcount);
      XtAddCallback(pbutton,XmNvalueChangedCallback,change_tstation_quality,
                   (XtPointer)(i));
      XtManageChild(pbutton);

      if(naflag==1)
              XtSetSensitive(pbutton,False);

    }

}

if(tstation[isave].xadd==-1 && tstation[isave].yadd==-1)
              initial_pos=0;

else if(tstation[isave].xadd==0 && tstation[isave].yadd==-1)
              initial_pos=2;

else if(tstation[isave].xadd==-1 && tstation[isave].yadd==0)
              initial_pos=1;

else if(tstation[isave].xadd==0 && tstation[isave].yadd==0)
              initial_pos=3;
if(initial_qual != 5 && initial_qual != 4) {
 
sep=XmCreateSeparator(rowcol,"swp",NULL,0);

XtManageChild(sep);

}

t=XmStringCreateLocalized("Station Location");
argcount=0;
XtSetArg(args[argcount],XmNlabelString,t);argcount++;
pbutton=XmCreateLabel(rowcol,"Label",args,argcount);
XtManageChild(pbutton);
XmStringFree(t);

argcount=0;
XtSetArg(args[argcount],XmNpacking,XmPACK_COLUMN); argcount++;
XtSetArg(args[argcount],XmNnumColumns,2);argcount++;
XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
rowcol3=XmCreateRadioBox(rowcol,"Edit tstations",args,argcount);

st[0]="upper left";
st[1]="lower left";
st[2]="upper right";
st[3]="lower right";

for(i=0;i<4;i++) {

   argcount=0;

   if(i==initial_pos)
          XtSetArg(args[argcount],XmNset,True);

   else
          XtSetArg(args[argcount],XmNset,False);

   argcount++;

   pbutton=XmCreateToggleButton(rowcol3,st[i],args,argcount);
   XtAddCallback(pbutton,XmNvalueChangedCallback,change_tstation_location,
                (XtPointer)(i));
   XtManageChild(pbutton);

 }

/* Create the station consistency portion of the Edit
   Temperature window.  This is new for ob83. */
   sep2 = XmCreateSeparator (rowcol, "swp2", NULL, 0);
   XtManageChild (sep2);

   t = XmStringCreateLocalized ("Station Consistency");
   argcount = 0;
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   pbutton = XmCreateLabel (rowcol, "Label", args, argcount);
   XtManageChild (pbutton);
   XmStringFree (t);

   argcount = 0;
   XtSetArg (args[argcount], XmNpacking, XmPACK_COLUMN);
   argcount++;
   XtSetArg (args[argcount], XmNorientation, XmHORIZONTAL);
   argcount++;
   XtSetArg (args[argcount], XmNisAligned, True);
   argcount++;
   XtSetArg (args[argcount], XmNnumColumns, 6);
   argcount++;
   rowcol4 =
      XmCreateRowColumn (rowcol, "Edit Stations", args, argcount);

   for (m = 0; m < 6; m++)
   {
      argcount = 0;

      t = XmStringCreateLocalized (ttimefile[dqcBaseIndex][m]);
      XtSetArg (args[argcount], XmNlabelString, t);
      argcount++;
      pbutton = XmCreateLabel (rowcol4, "Label", args, argcount);
      XtManageChild (pbutton);
      XmStringFree (t);

      if (tdata[pcpn_day].stn[isave].tlevel2[m].data <= -99)
      {
         strcpy (muf, "M");
      }
      else
          sprintf (muf, "%d",
                    tdata[pcpn_day].stn[isave].tlevel2[m].data);


      temperatureValues[m]=tdata[pcpn_day].stn[isave].tlevel2[m].data;

      XtSetArg (args[argcount], XmNvalue, muf);
      argcount++; 

      textk[m] =
          XmCreateTextField (rowcol4, "Point Temperature", args, argcount);
      XtManageChild (textk[m]);

   }
   /* Create the Apply, Close, and Graph buttons. */
   argcount = 0;
   XtSetArg (args[argcount], XmNpacking, XmPACK_COLUMN);
   argcount++;
   XtSetArg (args[argcount], XmNorientation, XmVERTICAL);
   argcount++;
   XtSetArg (args[argcount], XmNnumColumns, 3);
   argcount++;
   XtSetArg (args[argcount], XmNspacing, 50);
   argcount++;
   XtSetArg (args[argcount], XmNadjustLast, False );
   argcount++;
   XtSetArg (args[argcount], XmNmarginWidth, 30 );
   argcount++; 
   XtSetArg (args[argcount], XmNmarginHeight, 30 );
   argcount++;
   rowcol5 = XmCreateRowColumn (rowcol, "Buttons", args, argcount);

   argcount = 0;
   t = XmStringCreateLocalized ("Apply");
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   XtSetArg (args[argcount], XmNwidth, 45);
   argcount++;
   XtSetArg (args[argcount], XmNrecomputeSize, False);
   argcount++;
   XtSetArg (args[argcount], XmNalignment, XmALIGNMENT_CENTER);
   argcount++;

   applyButton = XmCreatePushButton (rowcol5, "Apply Button", args, argcount);
   XmStringFree ( t );
   XtManageChild ( applyButton );

   argcount = 0;
   t = XmStringCreateLocalized ("Close");
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   XtSetArg (args[argcount], XmNwidth, 45);
   argcount++;
   XtSetArg (args[argcount], XmNrecomputeSize, False);
   argcount++;
   XtSetArg (args[argcount], XmNalignment, XmALIGNMENT_CENTER);
   argcount++;

   closeButton = XmCreatePushButton (rowcol5, "Close Button", args, argcount);
   XmStringFree ( t );
   XtManageChild ( closeButton );

   argcount = 0;
   t = XmStringCreateLocalized ("Graph");
   XtSetArg (args[argcount], XmNlabelString, t);
   argcount++;
   XtSetArg (args[argcount], XmNwidth, 45);
   argcount++;
   XtSetArg (args[argcount], XmNrecomputeSize, False);
   argcount++;
   XtSetArg (args[argcount], XmNalignment, XmALIGNMENT_CENTER);
   argcount++;

   graphButton = XmCreatePushButton (rowcol5, "Graph Button", args, argcount);
   XmStringFree ( t );
   XtManageChild ( graphButton ); 

   XtAddCallback (applyButton, XmNactivateCallback, change_tcustom_file,
                  (XtPointer) isave);

   XtAddCallback (closeButton, XmNactivateCallback, cancel_tedit,
                  (XtPointer) NULL);

   XtAddCallback (graphButton, XmNactivateCallback, tgraph_file, 
                  (XtPointer) isave);

   atom = XmInternAtom(XtDisplay(edit_dialog), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(edit_dialog, atom, cancel_tedit, NULL);

   XtManageChild(rowcol);
   XtManageChild(rowcol1);
   XtManageChild(rowcol3);
   XtManageChild(rowcol4);
   XtManageChild(rowcol5);

   if ( first == 1 )
   {
      XtVaGetValues ( edit_dialog, XmNx, & xpos, NULL );
      XtVaGetValues ( edit_dialog, XmNy, & ypos, NULL );

      yoffset = ypos - yposition;
      xoffset = xpos - xposition;
      first = 0;
   }
               
}

void change_tstation_location (Widget w, XtPointer data, XtPointer call_data)
{

if((int)data==0) {

     tstation[isave].xadd=-1;
     tstation[isave].yadd=-1;

   }

else if((int)data==2) {

     tstation[isave].xadd=0;
     tstation[isave].yadd=-1;

   }

else if((int)data==1) {

     tstation[isave].xadd=-1;
     tstation[isave].yadd=0;

   }

else if((int)data==3) {

     tstation[isave].xadd=0;
     tstation[isave].yadd=0;

   }

return;

}

void change_tcustom_file(Widget w,XtPointer data, XtPointer call_data)

{

extern Widget rowcol1;
extern int map_flag;
extern int grids_flag;
extern int points_flag;
extern int pcp_flag;
extern Widget diswidget[6];
extern struct tdata tdata[10];
extern int pcpn_time;
extern int pcpn_time_step;
extern int pcpn_day;
extern char tstation_list_custom_file[100];
int i;
extern int max_tstations;
//extern struct station tstation[1000];
FILE *fp;
Cardinal argcount;
char * pString=NULL;
Arg args[10];
int fieldValue;
int time_pos=0;
char *p;
int idif;
int ival;
char *cstr;
int k;
extern int pcp_in_use[500];
extern Widget rpbutton;

mSetCursor ( M_WATCH );

if(pcpn_time_step==0)
            time_pos=pcpn_time;

else if(pcpn_time_step==1)
            time_pos=4;
	    
else if(pcpn_time_step==2)
            time_pos=5;


fp=fopen(tstation_list_custom_file,"w");

if(fp != NULL) {
	bzero(message,150);
	sprintf(message,"Opened file: %s\n",tstation_list_custom_file);
	logMessage(message);

for(i=0;i<max_tstations;i++)
     fprintf(fp,"%s %s %d %d\n",tstation[i].hb5,tstation[i].parm,
             tstation[i].xadd,tstation[i].yadd);

fclose(fp);

}
else
{
	bzero(message,150);
	sprintf(message,"Could not open file: %s\n",tstation_list_custom_file);
	logMessage(message);
}

argcount=0;
XtSetArg(args[argcount],XmNvalue,&cstr);argcount++;              
XtGetValues(textz,args,argcount); 

ival=atoi(cstr);
p=strchr(cstr,'M');
XtFree(cstr);     
     
/* use manually entered data */

idif=fabs(ival-tdata[pcpn_day].stn[isave].tlevel2[time_pos].data);

if(idif > 1 && p == NULL && reset_value==0) {
        
        tdata[pcpn_day].stn[isave].tlevel2[time_pos].data=ival;
        tdata[pcpn_day].stn[isave].tlevel2[time_pos].qual=2;
              
      }

else {
 
      tdata[pcpn_day].stn[isave].tlevel2[time_pos].qual=new_qual;

      
}

/* Add logic to read consistency fields and update the 
   other temperature fields if necessary here. */
for (k=0; k<6; k++)
{

   if ( k != time_pos )
   {
      pString = XmTextGetString(textk[k]);

      if ( pString != NULL )
      {
         p = strchr(pString, 'M');

         if ( p != NULL )
         {
            tdata[pcpn_day].stn[isave].tlevel2[k].data=-99;
            tdata[pcpn_day].stn[isave].tlevel2[k].qual=-99;
         }
         else
         {
            fieldValue=atoi(pString); 
            idif=fabs(fieldValue-tdata[pcpn_day].stn[isave].tlevel2[k].data);

            if ( idif > 1 )
            {
              tdata[pcpn_day].stn[isave].tlevel2[k].data=fieldValue;
              tdata[pcpn_day].stn[isave].tlevel2[k].qual=2;
            } 
         }
      }

      XtFree (pString);
      pString = NULL;
   }
}

if(tdata[pcpn_day].used[time_pos]!=0)
                           tdata[pcpn_day].used[time_pos]=2;
			   
if(pcpn_time_step==0)
            time_pos=150+pcp_flag;

else if(pcpn_time_step==1)
            time_pos=190+pcpn_day;
	    
else if(pcpn_time_step==2)
            time_pos=200+pcpn_day;

pcp_in_use[time_pos]=-1;          
			   
for(k=0;k<4;k++) {
                  
             time_pos=150+pcpn_day*4+k;

             pcp_in_use[time_pos]=-1;     

             if(tdata[pcpn_day].used[k]!=0)
                           tdata[pcpn_day].used[k]=2;
   }

for(k=1;k<7;k++)
{
               XtSetSensitive(diswidget[k],False);
}
      
if(pcpn_time_step==0)
                   time_pos=150+pcp_flag;

else if(pcpn_time_step==1)
                   time_pos=190+pcpn_day;
		   
else if(pcpn_time_step==2)
                   time_pos=200+pcpn_day;
		   
if(points_flag==1 && pcp_in_use[time_pos]==-1)
                   k=0;

else if(points_flag==1 && grids_flag==-1 && map_flag==-1)
                   k=0; 
                   
else if(points_flag==-1 && grids_flag==1 && map_flag==-1)
                   k=1;   
                    
else if(points_flag==-1 && grids_flag==-1 && map_flag==1)
                   k=2;   
                     
else if(points_flag==1 && grids_flag==1 && map_flag==-1)
                   k=3;   
                        
else if(points_flag==1 && grids_flag==-1 && map_flag==1)
                   k=4;   
                          
else if(points_flag==-1 && grids_flag==-1 && map_flag==-1)
                   k=5;   
                        
XtSetArg(args[0],XmNmenuHistory,diswidget[k]);
XtSetValues(rowcol1,args,1);              

XtSetSensitive(rpbutton,True);
 
update_bad_tvalues(pcpn_day);

logMessage("estimate\n");

estimate_daily_tstations(pcpn_day, tstation, max_tstations); 
 
logMessage("qc\n");

quality_control_tstations(pcpn_day, tstation, max_tstations);

logMessage("restore\n");

restore_bad_tvalues(pcpn_day, tstation, max_tstations); 

send_expose ( );

mSetCursor (M_NORMAL);
return;

}


//static int hmflag=0;

void change_tstation_quality (Widget w, XtPointer data, XtPointer call_data)
{


   extern int pcpn_time;
   extern int pcpn_time_step;
   int time_pos;

  logMessage ("thru station_quality %d\n", (int) data);
   if (pcpn_time_step == 0)
      time_pos = pcpn_time;

   else
      time_pos = 4;


   new_qual = func[(int) data];



}

void tgraph_file (Widget w, XtPointer data, XtPointer call_data)
{

int isave;

/*strcpy(pc,tstation[isave].parm);

pc[3]='R';

if(pc[4]!='Z' && pc[4]!='M')
	   pc[5]='Z';

else {
	  
   if(pcpn_time_step==0)
                   pc[5]='Z';
   else if(pcpn_time_step==1)
                   pc[5]='X';
   else if(pcpn_time_step==2)
                   pc[5]='N';
		   
  }
  
ctime=tdata[0].data_time;  

gm=gmtime(&ctime);
*/

 /* Call Java class TimeSeriesLite from the "graph" button */
   
   isave = (int) data;
   
   char header[] = "graph_file() to launch timeserieslite for temperature";
   
  logMessage("Before startJavaVM() in %s\n", header);
   startjavavm();
  logMessage("After startJavaVM() in %s\n", header);
   
   displayTempTimeSeriesLite(isave);

}

/***************************************************
  displayTempTimeSeriesLite()
  PURPOSE call Java class TimeSeriesLite to display
  timeseries of temperature for the specified location and PE etc.
  Use Java Native Interface (JNI) to handle the function
  calling between two languages.
****************************************************/  
 void displayTempTimeSeriesLite(int isave)
{
   extern int pcpn_time_step;
   const char * jdbcUrlString = getenv("JDBCURL");
   char  stationId[10]="";
   char  stationParm[10]="";
     
   
   strcpy(stationId, tstation[isave].hb5);
   
   /* For temperature, use the shef extremum code 'X' for
   the daily maximum temperature, 'N' for the daily minimum
   temperature and 'Z' for the regular hourly temperature.
   Note if 6hr mode is selected, still display hourly temperature
   timeseries since currently pdc_pp does not generate the 6hr
   temperature timeseries, also user still can figure out 6hr
   temperature from the hourly temperature */
                  
   strcpy(stationParm, tstation[isave].parm);
      
      
   if (pcpn_time_step == 0)
      stationParm[5] = 'Z'; /* hourly temperature */
   else if (pcpn_time_step == 1)
      stationParm[5] = 'X'; /* maximum temperature */
   else
      stationParm[5] = 'N'; /* minimum temperature */     
      
   callPDCTimeSeriesLiteThroughJNI(jdbcUrlString,
                                   stationId,
				   stationParm);

   return;

}

void cancel_tedit ()
{

   XtVaGetValues ( edit_dialog, XmNx, & xposition, NULL );
   XtVaGetValues ( edit_dialog, XmNy, & yposition, NULL );
   XtPopdown (edit_dialog);
   XtDestroyWidget (edit_dialog);
   edit_dialog = NULL;

}

void reset_tstation_quality (Widget w, XtPointer data, XtPointer call_data)
{

   extern int pcpn_day;
   extern struct tdata tdata[10];
   extern int pcpn_time;
   extern int pcpn_time_step;
   int time_pos = 0;
   int k;


if(pcpn_time_step==0)
            time_pos=pcpn_time;

else if(pcpn_time_step==1)
            time_pos=4;
	    
else if(pcpn_time_step==2)
            time_pos=5;
	    
if((int)data==1) {

   for(k=0;k<6;k++) {

   tdata[pcpn_day].stn[isave].tlevel2[time_pos].qual=
   tdata[pcpn_day].stn[isave].tlevel1[time_pos].qual;
  
   tdata[pcpn_day].stn[isave].tlevel2[time_pos].data=
   tdata[pcpn_day].stn[isave].tlevel1[time_pos].data;
   
   }

   reset_value=1;
   new_qual=tdata[pcpn_day].stn[isave].tlevel1[time_pos].qual;

 }

else
    reset_value=0;



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob90/ohd/pproc_lib/src/GageQCGui/RCS/edit_maxminstations.c,v $";
 static char rcs_id2[] = "$Id: edit_maxminstations.c,v 1.8 2008/01/11 17:02:34 lawrence Exp $";}
/*  ===================================================  */

}


