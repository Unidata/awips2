#include "gageqc_defs.h"
#include "gageqc_gui.h"

extern int group_edit;
Widget group_dialog;
extern Widget maxmin_widget;
extern Widget gpbutton;
extern Widget pcpn_widget;
extern unsigned long cmap [ NUM_COLORMAP_LEVELS ];
extern int func[];
int group_qual=8;


void change_group_qual(Widget w,XtPointer data,XtPointer junk)
{
	group_qual=func[(int)data];
}


void change_pcpn_zoom_mode ( Widget w,
                             XtPointer data,
                             XtPointer call_data )
{

Cardinal argcount;
Arg args[10];
Widget rowcol,pbutton;
char *st[5];
int i;

group_edit=1;
               
argcount=0;
XtSetArg(args[argcount],XmNdeleteResponse,XmDO_NOTHING);argcount++;
XtSetArg(args[argcount],XmNautoUnmanage,False);argcount++;
XtSetArg(args[argcount],XmNwidth, 300);argcount++;
XtSetArg(args[argcount],XmNheight,200);argcount++;

if((int)data==0)
group_dialog=XmCreateMessageDialog(pcpn_widget,"Group edit Stations",args,argcount);

if((int)data==1)
group_dialog=XmCreateMessageDialog(maxmin_widget,"Group edit Stations",args,argcount);

XtUnmanageChild(XmMessageBoxGetChild(group_dialog,XmDIALOG_CANCEL_BUTTON));
XtUnmanageChild(XmMessageBoxGetChild(group_dialog,XmDIALOG_HELP_BUTTON));
XtUnmanageChild(XmMessageBoxGetChild(group_dialog,XmDIALOG_SEPARATOR));

if((int)data==0)
   XtAddCallback(group_dialog,XmNokCallback,(XtCallbackProc)apply_group,NULL);
   
if((int)data==1)
   XtAddCallback(group_dialog,XmNokCallback,(XtCallbackProc)apply_tgroup,NULL);

argcount=0;
XtSetArg(args[argcount],XmNpacking,XmPACK_COLUMN); argcount++;
XtSetArg(args[argcount],XmNnumColumns,2);argcount++;
XtSetArg(args[argcount],XmNorientation,XmVERTICAL);argcount++;
              
rowcol=XmCreateRadioBox(group_dialog,"Edit Stations",args,argcount);

st[0]="Verified";
st[1]="Screened (Force)";
st[2]="Questionable";
st[3]="Bad";

for(i=0;i<4;i++) {

                 argcount=0;
                 XtSetArg(args[argcount],XmNselectColor,cmap[4]);argcount++;

                 if(func[i]==group_qual)
                    XtSetArg(args[argcount],XmNset,True);

                 else
                    XtSetArg(args[argcount],XmNset,False);

                 argcount++;

                 pbutton=XmCreateToggleButton(rowcol,st[i],args,argcount);
                 XtAddCallback(pbutton,XmNvalueChangedCallback,change_group_qual,
                   (XtPointer)(i));

                 XtManageChild(pbutton);

                 }
       
XtManageChild(rowcol);
XtManageChild(group_dialog);
XtSetSensitive(gpbutton,False);

}
