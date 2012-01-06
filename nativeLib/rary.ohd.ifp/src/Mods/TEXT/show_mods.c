
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <sys/shm.h>
#include <sys/ipc.h>
#include <sys/types.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <string.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h>

#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>


#include "libXs.h"

#include "Mods_range_list.h"
#include "Mods_globalDefs.h"
#include "Mods_everythingStruct.h"
#include "Mods_defStruct.h"
#include "Mods_info.h"
#include "ifp_atoms.h"
#include "libXifp.h"

//int parsegment(char *, char *[]);

extern int seg2pos(char *);

void segmentSelectionCB(Widget, Mods_everythingStruct *,
			    XmPushButtonCallbackStruct *,ifp_modsShell_p );

void fgroupSelectionCB(Widget, Mods_everythingStruct *,
			    XmPushButtonCallbackStruct *,ifp_modsShell_p);

void rangeSelectionCB(Widget, Mods_everythingStruct *,
			    XmPushButtonCallbackStruct *, ifp_modsShell_p);

void range_listCancelCB(Widget w, XtPointer ptr, XtPointer cbs );

void range_listOkCB(Widget w, XtPointer ptr, XtPointer cbs );

void range_list_selectCB(Widget w, XtPointer ptr, XtPointer cbs );

void ErrorDialog(Widget widget, char *msg);

//void loadXmList ( Widget , char *[], int);

Widget           global_toplevel;
char            		ret_segments[100][20];

extern				SEL_RANGE range_selected;

int				actual_numsegments = 0;
char				*segment_name = "         ";

Mods_everythingStruct 		*tmpdataStruct;

char *get_fgroup_name()
{
   long      nitems, left;
   long      offset = 0;
   int       format;
   Atom      type;
   char      *forecastGroup_name;

   Display   *display;
   Window    root;

   display = XtDisplay(global_toplevel);
   root    = DefaultRootWindow(display);


  if(XGetWindowProperty
   (
    display,
    root,
    IFPA_forecast_group,
    offset,
    (long) 9,
    FALSE,
    IFPA_forecast_group_type,
    &type,
    &format,
    &nitems,
    (unsigned long *)&left,
    (unsigned char **)&forecastGroup_name
   ) == Success && type == IFPA_forecast_group_type)
   {
     ; /*  Do nothing - continue */
   }
  else
     {
	   printf("The forecast group name is not available in ");
           printf("Cannot generate a list of mods for file\n");
	   forecastGroup_name = NULL;
     }
  return (forecastGroup_name);

}

void fgroupSelectionCB(Widget w, Mods_everythingStruct *data,
			    XmPushButtonCallbackStruct *call_data, ifp_modsShell_p widgetData)
{


   if( data->selectedModDef->modFGroup == SEGMENT)
       data->fgroupModsselected = SEGMENT;
   else
       data->fgroupModsselected = FGROUP;
}

void rangeSelectionCB(Widget w, Mods_everythingStruct *data,
			    XmPushButtonCallbackStruct *call_data, ifp_modsShell_p widgetData)

{
	long      	nitems, left;
	long      	offset = 0;
	int       	format;
	Atom      	type;


	Display   	*display;
	Window    	root;

	int		j;
	int   		num_seg;
	char            *run_segments;


        range_listDS = NULL;
	display = XtDisplay(w);
	root    = DefaultRootWindow(display);


   if ( ! range_listDS )
   {
  	create_range_listDS( XtParent(w));
        data->fgroupModsselected = RANGE;


	XtAddCallback(range_listCancelPB, XmNactivateCallback, range_listCancelCB, NULL);

	XtAddCallback(range_listOkPB, XmNactivateCallback, range_listOkCB, NULL);

	XtAddCallback(rangelist, XmNextendedSelectionCallback, range_list_selectCB, NULL);

   }

   if ( !XtIsManaged (range_listDS))
   {
 	 XtManageChild(range_listFO);
	 XtManageChild(range_listDS);

   }
	/**/
	if(XGetWindowProperty
	(
	display,
	root,
	IFPA_current_segment,
	offset,
	(long) 9,
	FALSE,
	IFPA_current_segment_type,
	&type,
	&format,
	&nitems,
	(unsigned long *)&left,
	(unsigned char **)&segment_name
	) == Success && type == IFPA_current_segment_type)
	{
	 ;
        }

	/**/
	XGetWindowProperty
		(
		display,
		root,
		IFPA_run_segments,
		offset,
		(long) 801,
		FALSE,
		IFPA_run_segments_type,
		&type,
		&format,
		&nitems,
		(unsigned long *)&left,
		(unsigned char **)&run_segments
		 );

                num_seg = parsegment(run_segments,ret_segments);
		actual_numsegments = num_seg;
		loadXmList ( rangelist , ret_segments, num_seg);
		tmpdataStruct = data;
		tmpdataStruct->rangeModsSaved = 0;

}

void segmentSelectionCB(Widget w, Mods_everythingStruct *data,
			    XmPushButtonCallbackStruct *call_data, ifp_modsShell_p widgetData)
{
        data->fgroupModsselected = SEGMENT;
}


int parsegment(char *buf, char segments[100][20])
{
 	int 	n,
		m,
		nsegments;

	nsegments = strlen(buf)/8;

	for(n = 0; n < nsegments; n++)
	{
		memset(segments[n],'\0',20);
		memcpy((char *)&segments[n],(char *)&buf[n*8],8);
		for (m =0; m < strlen(segments[n]); m++)
			if (segments[n][m] == ' ') segments[n][m] ='\0';

	}

	return(nsegments);

}

void loadXmList ( Widget w , char segments[100][20], int nsegs)
{

	int		i, n, pos;
	char		buf[20];

	XmStringTable	xmStr;

	XmListDeleteAllItems(w);

	xmStr = (XmStringTable) XtMalloc (nsegs * sizeof(XmString *));


	for ( i = 0; i < nsegs; i++)
	{
		strcpy(buf, segments[i]);
		xmStr[i] = XmStringCreateSimple(buf);
        }

	XmListAddItems(w, xmStr, nsegs, 1);

	pos = seg2pos(segment_name);

	if ( pos <= 0 )pos = nsegs;
	XmListSelectPos(w, pos, True);

	for ( i = 0; i < nsegs; i++)
			XmStringFree ( xmStr[i]);

	XtFree((char *) xmStr);

	return;
}

void range_list_selectCB( Widget w, XtPointer ptr, XtPointer cbs )
{
	return;
}

void range_listOkCB( Widget w, XtPointer ptr, XtPointer cbs )
{

	extern SEL_RANGE range_selected;

	int 	*poslist = NULL;
	int 	cnt      = 0;
	int 	pos, i;
	int	currentRangeModSaved;
	Display   	*display;
	Window    	root;

	display = XtDisplay(w);
	root    = DefaultRootWindow(display);

	XmListGetSelectedPos(rangelist, &poslist, &cnt);

	if ( cnt <= 1)
	{
	        ErrorDialog( w, "INVALID RANGE SELECTION");
  		tmpdataStruct->rangeModsSaved = 0;
		return;
	}

	for(i=0;i<cnt;i++)
	{
		pos = *(poslist+i);
		sprintf(range_selected.seg_str[pos-1],"%s",ret_segments[pos-1]);
		if(i==0)
		sprintf(range_selected.first_rangeseg,"%s",ret_segments[pos-1]);
	}

	range_selected.first = *poslist - 1;
	range_selected.last  =  pos - 1;

	range_selected.min_range = range_selected.first;
	strcpy(range_selected.seg_str[range_selected.min_range],range_selected.first_rangeseg);


       	XtUnmanageChild(range_listDS);
	tmpdataStruct->rangeModsSaved = 1;
	currentRangeModSaved= tmpdataStruct->rangeModsSaved;

	XChangeProperty(
	display,
	root,
	IFPA_rangemods_saved,
	IFPA_rangemods_saved_type,
	8,
	PropModeReplace,
	(unsigned char *)&currentRangeModSaved,
	sizeof(int)
	);
	return ;

}

void range_listCancelCB(Widget w, XtPointer ptr, XtPointer cbs )
{

        XtUnmanageChild(range_listDS);
  	tmpdataStruct->rangeModsSaved = 0;
	return ;

}


int seg2pos( char *segment )
{
	extern char  ret_segments[100][20];
	extern int   actual_numsegments;

	int n;

	for ( n = 0; n < 9; n++)
		if ( *(segment + n) == ' ') *(segment + n) = '\0';

	for ( n = 0; n < actual_numsegments; n++)
	{
		if (strcmp(segment, ret_segments[n]) == 0)
		{
			return ( n+1 );

		}

	}
	/* if not found return 0 */
	return (0);
}

void ErrorDialog(Widget widget, char *msg)
{
	static 	Widget	msgBox;
	Arg		arg[4];
	int		ac;

	if ( ! msgBox )
	{
		ac = 0;
		XtSetArg(arg[ac], XmNtitle, "Error Dialog"); ac++;
		XtSetArg(arg[ac], XmNdialogType, XmDIALOG_ERROR); ac++;
		XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++;


		msgBox = XmCreateMessageDialog(widget, "msgBox", arg, ac);
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));


	}
	ac = 0;
	XtSetArg(arg[ac], XmNmessageString,
		 XmStringCreateLtoR(msg,
				    (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); ac++;
	XtSetValues(msgBox, arg, ac);

	XtManageChild(msgBox);
	XtPopup(XtParent(msgBox), XtGrabNone);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/show_mods.c,v $";
 static char rcs_id2[] = "$Id: show_mods.c,v 1.5 2006/04/18 15:29:56 aivo Exp $";}
/*  ===================================================  */

}
