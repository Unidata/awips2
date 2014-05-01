/* File: helpEvntHdlr.c
 *
 * Prevents an XProtocol error for a bad windowID when a
 * ButtonRelease event is spawned by releasing the mouse
 * button outside of the widget.
 *
 * Creates a question mark cursor.
 *
 */


#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>


void         help_event_handler();
void         destroy_help_message();
static Pixel pixel_name();
static void  remove_widget_eventHandler();

int     IsContextHelpWindowDown;        /* This global is a flag used in the event loop to test if      */
					/* we've passed through 'destroy_help_message' for the          */
					/* destruction of the help_shell... this kludge is needed to    */
					/* avoid an XProtocol error for a bad windowID when a           */
					/* ButtonRelease event is spawned by releasing the mouse button */
					/* outside of the widget we just got context-sensitive help on. */

typedef struct  {
		Widget  top_help_shell;
		char    *widget_name;
		} help_cb_struct;       /* help call back data structure */


void help_event_handler(w, data, event)
	Widget         w;
	help_cb_struct *data;
	XEvent         *event;
{

	Widget         message;
	Arg            wargs[5];
	int            n;
	char           help_widget_name[100];
	char           *string;
	char           labelHeight_str[5];
	char           labelWidth_str[5];
	char           geometry[15];
	XmString       help_message;
	Cursor         question_mark = (Cursor)NULL;
	Dimension      labelHeight;
	Dimension      labelWidth;


/* -------------------------------------------------------------------------------------
 printf("In 'help_event_handler()', detail = %d...\n", event->xcrossing.detail);
 printf("In 'help_event_handler()',   mode =  %d...\n", event->xcrossing.mode);
 printf("w = %d, event->type = %d\n", w, event->type);
   ------------------------------------------------------------------------------------- */

 if(event->xcrossing.mode != NotifyNormal) return;
 if((event->xcrossing.state & ShiftMask) != ShiftMask) return;

/* Create a question mark cursor if not already done...         */
 if(question_mark == (Cursor)NULL)
    question_mark = XCreateFontCursor(XtDisplay(w), XC_question_arrow);

/* Define the question mark cursor for the current window...    */
 XDefineCursor(XtDisplay(w), XtWindow(w), question_mark);


 memset(help_widget_name, '\0', 100);
 strcpy(help_widget_name, "Help_for_");
 strcat(help_widget_name, data->widget_name);

 message = XtVaCreateManagedWidget(help_widget_name,
				       xmLabelWidgetClass,
				       data->top_help_shell,
				       NULL);
 XtAddCallback(message, XmNdestroyCallback, remove_widget_eventHandler, w);
 XtAddEventHandler(w, LeaveWindowMask, FALSE, destroy_help_message, message);

 IsContextHelpWindowDown = FALSE;
 XtPopup(data->top_help_shell, XtGrabNone);

}

void destroy_help_message(w, message, event)
	Widget         w;
	Widget         message;
	XEvent         *event;
{

/* -------------------------------------------------------------------------------------
 printf("In 'destroy_help_message()', detail = %d...\n", event->xcrossing.detail);
 printf("In 'destroy_help_message()',   mode =  %d...\n", event->xcrossing.mode);
 printf("w = %d, shell = %d, event->type = %d\n", w, shell, event->type);
   ------------------------------------------------------------------------------------- */


 XUndefineCursor(XtDisplay(w), XtWindow(w));

 XtPopdown(XtParent(message));
 XtDestroyWidget(message);
 XFlush(XtDisplay(w));

 IsContextHelpWindowDown = TRUE;

/* printf("leaving destroy_help_message\n");    */

}


Pixel pixel_name(w, color_name)
	Widget  w;
	char    *color_name;
{
	Display         *dpy    = XtDisplay(w);
	int             scr     = DefaultScreen(dpy);
	Colormap        cmap    = DefaultColormap(dpy, scr);
	XColor          color, ignore;


 /*      Alloc named color...           */
 if(XAllocNamedColor(dpy, cmap, color_name, &color, &ignore)) return (color.pixel);
 else   {
	printf("Warning:  Couldn't alloc color %s\n",color_name);
	return (BlackPixel(dpy, scr));
	}
}


void  remove_widget_eventHandler(w, event_widget, call_data)
	Widget  w;              /* The shell widget being destroyed...                          */
	Widget  event_widget;   /* The widget we're getting context-sensitive help on...        */
	XmAnyCallbackStruct *call_data;
{

/* printf("Inside 'remove_widget_eventHandler'..., w = %d, event_widget = %d\n", w, event_widget);      */

 XtRemoveEventHandler(event_widget, LeaveWindowMask, FALSE, destroy_help_message, w);

 XFlush(XtDisplay(w));
/* printf("leaving remove_widget_eventHandler\n");              */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/helpEvntHdlr.c,v $";
 static char rcs_id2[] = "$Id: helpEvntHdlr.c,v 1.3 2006/04/07 14:10:51 aivo Exp $";}
/*  ===================================================  */

}


