// +++ 	Name: showBanner.c
//Purpose: This program display a small window indicating the
//	current test mode status on all screens (3 heads if running from
//	graphical workstation and 1 head on text workstation). Since it is
//	launched every few seconds, a blinking effect is noticed on the 
//	screen. For a panic mode, the text message  will be turning on and off
//	every second also.
//
//Inputs:  This program takes four input arguments to run. 
//	(1) Mode: test|practice|obnoxious|norealization
//	(2) Time in seconds for the window to stay (must be 1 or greater)
//	(3) X-screen location for the display window (related from top-left)
//	(4) Y-screen location for the display window (related from top-left)
//
//15-may-05 P. Wu - Make the banner movable in test/practice mode.
// ---*************************************************************************
#include <Xm/Xm.h>          // Required by all Motif applications 
#include <Xm/Label.h>       // Required by XmLabel widget 
#include <stdlib.h>         // Needed for exit() 
#include <stdio.h>          // Needed to use fprintf 
#include <time.h>	    // for setitimer 
#include <sys/time.h>	    // for setitimer 
#include <unistd.h>	    // for pause 
#include <signal.h>	    // for signal 
#include "tmbUtil.H"
#include "testmode.H"
#define MAX_DISPLAYS 5
#define FONT_NAME           "-*-helvetica-bold-r-normal--12-*"
#define INTERVAL 2000       // number of milliseconds to go off 

#define RES_CONVERT( res_name, res_value) \
        XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1

struct _resrcs {
      XFontStruct    *font; 
      Dimension      height; 
} Resrcs;

static XtResource resources[] = {
    { XmNfont, XmCFont, XmRFontStruct, sizeof (XFontStruct *),
         XtOffsetOf(struct _resrcs, font), XmRString, (XtPointer*)XtDefaultFont },
    { "height", "Height", XmRDimension, sizeof (Dimension), 
         XtOffsetOf(struct _resrcs, height), XmRImmediate, (XtPointer *) 300 },
};


int numDisplays;            		// Number of displays
static Widget msg[MAX_DISPLAYS];       // Widgets created by this application 
char displayNames[MAX_DISPLAYS][128];
Display *display[MAX_DISPLAYS];
Widget shell[MAX_DISPLAYS];
Window window[3];
int Mode = 0;


// +++ 	Function : UpdateBlinking
//Purpose: This routine will perform blinking every second or so in case
//         of panic mode.
// ---*************************************************************************
static void UpdateBlinking ( XtPointer clientData, XtIntervalId id ) 
{
    static int toggle=1;
    int i;
    unsigned long time_to_blink;

    Widget w = ( Widget ) clientData;

    if (toggle==0) {
         for (i=0; i<numDisplays;i++) {
            XtVaSetValues(msg[i], RES_CONVERT(XmNbackground,
                          "Gray"), NULL);
		 // --XUnmapWindow(display[i],window[i]);
	    XtUnmapWidget(shell[i]);
	    //XLowerWindow(display[i],window[i]);
            XmUpdateDisplay(shell[i]);
	 }
         toggle = 1;
    }
    else {
         for (i=0; i<numDisplays;i++) {
            if (Mode == TEST_MODE)
            	XtVaSetValues(msg[i], RES_CONVERT(XmNbackground,
                          "Black"), NULL);
            if (Mode == PRACTICE_MODE)
            	XtVaSetValues(msg[i], RES_CONVERT(XmNbackground,
                          "Orange"), NULL);
            if (Mode == PANIC_MODE)
            	XtVaSetValues(msg[i], RES_CONVERT(XmNbackground,
                          "Red"), NULL);
		 // --XMapWindow(display[i],window[i]);
	    XtMapWidget(shell[i]);
            XmUpdateDisplay(shell[i]);
	    //XRaiseWindow(display[i],window[i]);
	 }
         toggle = 0;
    }
    
    // Xt removes timeouts when they occur, so re-register the function about 
    // 1 second.
    
    time_to_blink = INTERVAL;

    XtAppAddTimeOut ( XtWidgetToApplicationContext ( w ),  time_to_blink, 
                    (XtTimerCallbackProc) UpdateBlinking, (XtPointer) w );
}


// +++ 	Function : set_display
//Purpose: Set up all Motif widgets for display.
// ---*************************************************************************
void set_display(int i, char **argv, XmFontList fontlist, int xloc, int yloc,
                XmString xmstr, int realization)
{

        shell[i] =XtAppCreateShell(argv[0],"Tmcp",
                  applicationShellWidgetClass, display[i],NULL,0);

        // Create a Motif XmLabel widget to display the string

        msg[i] = XtVaCreateManagedWidget ( "message", 
                               xmLabelWidgetClass, shell[i], 
                               XmNlabelString, xmstr,
                               XmNfontList, fontlist,
                               XmNmarginHeight, 5,
                               NULL);

        XtVaSetValues(shell[i], XmNtitle, displayNames[i], NULL);
        XtVaSetValues(shell[i], XmNdeleteResponse, XmDO_NOTHING, NULL);

        XtVaSetValues(shell[i], XmNx, xloc, XmNy, yloc, NULL);
        if (strcmp(argv[1],"obnoxious") == 0)
            XtVaSetValues(msg[i], RES_CONVERT(XmNbackground, "RED"),
                          NULL);
        else if (strcmp(argv[1],"practice") == 0)
            XtVaSetValues(msg[i], RES_CONVERT(XmNbackground,
                          "ORANGE"), NULL);
        else {
            XtVaSetValues(msg[i], RES_CONVERT(XmNbackground,
                          "BLACK"), NULL);
            XtVaSetValues(msg[i], RES_CONVERT(XmNforeground,
                          "WHITE"), NULL);
	}
        XtVaSetValues(shell[i], XmNinput, False, NULL);


        if (strcmp(argv[1],"test") == 0 ||
            strcmp(argv[1],"practice") == 0) {
           XtVaSetValues(shell[i], XmNmaxHeight, 25, NULL);
           XtVaSetValues(shell[i], XmNmaxWidth, 150, NULL);
           XtVaSetValues(shell[i], XmNminHeight, 20, NULL);
           XtVaSetValues(shell[i], XmNminWidth, 150, NULL);
	}
	else {
           XtVaSetValues(shell[i], XmNmaxHeight, 25, NULL);
           XtVaSetValues(shell[i], XmNmaxWidth, 720, NULL);
           XtVaSetValues(shell[i], XmNminHeight, 20, NULL);
           XtVaSetValues(shell[i], XmNminWidth, 720, NULL);
	}



        if (realization) {
            XtRealizeWidget(shell[i]);
           // XtVaSetValues(shell[i],XmNx,xloc,XmNy,yloc, NULL);
            XmUpdateDisplay(shell[i]);

	}
	// Set window
	window[i] = XtWindow(shell[i]);

}

// +++ 	Function : timeToQuit
//Purpose: Timer went off and time for the program to exit.
// ---*************************************************************************
void timeToQuit(void) {
    exit(0);
}

// +++ 	Function : main
//Purpose: The main performs the following functions-
//  (1) Check input arguments
//  (2) Perform X-Window initialization
//  (3) Determine whether this is 1 or 3-head display base on workstations
//  (4) Create a window for each display
//  (5) In case of obnoxious mode, set up blinking timer
//  (6) Go to the event loop.
// ---*************************************************************************
int main ( int argc, char **argv )
{
    Widget toplevel;            // Widgets created by this application 
    XtAppContext app;           // An application context, needed by Xt 
    XmString     xmstr;         // Compound string used by Motif 
    char hostname[256];
    struct itimerval it_val;	// for setting itimer 
    int interval; 
    int seconds; 
    char message[512];
    int i;
    int n;
    Arg arglist[3];
    char user[128];
    char display_env[128];
    int realization=1;
    int xloc;
    int yloc;
    XmFontList  fontlist;
    XmFontListEntry  entry;

    // logVerbose << "Startup" << std::endl;
    sprintf(display_env,"%s",getenv("DISPLAY"));
    sprintf(user,"%s",getenv("USER"));
    n = check_local_login(user);
    if (n == 0) {
        // logProblem << "Problem with login." << std::endl;
        exit(1);
    }

    numDisplays = MAX_DISPLAYS;

    // Initialize Xt stuff

    XtToolkitInitialize();
    app = XtCreateApplicationContext();    
    XtAppSetFallbackResources(app, NULL);

    //display[0] = XtOpenDisplay(app,":0.0",argv[0],"Tmcp",
    if (strcmp(argv[1],"norealization") == 0)
    		display[0] = XtOpenDisplay(app,":0.0",argv[0],"Tmcp",
	        NULL, 0, &argc, argv);
	else
    		display[0] = XtOpenDisplay(app,display_env,argv[0],"Tmcp",
	        NULL, 0, &argc, argv);
    if (display[0] == NULL)
        exit(103);
        //exit(3);

    XSynchronize(display[0], 1);

    // Set maximum screens limits for display to 5
    numDisplays = ScreenCount(display[0]);
    if (numDisplays > MAX_DISPLAYS)
	    numDisplays = MAX_DISPLAYS;
    
    //numDisplays = 1;

    n =  0;
    XtSetArg(arglist[n], XmNallowShellResize, True);  n++;
    toplevel = XtAppCreateShell(argv[0], NULL,
                                applicationShellWidgetClass,
                                display[0], arglist,  n);

    XtGetApplicationResources(toplevel, &Resrcs, resources, 
        XtNumber(resources), NULL, 0);

    entry = XmFontListEntryCreate("tag", XmFONT_IS_FONT, Resrcs.font);
    fontlist = XmFontListAppendEntry(NULL, entry);

    if ( argc != 5 ) // Make sure there is exactly 5 arguments
    {
        //logProblem << "Usage: showBanner mode time X Y" << std::endl;
        fprintf (stderr, "Usage:  showBanner mode time X Y \n" );
        exit (1);
    }

    if (strcmp(argv[1],"test") == 0) {
	    sprintf(message,"Test (Comms Live)");
	    Mode = TEST_MODE;
    }
    if (strcmp(argv[1],"practice") == 0) {
	    sprintf(message,"Practice (In House)");
	    Mode = PRACTICE_MODE;
    }
    if (strcmp(argv[1],"obnoxious") == 0) {
	    sprintf(message,"A problem has been detected in the test or practice"
	    " mode. Please close all applications and exit.");
	    Mode = PANIC_MODE;
    }
        
    if (strcmp(argv[1],"norealization") == 0)
        realization=0;

    xloc = atoi(argv[3]);
    yloc = atoi(argv[4]);

    // Check the x and y position range
    if (xloc < 0 || xloc > 1000) xloc = 5;
    if (yloc < 0 || yloc > 975) yloc = 975;

    gethostname(hostname,256);

    for (i=0; i< numDisplays; i++)
            sprintf(displayNames[i],"%s:0.%d",hostname,i);
        

    // Upon SIGALRM, call timeToQuit(). Set interval timer.  
    // We want frequency in ms, but the setitimer call needs seconds and useconds. 
    
    if (realization==0 || strcmp(argv[1],"obnoxious") == 0) {
    	if (signal(SIGALRM, (void (*)(int)) timeToQuit) == SIG_ERR) {
        	perror("Unable to catch SIGALRM");
        	exit(1);
   	}

    	seconds     = atoi(argv[2]);
   	interval    = seconds * INTERVAL;
    	it_val.it_value.tv_sec  = interval/1000;
    	it_val.it_value.tv_usec = (interval*1000) % 1000000;	
    	it_val.it_interval = it_val.it_value;

    	if (setitimer(ITIMER_REAL, &it_val, NULL) == -1) {
        	perror("error calling setitimer()");
        	exit(1);
    	}
    }

    // Convert the first argument to the form expected by Motif 
    xmstr = XmStringCreateLtoR ( message, XmFONTLIST_DEFAULT_TAG );
    // Call once here  for the first display
    set_display(0, argv, fontlist,xloc, yloc, xmstr,realization);


    // Realize the toplevel and enter an event loop.

    for (i=1; i<numDisplays;i++)
    {
	    display[i]=XtOpenDisplay(app,displayNames[i],argv[0],"Tmcp",
			    NULL, 0, &argc, argv);
	    if (display[i] == NULL)
                    exit(3);
	    else {
                set_display(i, argv ,fontlist,xloc,yloc, xmstr,realization);

	    }
    }

    if (realization !=0) {

    	UpdateBlinking ( ( XtPointer ) msg[0], (XtIntervalId) NULL );
    }

    XmStringFree ( xmstr );  // Free the compound string
    XtAppMainLoop ( app );
}

