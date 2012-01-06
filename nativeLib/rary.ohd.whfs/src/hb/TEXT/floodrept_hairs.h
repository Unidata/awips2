/*
	File:		floodrept_hairs.h
	Date:		May 1997
	Author:		Paul Taylor

	Purpose:	To provide xhair routines for Flood Report DS.	
*/

#ifndef floodrept_hairs_h
#define floodrept_hairs_h


typedef struct _CrossHairs
{
   int		firstx;
   int		firsty;
   int		startx;
   int		starty;
   int		endx;
   int		endy;
   GC		gc;
   
   Position	top,
      		bottom,
		left,
		right;
   
} CrossHairs;


void    floodrept_xhairAddHandlers	(void);

void	floodrept_xhairButtonPressRelease(Widget w, XtPointer ptr, XEvent *event);
void	floodrept_xhairKeyRelease	 (Widget w, XtPointer ptr, XEvent *event);

void	floodrept_xhairMotion		(Widget w, XtPointer ptr, XEvent *event);
void	floodrept_xhairEnterWindow	(Widget w, XtPointer ptr, XEvent *event);
void	floodrept_xhairLeaveWindow	(Widget w, XtPointer ptr, XEvent *event);


GC	floodrept_xhairSetup		(void);
void    floodrept_xhairSetStageLabel	(void);
void	floodrept_xhairCheckBounds	(void);
void	floodrept_xhairSetBounds	(void);
void	floodrept_xhairDraw		(void);
void	floodrept_xhairErase		(void);


/*
	consider putting xhairSetup in Tools.
	Maybe even make xhairSetup do all the setup, including adding
	the EventHandlers and you just register your crosshairs and your
	Drawing Area in a single call.
*/


#endif

 
