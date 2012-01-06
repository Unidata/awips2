/****************************************************************************
 *
 * Header: map_toolbar.h
 *
 * Description: contains the prototypes for map_toolbar.c
 *
 ***************************************************************************/

/*-------------------------------------------------------------------------
 
 Prototypes definations

 --------------------------------------------------------------------------*/

extern void _create_map_toolbar(Widget,int,int);
extern void _cursor_location(Widget,XtPointer,XEvent *,Boolean *);
extern void _leave_window(Widget,XtPointer,XEvent *,Boolean *);
extern void _manage_toolbar();
extern void _resize_map_toolbar( Widget wid , XtPointer  client_data ,
                                 XtPointer call_data ) ;
extern void _unmanage_toolbar();
