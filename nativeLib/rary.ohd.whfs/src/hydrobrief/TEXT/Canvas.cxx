

#include "Canvas.H"

//*************************************************************************

Canvas::Canvas(Widget initDrawingArea)
{

     //printf("inside Canvas constructor\n");
     drawingArea = initDrawingArea;   
   
     setupGraphics();
   
     setupEventHandling();
     
     
     return;
   
}

//*************************************************************************

Canvas::~Canvas()
{
   
   free();

   return;
}

//************************************************************************
void Canvas::free()
{
     if (pixmap)
     {
	  XFreePixmap(display, pixmap);
	  pixmap = 0;
     }
     
     if (gc)
     {
	  XtReleaseGC(drawingArea, gc);
	  gc = NULL;
     }   
     
     return; 
}
//************************************************************************

void Canvas::setupGraphics()
{   
     
     //printf("inside Canvas::setupGraphics\n");
     
     /*
     Set the general graphics fields that are used
     by the data and scale Drawing Areas.
     */
     display = XtDisplay(drawingArea);
     screen = XtScreen(drawingArea);
     
     
     /*
     Set the specific graphics fields that are used
     by the data and scale Drawing Areas.
     */
     window = XtWindow(drawingArea);
     gc = XCreateGC(display, window, 0, NULL);
     
     
     /*
     get Height and Width of the Drawing Area 
     */
     height = getWidgetHeight(drawingArea);    
     width = getWidgetWidth(drawingArea);
     
     
     /*
     Create the pixmap
     */
     pixmap = XCreatePixmap(display, window,
			    width, height,
			    DefaultDepthOfScreen(screen));     
     return;   
}

//**************************************************************************

void Canvas::setupEventHandling()
{
     
     // drawingArea
     XtAddCallback(drawingArea, XmNexposeCallback,
		   &Canvas::exposeCallback, this);
     
     
      
     return;
}

//**************************************************************************

void Canvas::exposeCallback(Widget w, XtPointer ptr, XtPointer cbs)
{
     Canvas *canvas = (Canvas *) ptr;
     
     canvas->update();  
      
     return;
}

//**************************************************************************

void Canvas::update()
{
     CopyPixmapToDA(drawingArea, pixmap);
     return;
}

//**************************************************************************

void Canvas::reinit()
{
     //
     // allows the widget to be readjusted to the same drawingArea.
     // This is done when the drawing area's dimensions have been changed
     
     
     //
     //  get rid of old graphics-associated memory
     //
     free();
     
     
     //
     //  set the graphics parameters
     //    
     setupGraphics();
     
     return;   
}
//************************************************************************
