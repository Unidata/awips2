#ifndef CANVAS_HXX
#define CANVAS_HXX

#include "MotifMaster.h"

extern "C"
{
#include "Xtools.h"
}

class Painter;

class Canvas
{
    
     
public:
   
     Canvas(Widget initDrawingArea);
     virtual ~Canvas();
        
     void reinit();
     
     static void exposeCallback(Widget w, XtPointer ptr, XtPointer cbs);
     virtual void update();
      
     Widget    getDrawingArea();
     
     Dimension getHeight();
     Dimension getWidth();
     
     Pixmap getPixmap();
     Window getWindow();
     GC     getGC();
     Screen* getScreen();
     Display* getDisplay();
    
     
protected:   
   
   
     // methods
     virtual void free();
     void setupGraphics();
     void setupEventHandling();
     
     
     // data

     Widget drawingArea;
     Dimension height;
     Dimension width;
 
     
     // graphics data
     Pixmap pixmap;
     Window window;
     GC     gc;
     Screen *screen;
     Display *display;
 
};

//************************************************************************

inline Pixmap Canvas::getPixmap()
{
     return pixmap;   
}

//************************************************************************

inline Widget Canvas::getDrawingArea()
{
     return drawingArea;   
}

//************************************************************************

inline Window Canvas::getWindow()
{
     return window;   
}

//************************************************************************

inline GC Canvas::getGC()
{
     return gc;   
}

//************************************************************************

inline Screen * Canvas::getScreen()
{
     return screen;   
}

//************************************************************************

inline Display * Canvas::getDisplay()
{
     return display;   
}

//************************************************************************

inline Dimension Canvas::getHeight()
{
     return height;   
}

//************************************************************************

inline Dimension Canvas::getWidth()
{
     return width;   
}

//************************************************************************




#endif
