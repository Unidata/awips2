#ifndef PAINTER_HXX
#define PAINTER_HXX



extern "C"
{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "MotifMaster.h"
}

#include "Canvas.H"
   


class Painter
{
   // friend of Canvas
   
public:
   
   Painter();
   ~Painter();
   
   void setCanvas(Canvas *initCanvas);
   
   // drawing related methods
   void draw();
   void clearBackground();
 
   
protected:
   
   Canvas *canvas;   
   
};

//************************************************************************

inline void Painter::setCanvas(Canvas *initCanvas)
{
     canvas = initCanvas;   
   
     return;
}

//************************************************************************
   
   
   


#endif
 
