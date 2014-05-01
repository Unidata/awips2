
#include "Painter.H"


//**********************************************************************

Painter::Painter()
{
   
     return;
}

//**********************************************************************

Painter::~Painter()
{
   
     return;
}


//**********************************************************************

void Painter::draw()
{
   
      clearBackground();  
   
}

//**********************************************************************

void Painter::clearBackground()
{
   
	/*
		Draw window base and border rectangle.
	*/
	SetColor(canvas->getGC(), canvas->getDrawingArea(), "black");
	XFillRectangle(canvas->getDisplay(),
		       canvas->getPixmap(),
		       canvas->getGC(),
		       0, 0,
		       canvas->getWidth(), 
		       canvas->getHeight());
	
	
	return;
	
}

//**********************************************************************

