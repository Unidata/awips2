#include "winline.h"
#include "sharp95.h"
#include <math.h>
#define TRUE 1
#define FALSE 0
/**************************************************************************/
/* Routines to emulate Window/Line Drawing functions used in Microsoft    */ 
/* Quick C and Quick Basic Graphic Programs                               */
/* Developed/Integrated to work with X-Windows Motif by:                  */
/* Larry J. Hinson AWC/KCMO                                               */
/**************************************************************************/

int viewmaxx,viewmaxy,vxoff,vyoff,l_xCur,l_yCur;
float windowdx,windowdy,vwslopex,vwslopey,windowx1,windowy1;
float costheta,sintheta;
XFontStruct *font_structl=NULL;

int g_useiwindowparm=0;

void InitWindowSystem(Widget w,int useinternal) {
  g_useiwindowparm=useinternal;
  if (g_useiwindowparm) {
    l_display = XtDisplay (w);
    l_window = XtWindow (w);
    l_gc = XCreateGC(l_display,l_window,0,0);
  }
}

void ExitWindowSystem() {
  g_useiwindowparm=0;
}
    
void setfontl(int font) {
       Font          font_info;
       static char   font_1 [] =
       { "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"};

       static char   font_2 [] =
       { "-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1" };

       static char   font_3 [] =
       { "-adobe-times-bold-r-normal--17-120-100-100-p-88-iso8859-1" };

       static char   font_4 [] =
       { "-adobe-new century schoolbook-bold-*-*-*-34-*-*-*-*-*-*-*" };

       static char   font_5 [] =
       { "-*-symbol-medium-*-*-*-14-140-75-75-p-85-*-*" };

       if ( font == 1 )
          {
          font_info = XLoadFont(l_display, font_1 );
          }
       else if ( font == 2 )
          {
          font_info = XLoadFont(l_display, font_2 );
          }
       else if ( font == 3 )
          {
          font_info = XLoadFont(l_display, font_3 );
          }
       else if ( font == 4 )
          {
          font_info = XLoadFont(l_display, font_4 );
          }
       else if ( font == 5 )
          {
          font_info = XLoadFont(l_display, font_5 );
          }

       XSetFont(l_display, l_gc, font_info );
       if(font_structl != NULL) XFreeFontInfo(NULL,font_structl,0);
       font_structl = XQueryFont ( l_display, font_info);
}

float fnP (float j) {
  return(log(j)/log(1000)*1000);}
  
void setwindow(float a,float b,float c,float d) {
    windowdx=c-a;windowdy=d-b;
    vwslopex=viewmaxx/windowdx;vwslopey=viewmaxy/windowdy;
    windowx1=a;windowy1=b;
}

void setwindowscreen(float a,float b,float c,float d) {
	windowdx=c-a;windowdy=b-d;
	vwslopex=viewmaxx/windowdx;vwslopey=viewmaxy/windowdy;
	windowx1=a;windowy1=d;
}

void view(int a,int b,int c,int d) {
        vxoff = a;
        vyoff = b;
	viewmaxx=c-a;viewmaxy=d-b;
}

void line_w(float a,float b,float c,float d) {
	int vyx1,vyx2,vyy1,vyy2;
	vyx1=vwslopex*(a-windowx1);
	vyx2=vwslopex*(c-windowx1);
	vyy1=vwslopey*(b-windowy1);
	vyy2=vwslopey*(d-windowy1);
	if (g_useiwindowparm) {
	  l_moveto(vyx1,vyy1);
	  l_lineto(vyx2,vyy2);
	  l_moveto(vyx2,vyy2);
	} else {
 	  moveto(vyx1,vyy1);
	  lineto(vyx2,vyy2);
	  moveto(vyx2,vyy2);
	}
}

/* Define local primitives */
void l_moveto(int x,int y) {
  l_xCur = x;
  l_yCur = y;
}

void l_lineto(int x,int y) {
  XDrawLine(l_display,l_window,l_gc,l_xCur+vxoff,l_yCur+vyoff,x+vxoff,y+vyoff);
}

void lineto_w(float a,float b) {
	int vyx1,vyy1;
	vyx1=vwslopex*(a-windowx1);
	vyy1=vwslopey*(b-windowy1);
	if (g_useiwindowparm) 
	  l_lineto(vyx1,vyy1);
	else
	  lineto(vyx1,vyy1);
}

void moveto_w(float a, float b) {
  int vyx1,vyy1;
  vyx1=vwslopex*(a-windowx1);
  vyy1=vwslopey*(b-windowy1);
  if (g_useiwindowparm)
    l_moveto(vyx1,vyy1);
  else
    moveto(vyx1,vyy1);
}

int pmap(float x,int oper) {
    switch (oper) {
    case 0:
      return(vwslopex*(x-windowx1));
    case 1:
      return(vwslopey*(x-windowy1));
    case 2:
      return(x/vwslopex+windowx1);
    case 3:
      return(x/vwslopey+windowy1);
    }
}

void setcolorfg(int colr) {
  if (g_useiwindowparm) {
    XSetForeground(l_display,l_gc,pixels[colr]);
  }
}

void outtextxy_w(float x, float y, char *buf) {
  if (g_useiwindowparm) {
    outtextxy((int) pmap(x,0), (int) pmap (y,1), buf);
  }
}

void outtextxy(int x,int y,char *buf) {
  if (g_useiwindowparm) {
    XDrawString(l_display,l_window,l_gc,x,y+7,buf,strlen(buf));
  }
}

void outtextxy_fill(int x, int y, int colr, char *buf) {
  setcolorfg(0);
  XFillRectangle(l_display,l_window,l_gc,x,y,textwidth(buf),textheight());
  setcolorfg(colr);
  outtextxy(x,y,buf);
}
  
void rectanglel(int x1,int y1, int x2, int y2) {
  XDrawRectangle(l_display,l_window,l_gc,x1+vxoff,y1+vyoff,x2-x1+vxoff,y2-y1+vyoff);
}

void rectanglel_fill(int x,int y, int colr, int width, int height) {
  setcolorfg(colr);
  XFillRectangle(l_display, l_window, l_gc, x, y, width, height);
}

void pset(int x,int y) {
  XDrawPoint(l_display,l_window,l_gc,x+vxoff,y+vyoff);
}

void pset_w(float x, float y) {
  pset((int)pmap(x,0),(int)pmap(y,1));
}

void circle_w(float x,float y,int radius) {
  circle(pmap(x,0),pmap(y,1),radius);
}

void circle(int x,int y,int radius) {
  XDrawArc(l_display,l_window,l_gc,x+vxoff-radius/2,y+vyoff-radius/2,radius,radius,0,360*64);
}

void setlinestylel(short style, short width)
{
       int           dash_offset = 1, n;
       static char   dash_dash[]  = {3,3};
       static char   dash_dot[]  = {4,4,2,4};
       static char   dash_long[]  = {3,3,6,3};
       short         line_width;

       if ( width < 0 || width > 10 )
              line_width = 1;
       else
              line_width = width;

       if ( style == 1 )
          {
          XSetLineAttributes ( l_display, l_gc, line_width,
                     LineSolid, CapButt, JoinRound );
          }
       else
          {
          XSetLineAttributes ( l_display, l_gc, line_width,
                     LineOnOffDash, CapButt, JoinRound );
          }

      if ( style == 2 )
         { XSetDashes ( l_display , l_gc, dash_offset, dash_dash, 2 ); }
      else if ( style == 3 )
         { XSetDashes ( l_display , l_gc, dash_offset, dash_dot, 4 ); }
      else if ( style == 4 )
         { XSetDashes ( l_display, l_gc, dash_offset, dash_long,4 ); }
}

int textheight () {
       return font_structl->ascent+font_structl->descent;
}

int textwidth (char *buf) {
       return XTextWidth (font_structl, buf, strlen(buf) );
}

        






