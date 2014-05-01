/**************************************************************************/
/* Header File Routines to emulate Window/Line Drawing functions used in  */ 
/* Microsoft Quick C and Quick Basic Graphic Programs                     */
/* Developed/Integrated to work with X-Windows Motif by:                  */
/* Larry J. Hinson AWC/KCMO                                               */
/**************************************************************************/
#ifndef WIN_LINE
#include <X11/Intrinsic.h>
#include <X11/X.h>
#include <Xm/Xm.h>
  float fnP (float j);
  void setwindow(float a,float b,float c,float d);
  void setwindowscreen(float a,float b,float c,float d);
  void view(int a,int b,int c,int d);
  void line_w(float a,float b,float c,float d);
  void lineto_w(float a,float b);
  void moveto_w(float a,float b);
  int pmap(float x,int oper);
  void InitWindowSystem(Widget w,int useinternal);
  void ExitWindowSystem();
  void l_moveto(int x,int y);
  void l_lineto(int x,int y);
  void setcolorfg(int colr);
  void outtextxy_w(float x, float y, char *buf); 
  void outtextxy(int x,int y,char *buf);
  void rectanglel(int x1,int y1, int x2, int y2);
  void pset(int x, int y);
  void pset_w(float x, float y);
  void circle(int x,int y,int radius);
  void circle_w(float x,float y,int radius);
  void setlinestylel(short style, short width);
  int textheight ();
  int textwidth ();
  void outtextxy_fill(int x, int y, int colr, char *buf);
  void rectanglel_fill(int x,int y, int colr, int width, int height);
  Display* l_display;
  Window l_window;
  GC l_gc;

#define WIN_LINE
#endif
