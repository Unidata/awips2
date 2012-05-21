/*Preliminary Header file for Turtle Graphics*/
#include <X11/Intrinsic.h>
#include <X11/X.h>
#include <Xm/Xm.h>
#define CIRCUMFERENCE 360
#define HALFCIRCUMFERENCE 180
#define FALSE 0
#define TRUE 1
#define WIDTH 640
#define HEIGHT 480
/***************************************************************************/
/*Vector Graphic Drawing Routines Header File                              */
/*Derived From Microsoft Quick C Turtle Demo Programs                      */
/*Developed/Integrated to Emulate Microsoft/GW-BASIC graphic DRAW function */
/* in X-Windows Motif by:                                                  */
/*Larry J. Hinson  AWC/KCMO                                                */
/***************************************************************************/
Display* g_display;
Window g_window;
GC g_gc;
/*************************************************************************/
/*Symbols for Icing for use by DRAW function in EXPDRAW.C                */
/*************************************************************************/
static char Trace_Ice_Sym[] = {"D2F4R6E4U2"};
static char Light_Ice_Sym[] = {"D2F4R6E4U2BL7D8"};
static char Moderate_Ice_Sym[] = {"D2F4R6E4U2BL6ND8BL2ND8"};
static char Severe_Ice_Sym[] = {"D2F4R6E4U2BL5ND8BL2ND8BL2ND8"};
int initturtle(int width,int height);
int OnScreen();
int PenDown( int fPenDown );
short PenColor( short ciCur );
short Turn( short angCur );
short TurnTo( short angCur );
short MoveToP( short x, short y );
short MoveP( double dxy);
short nMoveP( double dxy);
short MovePD(short dx,short dy);
short nMovePD(short dx,short dy);
short SetEndPix(short dx,short dy,int op);
void qckprnt(int x,int y, char *wrk, int colr);
int draw(char *string);
void execdraw(char *string);
void drawloc(int x, int y, int color);
void drawwind(int x,int y,int dir,int speed,int color);
int getmaxx();
int getmaxy();
static double correction;
/*enum logical{FALSE,TRUE};*/
extern struct turtle{
	int fPenDown;
	int ciCur;
	int angCur;
	float xCur;
	float yCur;
	int xmax;
	int ymax;
} tc;


