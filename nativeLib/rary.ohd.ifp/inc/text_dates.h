/* 
//======================================= //
// AV created for set_dates
//    Change set_dates pushbutton widget to Text widgets 
// ======================================= //
*/

#ifndef  _text_dates
#define  _text_dates

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <time.h> 
#include <ctype.h> 

#include <X11/Xlib.h> 
#include <Xm/Xm.h> 
#include <Xm/Text.h> 
#include <Xm/Label.h> 
#include <Xm/Form.h> 
#include <Xm/ArrowB.h> 
#include <Xm/DrawnB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h> 

#define  NBUTTONS	4

typedef struct _MYDATETYPE  
{


   int  fieldFlag,
	fg, 
	bg;

   int	year, 
	month, 
	day, 	
	hour;

   char	*sMonth;

   Widget  w[6];

   GC	gc;

}MYDATETYPE; 

int getMyColor ( Display *display, char *colorName);
void SetColor (Widget w, int color );
void ResetColor( Widget w);

int isValidMonth( char *pC);
int isAllDigits ( char *pC);
int isALlChars  ( char *pC);
int GetMonthNumbyStr(char *pbuf);
int isCurTimeInvalid();
char *GetMonthByNum( int n );

void ParseTime();

void PushCB ( Widget w, XtPointer client, XtPointer data );
void ArrowUpCB (  Widget w, XtPointer client, XtPointer data );
void ArrowDownCB (  Widget w, XtPointer client, XtPointer data ) ;

void TextWClickCB (  Widget w, XtPointer client, XtPointer data );
void TextWGetTextCB (  Widget w, XtPointer client, XtPointer data );

void initializeDate(int indx );
void AddAllCallBacks() ;
void init_AdateGC ( Widget w, int n);
void UpdateTimeChange(int n);

void ShowErrorDialog( Widget widget, char *msg );
void UpdateDates(Widget w);

#endif 

