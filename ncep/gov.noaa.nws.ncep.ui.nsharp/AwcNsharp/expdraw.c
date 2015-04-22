#include "sharp95.h"
#include "expdraw.h"
#include <ctype.h>
#include <math.h>
#include <stdio.h>
/***************************************************************************/
/*Vector Graphic Drawing Routines                                          */
/*Derived From Microsoft Quick C Turtle Demo Programs                      */
/*Developed/Integrated to Emulate Microsoft/GW-BASIC graphic DRAW function */
/* in X-Windows Motif by:                                                  */
/*Larry J. Hinson  AWC/KCMO                                                */
/***************************************************************************/

#define lineto(a,b) LineTo(a,b)
#define moveto(a,b) MoveTo(a,b)
struct turtle tc;
char nu[17][50],az[26][50];
int dysw=0;
int sizeconst=1;



int OnScreen() {
	if (tc.xCur>0 && tc.xCur<tc.xmax && tc.yCur>0 && tc.yCur<tc.ymax)

		return TRUE;
	else
		return FALSE;
}

int initturtle(int width, int height) {
        XSetForeground(g_display,g_gc,pixels[5]);
	tc.xmax=width;          
	tc.ymax=height;
	correction=(double)tc.ymax/(double)tc.xmax*1.4;
	correction = 1.0;
	return TRUE;
}

/* PenDown - Sets the visibility of the pen used by Move and MoveTo. The
 * state can be TRUE (visible), FALSE (invisible), or DEFAULT (return
 * current without changing).
 *
 * Params: fPenDown
 *
 * Return: current pen state
 *
 * Uses:   tc
 */
int PenDown( int fPenDown )
{
		switch( fPenDown )
		{
				case FALSE:
						tc.fPenDown = FALSE;
						break;
				default:
						tc.fPenDown = TRUE;
						break;
		}
		return tc.fPenDown;
}

/* PenColor - Sets the color index of the pen.
 *
 * Params: ciCur - any color index of DEFAULT to return without changing
 *
 * Return: current pen color index
 *
 * Uses:   tc
 */
short PenColor( short ciCur )
{
		/*SVGA_set_color(ciCur);*/
		XSetForeground(g_display,g_gc,pixels[ciCur]);
		return (tc.ciCur=ciCur);
}

/* Turn - Sets a new direction relative to the current direction.
 *
 * Params: angCur - a positive (clockwise) or negative (counterclockwise)
 *           angle in degrees
 *
 * Return: new current absolute angle
 *
 * Uses:   tc
 */
short Turn( short angCur )
{
		return( tc.angCur = ((tc.angCur + angCur) % CIRCUMFERENCE) );
}

/* TurnTo - Sets a new absolute direction.
 *
 * Params: angCur - a positive (clockwise) or negative (counterclockwise)
 *           angle in degrees (0 points to 12 o'clock)
 *
 * Return: new current absolute angle
 *
 * Uses:   tc
 */
short TurnTo( short angCur )
{
		if( angCur < 0 )
				return( tc.angCur = 360 - (angCur % CIRCUMFERENCE) );
		else
				return( tc.angCur = angCur % CIRCUMFERENCE );
}

/*Fixed Pixel Graphics - Added 1/2/92 */
/*Including MoveToP, MoveP,nMoveP */
short MoveToP( short x, short y )
{
		if( tc.fPenDown )
			 /*SVGA_line_to_abs( x, y );*/
			 XDrawLine(g_display,g_window,g_gc,tc.xCur,tc.yCur,x,y);
			 
			 
		/*else
			 SVGA_set_loc_abs( x, y );*/
			 
		tc.xCur = x;
		tc.yCur = y;
		return OnScreen();
}

short MoveP( double dxy )
{
		double dx, dy;          /* Differences of X and Y */
		double angT;

		/* Calculate new X and Y positions. */
		angT = (tc.angCur - 90) * (PI / HALFCIRCUMFERENCE);
		dx = dxy * cos( angT );
		dy = dxy * sin( angT ) * correction;
                if (dysw) dy=-dy;
		/* Move, drawing if pen down, then update position */
		if( tc.fPenDown )
 		   XDrawLine(g_display,g_window,g_gc,tc.xCur,tc.yCur,(int)(tc.xCur + dx+.5),(int)(tc.yCur + dy+.5));
			 
		tc.xCur += dx; 
		tc.yCur += dy; 
		return OnScreen();
}

short nMoveP( double dxy )
{
		double dx, dy;          /* Differences of X and Y */
		double angT;

		/* Calculate new X and Y positions. */
		angT = (tc.angCur - 90) * (PI / HALFCIRCUMFERENCE);
		dx = dxy * cos( angT );
		dy = dxy * sin( angT ) * correction;
		if (dysw) dy=-dy;
		/* Move, drawing if pen down, then update position */
		if( tc.fPenDown ) {
				XDrawLine(g_display,g_window,g_gc,tc.xCur,tc.yCur,(int)(tc.xCur + dx+.5),(int)(tc.yCur + dy+.5));
				}
		else {
				tc.xCur=tc.xCur+dx;
				tc.yCur=tc.yCur+dy;
	        }
				
		return OnScreen();
}

/*Fixed Drawing Pixel Graphics-For drawing letters - Added 3-21-1997 */
short MovePD(short dx,short dy) {
    if (dysw) dy=-dy;
		if( tc.fPenDown ) {
				/*SVGA_line_to_abs(tc.xCur + dx, tc.yCur + dy );*/
				XDrawLine(g_display,g_window,g_gc,tc.xCur,tc.yCur,tc.xCur+dx,tc.yCur+dy);
		}
		/*else
				SVGA_set_loc_abs( tc.xCur + dx, tc.yCur + dy );*/
		tc.xCur += dx;
		tc.yCur += dy;
		return OnScreen();
}


short nMovePD(short dx,short dy)
{
		if (dysw) dy=-dy;
		if( tc.fPenDown ) {
				/*SVGA_line_to_abs(tc.xCur + dx,tc.yCur + dy);
				SVGA_set_loc_abs(tc.xCur,tc.yCur);*/
				XDrawLine(g_display,g_window,g_gc,tc.xCur,tc.yCur,tc.xCur+dx,tc.yCur+dy);
				}
		else {
		                /*SVGA_set_loc_abs(tc.xCur + dx, tc.yCur + dy);*/
		                tc.xCur+=dx;
		                tc.yCur+=dy;
		     }
				
		return OnScreen();
}

short SetEndPix(short dx,short dy,int op)
{
   if (dysw) dy=-dy;
	 if (op==0)
			/*SVGA_plot_pixel(tc.xCur+dx,tc.yCur+dy);*/
			XDrawPoint(g_display,g_window,g_gc,tc.xCur+dx,tc.yCur+dy);
	 if (op==1)
			/*SVGA_plot_pixel(tc.xCur,tc.yCur);*/
			XDrawPoint(g_display,g_window,g_gc,tc.xCur,tc.yCur);
   return(0);
}

void qckprnt(int x,int y,char *wrk, int colr) {
	char buf[80];
	char letter;
	int i;

	PenColor(colr);
	sprintf(buf,"S2BM%d,%d;",x,y);
	draw(buf);
	for (i=0;i<strlen(wrk);i++) {
		letter=toupper(wrk[i]);
		if (letter>',' && letter<=';')
			 draw (nu[letter-43]);
		else if (letter==' '|| letter=='*')
			 draw ("BR4");
		else if (letter==13)
			 draw(nu[59-43]);
		else if (letter=='<')
			 draw ("BU7BR4G4F3BR5");
		else if (letter=='>')
			 draw ("BU6BR4F3G3BR5");
		else
			 draw(az[letter-65]);
	}
}



int draw(char *string) {
	/*DRAWING STRING INTERPRETER*/

	char execstring[20];
	int p=0,numericsw=0,i;
	/*SVGA_line_pattern((unsigned int) 0xffff);*/

	for (i=0;i<=strlen(string);i++) {
		if (isalpha(string[i])) {
			if (numericsw) {
				execstring[p]='\0';
				execdraw(execstring);
				numericsw=0;
				p=0;
			}
			execstring[p]=string[i];
			p++;
		}
		if (isdigit(string[i])|| string[i]==',') {
			execstring[p]=string[i];
			p++;
			numericsw=-1;
		}
	}
	if (numericsw) {
		execstring[p]='\0';
		execdraw(execstring);
		numericsw=0;
	}
	return(0);
}


void execdraw(char *string) {
	/*B N U D L R E F G H Mx,y C S*/
	char execstring[20],operation;
	int x=0,y=0,beginsw=0,noupdatesw=0,success=0,distance=0,i;
	/*clean string to execstring from ';' and ' '*/
	int p=0;
	for (i=0;i<strlen(string);i++) {
		string[i]=toupper(string[i]);
		if (string[i]!=';' && string[i]!=' ') {
			execstring[p]=string[i];
			p++;
		}
	}
	execstring[p]='\0';
	if (strncmp(execstring,"BM",2)==0)
		if(sscanf(execstring,"BM%d,%d",&x,&y)) {
			PenDown(FALSE);
			MoveToP(x,y);
			return;
		}
		else
			/*printf("Illegal draw command: %s\n",execstring);*/
	;
	if (execstring[0]=='B') beginsw=-1;
	if (execstring[0]=='N') noupdatesw=-1;
	/*determine operation*/
	if (beginsw || noupdatesw) {
		operation=execstring[1];
		success=sscanf(execstring,"%*2s%d",&distance);
		
	}
	else {
		operation=execstring[0];
		success=sscanf(execstring,"%*1s%d",&distance);
	}
	if (success) {
		int dx,dy; 
		switch (operation) {
			case 'U':
				dx=0;dy=-distance*sizeconst;
				break;
			case 'D':
				dx=0;dy=distance*sizeconst;
				break;
			case 'L':
				dx=-distance*sizeconst;dy=0;
				break;
			case 'R':
				dx=distance*sizeconst;dy=0;
				break;
			case 'E':
				dx=distance*sizeconst;dy=-distance*sizeconst;
				break;
			case 'F':
				dx=distance*sizeconst;dy=distance*sizeconst;
				break;
			case 'G':
				dx=-distance*sizeconst;dy=distance*sizeconst;
				break;
			case 'H':
				dx=-distance*sizeconst;dy=-distance*sizeconst;
				break;
			case 'C':
				/*setcolor(distance);*/
				PenColor(distance);
				return;
				break;
			case 'S':
				sizeconst=distance;
				return;
				break;
			default:
				/*printf("Illegal Draw Command %s\n",execstring);*/
				success=0;
		}
		if (success)
			if (beginsw) {
				PenDown(FALSE);
				MovePD(dx,dy);
				}
			else if (noupdatesw) {
					PenDown(TRUE);
					nMovePD(dx,dy);
					SetEndPix(dx,dy,0);
					}

			else {
					PenDown(TRUE);
					MovePD(dx,dy);
					SetEndPix(dx,dy,1);
			}

		else
			;/*printf("Illegal Draw Command %s\n",execstring);*/
		return;
	}
	return;
}

int getmaxx() {
  return(WIDTH);
}

int getmaxy() {
  return(HEIGHT);
}

void drawloc(int x, int y, int color) {
  char buf[80];
  PenColor(12);
  sprintf(buf,"BM%d,%dNU2NR2ND2NL2",x,y);
  draw(buf);
}
  

void drawwind(int x,int y,int dir,int speed,int color) {
	int dirwork,rightdigit,fifties,knots,tens,i;
	if (speed==0) return;
	/*Draw + sign */
	PenColor(12);
	PenDown(FALSE);
	MoveToP(x,y);
	PenDown(TRUE);
	TurnTo(0);
	nMoveP(2.0);
	TurnTo(180);
	nMoveP(2.0);
	TurnTo(270);
	nMoveP(2.0);
	TurnTo(90);
	nMoveP(2.0);
	/*Draw Direction Stick*/
	PenColor(color);
	TurnTo(dir);
	MoveP(25.0);
	rightdigit=speed % 10;
	if (rightdigit > 7) knots=speed-speed % 10 + 10;
	if (rightdigit > 2 && rightdigit<8) knots=speed - speed % 10 + 5;
	if (rightdigit < 3) knots=speed-speed%10;
	if (knots>=50) {
  	  fifties=knots/50;
	  for (i=1;i<=fifties;i++) {
	    /*DrawFlag*/
	    /* a. Draw left part of flag */
	   
           dirwork=dir+90;
	   if (dirwork>360)  dirwork-=360;
	   TurnTo(dirwork);
	   MoveP(12.0);
	   /* b. Draw right part of flag */
	   
           dirwork=dir+90+150;
	   if (dirwork>360)  dirwork-=360;
	   TurnTo(dirwork);
	   
           nMoveP(13.8);
	   /*Flag 1 complete*/
	   /*Now reposition for next flag */
	   PenDown(FALSE);
	   MoveToP(x,y);
	   TurnTo(dir);
	   PenDown(TRUE);
	   MoveP(25.0);
	   Turn(180);
	   MoveP(12*i); /* Change 10 to 12 */
	   knots-=50;
 	 }
	 TurnTo(dir);
        }
	if (knots<50) {
	  tens=(knots - knots % 10)/10;
	  for(i=1;i<=tens;i++) {
	    /* NR15D2 */
	   
           Turn(60);
	   nMoveP(12.0);
          
	   Turn(120);
	   MoveP(5.0); /* Change 3 to 5 */
	   Turn(-180);
	  }
	  if ((knots % 10)==5) {
	   if(speed<=7) MoveP(-5.0);  /* Change 3 to 5 */
	   /* NR8 */
	   
           Turn(60);
	   nMoveP(6.0);
	   Turn(-60);
         }
       }
}

