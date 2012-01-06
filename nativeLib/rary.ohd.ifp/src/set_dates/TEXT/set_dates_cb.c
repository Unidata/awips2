#include "text_dates.h"
#include "ifp_atoms.h"
#include "libXifp.h"

extern MYDATETYPE Adate[3], *pAdate[2];  
extern Widget UpArrow, DownArrow;
extern SelFlag;

void  initializeDate( int indx)
{

 int i, n;

 char ss[10];




    init_AdateGC ( pAdate[indx]->w[indx], indx );

    pAdate[indx]->fieldFlag = 99;

    for ( i = 0; i < NBUTTONS; i++) {

        SetColor( pAdate[indx]->w[i],  pAdate[indx]->fg);

    }


   
    XmTextSetString(pAdate[indx]->w[0],  pAdate[indx]->sMonth);

    sprintf(ss,"%d", pAdate[indx]->day);
    XmTextSetString(pAdate[indx]->w[1], ss);

    sprintf(ss,"%d", pAdate[indx]->year);
    XmTextSetString(pAdate[indx]->w[2], ss);

    sprintf(ss,"%d", pAdate[indx]->hour);
    XmTextSetString(pAdate[indx]->w[3], ss);

}


int isCurTimeInvalid() {

   char *pbuf;

   int imonth, iday, iyear;
   int ihour;

   int n, ierror = 0;
   
   for ( n = 0; n < 2; n++) {

    /* Validate Month */
    pbuf = XmTextGetString(pAdate[n]->w[0]);
    
    if ( isAllChars(pbuf)   != 1 ) ierror = 1;
    else if ( isValidMonth(pbuf) != 1 ) ierror = 1;
    else imonth = GetMonthNumbyStr(pbuf);	
    
    
    /* Validate Day */
    pbuf = XmTextGetString(pAdate[n]->w[1]);
    if ( isAllDigits(pbuf) != 1) ierror = 2; 
    else
    iday = atoi(pbuf);

    /* Validate Year */
    pbuf = XmTextGetString(pAdate[n]->w[2]);
    if ( isAllDigits(pbuf) != 1) ierror = 3; 
    else
    iyear = atoi(pbuf);

    /* Validate Hour*/
    pbuf = XmTextGetString(pAdate[n]->w[3]);
    if ( isAllDigits(pbuf) != 1) ierror = 4; 
    else if (ihour = atoi(pbuf) > 24) ierror = 4;
    else
    ihour = atoi(pbuf);
    

    if ( ierror == 1) {
       ShowErrorDialog(pAdate[n]->w[0], "*** INVALID MONTH ***");
    } else if ( ierror == 2) {
       ShowErrorDialog(pAdate[n]->w[1], "*** INVALID DAY   ***");
    } else if ( ierror == 3) {
       ShowErrorDialog(pAdate[n]->w[2], "*** INVALID YEAR  ***");
    } else if ( ierror == 4) {
       ShowErrorDialog(pAdate[n]->w[3], "*** INVALID HOUR   ***");
    } else {
        pAdate[n]->month = imonth;
	pAdate[n]->year = iyear;
	pAdate[n]->day  = iday;
	pAdate[n]->hour = ihour;
    }

    }
  

    
    return ierror;
}

void AddAllCallBacks(int indx) 
{

   int i, n;

   for ( n = 0; n < 2; n++)  {
      for ( i = 0; i < NBUTTONS; i++)  
      {
         XtAddCallback(pAdate[indx]->w[i],  XmNactivateCallback, TextWGetTextCB, (XtPointer *)i);
         XtAddCallback(pAdate[indx]->w[i],  XmNfocusCallback,    TextWClickCB,   (XtPointer *)i);
      }

   }

}

void ArrowUpCB (  Widget w, XtPointer client, XtPointer data ) {

   int n = SelFlag;
   
   XmArrowButtonCallbackStruct *cbs =  (XmArrowButtonCallbackStruct *) data; 
   if ( cbs->reason == XmCR_ARM) return;

   
 
    
  /* ==== Time is not valid  ===== */
  
  if ( isCurTimeInvalid() != 0) 
  {
      return;
  }
  
   switch (pAdate[n]->fieldFlag ) {

      	case 0:
	pAdate[n]->month++;
	
	break;
      	case 1:
	pAdate[n]->day++;
	
	break;
      	case 2:
	pAdate[n]->year++;
	
	break;
      	case 3:
	pAdate[n]->hour++;
	
	break;
        default:
	break;
   }

 
  UpdateTimeChange(n);
  
 
}


void UpdateDates(Widget w)

{
   date            mydate, 
                  *the_date    = &mydate,
                  *end_obs_date= &mydate,
                  *end_date    = &mydate,
   		  *start_date  = &mydate; 
   int i;
   
   int             type;   /* type of data stored in the window property */
   int             format; /* format of the stored data */
   int             nitems; /* number of bytes retrieved */
   int             left;   /* remaining bytes stored in the window */
   long            offset = 0;
   Display         *display;
   Window          root;

   display = XtDisplay(w);
   root = DefaultRootWindow(display);
   
   if(XGetWindowProperty(
				display,
				root,
				IFPA_display_start_date,
				offset,
				(long) sizeof(date),
				FALSE,
				(Atom)IFPA_display_start_date_type,
				(Atom *)&type,
				(int *)&format,
				(unsigned long *)&nitems,
				(unsigned long *)&left,
				(unsigned char **)&start_date
				) == Success && type == IFPA_display_start_date_type)
  
   for (i = 0 ; i < 2; i++)
   {
   switch (i)
   {
     case 0:
    
          end_obs_date->month = pAdate[i]->month;
          end_obs_date->day = pAdate[i]->day;
          end_obs_date->year = pAdate[i]->year;
          end_obs_date->hour = pAdate[i]->hour;
          strcpy(end_obs_date->time_zone , start_date->time_zone);
          XChangeProperty
		(
		display,
		root,
		IFPA_display_end_obs_date,
		IFPA_display_end_obs_date_type,
		8,
		PropModeReplace,
		(unsigned char *)end_obs_date,
		sizeof(date)
		);
                
       
          break;
     case 1:

 
         end_date->month = pAdate[i]->month;
         end_date->day = pAdate[i]->day;
         end_date->year = pAdate[i]->year;
         end_date->hour = pAdate[i]->hour;
         strcpy(end_date->time_zone , start_date->time_zone);
         XChangeProperty
		(
		display,
		root,
		IFPA_display_end_date,
		IFPA_display_end_date_type,
		8,
		PropModeReplace,
		(unsigned char *)end_date,
		sizeof(date)
		);
     
         break;
     default:
         break;
     }
   }

}
void ArrowDownCB (  Widget w, XtPointer client, XtPointer data ) {

   int n = SelFlag;

   XmArrowButtonCallbackStruct *cbs =  (XmArrowButtonCallbackStruct *) data; 
   if ( cbs->reason == XmCR_ARM) return;
   
  /* wait(1500);*/

   /* Time is not valid  */
   if ( isCurTimeInvalid() != 0) 
   {
      printf("Set Dates input ERROR\n");
      return;
   }

   switch (pAdate[n]->fieldFlag ) {
      	case 0:
	pAdate[n]->month--;
	
	break;
      	case 1:
	pAdate[n]->day--;
	
	break;
      	case 2:
	pAdate[n]->year--;
	
	break;
      	case 3:
	pAdate[n]->hour--;
	
	break;
        default:
	break;
   }

   UpdateTimeChange(n);
   
}

char *GetMonthByNum( int n ) 
{

   char *sMonths[] = {"Jan","Feb","Mar","Apr","May","Jun", 
                  "Jul","Aug","Sep","Oct","Nov","Dec"};

   return (sMonths[n-1]);

}


int GetMonthNumbyStr ( char *p ) {

   int n, m;

   char *s[] = { "JAN","FEB","MAR","APR","MAY","JUN", 
                  "JUL","AUG","SEP","OCT","NOV","DEC"};

   for ( n = 0; n < sizeof(p) ; n++) {
	p[n] = toupper(p[n]);
   }
   

   for ( m = 0; m < 2; m++) {
   for ( n = 0; n < 12; n++) {
	
      if ( strcmp(p,s[n]) == 0) { 
         return (n+1);
      }

   }
   }

   return 0;


}
void UpdateTimeChange(int n)
{
   char buf[80], ss[10];

   char *sMon[] = { "Jan","Feb","Mar","Apr","May","Jun", 
                  "Jul","Aug","Sep","Oct","Nov","Dec"};

   int  y,m,d,h;

   struct  tm tm;
        
   time_t  ttime;

   memset (&tm, 0, sizeof (tm));
        
   tm.tm_year  = pAdate[n]->year  - 1900;
   tm.tm_mon   = pAdate[n]->month - 1;
   tm.tm_mday  = pAdate[n]->day;
   tm.tm_hour  = pAdate[n]->hour;
   tm.tm_min   = 0;
   tm.tm_sec   = 0;
   tm.tm_isdst = -1;

   ttime  = mktime(&tm);
   /*
   // =========================================================;
   // ====== D O N O T  U S E  G M TIME IN STRFTIME ===========;
   //strftime(buf, sizeof(buf), "%Y %m %d %H",  gmtime(&ttime));
   // =========================================================;
   */
   strftime(buf, sizeof(buf), "%Y %m %d %H",  localtime(&ttime));

   tm.tm_hour+=1;
	
   sscanf(buf,"%d %d %d %d",&y,&m,&d,&h);
   pAdate[n]->year  = y;
   pAdate[n]->month = m; 
   pAdate[n]->day   = d; 
   pAdate[n]->hour  = h; 
   /*
   // ============================================
   // ============ 24 hour can be entered ========
   // ============================================
   */
   if ( pAdate[n]->hour == 0) {
      pAdate[n]->hour = 24;
      pAdate[n]->day-=1;
   }
   if (  pAdate[n]->day == 0) { 
      d = day_in_month( pAdate[n]->year, pAdate[n]->month);
      pAdate[n]->day = d; 
   }

   sprintf(ss,"%s", sMon[pAdate[n]->month-1]);
   XmTextSetString(pAdate[n]->w[0], ss);

   sprintf(ss,"%d", pAdate[n]->day);
   XmTextSetString(pAdate[n]->w[1], ss);

   sprintf(ss,"%d", pAdate[n]->year);
   XmTextSetString(pAdate[n]->w[2], ss);

   sprintf(ss,"%d", pAdate[n]->hour);
   XmTextSetString(pAdate[n]->w[3], ss);
   
   

}

void TextWClickCB (  Widget w, XtPointer client, XtPointer data ) {

   
   ResetColor( w);

}

void TextWGetTextCB (  Widget w, XtPointer client, XtPointer data ) {

   printf("Pressed Enter from Text Window ....\n");
   printf("Text: %s\n", XmTextGetString(w));

}
void PushCB ( Widget w, XtPointer client, XtPointer data ) {

   printf("Button Press....\n");
}


/* *************************************************** */
/*      Get color index based on color's name          */
/* *************************************************** */
int getMyColor ( Display *display, char *colorName)
{
	XColor          xcolor,
			unused;
			

	Colormap        cmap;

	cmap    = XDefaultColormap(display,DefaultScreen(display));

	XAllocNamedColor(display, cmap, colorName, &xcolor, &unused);

	return (xcolor.pixel);
}


void init_AdateGC (Widget w, int n) {

 pAdate[n]->bg    = getMyColor(XtDisplay(w),"Gray");
 pAdate[n]->fg    = getMyColor(XtDisplay(w),"LightGray");

}


void SetColor (Widget w, int color ) {

   int n;

   Arg args[10];

   n = 0;
   XtSetArg(args[n], XmNbackground, color);n++;
   XtSetValues(w, args, n );

}

/* ******************************************************** */
/* Reset to original background color. except one is active */
/* ******************************************************** */
void ResetColor( Widget w) {

int m, n;

 for ( m = 0; m < 2; m++)
 for ( n = 0; n < NBUTTONS; n++) {

    if (pAdate[m]->w[n] == w) {
        SetColor(pAdate[m]->w[n], pAdate[m]->bg);
        pAdate[m]->fieldFlag = n;
	SelFlag = m;
    } else {

        SetColor(pAdate[m]->w[n], pAdate[m]->fg);
    }
 }


}


static int mtbl [] = { 0,31,59,90,120,151,181,212,243,273,304,334,365 };

/* ***************************************************** */
/* check for valid day in month when increment/decrement */
/* ***************************************************** */
int day_in_month ( int year, int month )
{

	int days;
	days = mtbl[month] - mtbl[month-1];
	if ( month == 2 && year % 4 == 0 && ( year % 100 != 0 || year % 400 == 0))
		days++;
	return ( days );
}

/* *************************************************** */
/* check for valid day number when increment/decrement */
/* *************************************************** */
int day_number(int year, int month, int day )
{
	long days_ctr;

	year-=1900;
	days_ctr = ((long)year * 365L) + (( year + 3) / 4);
	days_ctr+=mtbl[month-1] + day + 6;
	if ( month > 2 && (year % 4 == 0 ))
		days_ctr++;

	return ( (int) (days_ctr % 7L));
}



int isValidMonth ( char *p ) {

   int n, m;

   char *s[] = { "JAN","FEB","MAR","APR","MAY","JUN", 
                  "JUL","AUG","SEP","OCT","NOV","DEC"};

   for ( n = 0; n < sizeof(p) ; n++) {
	p[n] = toupper(p[n]);
   }
   

   for ( m = 0; m < 2; m++) {
   for ( n = 0; n < 12; n++) {
	
      if ( strcmp(p,s[n]) == 0) { 
         return 1;
      }

   }
   }

   return 0;


}

int isAllChars ( char *pC) {

   int n;

   if ( pC == NULL) return 0;

   for ( n = 0; n < strlen(pC); n++) {

      if ( ! isalpha(pC[n]) ) return 0;
   }

   return 1;

}

int isAllDigits( char *pC) {

   int n;

   if ( pC == NULL) return 0;

   for ( n = 0; n < strlen(pC); n++) {
      if ( ! isdigit(pC[n]) ) return 0;
   }

   return 1;
}

void ShowErrorDialog( Widget widget, char *msg )
{

	static Widget		msgBox;
	Arg			arg[4];
	int			ac;
	
	
	if ( !  msgBox )
	{
		ac = 0;
		XtSetArg(arg[ac], XmNtitle, "Error Dialog"); ac++;
		XtSetArg(arg[ac], XmNdialogType, XmDIALOG_ERROR); ac++;
		XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++; 
		
		msgBox = XmCreateMessageDialog(widget, "msgBox", arg, ac);
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));
	

	}

	ac = 0;
	XtSetArg(arg[ac], XmNmessageString,
		 XmStringCreateLtoR(msg, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); ac++;
	XtSetValues(msgBox, arg, ac);
	
	
	XtManageChild(msgBox);
	XtPopup(XtParent(msgBox), XtGrabNone);

	return;	

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/set_dates/RCS/set_dates_cb.c,v $";
 static char rcs_id2[] = "$Id: set_dates_cb.c,v 1.3 2006/03/29 14:27:15 aivo Exp $";}
/*  ===================================================  */

}



