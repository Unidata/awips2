/*
	File:		e19a_show.c
	Date:		4/17/1997
	Author:		Paul Taylor

	Purpose:	Provides support for the E-19A Report.
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include <datetime.h>
#include <time.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"

#include "GeneralUtil.h"

#include "textrept.h"
#include "textrept_show.h"
#include "e19_show.h"
#include "e19a_show.h"

#include "cvt_latlon.h"


#define	DEBUG_MODE	0	/* 0=False, 1=True */

/*
	Globals.
*/
static e19a_info_type*	e19aInfoPtr = NULL;

static char
   e19a_hdr_summary[]    ="                           U.S. DEPARTMENT OF COMMERCE           NWS-FORM E-19A";

static int		e19a_first_page = E19A_SUMMARY;
static int		e19a_last_page = E19A_SUMMARY;

/*
	Functions.
*/
void	E19A_AddCallbacks(void)
{
   return;
}

//---------------------------------------------------------

void	E19A_RemoveCallbacks(void)
{
   return;
}

//---------------------------------------------------------

char*	E19A_PE_Manager(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;

   /*
   	Return the complete E-19A text.
   */
   text = E19A_GetText(E19A_ALLPAGES, TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE);
   
   if ( bufInfoPtr != NULL )
   {
      Text_AddString(&bufInfoPtr->e19a_buf, text, 1);
      Text_FreeString(&text);
      return(bufInfoPtr->e19a_buf);
   }
   else
   {
      return NULL ;
   }
   
}
//---------------------------------------------------------

void	E19A_PE_Print(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);
	
	buf = E19A_PE_Manager();	  /* obtain complete text buffer */

        /* output buffer to printer */
	if ( buf != NULL ) TextReports_P_OutputBuffer ( buf ) ;
	
	UnsetCursor(textreptFO);
	
	return;
}
//---------------------------------------------------------

	      
void	E19A_PE_Email(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);

	buf = E19A_PE_Manager();	  /* obtain complete text buffer */

        /* email buffer to internet address */
	if ( buf != NULL ) TextReports_E_OutputBuffer ( buf ) ;
	
	UnsetCursor(textreptFO);

	return;
}
//---------------------------------------------------------

	      
void	E19A_LoadTextWidget(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;
   

   /*
   	Get the complete E19 text
	and display it.
   */
   text = E19A_GetText(E19A_ALLPAGES, TEXTREPORTS_REASON_NORMAL);

   if ( bufInfoPtr != NULL )
   {
      if ( text != NULL )
      {
         Text_AddString( & bufInfoPtr->e19a_buf , text, 1 ) ;
         Text_FreeString(&text);
      }

      if (bufInfoPtr->e19a_buf != NULL)
         XtVaSetValues(textreptTE, XmNvalue, bufInfoPtr->e19a_buf, NULL);
      else
         fprintf(stderr,"ERROR: Unable to load text properly...\n");
   }
   
   return;
}
//---------------------------------------------------------


char*	E19A_GetText(int page, int reason)
{
   e19a_info_type	*info = get_E19A_Info(False);
   char*  		text = NULL;

   switch(page)
   {
      case E19A_ALLPAGES:	text = E19A_AllPages	(reason);	break;

      case E19A_SUMMARY:	text = E19A_Summary	(info, reason);	break;

      default:	fprintf(stderr, "ERROR: Could not get text...\n");
	 	break;
   }

   return(text);
}
//---------------------------------------------------------


char*	E19A_AllPages(int reason)
{
   char*  alltext = NULL;
   char*  text = NULL;
   
   int first = 0 ;
   int i ;

   /*
   	Get the text for the given page(s).
   */
   for(i=e19a_first_page; i<=e19a_last_page; i++)
   {
      text = E19A_GetText(i, reason);

      if ( text != NULL )
      {
         Text_Paginate(&text);
      
         if ( first ==  0 )
         {
      	    Text_AddString(&alltext, text, 1);
            first = 1 ;
         }
         else
         {
	    Text_AddString(&alltext, text, 0);
         }

         Text_FreeString(&text);
      }
   }

   return(alltext);
}
//---------------------------------------------------------


char*	E19A_Summary(e19a_info_type *info, int reason)
{
   Gage		*gage = NULL,
      		*gagePtr = NULL;
   
   char		where[BUFSIZ];
   char*	buf = NULL;
   char*	remark = NULL;
   
   char		tmp1[E19A_BUFSIZ];
   char		tmp2[E19A_BUFSIZ];
   char		tmp3[E19A_BUFSIZ];
   char		tmp4[E19A_BUFSIZ];
   char		tmp5[E19A_BUFSIZ];
   char		tmp6[E19A_BUFSIZ];
   char		tmp7[E19A_BUFSIZ];
   char		tmp8[E19A_BUFSIZ];
   char		tmp9[E19A_BUFSIZ];
   
   time_t	curr_time;
   struct tm*	tm_ptr;
   long		current_year;
   char		ten_years_ago[BUFSIZ];
   char		zero_years_ago[BUFSIZ];
   
   int		i;

   
   /*
   	Get current time and convert to a YYYY value.
   */
   time(&curr_time);
   tm_ptr = localtime(&curr_time);
   strftime(tmp1, 4+1, "%Y", tm_ptr);
   current_year = atol(tmp1);

   /*
   	Title
   */
   memset ( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1, "%s", e19a_hdr_summary);
   strcat (tmp1, "\n");
   strcat (tmp1, "                 NATIONAL OCEANIC AND ATMOSPHERIC ");
   strcat (tmp1, "ADMINISTRATION\n");
   strcat (tmp1, "                            NATIONAL WEATHER SERVICE\n\n");
   strcat (tmp1, "			  REPORT ON RIVER GAGE STATION\n\n");
   strcat (tmp1, "------------------------------------  SITE  --------------");
   strcat (tmp1, "----------------------\n\n");
   Text_AddString(&buf, tmp1, 1);   
   
   
   memset ( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset ( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset ( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset ( ( void * ) tmp4, '\0', sizeof(tmp4));
   sprintf(tmp1,"         LID: %-11s %10s PROXIMITY: %-s\n",
	   info->loc != NULL ? info->loc->lid : "" , 
           " ", 
           info->descr != NULL ? info->descr->proximity : "" ) ;
   sprintf(tmp2,"        NAME: %-s\n      STREAM: %-s\n",
	   info->loc != NULL ? info->loc->name : "" , 
           info->river != NULL ? info->river->stream : "" ) ;
   
   sprintf(tmp4, "%-s, %-s", 
           info->loc ? info->loc->county : "" , 
           info->loc ? info->loc->state : "" ) ;
   sprintf(tmp3,"COUNTY/STATE: %-26s BASIN: %-30s\n\n",
	   tmp4, 
           info->loc ? info->loc->rb : "" ) ;
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);

   
   memset ( ( void * ) tmp8, '\0', sizeof(tmp8));
   memset ( ( void * ) tmp9, '\0', sizeof(tmp9));

   if ( info->river != NULL )
   {
      DataToString(&info->river->da, DOUBLE, tmp8, "%9.2lf", "         ");
      DataToString(&info->river->fs, DOUBLE, tmp9, "%8.2lf", "        ");
   }
   else
   {
      strcpy ( tmp8 , "         " ) ; 
      strcpy ( tmp9 , "        " ) ; 
   }

   sprintf(tmp1,"  DRAINAGE:%s          FLOOD STAGE: %s      ",
	   tmp8, tmp9);
   sprintf(tmp2,"STATION NO: %-11s\n",
	   info->loc ? info->loc->sn : "" ) ;
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   
   memset ( ( void * ) tmp8, '\0', sizeof(tmp8));
   memset ( ( void * ) tmp9, '\0', sizeof(tmp9));
  
   if ( info->river != NULL )
   {
      DataToString(&info->river->mile, DOUBLE, tmp8, "%8.2lf", "        ");
      DataToString(&info->river->wstg, DOUBLE, tmp9, "%8.2lf", "        ");
   }
   else
   {
      strcpy ( tmp8 , "        " ) ;
      strcpy ( tmp9 , "        " ) ;
   }

   sprintf(tmp1,"RIVER MILE: %s         ACTION STAGE: %s      ",
	   tmp8, tmp9);
   sprintf(tmp2,"   USGS NO: %-11s\n",
	   info->river != NULL ? info->river->gsno : "" ) ;
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);

   
   memset ( ( void * ) tmp8, '\0', sizeof(tmp8));
   memset ( ( void * ) tmp9, '\0', sizeof(tmp9));

   if ( info->river != NULL )
   {
      DataToString(&info->river->zd, DOUBLE, tmp8, "%8.3lf", "        ");
      DataToString(&info->river->bf, DOUBLE, tmp9, "%8.2lf", "        ");
   }
   else
   {
      strcpy ( tmp8 , "        " ) ;
      strcpy ( tmp9 , "        " ) ;
   }

   sprintf(tmp1,"ZERO DATUM: %s       BANKFULL STAGE: %s      ",
	   tmp8, tmp9);
   sprintf(tmp2,"   NESS ID: %-8s\n",
	   info->dcp != NULL ? info->dcp->goes : "" ) ;
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   
   memset ( ( void * ) tmp8, '\0', sizeof(tmp8));
   memset ( ( void * ) tmp9, '\0', sizeof(tmp9));

   if ( info->river != NULL )
   {
      DataToString(&info->river->cb,   DOUBLE, tmp8, "%8.3lf", "        ");
      DataToString(&info->river->pool, DOUBLE, tmp9, "%8.2lf", "        ");
   }
   else
   {
      strcpy ( tmp8 , "        " ) ;
      strcpy ( tmp9 , "        " ) ;
   }
   sprintf(tmp1,"  CHECKBAR: %s          NORMAL POOL: %s      ",
	   tmp8, tmp9);
   sprintf(tmp2,"       RFC: %-6s\n",
	   info->loc != NULL ? info->loc->rfc : "" ) ;
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);

   if ( info->river != NULL )
   {
      sprintf(tmp1 , "  LATITUDE: %-10s      TIDAL EFFECTS: %-9s     " ,
   	      cvt_latlon_from_double ( info->river->lat ) ,
              info->river->tide ) ;
   }
   else
   {
      sprintf(tmp1,"  LATITUDE: %-10s      TIDAL EFFECTS: %-9s     ",
	      "          " , 
              "         " ) ;
   }

   sprintf(tmp2,"       HSA: %-4s\n",
	   info->loc != NULL ? info->loc->hsa : "" ) ;

   if ( info->fcat != NULL )
   {
      DataToString(&info->fcat->major_stage,    DOUBLE, tmp7,
                   "%8.2lf", "        ");
      DataToString(&info->fcat->moderate_stage, DOUBLE, tmp8, 
                   "%8.2lf", "        ");
      DataToString(&info->fcat->minor_stage,    DOUBLE, tmp9, 
                   "%8.2lf", "        ");
   }
   else
   {
      strcpy ( tmp7 , "        " ) ;
      strcpy ( tmp8 , "        " ) ;
      strcpy ( tmp9 , "        " ) ;
   }

   if ( info->river != NULL )
   {
      sprintf(tmp3," LONGITUDE: %-10s          FLOODCATS:    MAJOR: %-s\n",
	      cvt_latlon_from_double(info->river->lon), tmp7);
   }
   else
   {
      sprintf(tmp3," LONGITUDE: %-10s          FLOODCATS:    MAJOR: %-s\n",
	      "          " , tmp7 ) ;
   }

   sprintf(tmp4," %30s            MODERATE: %-s\n", " ", tmp8);
   sprintf(tmp5,"       %-10s %-21s       MINOR: %-s\n",
	   " ", " ", tmp9);
   sprintf(tmp6,"PERIOD OF RECORD: %-31s\n\n" , 
           info->river != NULL ? info->river->por : "" ) ;
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);
   Text_AddString(&buf, tmp6, 0);

   
   sprintf(tmp3,"----------------------------------  OBSERVER  --------------");
   strcat (tmp3,"--------------------\n\n");
   Text_AddString(&buf, tmp3, 0);

   
   memset ( ( void * ) tmp4, '\0', sizeof(tmp4));

   if ( ( info->obs != NULL ) && ( strlen(info->obs->firstname) > 0 ) )
   {
      strcat(tmp4, info->obs->firstname);
      strcat(tmp4, " ");
   }

   if ( ( info->obs != NULL ) && ( strlen(info->obs->lastname) > 0 ) )
   {
      strcat(tmp4, info->obs->lastname);
   }

   memset ( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1,"  %-40s\n",
	   tmp4);
   
   if ( info->obs != NULL )
   {
      date_t_to_USA_date ( info->obs->dos, tmp9 );
      sprintf(tmp2,"  %-30s  SERVICE DATE: %-10s    SPONSOR: %-8s\n",
	      info->obs->a1, tmp9, info->obs->spons);
   }
   else
   {
      sprintf(tmp2,"  %-30s  SERVICE DATE: %-10s    SPONSOR: %-8s\n",
	      " " , 
              " " ,
              " " ) ;
   }

   memset ( ( void * ) tmp9, '\0', sizeof(tmp9));

   if ( info->obs != NULL )
   {
      DataToString(&info->obs->rate, DOUBLE, tmp9, "%7.2lf", "       ");
      sprintf(tmp3,"  %-30s        CD-404: %-9s        RATE: $%s\n",
	      info->obs->a2, info->obs->ornr, tmp9);
      memset ( ( void * ) tmp4, '\0', sizeof(tmp4));
      sprintf(tmp4,"  %-30s    HOME PHONE: %-18s\n",
   	      info->obs->a3, info->obs->hphone);
      sprintf(tmp5,"  %-30s    WORK PHONE: %-18s\n",
	      " ", info->obs->phone);
      sprintf(tmp6,"  %-s  %-s  %-s",
	      info->obs->city, info->obs->state, info->obs->zip);
      sprintf(tmp7,"%-43s\n\n",
	      tmp6);
      sprintf(tmp9,"    EMAIL: %-s\n",
	      info->obs->email);
      sprintf(tmp8,"   DUTIES: %-s\n"
	      "RECIPIENT: %-16s   COMMS TYPE: %-11s      TASK: %-14s\n\n",
	      info->obs->rprt, info->obs->recip, info->obs->comm, info->obs->tsk);
   }
   else
   {
      sprintf ( tmp3 , "  %-30s        CD-404: %-9s        RATE: $%s\n" ,
	      " " , 
              " " , 
              " ") ;
      sprintf ( tmp4 , "  %-30s    HOME PHONE: %-18s\n" ,
	      " " , 
              " " ) ;
      sprintf(tmp5 , "  %-30s    WORK PHONE: %-18s\n" ,
	      " " , 
              " " ) ;
      sprintf(tmp6 , "  %-s  %-s  %-s" ,
	      " " , 
              " " , 
              " " ) ;
      sprintf(tmp7 , "%-43s\n\n" ,
	      tmp6);
      sprintf(tmp9,"    EMAIL: %-s\n",
	      " ");
      sprintf(tmp8,"   DUTIES: %-s\n"
	      "RECIPIENT: %-16s   COMMS TYPE: %-11s      TASK: %-14s\n\n",
	      " " , 
              " " , 
              " " , 
              " " ) ;
   }

   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0); /* omitted tmp6 */
   Text_AddString(&buf, tmp7, 0);
   Text_AddString(&buf, tmp9, 0);
   Text_AddString(&buf, tmp8, 0);
   
   sprintf(tmp1,"------------------------------------  GAGES  --------------");
   strcat (tmp1,"---------------------\n\n");

   if ( info->telem != NULL )
   {
      sprintf(tmp2,"TELEM TYPE: %-11s%5sTELEM OWNER: %-11s     PHONE: %-13s\n",
	      info->telem->type, " ", info->telem->owner, info->telem->phone);
   }
   else
   {
      sprintf(tmp2,"TELEM TYPE: %-11s%5sTELEM OWNER: %-11s     PHONE: %-13s\n",
	      " " , " ", " " , " " ) ;
   }

   if ( info->dcp != NULL )
   {
      sprintf(tmp3,"    DCP ID: %-8s%10sDCP OWNER: %-s\n\n",
	      info->dcp->goes, " ", info->dcp->owner);
   }
   else
   {
      sprintf(tmp3,"    DCP ID: %-8s%10sDCP OWNER: %-s\n\n" ,
	      " " , " " , " " ) ;
   }

   sprintf(tmp4,"     LATEST GAGE TYPE           START DATE              ");
   strcat (tmp4,"OWNER OF GAGE\n");
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   
   sprintf(where, " WHERE lid = '%s' and gend is null ORDER BY gbegin desc ",
	   TextReports_getCurrentLid());
   if ((gage = GetGage(where)) != (Gage *) NULL)  /* print gage if present */
   {
      gagePtr = (Gage *) ListFirst(&gage->list);
      
      if (gagePtr)
      {
         date_t_to_USA_date ( gagePtr->gbegin, tmp4);
	 sprintf(tmp1,"       %-11s                %10s              %-11s\n",
		 gagePtr->type, tmp4, gagePtr->owner);
	 Text_AddString(&buf, tmp1, 0);
	 
	 FreeGage(gagePtr);
      }
   }
   Text_AddString(&buf, "\n", 0);
   
   sprintf(tmp1,"-----------------------------------  CRESTS  --------------");
   strcat (tmp1,"---------------------\n\n");
   
   sprintf(tmp2,"              %30s LEVEL       DATE\n",
	   " ");
   
   sprintf(tmp3,"    HIGHEST BASED ON GAGE READING:         %s",
	   E19_HighestCrest(E19_GAGE_READING_CREST, NULL));
	   
   sprintf(tmp4," HIGHEST BASED ON HIGH WATERMARKS:         %s",
	   E19_HighestCrest(E19_HIGH_WATERMARKS_CREST, NULL));
   
   sprintf(ten_years_ago, "1/01/%-ld", current_year - 10);
   sprintf(tmp5,"         HIGHEST SINCE %10s:         %s",
	   ten_years_ago, E19_HighestCrest(E19_TIME_CREST,
					   &ten_years_ago[0]));
   
   sprintf(zero_years_ago,"1/01/%-ld", current_year);
   sprintf(tmp6,"         HIGHEST SINCE %10s:         %s\n",
	   zero_years_ago, E19_HighestCrest(E19_TIME_CREST,
					    &zero_years_ago[0]));
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);
   Text_AddString(&buf, tmp6, 0);
   
   
   memset ( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1,"-----------------------------------  REMARKS  -------------");
   strcat (tmp1,"---------------------\n\n");
   
   if ( info->river != NULL )
   {
      Text_AddString( & remark ,
		     Text_WordWrap ( info->river->remark , 80 , 0 ) ,
		     1 ) ;
   }

   Text_AddString ( &buf , tmp1 , 0 ) ;
   Text_AddString ( & buf , remark , 0 ) ;
   Text_AddString ( & buf , "\n\n\n" , 0 ) ;
   Text_FreeString ( & remark ) ;

   /* try to place FOOTER at the bottom */
   memset ( ( void * ) tmp2, '\0', sizeof(tmp2));
   for(i=0; i < (59 - Text_CountNewLines(buf)); i++)
   {
      strcat (tmp2, "\n");
   }

   Text_AddString(&buf, tmp2, 0);
   memset ( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset ( ( void * ) tmp4, '\0', sizeof(tmp4));
   memset ( ( void * ) tmp5, '\0', sizeof(tmp5));

   if ( info->river != NULL )
   {
      /* Modified by BryonL to use date_t_to_USA_date on 1/26/2005. */
      date_t_to_USA_date ( info->river->rrevise, tmp4 );
   }
   
   sprintf(tmp3, "HYDROLOGIST: %-32s  REVISED, PRINTED DATES: %-10s, %-10s\n" ,
	   ( char * ) TextReports_GetHydrologistName ( ) ,
           tmp4 , 
           ( char * ) TextReports_GetDate ( ) ) ;
   strcat (tmp5, tmp3);
   Text_AddString(&buf, tmp5, 0);
   return(buf);
}
//---------------------------------------------------------


e19a_info_type*		get_E19A_Info(int init_flag)
{
   e19a_info_type*	infoPtr = e19aInfoPtr;  /* refers to global pointer */

   
   if (init_flag)
   {
      E19A_FreeInfo(&infoPtr);
      
      if (infoPtr == (e19a_info_type *) NULL)
      {
	 infoPtr = (e19a_info_type *) malloc (sizeof(e19a_info_type));
	 memset ( ( void * ) infoPtr, 0, sizeof(e19a_info_type));
	 
	 E19A_SetupInfo(infoPtr);	/* access the database */
      }
   }

   
   return(infoPtr);
}
//---------------------------------------------------------


void	set_E19A_Info_Ptr(e19a_info_type *infoPtr)
{
   e19aInfoPtr = infoPtr;
   return;
}

//---------------------------------------------------------

void	E19A_SetupInfo(e19a_info_type *infoPtr)
{
   char where[BUFSIZ];
   
   
   /*
   	Get information for the following tables:
	
	Dcp, Location, Telem, Observer, Riverstat, & Floodcat.
   */

   sprintf(where, "WHERE lid = '%s'", TextReports_getCurrentLid());
   
   infoPtr->dcp = GetDcp(where);
   infoPtr->loc = GetLocation(where);
   infoPtr->telem = GetTelem(where);
   infoPtr->obs = GetObserver(where);
   infoPtr->river = GetRiverstat(where);
   infoPtr->fcat = GetFloodcat(where);
   infoPtr->descr = GetDescrip(where);
   
   
   return;
}
//---------------------------------------------------------


void	E19A_FreeInfo(e19a_info_type **infoPtr)
{
   if (*infoPtr != (e19a_info_type *) NULL)
   {
      FreeDcp		((*infoPtr)->dcp);
      FreeLocation	((*infoPtr)->loc);
      FreeTelem		((*infoPtr)->telem);
      FreeObserver	((*infoPtr)->obs);
      FreeRiverstat	((*infoPtr)->river);
      FreeFloodcat	((*infoPtr)->fcat);
      FreeDescrip	((*infoPtr)->descr);
      
      free(*infoPtr);
      
      e19aInfoPtr = NULL;	/* set global to NULL */
   }
   
   return;
}
//---------------------------------------------------------


