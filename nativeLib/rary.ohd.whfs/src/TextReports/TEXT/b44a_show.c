/*
	File:		b44a_show.c
	Purpose:	Provides support for the B-44A Report.

        History:
  
        BryonL   - Jan 25, 2005 - Modified to use date_t_to_USA_date as
                                  part of PostGres port.
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
#include "b44a_show.h"

#include "cvt_latlon.h"


#define	DEBUG_MODE	0	/* 0=False, 1=True */


/*
	Globals.
*/
static b44a_info_type*	b44aInfoPtr = NULL;

static char
   b44a_hdr_cooperative[] = "                           U.S. DEPARTMENT OF COMMERCE           NWS-FORM B-44A";

static int		b44a_first_page = B44A_COOPERATIVE;
static int		b44a_last_page = B44A_COOPERATIVE;


/*
	Functions.
*/
void	B44A_AddCallbacks(void)
{
   return;
}

//---------------------------------------------------------

void	B44A_RemoveCallbacks(void)
{
   return;
}

//---------------------------------------------------------

char*	B44A_PE_Manager(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;

   /*
   	Return the complete B-44A text.
   */
   text = B44A_GetText(B44A_ALLPAGES, TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE);

   if ( text != NULL )  
   {
      Text_AddString(&bufInfoPtr->b44a_buf, text, 1) ;
      Text_FreeString(&text) ;
      return (bufInfoPtr->b44a_buf) ;
   }
   else
   {
      return NULL ;
   }

}

//---------------------------------------------------------
void	B44A_PE_Print(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);
	
	buf = B44A_PE_Manager();	  /* obtain complete text buffer */

        /* output buffer to printer */
	if ( buf != NULL ) TextReports_P_OutputBuffer(buf);
	
	UnsetCursor(textreptFO);
	
	return;
}

//---------------------------------------------------------
	      
void	B44A_PE_Email(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);
	buf = B44A_PE_Manager();	  /* obtain complete text buffer */

        /* email buffer to internet address */
	if ( buf != NULL ) TextReports_E_OutputBuffer ( buf ) ;
	
	UnsetCursor(textreptFO);
	return;
}
//---------------------------------------------------------


void	B44A_LoadTextWidget(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;
   

   /*
   	Get the complete B44A text
	and display it.
   */
   text = B44A_GetText(B44A_ALLPAGES, TEXTREPORTS_REASON_NORMAL);
   
   if ( text != NULL )
   {
      Text_AddString(&bufInfoPtr->b44a_buf, text, 1);
      Text_FreeString(&text);
   }

   if (bufInfoPtr->b44a_buf != NULL)
      XtVaSetValues(textreptTE, XmNvalue, bufInfoPtr->b44a_buf, NULL);
   else
      fprintf(stderr,"ERROR: Unable to load text properly...\n");
   
   return;
}

//---------------------------------------------------------

char*	B44A_GetText(int page, int reason)
{
   b44a_info_type	*info = get_B44A_Info(False);
   char*  		text = NULL;

   switch(page)
   {
      case B44A_ALLPAGES:	text = B44A_AllPages	(reason);	break;

      case B44A_COOPERATIVE:	text = B44A_Cooperative	(info, reason);	break;

      default:	fprintf(stderr, "ERROR: Could not get text...\n");
	 	break;
   }

   return(text);
}
//---------------------------------------------------------


char*	B44A_AllPages ( int reason )
{
   char *  alltext = NULL ;
   char *  text = NULL ;
   
   int first = 0 ;
   int    i ;

   /*
   	Get the text for the given page(s).
   */
   for ( i = b44a_first_page ; i <= b44a_last_page ; i++ )
   {
      text = B44A_GetText ( i , reason ) ;

      if ( text != NULL )
      {
         Text_Paginate ( & text ) ;
      
         if ( first == 0 )
         {
            first = 1 ;
	    Text_AddString ( &alltext , text , 1 ) ;
         }
         else
         {
   	    Text_AddString ( &alltext , text , 0 ) ;
         }

         Text_FreeString(&text);
      }
   }

   return(alltext);
}
//---------------------------------------------------------


char*	B44A_Cooperative (b44a_info_type *info, int reason)
{
   char		*buf = NULL;
   char*	remark = NULL;

   char		tmp1[B44A_BUFSIZ];
   char		tmp2[B44A_BUFSIZ];
   char		tmp3[B44A_BUFSIZ];
   char		tmp4[B44A_BUFSIZ];
   char		tmp5[B44A_BUFSIZ];
   char		tmp6[B44A_BUFSIZ];
   char		tmp7[B44A_BUFSIZ];
   char		tmp8[B44A_BUFSIZ];
   char		tmp9[B44A_BUFSIZ];

   int		i;

   char		latitude[11];
   char		longitude[11];   
   
   memset(tmp1, '\0', sizeof(tmp1));
   memset(tmp2, '\0', sizeof(tmp2));
   memset(tmp3, '\0', sizeof(tmp3));
   memset(tmp4, '\0', sizeof(tmp4));
   memset(tmp5, '\0', sizeof(tmp5));
   memset(tmp6, '\0', sizeof(tmp6));

   sprintf(tmp1, "%s", b44a_hdr_cooperative);
   strcat (tmp1, "\n");
   strcat (tmp1, "                 NATIONAL OCEANIC AND ATMOSPHERIC ");
   strcat (tmp1, "ADMINISTRATION\n");
   strcat (tmp1, "                            NATIONAL WEATHER SERVICE\n\n");
   strcat (tmp1, "                      UNOFFICIAL COOPERATIVE STATION REPORT\n\n");
   strcat (tmp1, "----------------------------  IDENTIFICATION SECTION  --------------");
   strcat (tmp1, "------------\n\n");
   Text_AddString(&buf, tmp1, 1);

   
   if ( info->loc != NULL )
   {
      sprintf(tmp2, "%12s: %-37s  %3s: %-s\n",
	      "Station Name", info->loc->name, "LID", info->loc->lid);
   
      sprintf(tmp3, "%12s: %-20s    %18s: %-s\n",
	      "State", info->loc->state, "County", info->loc->county);
   }
   else
   {
      sprintf(tmp2, "%12s: %-37s  %3s: %-s\n",
	      "Station Name", " ", "LID", " ") ;
   
      sprintf(tmp3, "%12s: %-20s    %18s: %-s\n",
	      "State", " ", "County", " ") ;
   }
   
   if ( ( info->river != NULL ) &&
        ( info->river->lat ) &&
        (! IsNull(DOUBLE, (void*) &info->river->lat ) ) &&
        (! IsNull(DOUBLE, (void*) &info->river->lon ) ) )
   {
      strcpy(latitude,  cvt_latlon_from_double(info->river->lat));
      strcpy(longitude, cvt_latlon_from_double(info->river->lon));
   }
   else
   if ( ( info->loc != NULL ) &&
        (! IsNull(DOUBLE, (void*) &info->loc->lat)) &&
        (! IsNull(DOUBLE, (void*) &info->loc->lon)))
   {
      strcpy(latitude,  cvt_latlon_from_double(info->loc->lat));
      strcpy(longitude, cvt_latlon_from_double(info->loc->lon));
   }
   else
   {
      strcpy(latitude,  "MISSING");
      strcpy(longitude, "MISSING");
   }
   
   if ( info->loc != NULL )
   {
      DataToString(&info->loc->elev, DOUBLE, tmp5, "%8.2lf", "        ");
      sprintf(tmp4, "%12s: %-20s    %18s: %-s\n",
   	      "Latitude", latitude,
	      "Elevation", tmp5);

      memset(tmp5, '\0', sizeof(tmp5));
      sprintf(tmp5, "%12s: %-20s    %18s: %-s\n",
	      "Longitude", longitude,
	      "Hydrologic Unit No", info->loc->hu);
   
      memset(tmp7, '\0', sizeof(tmp7));
      date_t_to_USA_date ( info->loc->sbd, tmp7);
      sprintf(tmp6, "Station Begin Date: %-s\n       River Basin: %-s\n\n",
	      tmp7, info->loc->rb);
   }
   else
   {
      sprintf(tmp4, "%12s: %-20s    %18s: %-s\n",
   	      "Latitude", latitude,
	      "Elevation", " ");
      sprintf(tmp5, "%12s: %-20s    %18s: %-s\n",
	      "Longitude", longitude,
	      "Hydrologic Unit No", " " );
      sprintf(tmp6, "Station Begin Date: %-s\n       River Basin: %-s\n\n",
	      " ", " " ) ;
   }

   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);
   Text_AddString(&buf, tmp6, 0);
   
   memset(tmp1, '\0', sizeof(tmp1));
   memset(tmp2, '\0', sizeof(tmp2));
   memset(tmp3, '\0', sizeof(tmp3));
   memset(tmp4, '\0', sizeof(tmp4));
   memset(tmp5, '\0', sizeof(tmp5));
   memset(tmp6, '\0', sizeof(tmp6));
   memset(tmp7, '\0', sizeof(tmp7));
   memset(tmp8, '\0', sizeof(tmp8));
   memset(tmp9, '\0', sizeof(tmp9));

   sprintf(tmp1, "-------------------------------  OBSERVER SECTION  -----------");
   strcat (tmp1, "------------------\n\n");
   
   if ( info->obs != NULL )
   {
      memset(tmp3, '\0', sizeof(tmp3));
      date_t_to_USA_date ( info->obs->dos, tmp3); 
      memset(tmp4, '\0', sizeof(tmp4));

      if (strlen(info->obs->firstname) > 0)
      {
         strcat(tmp4, info->obs->firstname);
         strcat(tmp4, " ");
      }

      strcat(tmp4, info->obs->lastname);
      sprintf(tmp2, "Observer: %-41s  DOS: %-10s  Gender: %-s\n",
	      tmp4, tmp3, info->obs->gn);
      memset(tmp3, '\0', sizeof(tmp3));
      sprintf(tmp3, " Address: %-30s\n",
	      info->obs->a1);
      memset(tmp4, '\0', sizeof(tmp4));
      sprintf(tmp4, "          %-30s      Home Phone: %-s\n",
	      info->obs->a2, info->obs->hphone);
      sprintf(tmp5, "          %-30s    Office Phone: %-s\n",
	      info->obs->a3, info->obs->phone);
      sprintf(tmp6, "          %-30s       Recipient: %-s\n",
	      info->obs->city, info->obs->recip);
      sprintf(tmp7, "          %-2s  %-10s        %10s         Comms: %-s\n",
	      info->obs->state, info->obs->zip, " ", info->obs->comm);
      sprintf(tmp9, "   Email: %-s\n", info->obs->email);
      sprintf(tmp8, "  Duties: %-s\n\n", info->obs->rprt);
   }
   else
   {
      sprintf ( tmp2, "Observer: %-41s  DOS: %-10s  Gender: %-s\n" ,
	        " " , " " , " " ) ;
      memset ( tmp3 , '\0' , sizeof (tmp3 ) ) ;
      sprintf ( tmp3 , " Address: %-30s\n" ,
	      " " ) ;
      memset ( tmp4 , '\0', sizeof(tmp4));
      sprintf ( tmp4 , "          %-30s      Home Phone: %-s\n" ,
	        " " , " " ) ;
      sprintf ( tmp5 , "          %-30s    Office Phone: %-s\n" ,
	        " " , " " ) ;
      sprintf ( tmp6 , "          %-30s       Recipient: %-s\n" ,
	        " " , " " ) ;
      sprintf ( tmp7 , "          %-2s  %-10s        %10s         "
               "Comms: %-s\n",
	       " " , " " , " " , " " ) ;
      sprintf ( tmp9,  "   Email: %-s\n", " ");
      sprintf ( tmp8 , "  Duties: %-s\n\n" , " " ) ;
   }

   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);
   Text_AddString(&buf, tmp6, 0);
   Text_AddString(&buf, tmp7, 0);
   Text_AddString(&buf, tmp9, 0);
   Text_AddString(&buf, tmp8, 0);
   
   memset(tmp1, '\0', sizeof(tmp1));
   memset(tmp2, '\0', sizeof(tmp2));

   sprintf(tmp1, "--------------------------  STATION MANAGEMENT SECTION  ------");
   strcat (tmp1, "------------------\n\n");
   
   if ( info->loc != NULL ) 
   {
      sprintf(tmp2, "HSA: %-3s			WFO: %-3s"
	   "			RFC: %-s\n\n",
	   info->loc->hsa, info->loc->wfo, info->loc->rfc);
      strcat (tmp2, "-----------------------------------  REMARKS  -----"
                    "-----------");
      strcat (tmp2, "------------------\n\n");
      Text_AddString(&remark,
		  Text_WordWrap(info->loc->lremark, 80, 0),
		  1);
      Text_AddString(&buf, tmp1, 0);
      Text_AddString(&buf, tmp2, 0);
      Text_AddString(&buf, remark, 0);
      Text_FreeString(&remark);
   } 
   else
   {
      sprintf(tmp2, "HSA: %-3s			WFO: %-3s"
	   "			RFC: %-s\n\n" ,
	   " " , " " , " " ) ;
      strcat (tmp2, "-----------------------------------  REMARKS  -----"
                    "-----------");
      strcat (tmp2, "------------------\n\n");
      Text_AddString(&buf, tmp1, 0);
      Text_AddString(&buf, tmp2, 0);
      Text_AddString(&buf, remark, 0);
      Text_FreeString(&remark);
   }

   /* try to place FOOTER at the bottom */
   memset(tmp1, '\0', sizeof(tmp1));

   for(i=0; i < (59 - Text_CountNewLines(buf)); i++)
   {
      strcat (tmp1, "\n");
   }

   Text_AddString(&buf, tmp1, 0);

   
   memset(tmp2, '\0', sizeof(tmp2));
   sprintf(tmp2, "	Effective Date: %-10s	Hydrologist: %-s\n",
	   (char *) TextReports_GetDate(), (char *) TextReports_GetHydrologistName());
   Text_AddString(&buf, tmp2, 0);
   

   
   return(buf);
}
//---------------------------------------------------------


b44a_info_type*		get_B44A_Info(int init_flag)
{
   b44a_info_type*	infoPtr = b44aInfoPtr;  /* refers to global pointer */

   
   if (init_flag)
   {
      B44A_FreeInfo(&infoPtr);
      
      if (infoPtr == (b44a_info_type *) NULL)
      {
	 infoPtr = (b44a_info_type *) malloc (sizeof(b44a_info_type));
	 memset(infoPtr, 0, sizeof(b44a_info_type));

	 B44A_SetupInfo(infoPtr);	/* access the database */
      }      
   }

   
   return(infoPtr);
}
//---------------------------------------------------------


void	set_B44A_Info_Ptr(b44a_info_type *infoPtr)
{
   b44aInfoPtr = infoPtr;
   return;
}
//---------------------------------------------------------


void	B44A_SetupInfo(b44a_info_type *infoPtr)
{
   char where[BUFSIZ];
   
   
   /*
   	Get information for the following tables:
	
	Location, Observer, & Riverstat.
   */

   sprintf(where, "WHERE lid = '%s'", TextReports_getCurrentLid());
   
   infoPtr->loc = GetLocation(where);
   infoPtr->obs = GetObserver(where);
   infoPtr->river = GetRiverstat(where);

   
   return;
}
//---------------------------------------------------------


void	B44A_FreeInfo(b44a_info_type **infoPtr)
{
   if (*infoPtr != (b44a_info_type *) NULL)
   {
      FreeLocation	((*infoPtr)->loc);
      FreeObserver	((*infoPtr)->obs);
      FreeRiverstat	((*infoPtr)->river);
      
      free(*infoPtr);
      
      b44aInfoPtr = NULL;	/* set global to NULL */
   }
   
   return;
}
//---------------------------------------------------------


