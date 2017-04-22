/*
	File:		e19_show.c
	Date:		July/August 1996
	Author:		Paul Taylor
        Modification    Jingtao Deng (April, 2001)
	                --Minor modification about output format

			Bryon Lawrence (June, 2001)
                        --Changed the initial definitions of "minstage" and
                          "maxstage" to INT_MAX and INT_MIN, respectively. 
                          These are defined in "limits.h".  These are 
                          preferable to use over MAXINT which was defined
                          in "values.h"
			
			Russell Erb (August, 2001)
                        --Cleaned up compiler warnings

			Russell Erb (March, 2002)
                        --rewrote E19_StaffGage()

			Bryon Lawrence (February, 2004)
			--Changed "old" to "olddatum" to reflect the
			OB4 change in the name of the "old" column in 
			the Crest IHFS table.

			Russell Erb (March, 2003)
                        --added email address to E19_Contacts()

	Purpose:	Provides support for the E-19 Report.
*/


#include <limits.h>
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
#include <sqltypes.h>
#include <time.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"

#include "GeneralUtil.h"

#include "textrept.h"
#include "textrept_show.h"
#include "e19_show.h"

#include "cvt_latlon.h"


#define	DEBUG_MODE	0	/* 0=False, 1=True */

#define SEP_CHAR	124	/* special characters for staff gage */


/*
	Globals.
*/
static e19_info_type*	e19InfoPtr = NULL;

static char
   e19_hdr_cover[]      ="NWS FORM E-19 (COVER)",
   e19_hdr_mappage[]    ="                              MAP OF GAGE LOCATION",
   e19_hdr_benchmarks[] ="                                   BENCHMARKS",
   e19_hdr_gages[]      ="                                     GAGES",
   e19_hdr_history[]    ="                                     HISTORY",
   e19_hdr_crests[]     ="                                     CRESTS",
   e19_hdr_lowwater[]   ="                                LOW WATER RECORDS",
   e19_hdr_conditions[] ="                            CONDITIONS AFFECTING FLOW",
   e19_hdr_damage[]     ="                                     DAMAGE",
   e19_hdr_staffgage[]  ="                                RIVER STAGE DATA",
   e19_hdr_contacts[]   ="                                    CONTACTS";

      
static int		e19_first_page = E19_COVER;
static int		e19_last_page = E19_CONTACTS;



/*
	Functions.
*/

//---------------------------------------------------------

void	E19_AddCallbacks(void)
{
   /*
   	Support for Print/Email DS.
   */
   XtAddCallback(e19coverTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19coverTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19mapTB,   XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19mapTB,   XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19benchTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19benchTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19gagesTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19gagesTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19histTB,  XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19histTB,  XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19crestsTB,XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19crestsTB,XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19lwTB,    XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19lwTB,    XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19condTB,  XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19condTB,  XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19damageTB,XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19damageTB,XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19staffTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19staffTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtAddCallback(e19contactsTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtAddCallback(e19contactsTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   
   XtAddCallback(e19allTB,   XmNarmCallback,    E19_PE_ClearItems, NULL);
   XtAddCallback(e19allTB,   XmNdisarmCallback, E19_PE_SetCover, NULL);

   
   return;
}


//---------------------------------------------------------
void	E19_RemoveCallbacks(void)
{
   /*
   	Support for Print/Email DS.
   */
   XtRemoveCallback(e19coverTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19coverTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19mapTB,   XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19mapTB,   XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19benchTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19benchTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19gagesTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19gagesTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19histTB,  XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19histTB,  XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19crestsTB,XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19crestsTB,XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19lwTB,    XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19lwTB,    XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19condTB,  XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19condTB,  XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19damageTB,XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19damageTB,XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19staffTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19staffTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   XtRemoveCallback(e19contactsTB, XmNarmCallback,    E19_PE_ClearAll,   NULL);
   XtRemoveCallback(e19contactsTB, XmNdisarmCallback, E19_PE_CheckItems, NULL);
   
   
   XtRemoveCallback(e19allTB,   XmNarmCallback,    E19_PE_ClearItems, NULL);
   XtRemoveCallback(e19allTB,   XmNdisarmCallback, E19_PE_SetCover, NULL);
   
   return;
}

//---------------------------------------------------------

void	E19_checkPB(Widget w, XtPointer ptr, XtPointer cbs)
{
   long	pos;

   
   if(w==e19coverPB)	pos = E19_FindHeader(E19_COVER);
   if(w==e19mapPB)	pos = E19_FindHeader(E19_MAPPAGE);
   if(w==e19benchPB)	pos = E19_FindHeader(E19_BENCHMARKS);
   if(w==e19gagesPB)	pos = E19_FindHeader(E19_GAGES);
   if(w==e19histPB)	pos = E19_FindHeader(E19_HISTORY);
   if(w==e19crestsPB)	pos = E19_FindHeader(E19_CRESTS);
   if(w==e19lwPB)	pos = E19_FindHeader(E19_LOWWATER);
   if(w==e19condPB)	pos = E19_FindHeader(E19_CONDITIONS);
   if(w==e19damagePB)	pos = E19_FindHeader(E19_DAMAGE);
   if(w==e19staffPB)	pos = E19_FindHeader(E19_STAFFGAGE);
   if(w==e19contactsPB)	pos = E19_FindHeader(E19_CONTACTS);
   
   return;
}

//---------------------------------------------------------

char*	E19_PE_Manager(void)
{
   char*  alltext = NULL;
   char*  text = NULL;

   int	  all_flag = False;
   int	  i, j;


   if(XmToggleButtonGetState(e19allTB))
      all_flag = True;

   /*
	Find out which pages need to be printed,
	and keep appending them to create a "final buffer".
   */
   for(i=e19_first_page; i<=e19_last_page; i++)
   {
      j = False;
      
      switch(i)
      {
	 case E19_COVER:       j = XmToggleButtonGetState(e19coverTB);  break;
	 case E19_MAPPAGE:     j = XmToggleButtonGetState(e19mapTB);    break;
	 case E19_BENCHMARKS:  j = XmToggleButtonGetState(e19benchTB);  break;
	 case E19_GAGES:       j = XmToggleButtonGetState(e19gagesTB);  break;
	 case E19_HISTORY:     j = XmToggleButtonGetState(e19histTB);   break;
	 case E19_CRESTS:      j = XmToggleButtonGetState(e19crestsTB); break;
	 case E19_LOWWATER:    j = XmToggleButtonGetState(e19lwTB);     break;
	 case E19_CONDITIONS:  j = XmToggleButtonGetState(e19condTB);   break;
	 case E19_DAMAGE:      j = XmToggleButtonGetState(e19damageTB); break;
	 case E19_STAFFGAGE:   j = XmToggleButtonGetState(e19staffTB);  break;
	 case E19_CONTACTS:    j = XmToggleButtonGetState(e19contactsTB); break;
      }
      
      if(all_flag || j)
      {
	 text = E19_GetText(i, TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE);
	 Text_Paginate(&text);
	 
	 if(i==e19_first_page)
	    Text_AddString(&alltext, text, 1);
	 else
	    Text_AddString(&alltext, text, 0);
	 
	 Text_FreeString(&text);
      }
   }
   
   return(alltext);
}

//---------------------------------------------------------

void	E19_PE_ClearAll(Widget w, XtPointer ptr, XtPointer cbs)
{
	XmToggleButtonSetState(e19allTB, False, False);
	
	return;
}


//---------------------------------------------------------
void	E19_PE_SetCover(Widget w, XtPointer ptr, XtPointer cbs)
{
	if(!XmToggleButtonGetState(e19allTB))
	   XmToggleButtonSetState(e19coverTB, True, False);
	
	return;
}

//---------------------------------------------------------

void	E19_PE_ClearItems(Widget w, XtPointer ptr, XtPointer cbs)
{
	XmToggleButtonSetState(e19coverTB, False, False);
	XmToggleButtonSetState(e19mapTB,   False, False);
	XmToggleButtonSetState(e19benchTB, False, False);
	XmToggleButtonSetState(e19gagesTB, False, False);
	XmToggleButtonSetState(e19histTB,  False, False);
	XmToggleButtonSetState(e19crestsTB,False, False);
	XmToggleButtonSetState(e19lwTB,    False, False);
	XmToggleButtonSetState(e19condTB,  False, False);
	XmToggleButtonSetState(e19damageTB,False, False);
	XmToggleButtonSetState(e19staffTB, False, False);
	XmToggleButtonSetState(e19contactsTB, False, False);
	
	return;
}


//---------------------------------------------------------
void	E19_PE_CheckItems(Widget w, XtPointer ptr, XtPointer cbs)
{
   int	setall = True;

   if(XmToggleButtonGetState(e19coverTB))	setall = False;
   if(XmToggleButtonGetState(e19mapTB))		setall = False;
   if(XmToggleButtonGetState(e19benchTB))	setall = False;
   if(XmToggleButtonGetState(e19gagesTB))	setall = False;
   if(XmToggleButtonGetState(e19histTB))	setall = False;
   if(XmToggleButtonGetState(e19crestsTB))	setall = False;
   if(XmToggleButtonGetState(e19lwTB))		setall = False;
   if(XmToggleButtonGetState(e19condTB))	setall = False;
   if(XmToggleButtonGetState(e19damageTB))	setall = False;
   if(XmToggleButtonGetState(e19staffTB))	setall = False;
   if(XmToggleButtonGetState(e19contactsTB))	setall = False;
   
   if(setall)
      XmToggleButtonSetState(e19allTB, True, False);
   
   return;
}


//---------------------------------------------------------
void	E19_PE_Print(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;

	SetCursor(textreptFO, XC_watch);
	
	buf = E19_PE_Manager();		  /* obtain complete text buffer */
	TextReports_P_OutputBuffer(buf);  /* output buffer to printer */
	
	UnsetCursor(textreptFO);

	return;
}

//---------------------------------------------------------
	      
void	E19_PE_Email(Widget w, XtPointer ptr, XtPointer cbs)
{
	char*	buf = NULL;
	
	SetCursor(textreptFO, XC_watch);

	buf = E19_PE_Manager();		  /* obtain complete text buffer */
	TextReports_E_OutputBuffer(buf);  /* email buffer to internet address */
	
	UnsetCursor(textreptFO);

	return;
}

//---------------------------------------------------------

void	E19_LoadTextWidget(void)
{
   textrept_bufinfo*	bufInfoPtr = TextReports_GetBufInfo();
   char*  		text = NULL;

#if DEBUG_MODE
printf("in E19_LoadTextWidget()...\n");
#endif

   /*
   	Get the complete E19 text
	and display it.
   */
   text = E19_GetText(E19_ALLPAGES, TEXTREPORTS_REASON_NORMAL);
   Text_AddString(&bufInfoPtr->e19_buf, text, 1);

   Text_FreeString(&text);
   
#if DEBUG_MODE
printf("about to set the TextWidget value == <\n%s\n>!!!\n", bufInfoPtr->e19_buf);
#endif

   if (bufInfoPtr->e19_buf != NULL)
      XtVaSetValues(textreptTE, XmNvalue, bufInfoPtr->e19_buf, NULL);
   else
      fprintf(stderr,"ERROR: Unable to load text properly...\n");
   
   return;
}

//---------------------------------------------------------

char*	E19_GetText(int page, int reason)
{
   e19_info_type	*info = get_E19_Info(False);
   char*  		text = NULL;

#if DEBUG_MODE
printf("in E19_GetText()...\n");
#endif

   switch(page)
   {
      case E19_ALLPAGES:	text = E19_AllPages	(reason);	break;

      case E19_COVER:		text = E19_Cover	(info, reason);	break;
      case E19_MAPPAGE:		text = E19_MapPage	(info, reason);	break;
      case E19_BENCHMARKS:	text = E19_Benchmarks	(info, reason);	break;
      case E19_GAGES:		text = E19_Gages	(info, reason);	break;
      case E19_HISTORY:		text = E19_History	(info, reason);	break;
      case E19_CRESTS:		text = E19_Crests	(info, reason);	break;
      case E19_LOWWATER:	text = E19_LowWater	(info, reason);	break;
      case E19_CONDITIONS:	text = E19_Conditions	(info, reason);	break;
      case E19_DAMAGE:		text = E19_Damage	(info, reason);	break;
      case E19_STAFFGAGE:	text = E19_StaffGage	(info, reason);	break;
      case E19_CONTACTS:	text = E19_Contacts	(info, reason);	break;

      default:	fprintf(stderr, "ERROR: Could not get text...\n");
	 	break;
   }

   return(text);
}

//---------------------------------------------------------

long	E19_FindHeader(int page)
{
   char*		search = NULL;
   XmTextPosition	pos = 0;
   
   switch(page)
   {
      case E19_COVER:		search = e19_hdr_cover;        break;
      case E19_MAPPAGE:		search = e19_hdr_mappage;      break;
      case E19_BENCHMARKS:	search = e19_hdr_benchmarks;   break;
      case E19_GAGES:		search = e19_hdr_gages;        break;
      case E19_HISTORY:		search = e19_hdr_history;      break;
      case E19_CRESTS:		search = e19_hdr_crests;       break;
      case E19_LOWWATER:	search = e19_hdr_lowwater;     break;
      case E19_CONDITIONS:	search = e19_hdr_conditions;   break;
      case E19_DAMAGE:		search = e19_hdr_damage;       break;
      case E19_STAFFGAGE:	search = e19_hdr_staffgage;    break;
      case E19_CONTACTS:	search = e19_hdr_contacts;     break;

      default:	fprintf(stderr, "ERROR: Could not find header...\n");
	 	break;
   }

   if(XmTextFindString(textreptTE, 0, search, XmTEXT_FORWARD, &pos))
   {
      XmTextSetTopPosition(textreptTE, pos);
      XmTextShowPosition(textreptTE, pos);
   }
   else
      fprintf(stderr, "ERROR: Could not find header...\n");
   
   return((long)pos);
}

//---------------------------------------------------------

char*	E19_AllPages(int reason)
{
   char*  alltext = NULL;
   char*  text = NULL;
   
   int    i;

   /*
   	Get the text for the given page(s).
   */
  
   for(i=e19_first_page; i<=e19_last_page; i++)
   {
      text = E19_GetText(i, reason);
//      Text_Paginate(&text);
      
      if(i == e19_first_page)
	 Text_AddString(&alltext, text, 1);
      else
	 Text_AddString(&alltext, text, 0);

      Text_FreeString(&text);
   }

   return(alltext);
}

//---------------------------------------------------------

char*	E19_Cover(e19_info_type *info, int reason)
{
   char		*buf = NULL;
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
      
   Refer	*referPtr = NULL;
   int		i;
   int		count = 0;
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1, "%s", e19_hdr_cover);
   strcat (tmp1, "\n\n");
   strcat (tmp1, "			   U.S. DEPARTMENT OF COMMERCE\n");
   strcat (tmp1, "                 NATIONAL OCEANIC AND ATMOSPHERIC ");
   strcat (tmp1, "ADMINISTRATION\n");
   strcat (tmp1, "                            NATIONAL WEATHER SERVICE\n\n");
   strcat (tmp1, "                          REPORT ON RIVER GAGE STATION\n\n");

   
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp6, '\0', sizeof(tmp6));

   if ( info->river != NULL )
   {
      // Modified by BryonL to use date_t_to_USA_date on 1/26/2005. 
      date_t_to_USA_date ( info->river->rrevise, tmp6); 
   }

   sprintf(tmp2,  "%40s %10s, %10s\n\n\n",
	  "REVISED, PRINTED DATES:", tmp6,  (char *) TextReports_GetDate());
   //
  // sprintf(tmp2, "    %35s                       DATE: %10s\n\n\n",
//	   " ", TextReports_GetDate());
  

Text_AddString(&buf, tmp1, 1);


   Text_AddString(&buf, tmp2, 0);
   
   
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   sprintf(tmp3, "LOCATION: %-s\n  STREAM: %-s\n"
	   "   BASIN: %-30s   HSA: %-s\n"
	   "\n",
	   info->loc ? info->loc->name : "" ,
           info->river ? info->river->stream : "" ,
	   info->loc ? info->loc->rb : "" ,
           info->loc ? info->loc->hsa : "" ) ;
   Text_AddString(&buf, tmp3, 0);
   
   
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   sprintf(tmp4, "REFERENCES:\n");

   if ( info->refer != NULL )
   {
      referPtr = (Refer *) ListFirst ( &info->refer->list ) ;
      count = ListCount ( &referPtr->list) ;
   }

   if (count > 0)
   {
      while(referPtr)
      {
	 memset( ( void * ) tmp5, '\0', sizeof(tmp5));
	 sprintf(tmp5, "      %-s\n", referPtr->reference);
	 strcat (tmp4, tmp5);
	 
	 referPtr = (Refer *) ListNext(&referPtr->node);
      }
   }
   for(i=0; i<(16-count); i++)	// try to place ABBREVIATIONS at the bottom 
   {
      strcat (tmp4, "\n");
   }
   Text_AddString(&buf, tmp4, 0);
   

   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   sprintf(tmp5, "\nABBREVIATIONS:\n\n");
   strcat (tmp5, "  BM  - bench mark		EPA   - Environmental Protection Agency\n");
   strcat (tmp5, "  DS  - downstream		IBWC  - International Boundary and Water Comm.\n");
   strcat (tmp5, "  US  - upstream		MSRC  - Mississippi River Commission\n");
   strcat (tmp5, "  HW  - high water		MORC  - Missouri River Commission\n");
   strcat (tmp5, "  LW  - low water		NOAA  - National Oceanic and Atmospheric Admin.\n");
   strcat (tmp5, "  RB  - right bank		NOS   - National Ocean Survey\n");
   strcat (tmp5, "  LB  - left bank		NWS   - National Weather Service\n");
   strcat (tmp5, "  MGL - mean gulf level		TVA   - Tennessee Valley Authority\n");
   strcat (tmp5, "  MLW - mean low water		USACE - U.S. Army Corps of Engineers\n");
   strcat (tmp5, "  MSL - mean sea level		USBR  - U.S. Bureau of Reclamation\n");
   strcat (tmp5, "  MLT - mean low tide		USGS  - U.S. Geological Survey\n");
   strcat (tmp5, "  MT  - mean tide		USWB  - U.S. Weather Bureau\n");
   strcat (tmp5, "  WQ  - water quality		NGVD  - National Geodetic Vertical Datum\n");
   strcat (tmp5, "  RM  - reference mark		NAD   - North American Datum\n");
   strcat (tmp5, "  RP  - reference point\n");
   strcat (tmp5, "\n\n\n");
   Text_AddString(&buf, tmp5, 0);


   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   sprintf(tmp1, "					   LOCATION IDENTIFICATION: %-s\n",
	   info->loc ? info->loc->lid : "" );
   sprintf(tmp2, "						  NWS INDEX NUMBER: %-s\n",
	   info->loc ? info->loc->sn : "" );
   sprintf(tmp3, "						       USGS NUMBER: %-s\n",
	   info->river ? info->river->gsno : "" );
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);

   memset( ( void * ) tmp7, '\0', sizeof(tmp7));
   strcat (tmp7, "\f");
   Text_AddString(&buf, tmp7, 0);
   return(buf);
}

//---------------------------------------------------------

char*	E19_MapPage(e19_info_type *info, int reason)
{
   char		*buf = NULL;

   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   
   int		init_footer = True;
   
   
   
   //	Title
   
//
//sprintf(tmp1, "			      MAP OF GAGE LOCATION");
//
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_mappage);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);

   
   
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   sprintf(tmp2, "%20s: %-10s		SOURCE: %-s\n",
	   "LATITUDE", 
           info->river ? cvt_latlon_from_double(info->river->lat) : "" ,
	   info->river ? info->river->rsource : "" );

   sprintf(tmp3, "%20s: %-10s",
	   "LONGITUDE",
           info->river ? cvt_latlon_from_double(info->river->lon) : "" );
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);

   
   
   // try to place FOOTER at the bottom
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, 0), 0);

   
   
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   strcat(tmp4, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_MAPPAGE,
				 "GAGE MAP", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));

   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   strcat (tmp5, "\f");
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);

   
   return(buf);
}

//---------------------------------------------------------

char*	E19_Benchmarks(e19_info_type *info, int reason)
{
   Benchmark*	bmPtr = NULL;
   
   char*	buf = NULL;
   
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   
   int		num_cols = 74;
   int		left_margin = 24;
   
   char*	remark = NULL;
   char*	line_str = NULL;
   
   double	gage_zero;
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;
   

   //
   //	Title
   //
//
//sprintf(tmp1, "				 BENCHMARKS");

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_benchmarks);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));

   if ( info->river != NULL )
   {
      DataToString(&info->river->zd, DOUBLE, tmp3, "%8.3lf", "        ");
   }
   else
   {
      strcpy ( tmp3 , "        " ) ; 
   }

   sprintf(tmp1,"       ELEVATION OF GAGE ZERO: %s %7s VERTICAL DATUM: %-21s\n",
	   tmp3, " ", info->river ? info->river->vdatum : "" );
   
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3,'\0', sizeof(tmp3));

   if ( info->river != NULL )
   {
      DataToString(&info->river->cb, DOUBLE, tmp3, "%8.3lf", "        ");
   }
   else
   {
      strcpy ( tmp3 , "        " ) ; 
   }

   sprintf(tmp2,"     LEVELING AGENCY AND DATE: %-21s  CHECKBAR: %s\n",
	   info->river ? info->river->level : "" ,
           tmp3);
   
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   sprintf(tmp3,"                RATING AGENCY: %-21s\n\n",
	   info->river ? info->river->rated : "" );
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   
   count1 = Text_CountNewLines(buf);

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   sprintf(tmp2,"          BENCHMARK %4sDESCRIPTION%63s    GAGE ZERO      DATUM\n",
	   " ", " ");
   sprintf(tmp3,"          ----------%4s%74s    ---------     -------\n",
	   "----", "--------------------------------------------------------------------------");
   strcat (tmp1, tmp2);
   strcat (tmp1, tmp3);
   Text_AddString(&buf, tmp1, 0);
   
   count2 = Text_CountNewLines(tmp1);
   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;
   
   if ( info->bench != NULL ) 
   {
      bmPtr = (Benchmark *) ListFirst(&info->bench->list);
   }

   memset( ( void * ) tmp6, '\0', sizeof(tmp6));  
   strcat (tmp6, "\f");

   while (bmPtr)
   {
      //
	//  Get the remark field.
      	 // Compute the number of lines needed for this record.
      //
      Text_AddString(&remark,
		     Text_WordWrap(bmPtr->remark, num_cols, left_margin),
		     1);

     // Is there a remark to process? 
      if ( remark == NULL ) 
      {
	 bmPtr = (Benchmark *) ListNext(&bmPtr->node);
         continue ;
      }

      needed = (Text_CountNewLines(remark) + 1)  + 1;
      
      if (needed <= avail)
      {
	 //
	 //	Formatting for Line 1.
	 //
	 Text_GetLine(&remark, num_cols, &line_str);

         if ( line_str != NULL )
         {
	    strcpy(tmp1, line_str);
	    Text_FreeString(&line_str);
         }
	 
	 if(((IsNull(DOUBLE,(void *) &bmPtr->elev))     != ISNULL) &&
	    ((IsNull(DOUBLE,(void *) &info->river->zd)) != ISNULL))
	 {
	    gage_zero = bmPtr->elev - info->river->zd;
	    memset( ( void * ) tmp3, '\0', sizeof(tmp3));
	    DataToString(&gage_zero, DOUBLE, tmp3, "%8.3lf", "        ");
	 }
	 else
	 {
	    memset( ( void * ) tmp3, '\0', sizeof(tmp3));
	    memset( ( void * ) tmp3, ' ', 8);
	 }
	 DataToString(&bmPtr->elev, DOUBLE, tmp4, "%8.3lf", "        ");
	 memset( ( void * ) tmp2, '\0', sizeof(tmp2));
	 sprintf(tmp2,"          %-7s       %-74s     %s    %s\n",
		 bmPtr->bnum, tmp1, tmp3, tmp4);
	 Text_AddString(&buf, tmp2, 0);
	 
	 
	 //
	 //	Formatting for all additional lines.
	 //
	 while(remark)
	 {
	    Text_GetLine(&remark, left_margin+num_cols, &line_str);
	    strcpy(tmp1, line_str);
	    strcat(tmp1, "\n");
	    Text_AddString(&buf, tmp1, 0);
	    Text_FreeString(&line_str);
	 }
	 Text_FreeString(&remark);
	 Text_AddString(&buf, "\n", 0);
	 
	 
	 //
	 //	Advance pointer.
	//	Update avail.
	//	Continue with top of while loop.
	 //
	 bmPtr = (Benchmark *) ListNext(&bmPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp5, '\0', sizeof(tmp5));
	 strcat(tmp5, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_BENCHMARKS,
				       "BENCHMARKS", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));

	 
	 Text_AddString(&buf, tmp5, 0);
	 Text_AddString(&buf, tmp6, 0);
	 
	 //
	 //	Do column header.
	 //
	 memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	 memset( ( void * ) tmp2, '\0', sizeof(tmp2));
	 memset( ( void * ) tmp3, '\0', sizeof(tmp3));
	 sprintf(tmp2,"          BENCHMARK %4sDESCRIPTION%63s    GAGE ZERO      DATUM\n",
		 " ", " ");
	 sprintf(tmp3,"          ----------%4s%74s    ---------     -------\n",
		 "----", "--------------------------------------------------------------------------");
	 strcat (tmp1, tmp2);
	 strcat (tmp1, tmp3);
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 //
	//	Reset available value, keep pointer the same & continue
	//	from top of while loop.
	 //
	 avail = available + count1;
	 loop++;
	 continue;
	 
      }
   }
   Text_AddString(&buf, "\n", 0);


   
   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

   
   
   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   strcat(tmp5, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_BENCHMARKS,
				 "BENCHMARKS", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));
   Text_AddString(&buf, tmp5, 0);
   Text_AddString(&buf, tmp6, 0);
  
   return(buf);
}

//---------------------------------------------------------

char*	E19_Gages(e19_info_type *info, int reason)
{
   Gage*	gagePtr = NULL;

   char*	buf = NULL;
   
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
   char		tmp8[E19_BUFSIZ];
   
   char*	crit1 = NULL;
   char*	crit1_line_str = NULL;
   char*	crit2 = NULL;
   char*	crit2_line_str = NULL;
   
   char*	remark = NULL;
   char*	line_str = NULL;

   int		dcp_num_cols;
   int		dcp_left_margin;

   int		telm_num_cols;
   int		telm_left_margin;
   
   int		num_cols = 60;
   int		left_margin = 68;
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;
      

   //
   //	Title
   //
//
//sprintf(tmp1, "				    GAGES");
//
  
  memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_gages);
   strcat (tmp1, "\n\n");
   strcat(tmp1, "                 DCP                                        TELEM\n\n");
   Text_AddString(&buf, tmp1, 1);

   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   sprintf(tmp1,"        NESS ID: %-9s %12s  TYPE OF TELEMETRY: %-s\n",
	   info->dcp ? info->dcp->goes : "" , 
           " ", 
           info->telem ? info->telem->type : "");
   sprintf(tmp2,"          OWNER: %-11s %12s            OWNER: %-s\n",
	   info->dcp ? info->dcp->owner : "" , 
           " ", 
           info->telem ? info->telem->owner : "" );
   sprintf(tmp3,"    REPORT TIME: %-9s %12s       PHONE NUMBER: %-s\n",
	   info->dcp ? info->dcp->rptime : "" ,
           " ", 
           info->telem ? info->telem->phone : "" );
   sprintf(tmp4,"       INTERVAL: %-5s %12s               INTERVAL: %-s\n",
	   info->dcp ? info->dcp->rptfreq : "" , 
           " ", 
           info->telem ? info->telem->rptfreq : "" );
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   
   //
   //	Create the 1st criteria.
   //
   dcp_num_cols = 20;
   dcp_left_margin = 0;

   if ( info->dcp != NULL )
   {
      Text_AddString(&crit1,
		     Text_WordWrap ( info->dcp->criteria ,
		     dcp_num_cols , 
                     dcp_left_margin ) ,
		     1 ) ;
   }
   //
   //	Create the 2nd criteria.
   //
   telm_num_cols = 15;
   telm_left_margin = 0;

   if ( info->telem != NULL )
   {
      Text_AddString(&crit2,
		     Text_WordWrap(info->telem->criteria,
		     telm_num_cols, telm_left_margin),
		     1);
   }

   //
   //	Formatting for Line 1.
   //
   if ( crit1 != NULL )
   {
      Text_GetLine ( & crit1 , dcp_num_cols , & crit1_line_str ) ;
      strcpy ( tmp3 , crit1_line_str ) ;
      Text_FreeString(&crit1_line_str);
      memset( ( void * ) tmp6, '\0', sizeof(tmp6));
      sprintf(tmp6,"       CRITERIA: %-20s %12s",
	      tmp3, " ");   
      Text_AddString(&buf, tmp6, 0);
   
   } 

   if ( crit2 != NULL )
   {
      Text_GetLine(&crit2, telm_num_cols, &crit2_line_str);
      strcpy(tmp4, crit2_line_str);
      Text_FreeString(&crit2_line_str);
      memset( ( void * ) tmp6, '\0', sizeof(tmp6));
      sprintf(tmp6,"CRITERIA: %-20s\n",
   	      tmp4);
      Text_AddString(&buf, tmp6, 0);
   } 
   
   //
   //      Formatting for all additional lines.
   //
   while(crit1 || crit2)
   {
     if ( crit1 != NULL ) 
     {
        Text_GetLine(&crit1, dcp_num_cols, &crit1_line_str);
        strcpy(tmp3, crit1_line_str);
        Text_FreeString(&crit1_line_str);

        memset( ( void * ) tmp6, '\0', sizeof(tmp6));
        sprintf(tmp6,"                 %-20s %12s",
  	        tmp3, " ");
        Text_AddString(&buf, tmp6, 0);
      
      }      

     if ( crit2 != NULL ) 
     {
	Text_GetLine(&crit2, telm_num_cols, &crit2_line_str);
	strcpy(tmp4, crit2_line_str);
	Text_FreeString(&crit2_line_str);
	memset( ( void * ) tmp6, '\0', sizeof(tmp6));
	sprintf(tmp6,"          %-20s",
	        tmp4);
	Text_AddString(&buf, tmp6, 0);
	Text_AddString(&buf, "\n", 0);
     }

   }

   if ( crit1 != NULL ) Text_FreeString ( & crit1 ) ;
   if ( crit2 != NULL ) Text_FreeString ( & crit2 ) ;
   
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));

   if ( info->telem != NULL )
   {
      DataToString(&info->telem->cost, DOUBLE, tmp5, "%-7.2lf", "       ");
   }
   else
   {
      strcpy ( tmp5 , "       " ) ; 
   }

   sprintf(tmp4,"                 %18s     PAYOR/COST OF LINE: %-s / $ %s\n\n",
	   " ",
           info->telem ? info->telem->payor : "",
           tmp5 ) ;
   Text_AddString(&buf, tmp4, 0);
   

   
   count1 = Text_CountNewLines(buf);

   
   
   //
   //	Do column header.
   //
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1,"          GAGE TYPE   OWNER       MAINTENANCE BEGAN      ENDED     ");
   strcat (tmp1," GAGE LOCATION/REMARKS\n");
   strcat (tmp1,"          ----------- ----------- ----------- ---------- ----------");
   strcat (tmp1," --------------------------------------------------------"
	   "----\n");
   Text_AddString(&buf, tmp1, 0);

   
   
   count2 = Text_CountNewLines(tmp1);

   
   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;
   
   if ( info->gage != NULL ) gagePtr = (Gage *) ListFirst(&info->gage->list);

   memset( ( void * ) tmp8, '\0', sizeof(tmp8));
   strcat (tmp8, "\f");   


   while (gagePtr)
   {
      //
       //    Get the remark field.
      //	   Compute the number of lines needed for this record.
      //
      Text_AddString(&remark,
		     Text_WordWrap(gagePtr->remark, num_cols, left_margin),
		     1);
      needed = (Text_CountNewLines(remark) + 1)  + 1;

      if (needed <= avail)
      {
	 //
	 //	Formatting for Line 1.
	 //
	 Text_GetLine(&remark, num_cols, &line_str);

         if ( line_str != NULL )
         {
	    strcpy(tmp1, line_str);
	    Text_FreeString(&line_str);
         }
         else
	    strcpy(tmp1, "");

         date_t_to_USA_date ( gagePtr->gbegin, tmp3);
         date_t_to_USA_date ( gagePtr->gend, tmp4);
	 
	 memset( ( void * ) tmp2, '\0', sizeof(tmp2));
	 sprintf(tmp2,"          %-11s %-11s %-11s %10s %10s %-23s\n",
		 gagePtr->type, gagePtr->owner, gagePtr->maint,
		 tmp3, tmp4, tmp1);
	 Text_AddString(&buf, tmp2, 0);
	 
	 
	 //
	 //	Formatting for all additional lines.
	 //
	 while(remark)
	 {
	    Text_GetLine(&remark, left_margin+num_cols, &line_str);
	    strcpy(tmp1, line_str);
	    strcat(tmp1, "\n");
	    Text_AddString(&buf, tmp1, 0);
	    Text_FreeString(&line_str);
	 }
	 Text_FreeString(&remark);
	 
	 
	 //
	 //	Advance pointer.
	//	Update avail.
	 //	Continue from top of while loop.
	 //
	 gagePtr = (Gage *) ListNext(&gagePtr->node);
	 avail = avail - needed;
	 continue;

      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp7, '\0', sizeof(tmp7));
	 strcat(tmp7, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_GAGES,
				       "GAGES", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));

	 Text_AddString(&buf, tmp7, 0); 
	 Text_AddString(&buf, "\f", 0); 

	 
	 //
	 //	Do column header.
	 //
	 memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	 strcat (tmp1,"          GAGE TYPE   OWNER       MAINTENANCE BEGAN      ENDED     ");
	 strcat (tmp1," GAGE LOCATION/REMARKS\n");
	 strcat (tmp1,"          ----------- ----------- ----------- ---------- ----------");
	 strcat (tmp1," --------------------------------------------------------"
		 "----\n");
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 //
	//	Reset available value, keep pointer the same & continue
	//	from top of while loop.
	 //
	 avail = available + count1;
	 loop++;
	 continue;

      }
   }
   Text_AddString(&buf, "\n", 0);
   
   
   
   // try to place FOOTER at the bottom
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

   
   //
   //	Do footer.
   //
   memset( ( void * ) tmp7, '\0', sizeof(tmp7));
   strcat(tmp7, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_GAGES,
				 "GAGES", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));
   //Text_AddString(&buf, tmp7, 0);
 
   Text_AddString(&buf, tmp7, 0);
   Text_AddString(&buf, tmp8, 0);   
   
   return(buf);
}

//---------------------------------------------------------

char*	E19_History(e19_info_type *info, int reason)
{
   Pub*		pubPtr = NULL;
   Gage*	gagePtr = NULL;
   Datum*	datumPtr = NULL;
   
   char*	buf = NULL;
   
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;


   //
   //	Title
   //
//
//sprintf(tmp1, "				   HISTORY");
//
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_history);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);
   
   
   
   count1 = Text_CountNewLines(buf);

   

   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1,"      PUBLICATION/LOCATION OF RECORDS       STARTING DATE    ");
   strcat (tmp1,"ENDING DATE\n");
   strcat (tmp1,"      -------------------------------       -------------    ");
   strcat (tmp1,"-----------\n");
   Text_AddString(&buf, tmp1, 0);

   
   count2 = Text_CountNewLines(tmp1);

      
      
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;

   if ( info->pub != NULL ) pubPtr = ( Pub * ) ListFirst ( & info->pub->list );

   while (pubPtr)
   {
      date_t_to_USA_date ( pubPtr->pbegin, tmp2);
      date_t_to_USA_date ( pubPtr->pend, tmp3);
      memset( ( void * ) tmp1, '\0', sizeof(tmp1));
      sprintf(tmp1,"      %-25s      %7s%10s       %10s\n",
	      pubPtr->ppub, " ", tmp2, tmp3);
      needed = 1;
      
      
      if (needed <= avail)
      {
	 Text_AddString(&buf, tmp1, 0);
	 
	 pubPtr = (Pub *) ListNext(&pubPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp4, '\0', sizeof(tmp4));
	 strcat(tmp4, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_HISTORY,
				       "HISTORY", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));

	 
	 //
	 //	Do column header.
	 //
	 strcat (tmp1,"      PUBLICATION/LOCATION OF RECORDS       STARTING DATE    ");
	 strcat (tmp1,"ENDING DATE\n");
	 strcat (tmp1,"      -------------------------------       -------------    ");
	 strcat (tmp1,"-----------\n");
	 Text_AddString(&buf, tmp1, 0);
	 

	 avail = available + count1;
	 loop++;
	 continue;
	 
      }
      
   }
   Text_AddString(&buf, "\n\n", 0);

   
   
   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1,"      TYPE OF GAGE              OWNER       STARTING DATE    ");
   strcat (tmp1,"ENDING DATE\n");
   strcat (tmp1,"      ------------              -----       -------------    ");
   strcat (tmp1,"-----------\n");
   Text_AddString(&buf, tmp1, 0);

   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;
   
   if ( info->gage != NULL ) gagePtr = (Gage *) ListFirst(&info->gage->list);

   while (gagePtr)
   {
      
      date_t_to_USA_date ( gagePtr->gbegin, tmp2 );
      date_t_to_USA_date ( gagePtr->gend, tmp3 );
      
      memset( ( void * ) tmp1, '\0', sizeof(tmp1));
      sprintf(tmp1,"      %-11s %14s%-11s %10s       %10s\n",
	      gagePtr->type, " ", gagePtr->owner, tmp2, tmp3);
      needed = 1;
      
      
      if (needed <= avail)
      {
	 Text_AddString(&buf, tmp1, 0);
	 
	 gagePtr = (Gage *) ListNext(&gagePtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp4, '\0', sizeof(tmp4));
	 strcat(tmp4, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_HISTORY,
				       "HISTORY", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));

	 
	 //
	 //	Do column header.
	 //
	 memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	 sprintf(tmp1,"      TYPE OF GAGE              OWNER       STARTING DATE    ");
	 strcat (tmp1,"ENDING DATE\n");
	 strcat (tmp1,"      ------------              -----       -------------    ");
	 strcat (tmp1,"-----------\n");
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 avail = available + count1;
	 continue;
	 
      }
      
   }
   Text_AddString(&buf, "\n\n", 0);


   
   

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1,"      ZERO ELEVATION                        STARTING DATE\n");
   strcat (tmp1,"      --------------                        -------------\n");
   Text_AddString(&buf, tmp1, 0);


   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;
   
   if ( info->datum != NULL ) 
   {
      datumPtr = (Datum *) ListFirst(&info->datum->list);
   }

   while (datumPtr)
   {
      memset( ( void * ) tmp2, '\0', sizeof(tmp2));
      memset( ( void * ) tmp3, '\0', sizeof(tmp3));
      DataToString(&datumPtr->elev, DOUBLE, tmp2, "%8.3lf", "        ");
      date_t_to_USA_date ( datumPtr->ddate, tmp3);
      
      memset( ( void * ) tmp1, '\0', sizeof(tmp1));
      sprintf(tmp1,"      %s      %24s%10s\n",
	      tmp2, " ", tmp3);
      needed = 1;
      
      
      if (needed <= avail)
      {
	 Text_AddString(&buf, tmp1, 0);
	 
	 datumPtr = (Datum *) ListNext(&datumPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp4, '\0', sizeof(tmp4));
	 strcat(tmp4, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_HISTORY,
				       "HISTORY", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));

	 
	 //
	 //	Do column header.
	 //
	 memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	 sprintf(tmp1,"      ZERO ELEVATION                        STARTING DATE\n");
	 strcat (tmp1,"      --------------                        -------------\n");
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 avail = available + count1;
	 continue;
	 
      }
      
   }
   Text_AddString(&buf, "\n\n", 0);

   
   
   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
   

   
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   strcat(tmp4, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_HISTORY,
				 "HISTORY", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));

   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   strcat (tmp5, "\f");   
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);
   
   
   return(buf);
}


//---------------------------------------------------------
char*	E19_Crests(e19_info_type *info, int reason)
{
   Crest*	crestPtr = NULL;
   
   char*	buf = NULL;
   
   char		tmp0[E19_BUFSIZ];
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
   char		tmp8[E19_BUFSIZ];
   char		tmp9[E19_BUFSIZ];
   char		tmp10[E19_BUFSIZ];
   
   char*	remark = NULL;
   char*	line_str = NULL;

   int		num_cols = 50;
   int		left_margin = 73;
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;
   

memset( ( void * ) tmp10, '\0', sizeof(tmp10));
strcat (tmp10, "\f");
   //
   //	Title
   //
//
//sprintf(tmp1, "				    CRESTS");
//
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_crests);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);
   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   
   if ( info->river != NULL )
   {
      DataToString(&info->river->fs,   DOUBLE, tmp2, "%-6.2lf", "      ");
      DataToString(&info->river->wstg, DOUBLE, tmp3, "%-6.2lf", "      ");
      DataToString(&info->river->bf,   DOUBLE, tmp4, "%-6.2lf", "      ");

      if((IsNull(DOUBLE,(void *) &info->river->fq)) == ISNULL)
         sprintf(tmp5, "      ");
      else
         sprintf(tmp5, "%-8.0f", info->river->fq);

      if((IsNull(DOUBLE,(void *) &info->river->action_flow)) == ISNULL)
         sprintf(tmp0, "      ");
      else
         sprintf(tmp0, "%-8.0f", info->river->action_flow);
   }
   else
   {
      strcpy ( tmp2 , "      " ) ; 
      strcpy ( tmp3 , "      " ) ; 
      strcpy ( tmp4 , "      " ) ; 
      sprintf ( tmp5 , "      " ) ;
      sprintf ( tmp0 , "      " ) ;
   }

   sprintf(tmp1,"      FLOOD STAGE: %s   ACTION STAGE: %s    BANKFULL STAGE: %s\n",
	   tmp2, tmp3, tmp4);
   //
  // sprintf(tmp2,"       FLOOD FLOW: %s\n\n", tmp5);
   //
   sprintf(tmp2,"       FLOOD FLOW: %s    ACTION FLOW: %s\n\n", tmp5, tmp0);
   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);

   
   
   count1 = Text_CountNewLines(buf);
      
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   strcat (tmp3,"          DATE OF    TIME   CREST  FLOW   FROM HIGH  BASED ON  ");
   strcat (tmp3,"CAUSED BY\n");
   strcat (tmp3,"          CREST      LST    (ft)   (CFS)  WATERMARKS OLD DATUM ");
   strcat (tmp3,"ICE JAM   REMARKS\n");
   strcat (tmp3,"          ---------- ------ ------ ------ ---------- --------- ");
   strcat (tmp3,"--------- --------------------------------------------------\n");
   Text_AddString(&buf, tmp3, 0);

   
   count2 = Text_CountNewLines(tmp3);
   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2  -5;
   
   avail = available;
   avail -= 2;
   loop = 0;
   
   if ( info->crest != NULL ) 
   {
       crestPtr = (Crest *) ListFirst(&info->crest->list);
   }

   while (crestPtr)
   {
            
      //
        //   Get the remark field.
      //
      Text_AddString(&remark,
		     Text_WordWrap(crestPtr->cremark, num_cols, left_margin),
		     1);
      needed = Text_CountNewLines(remark) + 1;
      
      
      if (needed <= avail)
      {
        
	 //
	 //	Formatting for Line 1.
	 //
	 Text_GetLine(&remark, num_cols, &line_str);

	 if ( line_str != NULL )
         {
            strcpy ( tmp1 , line_str ) ;
	    Text_FreeString ( & line_str ) ;
         }
	 else
	    strcpy ( tmp1 , " " ) ;

         date_t_to_USA_date ( crestPtr->datcrst, tmp3);
	 DataToString(&crestPtr->stage, DOUBLE, tmp4, "%6.2lf", "      ");
	 
	 memset( ( void * ) tmp5, '\0', sizeof(tmp5));
	 if((IsNull(INT,(void *) &crestPtr->q)) == ISNULL)
	    sprintf(tmp5, "      ");
	 else
	    sprintf(tmp5, "%6ld", crestPtr->q);
	 
	 DataToString(&crestPtr->hw, CHAR, tmp6, "%c", " ");
	 DataToString(&crestPtr->olddatum, CHAR, tmp7, "%c", " ");
	 DataToString(&crestPtr->jam, CHAR, tmp8, "%c", " ");
	 
	 memset( ( void * ) tmp2, '\0', sizeof(tmp2));
	 sprintf(tmp2,"          %10s %-6s %s %s     %s %7s  %s %7s %s     %-17s\n",
		 tmp3, crestPtr->timcrst, tmp4, tmp5,
		 tmp6, " ", tmp7, " ", tmp8, tmp1);
	 Text_AddString(&buf, tmp2, 0);
	 
	 
	 //
	 //	Formatting for all additional lines.
	 //
	 while(remark)
	 {
	    Text_GetLine(&remark, left_margin+num_cols, &line_str);
	    strcpy(tmp1, line_str);
	    strcat(tmp1, "\n");
	    Text_AddString(&buf, tmp1, 0);
	    Text_FreeString(&line_str);
	 }
	 Text_FreeString(&remark);
	 
	 
	 crestPtr = (Crest *) ListNext(&crestPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp9, '\0', sizeof(tmp9));
	 strcat(tmp9, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_CRESTS,
				       "CRESTS", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));
	 Text_AddString(&buf, tmp9, 0);
	 Text_AddString(&buf, tmp10, 0);
	 
	  //
	// 	Do column header.
	 //
	 memset( ( void * ) tmp3, '\0', sizeof(tmp3));
	 
	 strcat (tmp3,"\n\n");
	 strcat (tmp3,"          DATE OF    TIME   CREST  FLOW   FROM HIGH  BASED ON  ");
	 strcat (tmp3,"CAUSED BY\n");
	 strcat (tmp3,"          CREST      LST    (ft)   (CFS)  WATERMARKS OLD DATUM ");
	 strcat (tmp3,"ICE JAM   REMARKS\n");
	 strcat (tmp3,"          ---------- ------ ------ ------ ---------- --------- ");
	 strcat (tmp3,"--------- --------------------------------------------------\n");
	 Text_AddString(&buf, tmp3, 0);	 
	 	 	 
	 avail = available + count1;
	 loop++;
	 continue;
	 
      }
      
       
   }
   
  // Text_AddString(&buf, "\n\n", 0);
   
   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

   
   
   memset( ( void * ) tmp9, '\0', sizeof(tmp9));
   strcat(tmp9, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_CRESTS,
				 "CRESTS", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));
   Text_AddString(&buf, tmp9, 0);
   Text_AddString(&buf, tmp10, 0);

   
   return(buf);
}

//---------------------------------------------------------

char*	E19_LowWater(e19_info_type *info, int reason)
{
   Lowwater*	lwPtr = NULL;
   
   char*	buf = NULL;
   
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
   
   char*	remark = NULL;
   char*	line_str = NULL;

   int		num_cols = 45;
   int		left_margin = 34;
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;
   

   //
   //	Title
   //
//
//sprintf(tmp1, "				LOW WATER RECORDS");
//
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_lowwater);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);

   
   
   count1 = Text_CountNewLines(buf);


   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1,"     DATE OF     STAGE    FLOW\n");
   strcat (tmp1,"     LOW WATER   (ft)     (CFS)   REMARKS\n");
   strcat (tmp1,"     ----------  -------  ------  -------------------------");
   strcat (tmp1,"--------------------\n");
   Text_AddString(&buf, tmp1, 0);
   

   
   count2 = Text_CountNewLines(tmp1);

      
      
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;
   
   if ( info->low != NULL ) 
   {
      lwPtr = ( Lowwater * ) ListFirst ( &info->low->list ) ;
   }

   while (lwPtr)
   {
      //
      //     Get the remark field.
      //
      Text_AddString(&remark,
		     Text_WordWrap(lwPtr->lwrem, num_cols, left_margin),
		     1);
      needed = Text_CountNewLines(remark) + 1;
      
      
      if (needed <= avail)
      {
	 //
	 //	Formatting for Line 1.
	 //
	 Text_GetLine(&remark, num_cols, &line_str);
	 memset( ( void * ) tmp1, '\0', sizeof(tmp1));
         
         if ( line_str != NULL )
         { 
	    strcpy(tmp1, line_str);
	    Text_FreeString(&line_str);
         }
	 
         date_t_to_USA_date ( lwPtr->lwdat, tmp3 );
	 DataToString(&lwPtr->stage, DOUBLE, tmp4, "%7.2lf", "       ");
	 
	 memset( ( void * ) tmp5, '\0', sizeof(tmp5));
	 if((IsNull(INT,(void *) &lwPtr->q)) == ISNULL)
	    sprintf(tmp5, "      ");
	 else
	    sprintf(tmp5, "%6ld", lwPtr->q);
	 
	 memset( ( void * ) tmp2, '\0', sizeof(tmp2));
	 sprintf(tmp2,"     %10s  %s  %s  %-45s\n",
		 tmp3, tmp4, tmp5, tmp1);
	 Text_AddString(&buf, tmp2, 0);
	 
	 
	 //
	 //	Formatting for all additional lines.
	 //
	 while(remark)
	 {
	    Text_GetLine(&remark, left_margin+num_cols, &line_str);
	    memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	    strcpy(tmp1, line_str);
	    strcat(tmp1, "\n");
	    Text_AddString(&buf, tmp1, 0);
	    Text_FreeString(&line_str);
	 }
	 Text_FreeString(&remark);
	 
	 
	 lwPtr = (Lowwater *) ListNext(&lwPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	 
	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp6, '\0', sizeof(tmp6));
	 strcat(tmp6, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_LOWWATER,
				       "LOW WATER", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));
	 
	 
	 //
	 //	Do column header.
	 //
	 strcat (tmp1,"     DATE OF     STAGE    FLOW\n");
	 strcat (tmp1,"     LOW WATER   (ft)     (CFS)   REMARKS\n");
	 strcat (tmp1,"     ----------  -------  ------  -------------------------");
	 strcat (tmp1,"--------------------\n");
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 avail = available + count1;
	 loop++;
	 continue;
	 
      }
   }
   Text_AddString(&buf, "\n\n", 0);

   
   
   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

   
   
   memset( ( void * ) tmp6, '\0', sizeof(tmp6));
   strcat(tmp6, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_LOWWATER,
				 "LOW WATER", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));

   memset( ( void * ) tmp7, '\0', sizeof(tmp7));
   strcat (tmp7, "\f");
   Text_AddString(&buf, tmp6, 0);
   Text_AddString(&buf, tmp7, 0);
   
   
   return(buf);
}
//---------------------------------------------------------


char*	E19_Conditions(e19_info_type *info, int reason)
{
   char*	buf = NULL;
   
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
   char		tmp8[E19_BUFSIZ];
   char		tmp9[E19_BUFSIZ];
   char		tmp10[E19_BUFSIZ];
   
   int		num_cols = 60;
   int		left_margin = 17;
   int		init_footer = True;
   

   //
   //	Title
   //
//
//sprintf(tmp1, "			    CONDITIONS AFFECTING FLOW");
//
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_conditions);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);

   if ( info->river != NULL )
   {
      DataToString(&info->river->mile, DOUBLE, tmp2, "%-6.1lf", "      ");
      DataToString(&info->river->da,   DOUBLE, tmp3, "%-6.1lf", "      ");
      DataToString(&info->river->pool, DOUBLE, tmp4, "%-6.1lf", "      ");
   }
   else
   {
      strcpy ( tmp2 , "      " ) ; 
      strcpy ( tmp3 , "      " ) ; 
      strcpy ( tmp4 , "      " ) ; 
   }

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   sprintf(tmp1,"     MILES ABOVE MOUTH: %s%sDRAINAGE AREA: %s%sPOOL STAGE: %s",
	   tmp2, "     ", tmp3, "     ", tmp4);
   strcat(tmp1, "\n\n\n");
   
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   memset( ( void * ) tmp5, '\0', sizeof(tmp5));
   memset( ( void * ) tmp6, '\0', sizeof(tmp6));
   memset( ( void * ) tmp7, '\0', sizeof(tmp7));
   memset( ( void * ) tmp8, '\0', sizeof(tmp8));
   memset( ( void * ) tmp9, '\0', sizeof(tmp9));

   sprintf(tmp2,"     STREAM BED: %s\n\n",
	   Text_WordWrap(info->descrip ? info->descrip->bed : NULL , 
                         num_cols, 
                         left_margin));
   
   sprintf(tmp3,"          REACH: %s\n\n",
	   Text_WordWrap( info->descrip ? info->descrip->reach : NULL , 
                          num_cols, 
                          left_margin));
   
   sprintf(tmp5,"     REGULATION: %s\n\n",
	   Text_WordWrap(info->descrip ? info->descrip->res : NULL , 
                         num_cols, 
                         left_margin));
   
   sprintf(tmp6,"      DIVERSION: %s\n\n",
	   Text_WordWrap(info->descrip ? info->descrip->divert : NULL , 
                         num_cols, 
                         left_margin));
   
   sprintf(tmp7,"         WINTER: %s\n\n",
	   Text_WordWrap(info->descrip ? info->descrip->ice : NULL , 
                         num_cols, 
                         left_margin));
   
   sprintf(tmp8,"     TOPOGRAPHY: %s\n\n",
	   Text_WordWrap(info->descrip ? info->descrip->topo : NULL , 
                         num_cols, 
                         left_margin));
   
   sprintf(tmp9,"        REMARKS: %s\n\n",
	   Text_WordWrap(info->descrip ? info->descrip->remark : NULL , 
                         num_cols, 
                         left_margin));

   Text_AddString(&buf, tmp1, 0);
   Text_AddString(&buf, tmp2, 0);
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   Text_AddString(&buf, tmp5, 0);
   Text_AddString(&buf, tmp6, 0);
   Text_AddString(&buf, tmp7, 0);
   Text_AddString(&buf, tmp8, 0);
   Text_AddString(&buf, tmp9, 0);
   Text_AddString(&buf, "\n\n", 0);



   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, 0), 0);

   
      
   memset( ( void * ) tmp9, '\0', sizeof(tmp9));
   strcat(tmp9, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_CONDITIONS,
				 "CONDITIONS", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));
   
   memset( ( void * ) tmp10, '\0', sizeof(tmp10));
   strcat (tmp10, "\f");
   Text_AddString(&buf, tmp9, 0);
   Text_AddString(&buf, tmp10, 0);
   
   return(buf);
}


//---------------------------------------------------------
char*	E19_Damage(e19_info_type *info, int reason)
{
   Flood*	floodPtr = NULL;
   
   char*	buf = NULL;
   
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   
   int		num_cols = 100;
   int		left_margin = 23;
   
   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;

   
   //
   //	Title
   //
//
//sprintf(tmp1, "				    DAMAGE");
//
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_damage);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);
   
   
   
   count1 = Text_CountNewLines(buf);
   
   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1,"              STAGE    AREAS AFFECTED\n");
   strcat (tmp1,"              -------  ---------------------------------------------");
   strcat (tmp1,"-------------------------------------------------------\n");
   Text_AddString(&buf, tmp1, 0);

   
   
   count2 = Text_CountNewLines(tmp1);

   
   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 5;
   avail = available;
   loop = 0;
   
   if ( info->flood != NULL ) 
   {
      floodPtr = (Flood *) ListFirst(&info->flood->list);
   }

   while (floodPtr)
   {
      DataToString(&floodPtr->stage, DOUBLE, tmp2, "%7.2lf", "       ");
      memset( ( void * ) tmp1, '\0', sizeof(tmp1));
      sprintf(tmp1,"              %s  %s\n\n", tmp2,
	      Text_WordWrap(floodPtr->damage, num_cols, left_margin));
      needed = Text_CountNewLines(tmp1);
      
      
      if (needed <= avail)
      {
	 Text_AddString(&buf, tmp1, 0);
	 
	 floodPtr = (Flood *) ListNext(&floodPtr->node);
	 avail = avail - needed;
	 continue;
	 
      }
      else if (needed > avail)
      {
	 // try to place FOOTER at the bottom 
	 Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);

	 
	 //
	 //	Do footer.
	 //
	 memset( ( void * ) tmp3, '\0', sizeof(tmp3));
	 strcat(tmp3, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				       "NWS FORM E-19", E19_DAMAGE,
				       "DAMAGE", NULL, E19_STANDARD_LEFT_MARGIN,
				       &init_footer, reason));
	 strcat(tmp3, "\n\n");
	 
	 
	 //
	 //	Do column header.
	 //
	 memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	 strcat (tmp1,"              STAGE    AREAS AFFECTED\n");
	 strcat (tmp1,"              -------  ---------------------------------------------");
	 strcat (tmp1,"-------------------------------------------------------\n");
	 Text_AddString(&buf, tmp1, 0);
	 
	 
	 avail = available + count1;
	 loop++;
	 continue;
	 
      }
   }
   Text_AddString(&buf, "\n\n", 0);

   

   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);


   
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   strcat(tmp3, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_DAMAGE,
				 "DAMAGE", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));


   memset( ( void * ) tmp4, '\0', sizeof(tmp4));
   strcat (tmp4, "\f");
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp4, 0);
   
   return(buf);
}

//---------------------------------------------------------

char*	E19_StaffGage(e19_info_type *info, int reason)
{
   Flood	*floodHead = NULL,
      		*floodPtr = NULL;
   
   Crest	*crestHead = NULL,
      		*crestPtr = NULL;
   
   char		*buf = NULL;

   char		where[BUFSIZ];
   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
   
   int		count1 = 0;
   int		num_cols = 85;		// width of flood text 
   int		left_margin = 12;

   int		flood_filler = 100;	// so flood section is space-buffered 
   int		crest_filler = 20;	// so crest section is space-buffered 

   int		init_footer = True;	// for correct numbering of pages 
   int		loop;			// for correct numbering of pages 

   int		linesAvailable = 0;	// ensures correct num of lines/page 
   int		linesNeeded = 0;	// ensures lines will fit on page 

   char*	record = NULL;
   char*	line_str = NULL;
   
   int		maxstage;	// maximum of flood and crest stage 
   int		minstage;	// minimum of flood and crese stage 
   int		scaleDiff=0;
   float	range;		// maxstage - minstage    
   float	feetStagePerLine = 0.0 ; // (range / lines_per_page) 

   float	currentStage;
   
   int		lines_per_page = 51;	// max printable lines per page 
   int		tickmark_gap   = 5;	// distance between tickmarks   
   int		currentTick = 0;	// loop control variable (for tickmarks) 

   
   // get the needed data for staff gage from tables Flood and Crest 
   
   memset( ( void * ) where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", TextReports_getCurrentLid());
   strcat(where, " ORDER BY stage desc ");
   floodHead = GetFlood(where);
   
   memset( ( void * ) where, '\0', sizeof(where));
   sprintf(where, " WHERE lid = '%s' ", TextReports_getCurrentLid());
   strcat(where, " AND    ((suppress != 'X') OR (suppress is null)) ");
   strcat(where, " ORDER BY stage desc ");
   crestHead = GetCrest(where);
   

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_staffgage);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);
   
   count1 = Text_CountNewLines(buf);

   // init available lines per page
   linesAvailable = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - 7;
   loop = 0;
   
 if ( (floodHead != NULL) || (crestHead != NULL) )
 {
   // find the min and max stage values found in the Flood and Crest tables
   minstage = INT_MAX;
   maxstage = INT_MIN;

   if ( floodHead != NULL )
	floodPtr = (Flood *) ListFirst(&floodHead->list);

   if (floodPtr)
   {
      while (floodPtr)
      {
	 if (floodPtr->stage < minstage)
	    minstage = (int) floodPtr->stage;
	 
	 if (floodPtr->stage > maxstage)
	    maxstage = (int) floodPtr->stage;
	 
	 floodPtr = (Flood *) ListNext(&floodPtr->node);
      }
   }

   if ( crestHead != NULL )
	crestPtr = (Crest *) ListFirst(&crestHead->list);

   if (crestPtr)
   {
      while (crestPtr)
      {
	 if (crestPtr->stage < minstage)
	    minstage = (int) crestPtr->stage;
	 
	 if (crestPtr->stage > maxstage)
	    maxstage = (int) crestPtr->stage;
	 
	 crestPtr = (Crest *) ListNext(&crestPtr->node);
      }
   }

   maxstage += 1;
      
   // determine the range & feetStagePerLine (feet per line) 
      
   range = maxstage - minstage;
   feetStagePerLine = (range / (float)lines_per_page);

   if (feetStagePerLine < 0.2)
	feetStagePerLine = 0.2;
   else
   if (feetStagePerLine < 0.4)
	feetStagePerLine = 0.4;
   else
   if (feetStagePerLine < 1.0)
	feetStagePerLine = 1.0;
   else
   if (feetStagePerLine < 2.0)
	feetStagePerLine = 2.0;
   else
   if (feetStagePerLine < 5.0)
	feetStagePerLine = 5.0;

   // set the maxstage based on the minstage + lines per page and feet/line 
   scaleDiff = ((lines_per_page-1) * feetStagePerLine) - range;

   maxstage += scaleDiff/2;
         
   // reset pointers 
   if ( floodHead != NULL ) floodPtr = ( Flood * ) ListFirst ( & floodHead->list ) ;
   if ( crestHead != NULL ) crestPtr = ( Crest * ) ListFirst ( & crestHead->list ) ;

   if (floodPtr)
   {
      // Compute the number of lines needed for first flood record. 
      DataToString(&floodPtr->stage, DOUBLE, tmp1, "%7.2lf", "       ");
      Text_AddString(&record, "  ", 1);
      Text_AddString(&record, tmp1, 0);
      Text_AddString(&record, " - ", 0);
      Text_AddString(&record, Text_WordWrap(floodPtr->damage, num_cols, left_margin), 0);
      linesNeeded = Text_CountNewLines(record) + 1;
   }

   currentStage = maxstage;	// currentStage is decremented by feetStagePerLine 
   
   for (currentTick = 0; currentTick < lines_per_page; currentTick++)
   {

	 if (linesNeeded <= linesAvailable)
	 {
	    //
	    //	---------------------
	    //	list the flood events
	//	---------------------
	    //
	    if ( floodPtr && (currentStage <= floodPtr->stage) )
	    {
	       // get one line at a time from record (place result in tmp4). 
	       
	       Text_GetLine(&record, num_cols, &line_str);
	       memset( ( void * ) tmp4, '\0', sizeof(tmp4));
	       memset( ( void * ) tmp4, ' ', (flood_filler * sizeof(char)));
	       strncpy(tmp4, line_str, strlen(line_str));
	       Text_FreeString(&line_str);
	       
	       Text_AddString(&buf, tmp4, 0);
	       
	       if (record == NULL) // (no more lines in this record) 
	       {
		  // get the next record 
		  floodPtr = (Flood *) ListNext(&floodPtr->node);
		  
		  if (floodPtr)
		  {
		     // Compute the number of lines needed for new flood record. 
		     DataToString(&floodPtr->stage, DOUBLE, tmp1, "%7.2lf", "       ");
		     Text_AddString(&record, "  ", 1);
		     Text_AddString(&record, tmp1, 0);
		     Text_AddString(&record, " - ", 0);
		     Text_AddString(&record, Text_WordWrap(floodPtr->damage, num_cols, left_margin), 0);
		     linesNeeded = Text_CountNewLines(record) + 1;
		  }
	       }
	    }
	    else  // no flood information on this line 
	    {
	       memset( ( void * ) tmp4, '\0', sizeof(tmp4));
	       memset( ( void * ) tmp4, ' ', (flood_filler * sizeof(char)));
	       Text_AddString(&buf, tmp4, 0);
	    }
	    
	    
	    //
	    //	-------------
	    //	draw the gage
	//	-------------
	    //
	    if (reason == TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE)
	    {
	       memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	       Text_AddString(&buf, tmp1, 0);
	    }
	    
	    
	    if ((currentTick % tickmark_gap) == 0)  // then show gage with tickmark 
	    {
	       Text_AddString(&buf, E19_BuildStaffGageString(&currentStage), 0);
	    }
	    else  // then show gage with delete/rubout characters 
	    {
	       Text_AddString(&buf, E19_BuildStaffGageString(NULL), 0);
	    }

	    
	    if (reason == TEXTREPORTS_REASON_PRINT_EMAIL_OR_SAVE)
	    {
	       memset( ( void * ) tmp1, '\0', sizeof(tmp1));
	       Text_AddString(&buf, tmp1, 0);
	    }
	    

	    //
	    //	---------------------
	    //	list the crest events
	//	---------------------
	    //
	    if ( crestPtr && (currentStage <= crestPtr->stage) )
	    {
	       memset( ( void * ) tmp2, '\0', sizeof(tmp2));
	       DataToString(&crestPtr->stage, DOUBLE, tmp2, "%8.2lf", "        ");
	       memset( ( void * ) tmp3, '\0', sizeof(tmp3));
               date_t_to_USA_date ( crestPtr->datcrst, tmp3 );
	       memset( ( void * ) tmp6, '\0', sizeof(tmp6));
	       sprintf(tmp6,"%s  %s", tmp2, tmp3);
	       Text_AddString(&buf, tmp6, 0);
	       
	       while ( crestPtr && (crestPtr->stage >= currentStage) )
		  crestPtr = (Crest *) ListNext(&crestPtr->node);
	    }
	    else  // no crest information on this line 
	    {
	       memset( ( void * ) tmp6, '\0', sizeof(tmp6));
	       memset( ( void * ) tmp6, ' ', (crest_filler * sizeof(char)));
	       Text_AddString(&buf, tmp6, 0);
	    }

	    
	    //
	    //	-------------------
	    //	end of current line
	//	-------------------
	    //
	       
	    Text_AddString(&buf, "\n", 0);
	    currentStage -= feetStagePerLine;
	    
	    linesAvailable--;
	    continue;

	 } // lines needed less than available 
	 
   } // for currentTick 

 } // if Floodhead or CrestHead not null 
   
   // free memory 

   Text_FreeString(&record);
   if (floodHead)
   {
      FreeFlood(floodHead);
      floodHead = NULL;
   }
   if (crestHead)
   {
      FreeCrest(crestHead);
      crestHead = NULL;
   }
   
   
   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
   
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   sprintf(tmp2, "REACH: %-80s	ELEVATION ZERO: %7.2f\n\n",
	   info->descrip ? info->descrip->reach : "" , 
           info->river ? info->river->zd : 0. );
   strcat(tmp3, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_STAFFGAGE,
				 "STAFF", tmp2, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));
   

   memset( ( void * ) tmp7, '\0', sizeof(tmp7));
   strcat (tmp7, "\f");
   Text_AddString(&buf, tmp3, 0);
   Text_AddString(&buf, tmp7, 0);


   return(buf);

}

//---------------------------------------------------------

char*	E19_Contacts(e19_info_type *info, int reason)
{
   char		*buf = NULL;
   
   Contacts	*contactsPtr = NULL;

   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];
   char		tmp5[E19_BUFSIZ];
   char		tmp6[E19_BUFSIZ];
   char		tmp7[E19_BUFSIZ];
   char		tmp8[E19_BUFSIZ];
   
   int		count = 0 ;
   int		num_cols, left_margin;

   int		available, avail;
   int		needed;
   int		count1, count2;
   int		loop;
   int		init_footer = True;

   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   strcat (tmp1, e19_hdr_contacts);
   strcat (tmp1, "\n\n");
   Text_AddString(&buf, tmp1, 1);
   
   
   
   count1 = Text_CountNewLines(buf);
   
   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));

   sprintf(tmp2, "     SQ  CONTACT/REMARKS                                    PHONE\n");
   sprintf(tmp3, "     --  -------------------------------------------------- ------------------\n");

   strcat(tmp1, tmp2);
   strcat(tmp1, tmp3);
   Text_AddString(&buf, tmp1, 0);


   count2 = Text_CountNewLines(tmp1);

   
   
   available = TEXTREPORTS_MAX_LINES_PER_PAGE - count1 - count2 - 4;
   avail = available;
   loop = 0;

   memset( ( void * ) tmp8, '\0', sizeof(tmp8));
   strcat (tmp8, "\f");

  
   if ( info->contacts != NULL )
   {
      contactsPtr = (Contacts *) ListFirst(&info->contacts->list);
      count = ListCount(&info->contacts->list);
   }

   if (count > 0)
   {
      while(contactsPtr)
      {
	 //
	 //	Do contact & phone.
	 //
	 memset( ( void * ) tmp4, '\0', sizeof(tmp4));
	 sprintf(tmp4, "     %2ld  %-51s%s\n",
		 contactsPtr->priority, contactsPtr->contact, contactsPtr->phone);
	 
	 //
	 //	Do email address.
	 //
	 memset( ( void * ) tmp6, '\0', sizeof(tmp6));
	 sprintf(tmp6, "         %-60s\n",
		 contactsPtr->email);
	 
	 //
	 //	Do remark.
	 //
	 num_cols    = 48;
	 left_margin = 11;
	 memset( ( void * ) tmp5, '\0', sizeof(tmp5));
	 sprintf(tmp5, "           %-48s\n\n",
		 Text_WordWrap(contactsPtr->remark, num_cols, left_margin));
	 
	 needed = (Text_CountNewLines(tmp5) + 1) + 1 + 1;
	 
	 if (needed <= avail)
	 {
	    Text_AddString(&buf, tmp4, 0);
	    Text_AddString(&buf, tmp6, 0);
	    Text_AddString(&buf, tmp5, 0);
	    
	    contactsPtr = (Contacts *) ListNext(&contactsPtr->node);
	    avail = avail - needed;
	    continue;
	    
	 }
	 else if (needed > avail)
	 {
	    // try to place FOOTER at the bottom 
	    Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);
	    
	    
	    //
	    //	Do footer.
	    //
	    memset( ( void * ) tmp7, '\0', sizeof(tmp7));
	    strcat(tmp7, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
					  "NWS FORM E-19", E19_CONTACTS,
					  "CONTACTS", NULL, E19_STANDARD_LEFT_MARGIN,
					  &init_footer, reason));
	    
	    Text_AddString(&buf, tmp7, 0);
	    Text_AddString(&buf, "\f", 0);
	    
	    //
	    //	Do column header.
	    //
	    sprintf(tmp2, "     %-55s  PHONE\n",
		    "CONTACT/REMARKS");
	    sprintf(tmp3, "     %-20s%-20s%-15s  %-18s\n",
		    "--------------------", "--------------------",
		    "---------------", "------------------");
	    Text_AddString(&buf, tmp1, 0);
	    Text_AddString(&buf, tmp2, 0);
	    Text_AddString(&buf, tmp3, 0);
	    
	    
	    avail = available + count1;
	    loop++;
	    continue;
	    
	 }
      }
   }


   
   // try to place FOOTER at the bottom 
   Text_AddString(&buf, E19_AdvanceToFooter(buf, FOOTER_POSITION, loop), 0);


   
   memset( ( void * ) tmp7, '\0', sizeof(tmp7));
   strcat(tmp7, E19_CreateFooter(info, E19_RREVISE_TYPE, (char *) TextReports_GetDate(),
				 "NWS FORM E-19", E19_CONTACTS,
				 "CONTACTS", NULL, E19_STANDARD_LEFT_MARGIN,
				 &init_footer, reason));

   Text_AddString(&buf, tmp7, 0);
   //Text_AddString(&buf, tmp8, 0);
   
   return(buf);
}

//---------------------------------------------------------

e19_info_type*	get_E19_Info(int init_flag)
{
   e19_info_type*	infoPtr = e19InfoPtr;  // refers to global pointer 

   
   if (init_flag)
   {
      E19_FreeInfo(&infoPtr);
      
      if (infoPtr == (e19_info_type *) NULL)
      {
	 infoPtr = (e19_info_type *) malloc (sizeof(e19_info_type));
	 memset(infoPtr, 0, sizeof(e19_info_type));
	 
	 E19_SetupInfo(infoPtr);	// access the database 
      }
   }

   
   return(infoPtr);
}
//---------------------------------------------------------


void	set_E19_Info_Ptr(e19_info_type* infoPtr)
{
   e19InfoPtr = infoPtr;
   return;
}
//---------------------------------------------------------


void	E19_SetupInfo(e19_info_type *infoPtr)
{
   char	where[BUFSIZ];
   char	temp_where[BUFSIZ];
   
   
   /*
   	Get information for the following tables:
	
	Benchmark, Crest, Dcp, Location, Telem, Observer,
	Riverstat, Gage, Datum, Pub, Refer, Lowwater, Descrip,
	Flood, Floodstmt, Floodcat, Reservoir,
	Rating, & Contacts.
   */

   memset( ( void * ) where, '\0', sizeof(where));
   sprintf(where, "WHERE lid = '%s'", TextReports_getCurrentLid());
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY bnum ");
   infoPtr->bench = GetBenchmark(temp_where);

   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY datcrst, timcrst ");
   infoPtr->crest = GetCrest(temp_where);
   
   infoPtr->dcp = GetDcp(where);
   infoPtr->loc = GetLocation(where);
   infoPtr->telem = GetTelem(where);
   infoPtr->obs = GetObserver(where);
   infoPtr->river = GetRiverstat(where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY gbegin, type ");
   infoPtr->gage = GetGage(temp_where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY ddate ");
   infoPtr->datum = GetDatum(temp_where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY pbegin, ppub ");
   infoPtr->pub = GetPub(temp_where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY reference ");
   infoPtr->refer = GetRefer(temp_where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY lwdat ");
   infoPtr->low = GetLowwater(temp_where);
   
   infoPtr->descrip = GetDescrip(where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY stage ");
   infoPtr->flood = GetFlood(temp_where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY impact_value, rf, datestart, dateend ");
   infoPtr->floodst = GetFloodstmt(temp_where);
   
   infoPtr->floodcat = GetFloodcat(where);
   infoPtr->res = GetReservoir(where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY stage ");
   infoPtr->rating = GetRating(temp_where);
   
   strcpy(temp_where, where);
   strcat(temp_where, " ORDER BY priority ");
   infoPtr->contacts = GetContacts(temp_where);
   
   return;
}
//---------------------------------------------------------


void	E19_FreeInfo(e19_info_type **infoPtr)
{
   if (*infoPtr != (e19_info_type *) NULL)
   {
      FreeBenchmark	((*infoPtr)->bench);
      FreeCrest		((*infoPtr)->crest);
      FreeDcp		((*infoPtr)->dcp);
      FreeLocation	((*infoPtr)->loc);
      FreeTelem		((*infoPtr)->telem);
      FreeObserver	((*infoPtr)->obs);
      FreeRiverstat	((*infoPtr)->river);
      FreeGage		((*infoPtr)->gage);
      FreeDatum		((*infoPtr)->datum);
      FreePub		((*infoPtr)->pub);
      FreeRefer		((*infoPtr)->refer);
      FreeLowwater	((*infoPtr)->low);
      FreeDescrip	((*infoPtr)->descrip);
      FreeFlood		((*infoPtr)->flood);
      FreeFloodstmt	((*infoPtr)->floodst);
      FreeFloodcat	((*infoPtr)->floodcat);
      FreeReservoir	((*infoPtr)->res);
      FreeRating	((*infoPtr)->rating);
      FreeContacts	((*infoPtr)->contacts);
      
      free((*infoPtr));

      e19InfoPtr = NULL;	/* set global to NULL */
   }
   
   return;
}
//---------------------------------------------------------


char*	E19_HighestCrest(int option, char* str_date)
{
   static char	crest_buf[BUFSIZ];

   Crest*	crestPtr = NULL;
   char  	where[BUFSIZ];
   
   char		tmp1[BUFSIZ];
   char		tmp2[BUFSIZ];
   
   long		date;
   

   /*
	Create the where clause from the desired option.
   */
   memset( ( void * ) where, '\0', sizeof(where));
   if(option == E19_GAGE_READING_CREST)
   {
      sprintf(where, " WHERE lid = '%s' and hw is null ", TextReports_getCurrentLid());
      strcat (where, " ORDER BY stage desc ");
   }
   else if(option == E19_HIGH_WATERMARKS_CREST)
   {
      sprintf(where, " WHERE lid = '%s' and hw = 'X' ", TextReports_getCurrentLid());
      strcat (where, " ORDER BY stage desc ");
   }
   else if(option == E19_TIME_CREST)
   {
      USA_date_to_date_t ( str_date, &date );
      sprintf(where, " WHERE lid = '%s' and datcrst >= '%ld' ",
	      TextReports_getCurrentLid(), date);
      strcat (where, " ORDER BY stage desc ");
   }

   
   /*
   	Create the return string and return.
   */
   memset( ( void * ) crest_buf, '\0', sizeof(crest_buf));
   if ((crestPtr = GetCrest(where)) != (Crest *) NULL)
   {
      memset( ( void * ) tmp1, '\0', sizeof(tmp1));
      DataToString(&crestPtr->stage, DOUBLE, tmp1, "%7.2lf", "       ");
      date_t_to_USA_date ( crestPtr->datcrst, tmp2 );
      
      sprintf(crest_buf, "%s     %10s",
	      tmp1, tmp2);
      
      FreeCrest(crestPtr);
   }
   strcat(crest_buf, "\n");
   
   
   return(crest_buf);   
}

//---------------------------------------------------------
char*	E19_GetNextDcpSet(char* in_buffer, int num, char** out_buffer)
{
   static char	in_buf[E19_BUFSIZ];	/* input buffer */
   static char	out_buf[E19_BUFSIZ];	/* output buffer */
   
   memset( ( void * ) in_buf, '\0', sizeof(in_buf));
   memset( ( void * ) out_buf, '\0', sizeof(out_buf));

   strcpy(in_buf, in_buffer);
   strncpy(out_buf, in_buf, num*3);
   
   *out_buffer = &out_buf[0]; /* return the output string */
   return(&in_buf[num*3]); /* return the pointer to the next in_buffer */
}

//---------------------------------------------------------

char*	E19_GetNextTelmSet(char* in_buffer, int num, char** out_buffer)
{
   static char	in_buf[E19_BUFSIZ];	/* input buffer */
   static char	out_buf[E19_BUFSIZ];	/* output buffer */
   
   memset( ( void * ) in_buf, '\0', sizeof(in_buf));
   memset( ( void * ) out_buf, '\0', sizeof(out_buf));

   strcpy(in_buf, in_buffer);
   strncpy(out_buf, in_buf, num*3);
   
   *out_buffer = &out_buf[0]; /* return the output string */
   return(&in_buf[num*3]); /* return the pointer to the next in_buffer */
}


//---------------------------------------------------------
char*	E19_AdvanceToFooter(char *buf, int linesFromTopOfPage, int loop)
{
   static char	tmp1[E19_BUFSIZ];
   int		i;
   

   /*
   	Place FOOTER at the bottom of page.
   */
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   for(i=0;  i <( ((TEXTREPORTS_MAX_LINES_PER_PAGE * loop)+linesFromTopOfPage)
                  - Text_CountNewLines(buf));  i++)
   {
      strcat (tmp1, "\n");
   }
   
   return(tmp1);
}

//---------------------------------------------------------

char*	E19_CreateFooter(e19_info_type *info, int revise_type, char *pdate, char *formName,
			 int pageType, char *pageTypeLabel, char *optHeader,
			 int leftMargin, int *init_footer, int reason)
{
   static char	*buf = NULL;
   
   static int	prevPageType = -1;
   
   static int	pageNum;
   static int	subpageNum;

   char		tmp1[E19_BUFSIZ];
   char		tmp2[E19_BUFSIZ];
   char		tmp3[E19_BUFSIZ];
   char		tmp4[E19_BUFSIZ];

   int		i;

   
   if (*init_footer)
   {
      prevPageType = -1;

      *init_footer = False;
   }
   
   
   /*
   	Set up proper pageNum & subpageNum which is based on the prevPageType.
	
	End result: all pages are numbered based on the pageType, however,
	if a given pageType continues onto the next page, a subpageNum is
	used as well (ex. "Page 5: CRESTS" -> "Page 5-2: CRESTS", etc). {
      strcat(tmp1, " ");
   
   */
   pageNum = pageType;
   if (pageType == prevPageType)
   {
      subpageNum++;
   }
   else
   {
      subpageNum = 1;
      prevPageType = pageType;
   }
   
      strcat(tmp1, " ");
   
   /*
   	Init buffer.

	Add left margin.
	Add (optional) header.
   */
   Text_AddString(&buf, "", 1);

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   if (optHeader != NULL)
   {
      for (i=0; i<leftMargin; i++)
      {
	 strcat(tmp1, " ");
      }
      strcat(tmp1, optHeader);
   }
   
   if ( buf != NULL )
   {
      Text_AddString(&buf, tmp1, 0);
   }
   else
   {
      Text_AddString(&buf, tmp1, 1);
   }

   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   memset( ( void * ) tmp2, '\0', sizeof(tmp2));
   memset( ( void * ) tmp3, '\0', sizeof(tmp3));
   memset( ( void * ) tmp4, '\0', sizeof(tmp4));

   
   sprintf(tmp2, "%-s %-s %-s, %-s",
	   info->river ? info->river->stream : "" ,
           info->descrip ? info->descrip->proximity : "" ,
           info->loc ? info->loc->name : "" , 
           info->loc ? info->loc->state : "" );
   if (revise_type == E19_LREVISE_TYPE)
   {
      if ( info->loc != NULL ) 
      {
         /* Modified by BryonL on 1/26/2005. */
         date_t_to_USA_date ( info->loc->lrevise, tmp4 );
      }
   }
   else /* revise_type == E19_RREVISE_TYPE */
   {
      if ( info->river != NULL )
      {
         /* Modified by BryonL on 1/26/2005. */
         date_t_to_USA_date ( info->river->rrevise, tmp4 );
      }
   }
   sprintf(tmp3, "LOCATION: %-57s Revised, Printed Dates: %-10s, %-10s\n",
	   tmp2, tmp4, pdate);
   for (i=0; i<leftMargin; i++)
   {
      strcat(tmp1, " ");
   }
   strcat(tmp1, tmp3);
   
   if (subpageNum > 1)
   {
      sprintf(tmp3, "      ID: %-11s                    HSA: %-20s  %-s PAGE %i-%i: %-s\n",
	      info->loc->lid, info->loc->hsa, formName, pageNum, subpageNum, pageTypeLabel);
   }
   else
   {
      sprintf(tmp3, "      ID: %-11s                    HSA: %-20s  %-s PAGE %i: %-s\n",
              info->loc->lid, info->loc->hsa, formName, pageNum, pageTypeLabel);
   }
   for (i=0; i<leftMargin; i++)
   {
      strcat(tmp1, " ");
   }
   strcat(tmp1, tmp3);
   
   if ( buf != NULL )
   {
      Text_AddString(&buf, tmp1, 0);
   }
   else
   {
      Text_AddString(&buf, tmp1, 1);
   }
   
   return(buf);
}


//---------------------------------------------------------


char*	E19_BuildStaffGageString(float* i)
{
   static char		tmp1[E19_BUFSIZ];
   
   
   memset( ( void * ) tmp1, '\0', sizeof(tmp1));
   if (i)
   {
      sprintf(tmp1, "%c%4.0f-%c", SEP_CHAR, *i, SEP_CHAR);
   }
   else
   {
      sprintf(tmp1, "%c-----%c", SEP_CHAR, SEP_CHAR);
   }
   
   
   return(&tmp1[0]);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

//---------------------------------------------------------



