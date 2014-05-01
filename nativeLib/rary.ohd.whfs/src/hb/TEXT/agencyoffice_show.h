/*
	File:		agencyoffice_show.h
	Date:		January 1998
	Author:		Paul Taylor
	
	Purpose:	Provides support for the
			Cooperating Agencies/Offices DS.
	
*/


#ifndef agencyoffice_show_h
#define agencyoffice_show_h


/*
	Standard includes.
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>


/*
	Motif/X11 includes.
*/
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <X11/cursorfont.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>


/*
	Local library includes.
*/
#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "ParamDefs.h"
#include "LocExtAgency.h"
#include "AgencyOfficeUnique.h"
#include "List.h"


/*
	Local includes.
*/
#include "user_prefs.h"
#include "agencyoffice.h"
#include "loc_cbs.h"
#include "agencyoffice_typedefs.h"



void	show_agencyoffice(Widget w, const char *lid);

AgencyOfficeRec * getAgencyOfficeRec(void);

void	ao_AddCallbacks(AgencyOfficeRec *aor);
void	agencyoffice_addTextFilterCallbacks(void);
void	ao_ApplyCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	ao_ListApplyCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	ao_TextApplyCallback(Widget w, XtPointer ptr, XtPointer cbs);
void    ao_DeleteCallback(Widget w, XtPointer ptr, XtPointer cbs);
void    ao_CloseCallback(Widget w, XtPointer ptr, XtPointer cbs);
void    ao_LoadWidgetsCallback(Widget w, XtPointer ptr, XtPointer cbs);
void    ao_ClearListSelectionsCallback(Widget w, XtPointer ptr, XtPointer cbs);  
void	ao_ClearWidgetsCallback(Widget w, XtPointer ptr, XtPointer cbs);

void	ao_LoadWidgets(AgencyOfficeUnique *agPtr);
void    ao_ClearWidgets(void);
void	ao_UnloadWidgets(AgencyOfficeUnique *aPtr);
int	ao_Save(LocExtAgency *leaPtr);
void    ao_Delete(LocExtAgency *leaPtr);
void    ao_createKeyClause(LocExtAgency *leaPtr, char *where);
void    copyAgencyInfo(LocExtAgency *dest,
		       AgencyOfficeUnique *source,
		       char *lid);

void	ao_LoadAvailableList(AgencyOfficeRec *aor);                                     
void	ao_LoadSelectedList(AgencyOfficeRec *aor); 

void 	agencyoffice_SetSensitivity(void);



#endif


