/*
	File:		loc_btns.c
	Date:		March 1996
	Author:		Dale Shelton / Chip Gobs
			Paul Taylor
	
	Purpose:	Provides support for creating buttons on OMs.	
*/


#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include "Network.h"
#include "State.h"
#include "Hsa.h"
#include "Wfo.h"
#include "Rfc.h"
#include "Xtools.h"
#include "loc_cbs.h"
#include "loc.h"
#include "TimeZone.h"
#include "DbmsDefs.h"

Widget	hsaPB;
Widget	wfoPB;
Widget 	rfcPB;
Widget  netPB;
Widget 	tzPB;


void	loc_create_btns(void)
{
	Network	*net, *netPtr;
	Wfo	*wfo, *wfoPtr;
	Rfc	*rfc, *rfcPtr;
        Hsa     *hsa, *hsaPtr;
	TimeZone	*tzHead, *tzPtr;
	char	timeZoneString[TZ_LEN + TZ_NAME_LEN + 5];
        int     cnt = 0, i = 0;
        XmStringTable xmStr;

        hsa = GetHsa(" ORDER BY hsa ");
        hsaPtr = (Hsa *) ListFirst(&hsa->list);
        cnt = ListCount(&hsaPtr->list);
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
        while (hsaPtr)
        {
                xmStr[i] = XmStringCreateSimple(hsaPtr->hsa);
                i++;
                hsaPtr = (Hsa *) ListNext(&hsaPtr->node);
        }
        XmListAddItems(hsaLI, xmStr, cnt, 1);
	
	
	net = GetNetwork("");
	netPtr = (Network *) ListFirst(&net->list);
	while (netPtr)
	{
		netPB = XtVaCreateManagedWidget(netPtr->network,
				xmPushButtonWidgetClass,
				netPDM,
				NULL);
		netPtr = (Network *) ListNext(&netPtr->node);
	}
	
	
	wfo = GetWfo(" ORDER BY wfo ");
	wfoPtr = (Wfo *) ListFirst(&wfo->list);
        cnt = ListCount(&wfoPtr->list);
        i = 0;
        xmStr = (XmStringTable) XtMalloc (cnt * sizeof(XmString *));
	while (wfoPtr)
	{
                xmStr[i] = XmStringCreateSimple(wfoPtr->wfo);
                i++;
		wfoPtr = (Wfo *) ListNext(&wfoPtr->node);
	}
        XmListAddItems(wfoLI, xmStr, cnt, 1);
	
	rfc = GetRfc(" ORDER BY rfc ");
	rfcPtr = (Rfc *) ListFirst(&rfc->list);
	while (rfcPtr)
	{
		rfcPB = XtVaCreateManagedWidget(rfcPtr->rfc,
				xmPushButtonWidgetClass,
				rfcPDM,
				NULL);
		rfcPtr = (Rfc *) ListNext(&rfcPtr->node);
	}
	
	
	tzHead = GetTimeZone(" ");
	tzPtr = (TimeZone *) ListFirst(&tzHead->list);
	while (tzPtr)
	{
		sprintf(timeZoneString, "%-8s (%s)", tzPtr->tzone, tzPtr->name);
		tzPB = XtVaCreateManagedWidget(timeZoneString,
				xmPushButtonWidgetClass,
				tzPDM,
				NULL);
		tzPtr = (TimeZone *) ListNext(&tzPtr->node);
	}
	
	/*
		Free memory associated with retrieve DBMS structs.
	*/
        if (hsa)  FreeHsa(hsa);
	if (net)  FreeNetwork(net);
	if (wfo)  FreeWfo(wfo);
	if (rfc)  FreeRfc(rfc);
	if (tzHead)  FreeTimeZone(tzHead);
	return;
}
