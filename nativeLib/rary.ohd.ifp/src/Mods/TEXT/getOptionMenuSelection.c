/****************************************************************/
/*                                                              */
/*	FILE:		getOptionMenuSelection.c	        */
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		09/26/94                                */
/*	Modified:	10/06/94 - changed include statements	*/
/*                      2/22/96  - changed warning message - dp */
/*                                                              */
/*                                                              */
/*      NOTE:	Determines the currently selected item in the	*/
/*		OptionMenu that's passed as the argument.	*/
/*		RETURNS: the LabelString of the selected item's	*/
/*		widget.						*/
/*                                                              */
/****************************************************************/




#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>



char *getOptionMenuSelection(Widget optionMenu)
{

        char		*name;
        XmString	xmStringName;
	Widget		selection = NULL;



	XtVaGetValues(optionMenu, XmNmenuHistory, &selection, NULL);
	
	if(selection != NULL) {
            
        	XtVaGetValues(selection, XmNlabelString, &xmStringName, NULL);
        	XmStringGetLtoR(xmStringName, XmFONTLIST_DEFAULT_TAG, &name);
        	XmStringFree(xmStringName);
		return(name);
	}
	else	{
 		/* operation with no mod option  .eg. dhm-op */
		return(NULL);
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getOptionMenuSelection.c,v $";
 static char rcs_id2[] = "$Id: getOptionMenuSelection.c,v 1.3 2006/05/10 16:50:54 aivo Exp $";}
/*  ===================================================  */

}
