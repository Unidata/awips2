/****************************************************************/
/*                                                              */
/*	FILE:		UpdateTextF.c				*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/02/94                                */
/*                                                              */
/*      NOTE:							*/
/*                                                              */
/****************************************************************/


#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h>




void updateTextFCharDisplay(Widget w, char *string)
{
	int i;
	/*  loop 3 times to force the string value of the 
	  TextField widget to update. This bug happened
	  when software is updated to X11 R6--MR#1265 
	  NOTE: this is a temporary fix. 
	*/
        for (i=0;i<2;i++) XmTextFieldSetString(w, string);
}



void updateTextFIntDisplay(Widget w, int value)
{

	int i;
	char	string[100];
	
	
	sprintf(string, "%d", value);
	/*  loop 3 times to force the string value of the
	  TextField widget to update. This bug happened
	  when software is updated to X11 R6--MR#
	   NOTE: this is a temporary fix. 
	*/
        for (i=0;i<2;i++) XmTextFieldSetString(w, string);
 	XmTextFieldSetString(w, string);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/UpdateTextF.c,v $";
 static char rcs_id2[] = "$Id: UpdateTextF.c,v 1.2 2000/12/19 16:33:16 jgofus Exp $";}
/*  ===================================================  */

}

