/****************************************************************/
/*                                                              */
/*	FILE:		Mods_handleDates.c			*/
/*                                                              */
/*							        */
/*                                                              */
/*	Coded by:	Tom Adams                               */
/*			NWS * Office of Hydrology * HRL         */
/*	Date:		11/01/94                                */
/*                                                              */
/*      NOTE:		code modified from date_funcs.c		*/
/*                                                              */
/****************************************************************/



#include <X11/Intrinsic.h>
#include <Xm/Xm.h>





/****************************************************************/
/*                                                              */
/*	char *ConvertMonthToString()				*/
/*                                                              */
/*	RETURNS:	char *					*/
/*                                                              */
/****************************************************************/

char *ConvertMonthToString(int theMonth)
{

	char	*string;



	string = XtMalloc(sizeof(char)*5);

	switch(theMonth)
			{
			case    1:
				strcpy(string, "Jan");
				break;

			case    2:
				strcpy(string, "Feb");
				break;

			case    3:
				strcpy(string, "Mar");
				break;

			case    4:
				strcpy(string, "Apr");
				break;

			case    5:
				strcpy(string, "May");
				break;

			case    6:
				strcpy(string, "Jun");
				break;

			case    7:
				strcpy(string, "Jul");
				break;

			case    8:
				strcpy(string, "Aug");
				break;

			case    9:
				strcpy(string, "Sep");
				break;

			case    10:
				strcpy(string, "Oct");
				break;

			case    11:
				strcpy(string, "Nov");
				break;

			case    12:
				strcpy(string, "Dec");
				break;

			default:
				strcpy(string, "None");
				break;
			}

	return(string);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/ConvertMonthToString.c,v $";
 static char rcs_id2[] = "$Id: ConvertMonthToString.c,v 1.1 1995/11/14 12:18:58 page Exp $";}
/*  ===================================================  */

}
