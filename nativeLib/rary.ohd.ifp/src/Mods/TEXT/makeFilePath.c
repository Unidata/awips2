/****************************************************************/
/*								*/
/*	FILE:		makeFilePath.c				*/
/* 								*/
/*	Concatenate a directory path and filename		*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		10/21/94				*/
/*								*/
/*								*/
/****************************************************************/
#include <stdlib.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

char *makeFilePath(char *path, char *name)
{

	char	*filePath;
	
	
	filePath = (char *)XtMalloc(sizeof(char)*(strlen(path)+strlen(name)+1));
	strcpy(filePath, path);
	strcat(filePath, name);


	return(filePath);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/makeFilePath.c,v $";
 static char rcs_id2[] = "$Id: makeFilePath.c,v 1.1 1995/11/14 12:19:49 page Exp $";}
/*  ===================================================  */

}
