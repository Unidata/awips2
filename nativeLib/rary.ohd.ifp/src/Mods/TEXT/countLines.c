/****************************************************************/
/*								*/
/*	FILE:		countLines.c				*/
/*								*/
/*	Count the number of lines in the file pointed to by fp	*/
/*								*/
/*	Coded by:	Tom Adams				*/
/*			NWS * Office of Hydrology * HRL		*/
/*	Date:		09/08/94				*/
/*								*/
/*      NOTE:							*/
/*								*/
/****************************************************************/


#include <stdio.h>


#define LINE_LENGTH 100


int countLines(FILE *fp)
{
	int	num = 0;
	char	line[LINE_LENGTH];
	char	*nextline;

	while((nextline = fgets(line, LINE_LENGTH, fp)) != NULL) num++;
	

	return(num);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/countLines.c,v $";
 static char rcs_id2[] = "$Id: countLines.c,v 1.1 1995/11/14 12:19:41 page Exp $";}
/*  ===================================================  */

}

