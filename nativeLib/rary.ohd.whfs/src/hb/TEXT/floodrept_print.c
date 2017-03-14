/*
	File:		floodrept_print.c
	Date:		April 1996
	Author:		Chip Gobs
	
	Purpose:	Provide support for the Flood Report DS.
*/


#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include "Xtools.h"
#include "floodrept.h"
#include "floodrept_print.h"
#include "hybase.h"

void	floodrept_print(void)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        XmString        *xmStr;
        Arg             arg[MAX_ARGS];
	FILE		*outfile;
        char            *text;
        int             cnt,
	   		ac,
			i;

	
        /*
                Get all items in the list.
        */
        ac = 0;
        XtSetArg(arg[ac], XmNitems, &xmStr); ac++;
	XtSetArg(arg[ac], XmNitemCount, &cnt); ac++;
        XtGetValues(floodreptLI, arg, ac);

		
	/*
		Open an output file.
	*/
	if ((outfile = fopen("dale.txt", "w")) == (FILE *) NULL)
	{
	  	fprintf(stderr, "Unable to open output file.\n");
		return;
	}

	
        /*
                Parse the selected items to get the
                text string.  If successful, write to
		the output file.
        */
	for (i = 0; i < cnt; i++)
	{
        	if (XmStringGetLtoR(xmStr[i], charset, &text))
        	{
                	fprintf(outfile, "%s", text);
			fprintf(outfile, "\n");
                	XtFree(text);
        	}
	}
	
	
	/*
		Close the output file, and return.
	*/
	fclose(outfile);
        return;  
}



