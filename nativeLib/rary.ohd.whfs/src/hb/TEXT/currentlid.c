
#include <Xm/Xm.h>
#include <Xm/List.h>
#include "DbmsDefs.h"
#include "hybase.h"
#include "hbAS.h"


char*	CurrentLid(void)
{
        XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
        XmString        *xmStr;
        Arg             arg[MAX_ARGS];
        static char     lid[LOC_ID_LEN + 1],
			*text,
                        *tok;
        int             ac;

        /*
                Get the currently selected list item.
        */
        ac = 0;
        XtSetArg(arg[ac], XmNselectedItems, &xmStr); ac++;
        XtGetValues(hbmainLI, arg, ac);


        /*
                Parse the selected items to get the
                station identifier.  If successful,
                show the Precip dialog, else show
                the error dialog.
        */
        memset(lid, '\0', sizeof(lid));
        if (XmStringGetLtoR(xmStr[0], charset, &text))
	{
                tok = strtok(text, " \t");
		strcpy(lid, tok);
		XtFree(text);
	}
	
	
        return(lid);
}
