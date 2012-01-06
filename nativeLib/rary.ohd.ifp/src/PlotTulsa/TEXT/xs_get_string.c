/* File: xs_get_string.c
 *
 * Gets an Xs string from an Xm string.
 *
 */

#include "libXs.h"
char *xs_get_string_from_xmstring(string)
   XmString     string;
{
      caddr_t   context;
      char      *text;
      XmStringCharSet   charset;
      XmStringDirection dir;
      Boolean   separator;
      char      *buf = NULL;
      int       done = FALSE;
      int       count = 0;

      XmStringInitContext((XmStringContext *)&context, string);
      while (!done && count < 100)
      {
	 if(XmStringGetNextSegment((XmStringContext)context, &text, &charset,
		       &dir, &separator))
	 {
	     count++;
	     if(separator)
		done = TRUE;
	     if(buf)
	     {
		buf = XtRealloc(buf, strlen(buf) + strlen(text) + 2);
		strcat(buf, text);
	     }
	     else
	     {
		buf = (char *) XtMalloc(strlen(text) + 1);
		strcpy(buf, text);
	     }
	     XtFree(text);
	  }
	  else
	     done = TRUE;
      }
      XmStringFreeContext((XmStringContext)context);
      return buf;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/xs_get_string.c,v $";
 static char rcs_id2[] = "$Id: xs_get_string.c,v 1.2 2006/03/28 20:44:40 aivo Exp $";}
/*  ===================================================  */

}
