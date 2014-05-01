#include "gui.h"
#include "sharp95.h"

/* Convenience function to pup up file selection dialog */

void file_browse_popup (char *path, char *tmpl, Widget toplevel, 
	void cbfunc(Widget, XtPointer, XtPointer) )
{
  static Widget load_filegem = NULL;
  XmString directory_str, pattern_str;

  if (!load_filegem)
    {
      load_filegem = XmCreateFileSelectionDialog (toplevel,
						  "File Selection Window", NULL, 0);
      XtAddCallback (load_filegem, XmNokCallback, cbfunc, NULL);
      XtAddCallback (load_filegem, XmNcancelCallback,
		     (XtCallbackProc) XtUnmanageChild, NULL);
      XtAddCallback (load_filegem, XmNokCallback,
		     (XtCallbackProc) XtUnmanageChild, NULL);
    }
  XtManageChild (load_filegem);

  directory_str = XmStringCreateLocalized (path);
  pattern_str = XmStringCreateLocalized (tmpl);
  XtVaSetValues (load_filegem, XmNdirectory, directory_str,
		 XmNpattern, pattern_str, NULL);
  XmStringFree (directory_str);
  XmStringFree (pattern_str);
}
