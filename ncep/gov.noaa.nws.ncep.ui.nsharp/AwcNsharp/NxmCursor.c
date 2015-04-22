#include "Nxmlib.h"
#include "NxmInit.h"

#include <X11/cursorfont.h>

void NxmCursorChange (parent, shape, color)
Widget		parent;
unsigned int	shape;
char		*color;
/************************************************************************
 * NxmCursorChange							*
 *									*
 * This function changes the cursor of the widget.			*
 *									*
 * Widget NxmCursorChange (parent, shape, color)			*
 *									*
 * Input parameters:							*
 *	parent	Widget		ID of parent widget			*
 *	shape	unsigned int	ID of parent widget			*
 *	color	char *		color of the cursor			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		05/94						*
 * S. Law/GSC		09/99	check if cursor has been created	*
 ***********************************************************************/
{
    int			ii;
    XColor		xcolr, ignore;
    static Cursor	cursor[XC_num_glyphs];
    static Boolean	first = TRUE;
/*---------------------------------------------------------------------*/

    if (!NXMisInitialized) {
	NxmInitialize (parent);
    }

    if (first) {
	for (ii = 0; ii < XC_num_glyphs; ii++) {
	    cursor[ii] = '\0';
	}

	first = FALSE;
    }

    if (cursor[shape] == '\0') {
	cursor[shape] = XCreateFontCursor (NXMdisplay, shape);
    }

    XDefineCursor (NXMdisplay, XtWindow (parent), cursor[shape]);

    xcolr.flags = DoRed | DoBlue | DoGreen;
    XAllocNamedColor (NXMdisplay, NXMcmap, color, &xcolr, &ignore);

    XRecolorCursor (NXMdisplay, cursor[shape], &xcolr, &xcolr);
}

