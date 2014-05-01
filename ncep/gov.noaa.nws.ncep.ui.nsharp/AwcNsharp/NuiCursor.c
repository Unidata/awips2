
#include "Nxmlib.h"
#include "NxmInit.h"

#include <X11/cursorfont.h>

/************************************************************************
 * NuiBusyCursor                                                        *
 *                                                                      *
 * Convenient function to create a busy cursor.                         *
 *                                                                      *
 * Widget NuiBusyCursor(parent)                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget       ID of parent widget                       *
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		05/94                                           *
 * S. Jacobs/NCEP	10/96	Added declaration for parent		*
 ***********************************************************************/

void    NuiBusyCursor( parent )
Widget	parent;
{
	NxmCursorChange( parent, XC_X_cursor, "white" );
}

/************************************************************************
 * NuiDefaultCursor                                                     *
 *                                                                      *
 * Convenient function to create a defult cursor.                       *
 *                                                                      *
 * Widget NuiDefaultCursor(parent)                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget       ID of parent widget                       *
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		05/94						*
 * S. Jacobs/NCEP	10/96	Added declaration for parent		*
 ***********************************************************************/

void    NuiDefaultCursor( parent )
Widget	parent;
{
	NxmCursorChange( parent, XC_top_left_arrow, "white" );
}

