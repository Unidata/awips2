/* MOUSE - Module of mouse functions. To use it, include the MOUSE.H file
 * in your program. The following functions are public:
 *
 *   MouseInit      - Initialize mouse
 *   GetMouseEvent  - Get information about most recent mouse event
 *   SetPtrVis      - Set visibility of pointer to HIDE or SHOW
 *   SetPtrPos      - Set position of pointer
 *   SetPtrShape    - Set shape of pointer in graphics modes, or
 *                    character and color in text modes
 *   GetPtrPos      - Get pointer position and button status
 *
 * The following structure is defined:
 *
 *   EVENT      -   Defines x, y, and mouse status of a mouse event
 */

#include <mouse.h>

/* Internal information used by various mouse functions. */
static struct MOUINFO
{
    int      fExist, fInit, fGraph;
    short    xVirtual,  yVirtual;
    short    xActual,   yActual;
    short    xLast,     yLast;
    unsigned fsBtnLast, cBtn;
} mi =
{
    1, 0, 0,
    0, 0,
    0, 0,
    0, 0,
    0, 0
};

/*NP*/
/* GetPtrPos - Get mouse pointer position and button status regardless of
 * whether there was an event.
 *
 * Params: pEvent - Pointer to event structure
 *
 * Return: 0 if no mouse, otherwise 1
 */
int GetPtrPos( EVENT *pEvent )
{
    return 1;
}

/*NP*/
/* SetPtrVis - Set pointer visibility.
 *
 * Params: state - SHOW or HIDE
 *
 * Return: 0 if no mouse, otherwise 1
 */
int SetPtrVis( enum _PTRVIS pv )
{
}

/*NP*/
/* SetPtrPos - Set mouse pointer position.
 *
 * Params: x - column position in text modes, actual x coordinate in graphics
 *         y - row position in text modes, actual y coordinate in graphics
 *
 * Return: 0 if no mouse, otherwise 1
 */
int SetPtrPos( short x, short y )
{

}

