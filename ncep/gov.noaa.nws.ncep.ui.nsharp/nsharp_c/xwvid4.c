/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  DOS Video Graphics Routines (Part #4)                      */
/*  These routines handle sub-screen windows and option boxes. */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*      --------------------------------------------------     */
/*  List of Routines in this module:                           */
/*                                                             */
/*  RESET_OPTIONS                                              */
/*                                                             */
/***************************************************************/
#define VIDEO
#include "gui.h"
#include "sharp95.h"


/*=============================================================================*/

void reset_options ( short mode, short pagenum )
	/*************************************************************/
	/*  RESET_OPTIONS                                            */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Asks the user if they want to reload original sounding,  */
	/*  or just clear screen.                                    */
	/*************************************************************/
	{
	redraw_graph( mode );
	define_parcel( sndgp->lplvals.flag, sndgp->lplvals.pres );
	if ( mode == 1 ) trace_parcel(sndgp->lplvals.pres, sndgp->lplvals.temp, sndgp->lplvals.dwpt);	
	show_page( pagenum );
	return;
	}
