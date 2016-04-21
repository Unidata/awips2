
/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "restore_date.h" /* For the "restore_date" routine prototype. */
#include "stage3.h" /* For the "date_prev" global variable. */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/********************************************************************/
/*  FUNCTION NAME:   restore_date                                   */
/*       FUNCTION:   restores previous date when cancel option of   */
/*                    choose dates is chosen                        */
/*********************************************************************

Function type:
   void

Called by function:
   (callback) cancel option on choose dates window

******************************** BEGIN restore_date *******/

void restore_date( Widget w , XtPointer clientdata , XtPointer calldata )
{

   /* Set the date back to the previously selected date. */
   date_st3 = date_prev ;
}

/******************************** END restore_date ************/
