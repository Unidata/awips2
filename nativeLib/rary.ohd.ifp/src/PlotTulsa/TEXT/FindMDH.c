/*-- Added by AV  ---*/
#include "c_call_f/mdyh2.h"
#include "c_call_f/set_fctime_cb.h"

extern void mdyh2();
extern void set_fctime_cb() ;
/* File:FindMDH.c
 *
 *  Takes a julian hour, converts it to days and hours then calls the mdyh2
 *  FORTRAN subroutine to get the month, day, year, hour, etc. info.
 *
 */
 
 
void FindMDH(julhour, julda, julhr, month, day, year, hour,
		   zondum, dlsdum, tz_code)
   int      julhour;       /* Julian hour          */
   int      *julda;        /* Julian day pointer   */
   int      *julhr;        /* Julian hour pointer  */
   int      *month;        /* Month pointer        */
   int      *day;          /* Day pointer          */
   int      *year;         /* Year pointer         */
   int      *hour;         /* Hour pointer         */
   int      *zondum;       /* Time zone number pointer based on GMT      */
   int      *dlsdum;       /* Daylight savings time flag pointer, 1 for
			      daylight savings time, 0 for standard time. */
   char     *tz_code;      /* Time zone code       */
{
   *julda = julhour/24 + 1.01;
   *julhr = julhour%24 + 0.01;

   SET_FCTIME_CB();

   MDYH2(julda, julhr, month, day, year, hour,
	  zondum, dlsdum, tz_code);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/FindMDH.c,v $";
 static char rcs_id2[] = "$Id: FindMDH.c,v 1.2 2002/02/11 19:24:40 dws Exp $";}
/*  ===================================================  */

}  /* end of FindMDH */
