/* ResJ_ccfcwtco.cxx

C 
C
C History:
C 5/15/98      Eric Markstrom, RTi Created Routine
C
C kda   I       pointer to int: current day
C
C khr   I       pointer to int: current hour
C
C
C ctemp I       pointer to floats carryover array containing the array CO vars for KDA, KHR
C
C
C num I         pointer to int: number of values in array CTEMP
C
C
C
*/

#include "ResJSys.h"

int ResJ_ccfcwtco( int *kda, int *khr, float *ctemp, int *num ){

   ResJ_ffcwtco( kda, khr, ctemp, num );
   return 0;


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJ_ccfcwtco.cxx,v $";
 static char rcs_id2[] = "$Id: ResJ_ccfcwtco.cxx,v 1.5 2006/10/26 15:31:44 hsu Exp $";}
/*  ===================================================  */

}
