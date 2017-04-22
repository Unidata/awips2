/* New main program to call ss_input as a fortran subroutine
   from this C main function on the HPs to get this to
   compile on the HPs.  2 Aug. 1993 - dp
*/
#include "c_call_f/ss_input.h"

main_ss_input_main()
{
   SS_INPUT();

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/bin_to_ss_input/RCS/main_ss_input.c,v $";
 static char rcs_id2[] = "$Id: main_ss_input.c,v 1.2 2002/02/11 19:19:28 dws Exp $";}
/*  ===================================================  */

}
