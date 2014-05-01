#define _POSIX_SOURCE                           /* dgb: 07/09/96 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global_external.h"

/*---------------------------------------------------------------------
  
       NAME
         SUBROUTINE metarcl
  
       PURPOSE                                                    
         Close files used by the metar decoder during each iteration.
  
       VERSION and UPDATES
         1.0    JUN 95   David G. Brandon  
                Original Version
                Also Translated to 'C' using FOR_C
         1.1    JUL 9 96 DGB
                Added check for zero length files, and removed them
                if zero length was found.
         1.2    JAN 08 00 
	        Set file pointers to 0 when closed for use in 
		linux compilers.
 
 *--------------------------------------------------------------------- */

void /*FUNCTION*/ metarcl()
{
struct stat buf;                                /* dgb:07/09/96 */

int i;
	/*	Close Files */

        if ( !test_.test_flag ) 
        {
           i =  fclose(luns_.lchn);
	   luns_.lchn = 0;                     /* dgb:01/09/00 */

          remove(files_.shef_in);


        }
		if( cont_.out_flag[0] == '+' ) 
        {
            i = fclose(luns_.jchn);
	    luns_.jchn = 0;                                /* dgb:01/09/00 */
            stat(tempfiles_.log_file,&buf);                /* dgb:07/09/96 */
            if ( buf.st_size == 0 )                       /* dgb:07/09/96 */   
                 remove(tempfiles_.log_file);              /* dgb:07/09/96 */    

        }

	/*	If there are errors, always write out error file.
	  	If the control variable is set to '+', write
		out error file.  If the control variable is set to
		'-', and there are no warnings, do not write. */

		if( error_.nerror != 0 || error_.nwarn != 0 )
		{
            i = fclose(luns_.icher);
	    luns_.icher = 0;                               /* dgb:01/09/00 */

            stat(tempfiles_.err_file,&buf);                /* dgb:07/09/96 */
            if ( buf.st_size == 0 )                       /* dgb:07/09/96 */   
                 remove(tempfiles_.err_file);              /* dgb:07/09/96 */    

		}
		else
		{

			if( cont_.error_flag[0] != '+' )
			{
                i = fclose(luns_.icher);
		luns_.icher = 0;                               /* dgb:01/08/00
		*/
                remove(tempfiles_.err_file);
			}
			else
			{

                 i = fclose(luns_.icher);
		 luns_.icher = 0;                            /* dgb:01/08/00 */
                 stat(tempfiles_.err_file,&buf);                /* dgb:07/09/96 */
                 if ( buf.st_size == 0 )                       /* dgb:07/09/96 */   
                      remove(tempfiles_.err_file);              /* dgb:07/09/96 */    

			}
		}
	return;

} /* end of function */

