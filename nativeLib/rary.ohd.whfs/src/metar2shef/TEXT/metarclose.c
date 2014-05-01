#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mtr.h"
#include "global_external.h"
/*---------------------------------------------------------------------
  
       NAME
         SUBROUTINE metarclose
  
       PURPOSE                                                    
         Close the files
  
       VERSION and UPDATES
         1.0    MAY 94   David G. Brandon
                Also Translated to 'C' using FOR_C
         1.1    JAN 08 2000 DGB
	        Set file pointers to 0 when closing...for use
		on some systems.
 *--------------------------------------------------------------------- */


extern FILE *fp_postlog;  /* dgb:10/01/95 */
void /*FUNCTION*/ metarclose()
{


   fflush( luns_.icher);

   if ( luns_.lchn != NULL ) 
   {
      fclose(luns_.lchn);
      luns_.lchn = 0;
   }
   if ( luns_.kchn != NULL ) 
   {
       fclose(luns_.kchn);
       luns_.kchn = 0;
   }
   if ( luns_.mchn != NULL ) 
   {
       fclose(luns_.mchn);
       luns_.mchn = 0;
   }
	
   if( error_.nerror != 0 || error_.nwarn != 0 )
   {
      if ( luns_.icher != NULL )
      { 
          fclose(luns_.icher);
	  luns_.icher = 0;                                  /* dgb:01/08/00 */
      }
   }
   else
   {

      if( cont_.error_flag[0] != '+' )
      {
         if ( luns_.icher != NULL )
         {
            fclose(luns_.icher);
            luns_.icher = 0;                                /* dgb:01/08/00 */
            remove(tempfiles_.err_file);
         }
      }
      else
      {
         if ( luns_.icher != NULL ) 
	 {
	    fclose(luns_.icher);
            luns_.icher = 0;                                /* dgb:01/08/00 */
	 }
      }

   }

   if( cont_.out_flag[0] == '+' )
   {
      if ( luns_.jchn != NULL ) 
      {
         fflush(luns_.jchn);
         fclose(luns_.jchn);
	 luns_.jchn = 0;                                    /* dgb:01/08/00 */
      } 
   }

   if ( fp_postlog != NULL ) 
   {
      fflush(fp_postlog);
      fclose(fp_postlog);
      fp_postlog = 0;                                       /* dgb:01/08/00 */
   }
   
   return;

} 

