#include "geminc.h"
#include "gemprm.h"

void gdc_gtmf ( char *gdfile, char  *cycle, char *availableTimes, int *iret )
/************************************************************************
C* Input parameters:                                                    *
C*      GDFILE          CHAR*           Grid file name                  *
C*      CYCLE           CHAR*           Cycle(Gempak format)            *
C* Output parameters:                                                   *
C*      AVAILABLETIMES  CHAR*           List of GEMPAK times            *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
 ************************************************************************/
{
   int ngdftm,ier=0;
   int maxt = 500;
   char gdatim[4]="";
   char mtime_list[500][20];
/*----------------------------------------------------------------------*/
   *iret = 0;
  
   gd_gtmf (gdfile, gdatim, cycle, &maxt, &ngdftm, mtime_list, 
            &ier,strlen (gdfile), strlen (gdatim), strlen (cycle),
             20 /* mtime_list size */ );
   if ( ier == 0 && ngdftm > 0 ) {
      int jj;
      for ( jj = 0; jj < ngdftm; jj ++ ) {
          int lens;
          mtime_list[jj][19] = '\0';
          lens = strcspn (mtime_list[jj], "\t \0");
          mtime_list[jj][lens] = '\0';

          if ( jj == 0 ) {
              strcpy ( availableTimes, mtime_list[jj]);
          }
          else {
              strcat ( availableTimes, mtime_list[jj]);
          }
          if ( jj < ngdftm - 1 ) {
              strcat ( availableTimes,"|" );
          }
      } 
   }
   else {
       *iret = ier;
   }
   return;
}
