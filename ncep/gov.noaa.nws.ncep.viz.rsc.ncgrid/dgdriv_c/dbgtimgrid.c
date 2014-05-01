#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_gtimgrid ( char *gridTimes, int *lGridTimes, int *iret )  
/************************************************************************
 *									*
 * db_gtimgrid               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      ier, ier1;
    char     diagMessage[2000];
    struct   timeb t_callback, t_current;
/*----------------------------------------------------------------------*/
  /*
   * If the pointer to the callback function is not set, return
   */
    if ( fhrsClbkPtr == NULL ) {
       ier = -17;
       er_wmsg ( "DB", &ier, "db_gtimgrid", &ier1, 2, strlen("db_gtimgrid") );
       *iret = -1;
       return;
    }

  /*
   * Initialization
   */
    *iret = 0;
    *lGridTimes = 0;
    gridTimes[0] = '\0';
    diagMessage[0] = '\0';

   /*
    * Execute the callback to get the cycle forecast hours
    */
    db_msgcave ("db_gtimgrid", "debug", "calling the callback function", &ier);
    ftime(&t_callback);
    fhrsClbkPtr();
    ftime(&t_current);
    sprintf (diagMessage, "%s %d", "time spent in callback ", (int) (1000.0 * (t_current.time - t_callback.time) + (t_current.millitm - t_callback.millitm))); 
    db_msgcave ("db_gtimgrid", "info", diagMessage, &ier);
    if ( fhrsStrLength > 0 && fhrsStrBack != NULL ) {
       sprintf (gridTimes, "%s", fhrsStrBack);
       *lGridTimes = strlen(gridTimes);
       G_FREE( fhrsStrBack, char );
       sprintf (diagMessage, "%s %s", "got this fhrs string", gridTimes);
       db_msgcave ("db_gtimgrid", "debug", diagMessage, &ier);
    }
    else {
       db_msgcave ("db_gtimgrid", "error", "!!! *could not get fhrs string* !!!", &ier);
       ier = -18;
       er_wmsg ( "DB", &ier, "FcstHrs", &ier1, 2, strlen("FcstHrs") );
       *iret = -1;
       return;
    }
       
    return;

}
