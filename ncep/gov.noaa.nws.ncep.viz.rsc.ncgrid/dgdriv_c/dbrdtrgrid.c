#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_rdtrgrid ( char *dimx, char *dimy, char *dataUri,
                   float *rdata, int *nword, int *iret )
/************************************************************************
 *									*
 * db_dataquery               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      jj, ier, ier1;
    char     message[1024], diagMessage[720];
    struct   timeb t_callback, t_current;
/*---------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    message[0] = '\0';
  /*
   * If the pointer to the callback function is not set, return
   */
    if ( dataClbkPtr == NULL ) {
       ier = -17;
       er_wmsg ( "DB", &ier, "db_rdtrgrid", &ier1, 2, strlen("db_rdtrgrid") );
       *iret = -1;
       return;
    }

  /*
   * If the data uri is not set, return
   */
    if ( strlen(dataUri) == 0 ) {
       ier = -19;
       er_wmsg ( "DB", &ier, "db_rdtrgrid", &ier1, 2, strlen("db_rdtrgrid") );
       *iret = -1;
       return;
    }

    sprintf(message,"%s", dataUri);
    strcat (message, ";");
    strcat (message, dimx);
    strcat (message, ";");
    strcat (message, dimy);
    sprintf (diagMessage, "%s %s", "calling the callback function with ", message);
    db_msgcave ("db_rdtrgrid", "debug", diagMessage, &ier);

    ftime(&t_callback);

    dataClbkPtr(message);

    ftime(&t_current);
    sprintf (diagMessage, "%s %d", "time spent in callback ", (int) (1000.0 * (t_current.time - t_callback.time) + (t_current.millitm - t_callback.millitm)));
    db_msgcave ("db_rdtrgrid", "debug", diagMessage, &ier);
    sprintf (diagMessage, "%s %d", "got rdataBackSize=", rdataBackSize);
    db_msgcave ("db_rdtrgrid", "debug", diagMessage, &ier);
    if ( rdataBackSize > 0 && rdataBack != NULL ) {
       sprintf (diagMessage, "%s %f", "got rdataBack[0]=", rdataBack[0]);
       db_msgcave ("db_rdtrgrid", "debug", diagMessage, &ier);
       for( jj=0; jj < rdataBackSize; jj++ ) {
          rdata[jj] = rdataBack[jj];
       }
       *iret = 0;
       *nword = rdataBackSize;
       G_FREE ( rdataBack, float );
       rdataBackSize = 0;
    }
    else {
       db_msgcave ("db_rdtrgrid", "error","!!! *could not get data* !!!", &ier);
       *iret = -1;
       *nword = 0;
    }
    return;
}
