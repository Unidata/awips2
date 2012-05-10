#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_getgnav (char *model, char *eventName,char *navTime, char *gridNav, int *lGridNav, int *iret )  
/************************************************************************
 *									*
 * db_getgnav               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      ier, ier1;
    char     diagMessage[720],queryText[320];
    struct   timeb t_callback, t_current;
/*----------------------------------------------------------------------*/
  /*
   * If the pointer to the callback function is not set, return
   */
    if ( navClbkPtr == NULL ) {
       ier = -17;
       er_wmsg ( "DB", &ier, "db_getgnav", &ier1, 2, strlen("db_getgnav") );
       *iret = -1;
       return;
    }

  /*
   * Initialization
   */
    *iret = 0;
    *lGridNav = 0;
    gridNav[0] = '\0';
    diagMessage[0] = '\0';
    sprintf (queryText, "%s|%s|%s",model,eventName,navTime);
   /*
    * Execute the callback to get the navigation
    */
    db_msgcave ("db_getgnav", "debug", "calling the callback function", &ier);
    ftime(&t_callback);
    navClbkPtr(queryText);
    ftime(&t_current);
    sprintf (diagMessage, "%s %d", "time spent in callback ", (int) (1000.0 * (t_current.time - t_callback.time) + (t_current.millitm - t_callback.millitm))); 
    db_msgcave ("db_getgnav", "info", diagMessage, &ier);
    if ( navStrLength > 0 && navStrBack != NULL ) {
       sprintf (gridNav, "%s", navStrBack);
       *lGridNav = strlen(gridNav);
       G_FREE( navStrBack, char );
       sprintf (diagMessage, "%s %s", "got this nav string", gridNav);
       db_msgcave ("db_getgnav", "debug", diagMessage, &ier);
    }
    else {
       db_msgcave ("db_getgnav", "error", "!!! *could not get nav string* !!!", &ier);
       ier = -18;
       er_wmsg ( "DB", &ier, "Navigation", &ier1, 2, strlen("Navigation") );
       *iret = -1;
       return;
    }
       
    return;

}
