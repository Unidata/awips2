#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_getduri ( char *queryType, char *source, char *model,
                  char *dattim, char *vcord, char *parm, char *evtname, 
                  int *level, int *level2,
                  char *dataURI, int *lDataURI, int *iret )
/************************************************************************
 *									*
 * db_getduri               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      ier, ier1, ier2, iplgn = 129;
    char     queryText[320], diagMessage[720];
    char     errStr[100], plgn[iplgn-1];
    struct   timeb t_callback, t_current;
/*---------------------------------------------------------------------*/
    if ( duriClbkPtr == NULL ) {
       ier = -17;
       er_wmsg ( "DB", &ier, "db_getgduri", &ier1, 2, strlen("db_getgduri") );
    }
     

   /*
    * Initialization
    */
    *iret = 0;
    dataURI[0] = '\0';
    queryText[0] = '\0';
    diagMessage[0] = '\0';
    *lDataURI = 0;

   /*
    * Populate the query strings 
    */

    db_getparm ( plgn, &ier2,  iplgn);
    if ( ier2 != 0 ) {
      *iret = -1;
      return;
    }
    if (strcmp ( queryType, "dataURI" ) == 0 ) {
      if ( strcmp ( source, "GRID") == 0 ) {
         sprintf (ePlugIn,   "%s", plgn);
         sprintf (eSrc, "%s", source);
         sprintf (eMdl, "%s", model);
         sprintf (gDattim, "%s", dattim);
         sprintf (gEventName, "%s", evtname);
         sprintf (gCord, "%s", vcord);
         if ( *level2 == -1 )
             sprintf (gLevel, "%d", *level);
         else
             sprintf (gLevel, "%d:%d", *level, *level2);
         sprintf (gParm, "%s", parm);
         sprintf (eLibClass, "%s", "GempakDataURIRequest");
      }
      else {
       ier = -9;
       sprintf (errStr, "%s+", queryType);
       strcat  (errStr, source);
       er_wmsg ( "DB", &ier, errStr, &ier1, 2, strlen(errStr) );
       *iret = -1;
       return;
      }
    }
    else {
      ier = -8;
      er_wmsg ( "DB", &ier, queryType, &ier1, 2, strlen(queryType) );
      *iret = -1;
      return;
    }

   /*
    * Get the query text
    */
    sprintf (queryText, "%s|%s|%s|%s|%s|%s", model, evtname, parm, vcord, gLevel, gDattim); 
/*
    db_getQueryText ( queryType, queryText, &ier);
    if ( ier !=0 ) {
       ier = -3;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = -1;
       return;
    }
 */   
   /*
    * Execute the callback to get the datauri
    */
    sprintf (diagMessage, "%s %s", "calling callback with ", queryText);
    db_msgcave ("db_getduri", "debug", diagMessage, &ier);
    ftime(&t_callback);
    duriClbkPtr(queryText);
    ftime(&t_current);
    sprintf (diagMessage, "%s %d", "time spent in callback ", 
            (int) (1000.0 * (t_current.time - t_callback.time) + (t_current.millitm - t_callback.millitm)));
    db_msgcave ("db_getduri", "debug", diagMessage, &ier);
    if ( duriStrLength > 0 && duriStrBack != NULL ) {
       if ( strstr(duriStrBack, "Error") == NULL ) {
         strcpy (dataURI, duriStrBack);
         *lDataURI = strlen(dataURI);
         G_FREE( duriStrBack, char );
         sprintf (diagMessage, "%s %s", "got this datauri string", dataURI);
         db_msgcave ("db_getduri", "debug", diagMessage, &ier);
       }
       else {
          sprintf (diagMessage, "%s %s", "error getting the data uri ->", duriStrBack);
          db_msgcave ("db_getduri", "debug", diagMessage, &ier);
          *iret = -1;
          return;
       }
    }
    else {
          sprintf (diagMessage, "%s%s%s%s%s%s%s%s%s%s%s%d", 
                   "could not get datauri string for model=", model,
                   " evtname=", evtname, " dattim=", dattim, 
                   " parm=", parm, " vcord=",vcord, " level=", *level);
          db_msgcave ("db_getduri", "debug", diagMessage, &ier);
          ier = -18;
          er_wmsg ( "DB", &ier, "dataURI", &ier1, 2, strlen("dataURI") );
          *iret = -1;
          return;
    }

    return;
}
