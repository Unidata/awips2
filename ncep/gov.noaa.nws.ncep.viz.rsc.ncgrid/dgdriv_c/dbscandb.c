#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>

void db_scandb ( const char *modelName, const char *dbTag, const char *ensTag, 
           const char *timeTmplt, char *fileNames, int *iret)
/************************************************************************
 *                                        *
 * db_scandb                              *
 *                                        *
 * m.gamazaychikov/SAIC 09/11   Created                     *
 ************************************************************************/
{
    int      ier, ier1, ierm;
    char     queryType[25];
    char     queryText[320], diagMessage[720];

    struct   timeb t_callback, t_current;
/*---------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    *iret = 0;
    if ( flnmClbkPtr == NULL ) {
       ier = -17;
       er_wmsg ( "DB", &ier, "db_scandb", &ier1, 2, strlen("db_scandb") );
       db_msgcave ("db_scandb", "error", "!!!* flnmClbkPtr=NULL *!!!", &ierm);
       *iret = ier;
       return;
    }
    sprintf (diagMessage, "%s %s %s %s %s %s %s %s", "modelName=", modelName, "dbTag=",dbTag, "ensTag=", ensTag, "timeTmplt=", timeTmplt);
    db_msgcave ("db_scandb", "debug", diagMessage, &ierm);

   /*
    * Populate the query strings 
    */
    fileNames[0] = '\0';
    queryText[0] = '\0';
    gEventName[0] = '\0';
    sprintf ( queryType, "%s", "scanDb" );
    sprintf ( ePlugIn,   "%s", "ncgrib" );
    sprintf ( eSrc, "%s", "GRID" );
    sprintf ( eParameters, "%s|%s|%s|%s", modelName,dbTag,ensTag,timeTmplt );
    sprintf ( eLibClass, "%s", "GempakScanDbRequest" );

   /*
    * Get the query text
    */
/*    db_getQueryText ( queryType, queryText, &ier );
     if ( ier !=0 ) {
       ier = -3;
       er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
       *iret = ier;
       return;
    }
*/
   /*
    * Execute the callback to get the filenames
    */
//    sprintf (diagMessage, "%s %s", "calling callback with ", queryText);
    /*JAVA callback function will create request constrain*/
    sprintf (diagMessage, "%s %s", "calling callback with ", eParameters);
    db_msgcave ("db_scandb", "debug", diagMessage, &ierm);
    ftime(&t_callback);
//    flnmClbkPtr(queryText);
    flnmClbkPtr(eParameters);
    ftime(&t_current);
    sprintf (diagMessage, "%s %d", "time spent in callback ", (int) (1000.0 * (t_current.time - t_callback.time) + (t_current.millitm - t_callback.millitm)));
    db_msgcave ("db_scandb", "info", diagMessage, &ierm);
    if ( flnmStrLength > 0 && flnmStrBack != NULL ) {
       strcpy (fileNames, flnmStrBack);
       G_FREE( flnmStrBack, char );
//       sprintf (diagMessage, "%s %s", "got this flnm string", fileNames);
//       db_msgcave ("db_scandb", "debug", diagMessage, &ierm);
    }
    else {
       sprintf (diagMessage, "%s %s %s", "!!! *could not get flnm string* !!! for", modelName,timeTmplt);
       db_msgcave ("db_scandb", "error", diagMessage, &ierm);
       ier = -18;
       er_wmsg ( "DB", &ier, "fileNames", &ier1, 2, strlen("fileNames") );
       *iret = ier;
       return;
    }

    return;
}
