#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"
#include <sys/timeb.h>


int db_gFileNames ( const char *dir, int isearch, struct dirent ***ret_namelist)
/************************************************************************
 *									*
 * db_gFileNames               						*
 *									*
 * m.gamazaychikov/SAIC	11/08	Created                                 *
 ************************************************************************/
{
    int      possible_ens_nmbr    = 100;
    int      possible_fcst_hrs    = 500;
    int      max_file_name_length  = 50;
    int      max_number_components    = 13;
    int      max_component_length = 75;
    int      possible_fileNames;
    int      ii, jj, ier, ier1, len;
    int      used, allocated,istmax;
    char     modelName[30], dbTag[5], ensTag[20], timeTmpl[75];
    char     *fileNames;
    char      **starr;
    struct dirent *entry=NULL, *entry2=NULL;
    struct dirent **namelist = NULL;

    char     diagMessage[720];
    int      ierm;
/*---------------------------------------------------------------------*/
    diagMessage[0] = '\0';
    sprintf (diagMessage, "%s %s", "dir=", dir);
    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);
    
   /*
    * Initialization
    */
    fileNames[0] = '\0';
    modelName[0] = '\0';
    dbTag[0] = '\0';
    ensTag[0] = '\0';
    timeTmpl[0] = '\0';

   /*
    * Allocate memmory for starr and get the components
    */
    starr = (char **)malloc((size_t)max_number_components * sizeof(char *));
    istmax = 0;
    for( jj=0; jj < max_number_components; jj++ )
       starr[jj] = (char *)malloc( max_component_length * sizeof(char));
    cst_clst (dir, '_', " ", max_number_components, max_component_length, starr, &istmax, &ier);

    sprintf (diagMessage, "%s %d", "after cst_clst istmax = ", istmax);
    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);
    sprintf (diagMessage, "%s", "dir components:");
    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);
    for ( jj=0; jj < istmax; jj++ ) {
      sprintf (diagMessage, "%s %s", "component ", starr[jj]);
      db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);
    }
    if ( istmax ==4 ) {
       sprintf ( modelName, "%s", starr[0] );
       sprintf ( dbTag, "%s", starr[1] );
       sprintf ( ensTag, "%s", starr[2] );
       sprintf ( timeTmpl, "%s", starr[3] );
    }
    else {
       if ( istmax > 4 ) {
           sprintf ( modelName, "%s", starr[0] );
           sprintf ( dbTag, "%s", starr[1] );
           sprintf ( ensTag, "%s", starr[2] );
           for ( jj = 3; jj < istmax; jj ++ ) {
               if ( starr[jj][0] == '[' ) {
                   sprintf ( timeTmpl, "%s", starr[jj] );
                   break;
               }
               else {
                   strcat ( ensTag,"_" );
                   strcat ( ensTag,starr[jj] ); 
               }
           }
           istmax = 4;
       }
       else {
           for ( jj = 0; jj < max_number_components; jj++ ) free( starr[jj] );
           if( starr ) free( (char **)starr );
           return(0);
       }
    }

   /*
    * Free memory allocated for starr
    */
    for ( jj = 0; jj < max_number_components; jj++ ) free( starr[jj] );
    if( starr ) free( (char **)starr );

   /*
    * Allocate memmory for fileNames
    */
    possible_fileNames = possible_ens_nmbr * possible_fcst_hrs;
    fileNames = (char *)malloc( possible_fileNames * max_file_name_length * sizeof(char));
    sprintf (diagMessage, "%s", "fileNames are not set -> retrieving them");
    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);

    db_scandb ( modelName, dbTag, ensTag, timeTmpl, fileNames, &ier);
    if ( ier != 0 ) {
        sprintf (diagMessage, "%s", "fileNames are not retrieved from DB");
        db_msgcave ("db_gFileNames", "error", diagMessage, &ierm);
        return (0);
    }   
//    sprintf (diagMessage, "%s%s", "retrieved these fileNames -> ", fileNames);
//    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);

   /*
    * Allocate memmory for starr and 
    * break the fileNames string into parts each representing 'file'
    */
    starr = (char **)malloc((size_t)possible_fileNames * sizeof(char *));
    istmax = 0;
    for( jj=0; jj < possible_fileNames; jj++ )
       starr[jj] = (char *)malloc( max_file_name_length * sizeof(char));
    cst_clst (fileNames, '|', " ", possible_fileNames, max_file_name_length, starr, &istmax, &ier);

   /*
    * Free memory allocated for fileNames 
    */
    free( fileNames );

    sprintf (diagMessage, "%s %d", "istmax =", istmax);
    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);

   /*
    * Fake scandir function to return back namelist structure
    */
    used = 0;
    allocated = 2;
    namelist = malloc(allocated * sizeof(struct dirent *));
 
    entry = (struct dirent *) malloc (sizeof(struct dirent));
    for  ( ii = 0; ii < istmax; ii++ ) {
       sprintf ( entry->d_name, "%s", starr[ii] );
       sprintf (diagMessage, "%s %s", "entry->d_name =", entry->d_name);
       db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);
       len = offsetof(struct dirent, d_name) + strlen(entry->d_name) + 1;
       if ((entry2 = malloc(len)) == NULL) {
          for ( jj = 0; jj < possible_fileNames; jj++ ) free( starr[jj] );
          if( starr ) free( (char **)starr );
          free (entry);
          ier = -7;
          er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
          return(0);
       }
       if (used >= allocated) {
            allocated *= 2;
            namelist = realloc(namelist, allocated * sizeof(struct dirent *));
            if (!namelist) {
               for ( jj = 0; jj < possible_fileNames; jj++ ) free( starr[jj] );
               if( starr ) free( (char **)starr );
               free (entry);
               ier = -7;
               er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
               ier = -7;
               er_wmsg ( "DB", &ier, NULL, &ier1, 2, 0 );
               return(0);
            }
       }
       memcpy(entry2, entry, len);
       namelist[used++] = entry2;
    }

    free (entry);
    for ( jj=0; jj < possible_fileNames; jj++ ) free( starr[jj] );
    if ( starr ) free( (char **)starr );

    *ret_namelist = namelist;
    sprintf (diagMessage, "%s %d", "leaving with istmax=", istmax);
    db_msgcave ("db_gFileNames", "debug", diagMessage, &ierm);
    return(istmax);       
}
