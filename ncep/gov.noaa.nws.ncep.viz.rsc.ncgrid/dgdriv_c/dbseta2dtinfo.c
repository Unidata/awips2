#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

A2Data_t          A2DtTable;

void db_seta2dtinfo ( const char *alias, const char *path, const char *template, 
                      int *iret )
{
    int      ii, jj, numtmplts, istmaxa, istmaxp, istmaxt, ier;
    int      max_number_alias    = 20;
    int      max_alias_length    = 30;
    int      max_number_path     = 20;
    int      max_path_length     = 26;
    int      max_number_template = 20;
    int      max_template_length = 49;
    char      **starra, **starrp, **starrt;

    char diagMessage[720];
    int  ierm;
/*---------------------------------------------------------------------*/
    sprintf (diagMessage, "%s%s%s%s%s%s", " got these alias=", alias, 
                                                    " path=", path, 
                                                    " template=", template);
    db_msgcave ("db_seta2dtinfo", "debug", diagMessage, &ierm);

    starra = (char **)malloc((size_t)max_number_alias * sizeof(char *));
    starrp = (char **)malloc((size_t)max_number_path * sizeof(char *));
    starrt = (char **)malloc((size_t)max_number_template * sizeof(char *));
    istmaxa = istmaxp = istmaxt = 0;
    for( jj=0; jj < max_number_alias; jj++ )
       starra[jj] = (char *)malloc( max_alias_length * sizeof(char));
    for( jj=0; jj < max_number_path; jj++ )
       starrp[jj] = (char *)malloc( max_path_length * sizeof(char));
    for( jj=0; jj < max_number_template; jj++ )
       starrt[jj] = (char *)malloc( max_template_length * sizeof(char));
    cst_clst (alias, '|', " ", max_number_alias, max_alias_length, starra, &istmaxa, &ier);
    cst_clst (path, '|', " ", max_number_path, max_path_length, starrp, &istmaxp, &ier);
    cst_clst (template, '|', " ", max_number_template, max_template_length, starrt, &istmaxt, &ier);

    if ( istmaxa == istmaxp && istmaxa == istmaxt ) {
        numtmplts = istmaxa;
    }
    else {
        *iret = -1;
        return;
    }
    if ( A2DtTable.info != NULL ) {
           free(A2DtTable.info);
           A2DtTable.numtmpl = 0;
           A2DtTable.info = NULL;
    }
    A2DtTable.numtmpl  = numtmplts;
    A2DtTable.info = (A2DTinfo *) malloc( numtmplts * sizeof(A2DTinfo) );
    if ( A2DtTable.info == NULL ) {
            *iret = -1;
            return;
    }
    ii =  0;
    while ( ii < numtmplts ) {
       sprintf ( A2DtTable.info[ii].alias,    "%s",   starra[ii] );
       sprintf ( A2DtTable.info[ii].path,     "%s",   starrp[ii] );
       sprintf ( A2DtTable.info[ii].template, "%s",   starrt[ii] );
       sprintf (diagMessage, "%s%d%s%s%s%s%s%s", "entry ", ii+1, 
             " setting alias=", A2DtTable.info[ii].alias, 
                      " path=", A2DtTable.info[ii].path, 
                  " template=", A2DtTable.info[ii].template);
       db_msgcave ("db_seta2dtinfo", "debug", diagMessage, &ierm);
       ii++;
    }

    for( jj=0; jj < max_number_alias; jj++ )
       free( starra[jj] );
    for( jj=0; jj < max_number_path; jj++ )
       free( starrp[jj] );
    for( jj=0; jj < max_number_template; jj++ )
       free( starrt[jj] );
    free( starra );
    free( starrp );
    free( starrt );
    return;
}
