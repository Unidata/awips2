#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

#define A2DB_TAG	"A2DB_"

extern  A2Data_t        A2DtTable;

void db_a2dtget ( char *alias, char *path, char *template, int *iret )  
/************************************************************************
 *									*
 * db_a2dtget               						*
 *									*
 * m.gamazaychikov/SAIC	09/11	Created                                 *
 ************************************************************************/
{
    int      ier, ii, ic, ipos, ic2, ipos2;
    char     name[49], tmpl[49];

    int      ierm;
    char     diagMessage[720];
/*----------------------------------------------------------------------*/

   /*
    * Initialization
    */
    *iret = 0;
    path[0] = '\0';
    template[0] = '\0';
    diagMessage[0] = '\0';
    

    db_msgcave ("db_a2dtget", "debug", "calling db_isdbfile", &ierm);
    db_isdbfile ( alias, &ier,  strlen (alias) ); 

   /*
    * For the A2DB data create path and template
    */
    if ( ier == 0 ) {

      /*
       * Remove the name of the storm/volcano from the alias.
       */
      /*
       * Find a match for the alias.
       */
       for ( ii = 0; ii < A2DtTable.numtmpl; ii++ )  {
          strcpy( path, A2DtTable.info[ii].path );
          cst_nocc ( A2DtTable.info[ii].template, '*', 1, 0, &ipos2, &ic2 );
          sprintf (diagMessage, "%s %d ", "ic2=", ic2 );
          db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
          if ( ic2 == 0 ) {
            /*
             * Remove the name of the storm/volcano from the alias.
             */
             cst_nocc ( alias, ':', 1, 0, &ipos, &ic );
             if  ( ic == 0 )  {
                sprintf (diagMessage, "%s %d ", "ic=", ic );
                db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                strcpy ( name, &alias[ipos+1] );
                alias[ipos] = CHNULL;
                sprintf (diagMessage, "%s %s", "name ", name);
                db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                sprintf (diagMessage, "%s %s", "new alias is", alias);
                db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
             }
             if ( strcmp( A2DtTable.info[ii].alias, alias ) == 0 )  {
                sprintf (diagMessage, "%s %d %s %s %s %s %s %s", "ii=", ii, "alias=", alias, 
                          "path=", A2DtTable.info[ii].path, 
                          "template=", A2DtTable.info[ii].template );
                db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                if  ( ic == 0 )  {
                   sprintf (diagMessage, "%s %d ", "ic=", ic );
                   db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                   strcpy( tmpl, A2DtTable.info[ii].template );
                   sprintf (diagMessage, "%s %s", "tmpl is ", tmpl);
                   db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                   cst_rpst ( tmpl, "*", name, template, &ier );
                   sprintf (diagMessage, "%s %s %s %s", "changing template ", A2DtTable.info[ii].template, " to ", template);
                   db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                }
                else {
                   strcpy( template, A2DtTable.info[ii].template );
                   sprintf (diagMessage, "%s %s ", "setting template to ", template);
                   db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                }
             }
          }
          else {
             if ( strcmp( A2DtTable.info[ii].alias, alias ) == 0 )  {
                sprintf (diagMessage, "%s %d %s %s %s %s %s %s", "ii=", ii, "alias=", alias, 
                          "path=", A2DtTable.info[ii].path, 
                          "template=", A2DtTable.info[ii].template );
                db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
                strcpy( template, A2DtTable.info[ii].template );
                sprintf (diagMessage, "%s %s ", "setting template to ", template);
                db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
             }
          }
       }
   /*
    * For the non-A2DB data return
    */
    }
    else {
       *iret = -1;
       sprintf (diagMessage, "%s %d", "returning iret= ",*iret);
       db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
    }
  
    sprintf (diagMessage, "%s %s %s %s", "returning path=", path, "template=", template );
    db_msgcave ("db_a2dtget", "debug", diagMessage, &ierm);
       
    return;

}
