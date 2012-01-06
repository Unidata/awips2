#include <stdio.h>
#include <stdlib.h>
#include <sqlca.h>
#include "closedb.h"
#include "RWBiasStat.h"

void read_rwbiasstat(    const char * fxa_local_site_id, 
                         float *min_gr_value,
                         int *npair_bias_select,
                         int *npair_svar_update,
                         int *std_cut,
                         int *lag_cut,
                         int *init_span,
                         int *bias_qc_opt,
                         int *num_span,
                         float mem_span_values[10])

/*

   this function reads records from the RWBiasStat table
   calling function: create_biastable_mesg

*/

{

   char where[50];
   long int irc;
   RWBiasStat  *rwstatHead, *rwstatPtr;

   /*---------------------------------------------*/
   /*   read record from RWBiasStat table         */
   /*   table has only one record                 */
   /*---------------------------------------------*/

   memset ( where, '\0', 50 );
   sprintf(where,"WHERE office_id = '%s'", fxa_local_site_id );
   printf ( "Querying RWBiasStat table:\n"
            "SELECT * from RWBiasStat %s\n", where );

   if((rwstatHead = GetRWBiasStat(where)))
   {
      rwstatPtr = (RWBiasStat*) ListFirst(&rwstatHead->list);

      if(rwstatPtr)
      {

         *min_gr_value = rwstatPtr->min_gr_value_bias;
         *npair_bias_select = rwstatPtr->npair_bias_select;
         *npair_svar_update = rwstatPtr->npair_svar_update;
         *std_cut = rwstatPtr->std_cut;
         *lag_cut = rwstatPtr->lag_cut;
         *init_span = rwstatPtr->init_span;
         *bias_qc_opt = rwstatPtr->bias_qc_opt;
         *num_span = rwstatPtr->num_span;

         mem_span_values[0] = rwstatPtr->mem_span1;
         mem_span_values[1] = rwstatPtr->mem_span2;
         mem_span_values[2] = rwstatPtr->mem_span3;
         mem_span_values[3] = rwstatPtr->mem_span4;
         mem_span_values[4] = rwstatPtr->mem_span5;
         mem_span_values[5] = rwstatPtr->mem_span6;
         mem_span_values[6] = rwstatPtr->mem_span7;
         mem_span_values[7] = rwstatPtr->mem_span8;
         mem_span_values[8] = rwstatPtr->mem_span9;
         mem_span_values[9] = rwstatPtr->mem_span10;

      }
      else
      {
         printf("PostgreSQL error %s reading RWBiasStat table -- program stopping\n",
                sqlca.sqlstate);

         /*-----------------------------*/
         /*   Close PostgreSQL database */
         /*-----------------------------*/
   
          closedb(&irc);
          if(irc !=0)
          {
            printf("PostgreSQL error# %ld ",irc);
            printf(" occurred attempting to close database \n");
            exit(1);
          }

         exit(1);
      }
    }
    else
    {
       printf("PostgreSQL error %s reading RWBiasStat table -- program stopping\n",
              sqlca.sqlstate);

       /*-----------------------------*/
       /*   Close PostgreSQL database */
       /*-----------------------------*/
   
        closedb(&irc);
        if(irc !=0)
        {
          printf("PostgreSQL error# %ld ",irc);
          printf(" occurred attempting to close database \n");
          exit(1);
        }

       exit(2);
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
