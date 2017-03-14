#include <stdlib.h>

#include "bias_table.h"
#include "mesg_hdr.h"
#include "startdb.h"
#include "closedb.h"

int biasmesgen_main(int argc, const char** argv)
{

   long int irc;

   /*-----------------------------*/
   /*   Open PostgreSQL database  */
   /*-----------------------------*/

    startdb(&irc);
    if(irc !=0)
    {
      printf("PostgreSQL error# %ld ",irc);
      printf(" occurred attempting to open database \n");
      exit(1);
    }

   /*----------------------------------------------------------------------*/
   /*   read AWIPS site identifier and site number from Admin table        */
   /*   site number is placed in MesgHdr structure                         */
   /*   site identifier is placed in biastable structure                   */
   /*----------------------------------------------------------------------*/

   get_site();

   printf("AWIPS Site ID  = %s\n",biastable.site_id);
   printf("AWIPS Site ID Number  = %03d\n",MesgHdr.src_id);

   /*-----------------------------*/
   /*   create bias table message */
   /*-----------------------------*/

    create_biastable_mesg();

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

return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
