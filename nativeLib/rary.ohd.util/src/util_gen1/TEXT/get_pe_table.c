/* Get the data base table name for specified SHEF PE code.

*/

#include <pe_map_table.h>
#include <stdlib.h>
   
/**********************************************************************/

void get_pe_table (char *pe_code, char *db_table, int *istat)

{
int   i, ldebug, nchar, ifound;

   ldebug=0;

   *istat=0;

   if (num_pe_maps == 0) {
      printf ("ERROR: Number of pe map entries is zero.\n");
      *istat=1;
      return;
      }

/* determine which table to use */
   ifound=0;
   for (i = 0; i < num_pe_maps; i++) {
      nchar=1;
      if (pe_code[0] == 'P')
         nchar=2;
      if (ldebug > 0)
          printf ("i=%i pe_code=%i pe_map[i].pe=%s\n",
                   i,pe_code,pe_map[i].pe);
      if ( ! strncmp(pe_code, pe_map[i].pe, nchar)) {
         strcpy (db_table, pe_map[i].table);
         ifound=1;
         break;
         }
      }

   if (ifound == 0) {
      printf ("ERROR: PE code %s not found in pe map table.\n",pe_code);
      *istat=1;
      return;
      }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_pe_table.c,v $";
 static char rcs_id2[] = "$Id: get_pe_table.c,v 1.2 2001/02/14 19:25:18 dws Exp $";}
/*  ===================================================  */

}
