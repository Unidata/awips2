/* Read file with SHEF PE code and data base table name information.

*/

#include <pe_map_table.h>
#include <stdio.h>
#include <stdlib.h>
   
/**********************************************************************/

void read_pe_mapfile (char *dirname, int *num_pe_maps, int *istat)

{
int   i, ldebug, len, ireturn;
char  filename[200];
char  record[80];   int lrecord=80;
char  field3[25];
FILE  *in_file;

   ldebug=0;

   *istat=0;   
   *num_pe_maps=0;

/* set file name */
   sprintf (filename, "%s/pe_map", dirname);
   
/* open file */
   if ((in_file = fopen(filename, "r")) == NULL) {
      printf ("ERROR: Cannot open pe map file %s.\n", filename);
      *istat=1;
      }
      else {
         printf ("Reading pe map file %s.\n", filename);
      /* get number of entries in file */
         while ( ! feof(in_file)) {
            fgets (record, lrecord, in_file);
            if (feof(in_file)) break;
         /* check if comment */
            check_if_comment (record,&ireturn);
            if (ireturn == 1) continue;
            *num_pe_maps=*num_pe_maps+1;
            if (ldebug > 0)
               printf ("num_pe_maps=%i record=%s\n",
                       *num_pe_maps,record);
            }
         if (*num_pe_maps == 0) {
            printf ("ERROR: No pe map information found in file %s.\n", filename);
            *istat=1;
            return;
            }
         pe_map = (pe_map_struct *)calloc(*num_pe_maps, sizeof(pe_map_struct));
         rewind (in_file);
      /* get pe code and pe table */
         i=0;
         while ( ! feof(in_file)) {
            fgets (record, lrecord, in_file);
            if (feof(in_file)) break;
         /* check if comment */
            check_if_comment (record,&ireturn);
            if (ireturn == 1) continue;
            field3[0]=0;
            sscanf (record, "%s %s %s", pe_map[i].pe, pe_map[i].table, field3);
            if (ldebug > 0)
               printf ("i=%i pe_map[i].pe=%s pe_map[i].table=%s field3=%s\n",
                       i,pe_map[i].pe,pe_map[i].table, field3);
            len=strlen(field3);
            if (len > 0) {
               check_if_comment (field3,&ireturn);
               if (ireturn == 0) 
                  strcpy (pe_map[i].table,field3);
               }
            i++;
            }
         fclose (in_file);
         }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/read_pe_mapfile.c,v $";
 static char rcs_id2[] = "$Id: read_pe_mapfile.c,v 1.2 2001/02/14 19:26:03 dws Exp $";}
/*  ===================================================  */

}
