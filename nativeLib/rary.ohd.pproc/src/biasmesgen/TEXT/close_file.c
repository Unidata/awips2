#include <stdlib.h>    
#include "bias_table.h"
#include "mesg_hdr.h"

void close_file(char old_filename[128], FILE *fp)

/*

   function to close and rename bias table message file     

   filename has ".temp" removed  after file is closed

   calling routine:  create_biastable_mesg

*/

{

   int len;
   char command[256], new_filename[128];

   /*------------------------------------*/
   /*   close file                       */
   /*------------------------------------*/

   fclose(fp);

   /*------------------------------------*/
   /*   rename file                      */
   /*------------------------------------*/

   len = strlen(old_filename);
   len = len - 5;
   new_filename[len] = '\0';
   memcpy(new_filename, old_filename, len);

/*   printf("len=%d   old_filename=##%s##\n",len,old_filename);  */
/*   printf("         new_filename=##%s##\n",new_filename);  */

   sprintf(command,"mv %s  %s",old_filename,new_filename);
   system(command);
   printf("file renamed to %s\n",new_filename);

}
