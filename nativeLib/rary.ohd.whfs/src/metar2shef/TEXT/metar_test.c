#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global_external.h"
extern int metarop();
extern int metardrive();
extern void  metarcl();
/*---------------------------------------------------------------------
  
       FUNCTION 
         metar_test
  
       PURPOSE  
         To test the shef_decoder.  The user will provide the filename
         of the file containing shef data.  Two files will be created,
         one containing the shef data and any errors.  The second file
         contains full shef-out records of all data.
  
       VERSION and UPDATES
         1.0    SEP 95   David G. Brandon
                         Original Version

  --------------------------------------------------------------------- */

void metar_test()
{

char buffer[80];
  
   fprintf(stdout,"\n------------------------------");
   fprintf(stdout,"\n  metar_decode: test option.\n");
   fprintf(stdout,"\n------------------------------");
   fprintf(stdout,"\n  input file:    ");
   fgets(buffer,sizeof(buffer),stdin);
   memset(&buffer[strlen(buffer) -1],0,1);
   strcpy(stats_.product_name,buffer);

   /* store file names */
   memset(files_.shef_in,0,sizeof(files_.shef_in));
   memset(files_.shef_decode_err,0,sizeof(files_.shef_decode_err));
   memset(files_.shef_out,0,sizeof(files_.shef_out));
   strcpy(files_.shef_in,buffer);
   sprintf(files_.shef_decode_err,"%s.err",buffer);
   sprintf(files_.shef_out,"%s.out",buffer);

   /* set flags */
   strncpy(&cont_.out_flag[0],  "+",1);
   strncpy(&cont_.error_flag[0],"+",1);
   strncpy(&cont_.post_flag[0], "-",1);

   /* call decoder */
   if ( metarop() == 0 )
   {
     metardrive();
     metarcl();
   }

}
