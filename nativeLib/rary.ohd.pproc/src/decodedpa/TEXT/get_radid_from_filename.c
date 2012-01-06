#include <string.h>   
#include "decodedpa.h"

int get_radid_from_filename(char *arg)
{

/*
   function to parse radar id from the filename

   calling function: main_decodedpa
*/

 char prod_id[3], filename[256];
 char *slash="/";
 char st1[2],st2[2];
 int i;

 filename[255]='\0';
 strcpy(filename,arg);

 st1[1]='\0';
 st2[1]='\0';
 strcpy(st2,slash);
 radid[3]='\0';
 prod_id[2]='\0';
 
 /*--------------------------------------------------------------*/
 /*   search for string = "81" in first 7 char after last "/"    */
 /*   radar id precedes "81" string, 81 is product code for DPA  */
 /*--------------------------------------------------------------*/

 for (i=strlen(filename) - 1; i > 0; i--)
 {
   sprintf(st1,"%c",filename[i]);
   if(strcmp(st1,st2) == 0) break;
 }

 sprintf(prod_id,"%c%c",filename[i+6],filename[i+7]);

 if(strcmp(prod_id,"81") == 0)
 {
   sprintf(radid,"%c%c%c",toupper(filename[i+2]),toupper(filename[i+3]),toupper(filename[i+4]));
 }
 else
 {
   return 1;
 }

 return 0;

}
