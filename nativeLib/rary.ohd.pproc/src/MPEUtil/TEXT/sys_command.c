#include <string.h>
#include <stdlib.h>

/*
   function to replace call system()
   calling routine: writemosaic
*/

void sys_command(char command[256], int *lencom)
{

char com1[256];

com1[*lencom]='\0';
strncpy(com1,command,*lencom);

system(com1);



}
