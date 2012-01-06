#include <string.h>
#include <stdio.h>
#include "version.h"
#include "print_versionf.h"


char *vername;

void MPEFieldGen_print_versionf(char *vername)
{
   strcpy(vername,version_number);
/*   printf("vername is %s,\n", vername);  */
   
}     
