#include <stdlib.h>
#include <string.h>
#include <sys/utsname.h>

struct utsname  uts_struct;
char *systname;

void MPEFieldGen_get_os_system(char *systname)
{
    uname(&uts_struct);
    strcpy(systname, (char *)uts_struct.sysname);
    
}    
