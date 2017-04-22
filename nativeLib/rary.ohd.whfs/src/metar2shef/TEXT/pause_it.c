#include <stdlib.h>
#include <stdio.h>

void pause_it( char *msg)
{
int  i;
fprintf(stdout,"\n %s",msg);
i = getc(stdin); 
/* sleep(1); */
}
