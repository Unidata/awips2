#include <stdio.h>
#include <time.h>

/* function gettime - get local time in sec */

void gettime(long int *t)

{
 long  tp;

 *t = time(&tp);


}
