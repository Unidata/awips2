#include <stdlib.h>
#ifdef LINX
 #define checksystem checksystem_
#else
 #define checksystem checksystem
#endif

/* This program gets one byte out of a word (i)
   if the machine is little endian, the least sig byte is stored
   at the most sig address  (ie a will be = 0x11)

   If the machine is big endian, the most sig byte is stored
   at the most sig address  (ie a will be = 0x44)

*/

checksystem(int *nflag)
{
long i = 0x44332211;
unsigned char* a = (unsigned char*) &i;

/*  *nflag=((*a != 0x11)==1) ? 1 : 0;  */
if ((*a != 0x11)==1) *nflag = 1;
else                 *nflag = 0;  


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/cvtgriddb/RCS/checksystem.c,v $";
 static char rcs_id2[] = "$Id: checksystem.c,v 1.1 2002/02/12 16:56:35 dws Exp $";}
/*  ===================================================  */

}

