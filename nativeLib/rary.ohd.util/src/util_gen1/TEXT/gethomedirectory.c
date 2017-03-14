/* File: ghd.c
 *
 *  Given the home directory name returns the length of the string.
 */

#include <stdio.h>

gethomedirectory(HOME_directory, length)
char    HOME_directory[100];
int     *length;
{
strcpy(HOME_directory, getenv("HOME"));
*length = strlen(HOME_directory);
/*printf("HOME_directory = %s length = %d\n",HOME_directory,*length);*/

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/gethomedirectory.c,v $";
 static char rcs_id2[] = "$Id: gethomedirectory.c,v 1.3 2003/08/27 15:24:41 aivo Exp $";}
/*  ===================================================  */

}
