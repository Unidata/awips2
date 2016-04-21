
#include <stdio.h>
#include <sys/io.h>
#include <sys/stat.h>
#include <sys/types.h>

int getFilesize(char *fileName)
{

struct stat  statb;

if (stat (fileName, &statb) < 0)
{
   printf("File %s not found \n",fileName);
   
   return 0;
}else 
{

   return statb.st_size;

}


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getFilesize.c,v $";
 static char rcs_id2[] = "$Id: getFilesize.c,v 1.2 2001/06/14 18:39:27 dws Exp $";}
/*  ===================================================  */

}
