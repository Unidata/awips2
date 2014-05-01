#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


chkdot(int *stat, int array[20])
{

char *ptr= (char * )&array[0];

     *stat = Check_4Command_Line(ptr);
     /* printf("checkdot status = %d \n", *stat); */
}


int Check_4Command_Line(char *buf)
{
   FILE         *fnamefp;

   char         *cptr,
		*cptr1;
   int          n,
		valid = 0;


	/* check for a command line. Return 1 if it is. Return 2 if it is 
	   FGROUP. Return 0 if it is not a command line */
        if (cptr = strstr (buf,".") )
        {
 		if (isdigit ( *(cptr+1)))
                {
                        valid = 0;
                }
                else
                {
			valid = 1;
			if (cptr1 = strstr (cptr,"FGROUP"))
			   valid = 2;
                }
        }

   return (valid);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/chkdot.c,v $";
 static char rcs_id2[] = "$Id: chkdot.c,v 1.2 2000/04/03 12:37:52 dws Exp $";}
/*  ===================================================  */

}
