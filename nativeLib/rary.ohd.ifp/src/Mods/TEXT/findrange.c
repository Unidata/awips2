#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* to find if the buffer read is a range mods type, then return a value = 1*/
int findrange(char *buf)
{
   char         *cptr,
		*cptr1,
		*cptr2,
		buf1[14];

   int          found = 0;
   
   /* skip the first line in buf */
   
   if ((cptr = strchr (buf,'\n') ))
   {
	if ((cptr1 = strstr (cptr," ")) )
	{
            /* clean up buf, 10/21/03 beta-r26 bug*/
            memset(buf1,'\0',14);
	    /*strncpy(buf1,cptr,(strlen(cptr) - strlen(cptr1)));*/ /*10/21/04 beta-r26 bug */
            /*check the firset 13 chars */
            strncpy(buf1,cptr,13); 
        
   	if ((cptr2 = strstr (buf1,"-")) )
       		{
  
                	if (isdigit ( *(cptr2+1)))
                        	found = 0;
                	else
                        	found = 1;               
        	}
	}
   }
/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/findrange.c,v $";
 static char rcs_id2[] = "$Id: findrange.c,v 1.2 2004/10/21 19:03:02 wkwock Exp $";}
/*  ===================================================  */

   return ( found );
}
