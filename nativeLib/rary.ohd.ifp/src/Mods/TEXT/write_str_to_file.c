/*  Routine to write a character string to a file.
 *  Inputs:  char *filename, 
 *           char *string,
 *           char *access_mode
 *  Returns: int - TRUE if it could successfully open file
                   FALSE if file open unsuccessful
 *
 *  Written by D. Page - 5 Oct. 1995
 */

#include <stdio.h>
#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif
 
int write_str_to_file(char *filename, char *string, char *access_mode)
{
    FILE    *filename_fp;
    
    filename_fp = fopen(filename, access_mode);
    if(filename_fp != NULL)
    {
       fprintf(filename_fp, string);
       fclose(filename_fp);
       return(TRUE);
    }
    else
    {
       printf("Problem opening file: %s\n", filename);
       return(FALSE);
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/write_str_to_file.c,v $";
 static char rcs_id2[] = "$Id: write_str_to_file.c,v 1.1 1995/11/14 12:19:58 page Exp $";}
/*  ===================================================  */

}
