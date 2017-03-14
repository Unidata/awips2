
#include <stdio.h>

void get_scale(datatype, time_scale)

/*
 * This function reads the /u/nwsrfs/datatype file
 *  and returns the time_scale (instantaneous = 1, accumulated = 2,
 *  or averaged = 3) for the given data type.
 *
 * Function originally written by George Smith - HRL
 *     November 1990
 */

char    *datatype;
int     *time_scale;
{
FILE    *file_pointer;
char    scale[5], one_line[80], localtype[5], current_type[5];
int     i;

memset(localtype, '\0', 5);          /* copy 4 characters of         */
strncpy(localtype, datatype, 4);     /* datatype into local variable */

for (i = 0; i < 4; i++)                          /* change blanks    */
    if(localtype[i] == ' ') localtype[i] = '\0'; /* to nulls         */

file_pointer = fopen("/u/nwsrfs/system/datatype", "r");

fgets(one_line, 80, file_pointer);      /* skip first 2 cards */
fgets(one_line, 80, file_pointer);

while(fgets(one_line, 80, file_pointer) != NULL)
   {                                      /* read one card image */
    sscanf(one_line, "%s", current_type);
    if(strcmp(current_type, localtype) == 0)
      {                                   /* have found wanted type */
       fgets(one_line, 80, file_pointer); /* read next card image   */
       sscanf(one_line, "%*s%*s%*s %s", scale);
       if(strcmp(scale, "INST") == 0) *time_scale = 1;
       else if (strcmp(scale, "ACCM") == 0) *time_scale = 2;
       else if (strcmp(scale, "MEAN") == 0) *time_scale = 3;
       return;
      }
   }
*time_scale = 0; /* get here only if datatype isn't found */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/get_scale.c,v $";
 static char rcs_id2[] = "$Id: get_scale.c,v 1.1 1995/09/08 14:59:37 page Exp $";}
/*  ===================================================  */

}
