 
#include <stdlib.h>
 
/********************************************************************/
/*                                                                  */
/*  Title:         antoi                                            */
/*  Date:          Jan 28, 1991                                     */
/*  Organization:  W/OSO242 - Graphics and Display Section          */
/*  Programmer:    Allan Darling                                    */
/*  Language:      C/370                                            */
/*                                                                  */
/*  Abstract:      This function will convert a character array     */
/*                 (string) of length (len) into an integer.        */
/*                 The integer is created via a call to the         */
/*                 function atoi.  This function extends the        */
/*                 functionality of atoi by removing the            */
/*                 requirement for a sentinal delimited string      */
/*                 as input.                                        */
/*                                                                  */
/*  Input: - Pointer to an array of characters.                     */
/*         - Integer indicating the number of character to include  */
/*           in the conversion.                                     */
/*                                                                  */
/*  Output:- An integer corresponding to the value in the character */
/*           array or MAXNEG (-2147483648) if the function is       */
/*           unable to acquire system storage.                      */
/*                                                                  */
/*  Modification History:                                           */
/*                 None                                             */
/*                                                                  */
/********************************************************************/
 
int antoi(char * string, int len)
{
 
    /*******************/
    /* local variables */
    /*******************/
 
    char * tmpstr;
    int i,
        retval;
 
 
    /*****************/
    /* function body */
    /*****************/
 
    tmpstr = malloc((len+1) * sizeof(char));
 
/*    if (tmpstr == NULL) return (-2147483648); */
    if (tmpstr == NULL) return (-2147483647);
 
    for (i = 0; i < len; i++)
       tmpstr[i] = string[i];
 
    tmpstr[len] = '\0';
 
    retval = atoi(tmpstr);
 
    free(tmpstr);
 
    return(retval);
 
} /* end antoi */
 
