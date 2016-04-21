/*  Function Mod_decode_single_field
 *
 *  This function takes a mod input field and parses the value entered.
 *  If there is a multiplying factor, it is also decoded.
 *  If a right parenthesis is found, the string being decoded is
 *   truncated at the right parenthesis, and the fact that a right
 *   parenthesis was found is returned through the argument list.
 *
 *  Originally coded by George Smith, HRL, 951130.
 */
 
#include <stdlib.h>
#include <string.h>
#include <X11/Intrinsic.h> /* to get values for TRUE/FALSE */
extern int is_valid_number(char *);
int Mod_decode_single_field( char  * string,
                             int   * rparen_found,
                             int   * num_vals,
                             float * value)
{
 char * rparen;
 char * aster;
 char * multiplier_str;
 char * mult_str_aster;
 int    multiplier; 

 *rparen_found = FALSE;
 multiplier_str = NULL;
 multiplier = 1;
 *num_vals = 0;
 
  /*
   * Check for right parenthesis.
   */
    rparen = strchr(string, ')' );
    if(rparen != NULL)
      {
     /*
      * Right parenthesis found, 
      * Remove parenthesis from string
      *  and set value returned through
      *  argument list. 
      */
       rparen[0] = '\0';
       *rparen_found = TRUE;
      } /* End right parenthesis found stanza. */

     /*
      * If right parenthesis found it has now been removed.
      * Can only have a number or simple multipling value (n*x)
      *  in string.
      * Decode.
      */
       aster = strchr(string, '*' );
       if(aster != NULL)
         {
        /*
         * Asterisk found.  Extract multiplying factor.
         */
          multiplier_str = (char *) malloc(((int)strlen(string) +1) * sizeof(char));
          strcpy(multiplier_str, string);
          mult_str_aster = strchr(multiplier_str, '*'); 
          mult_str_aster[0] = '\0';
        /*
         * Now see if multiplier is a valid integer.
         */
          multiplier = atoi(multiplier_str);
          
          if(multiplier < 1)
            {
           /*
            * Invalid multiplier, return.
            */
             if (multiplier_str != NULL);
                 free (multiplier_str);
             return (1);
            }
          else
            {
           /*
            * Valid multiplier, move value portion of string
            *  to beginning of string variable for subsequent
            *  processing, below.
            */
             strcpy(string, (aster+1));
            }
          } /* End of asterisk check. */
          
       /*
        * Now decode value field.
        */
        if(is_valid_number(string) == FALSE)
          {
         /*
          * Value entered is not a valid number, return.
          */
           if (multiplier_str != NULL);
               free (multiplier_str);
           return (1);
          }
        else
          {
         /*
          * Value is valid, convert to float, return.
          */
           *value = (float)atof(string);
           *num_vals = multiplier;
           if (multiplier_str != NULL);
               free (multiplier_str);
           return (0);
          }
/*
 * Error in logic if drop through to here.
 */          
 if (multiplier_str != NULL);
     free (multiplier_str);
 return (1);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mod_decode_single_field.c,v $";
 static char rcs_id2[] = "$Id: Mod_decode_single_field.c,v 1.1 1995/11/14 12:19:01 page Exp $";}
/*  ===================================================  */

}
