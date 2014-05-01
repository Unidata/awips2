/*  function Mod_text_parse
 *
 *  THIS function READS VALUES UNTIL END OF text OR A
 *   NON-NUMERIC VALUE IS FOUND.
 *  EXPANDS ANY REPEATED VALUES (I.E., ENTERED WITH
 *   A * AND A REPEAT FACTOR).
 *  If there's a syntax problem encountered, return
 *   status = 1, and any valid fields that have been
 *   decoded prior to encountering error.
 *
 * Originally coded by George Smith, HRL, 951130.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <X11/Intrinsic.h> /* to get values of TRUE/FALSE */

int Mod_decode_single_field( char  *,
                             int   *,
                             int   *,
                             float *);
                            
int Mod_text_parse(char  * text_string_in,
                   int   * num_vals, 
                   float * values)
{
 #include <string.h>
 
 char *  lparen;
 char *  aster;
 char *  text_string;
 char *  field;
 int     status;
 int     mult_paren, mult_value;
 int     loc_start_paren;
 float   multiplier, value;
 int     i, j;
 int     first_field, in_paren, rparen_found;
 int     num_vals_in_paren;
      
 *num_vals=0;
 
 text_string = (char *) malloc(((int)strlen(text_string_in) + 1) * sizeof(char));
 
 strcpy(text_string, text_string_in);
 
 in_paren = FALSE;
 first_field = TRUE;
 
 while ((field = 
          first_field ? strtok(text_string, " ,\t" ) : strtok(NULL, " ,\t" )
        ) != NULL)
   {
  /*
   * Cycle through each field in string.
   */
    first_field = FALSE;
    
    lparen = strchr(field, '(' );
 
    if(lparen == NULL)
     /*
      * Field does not contain left parenthesis.  Simply decode it.
      */
      {
       status = Mod_decode_single_field(field, &rparen_found, 
                                        &mult_value, &value);
       if(status > 0 ||
          (rparen_found && !in_paren))
         {
        /*
         * Error decoding field or right parenthesis unexpected, return.
         */
          free (text_string);
          return (1);
         }
       else
         {
        /*         
         * Have valid field.
         * Fill portion of values array and increment
         *  num_vals to be returned.
         */
          for (i = *num_vals; i < *num_vals + mult_value; i++)
             {
              values[i] = value;
             }
          *num_vals = *num_vals + mult_value;
         }
       if (rparen_found)
         {
        /*
         * Field contained right parenthesis.
         * We know that we're in a parenthetical phrase or we
         *  would have been caught in error checking stanza above.
         * Fill in values array with values to be multiplied in
         *  parenthetical phrase.
         */
          in_paren = FALSE;
          
          if (mult_paren > 1)
            {
           /*
            * Right parenthesis found, closing parenthetical
            *  phrase.  Fill in copies 2 to mult_paren of data
            *  entered inside parentheses.
            * Not much to do if parenthesis multiplier is 1.
            */
             num_vals_in_paren = *num_vals - loc_start_paren;
             for (i = 0; i < mult_paren -1 ; i++)
               {
                for (j = 0; j < num_vals_in_paren; j++)
                  {
                   values[*num_vals + j] = values[loc_start_paren + j];
                  }
                *num_vals = *num_vals + num_vals_in_paren;
                
               } /* End for (... i < mult_paren - 1 ...) loop */
            } /* End  mult_paren > 1 stanza */   
         } /* End rparen_found stanza */
      } /* End of no left parenthesis stanza */
    else 
      {
     /* Left parenthesis found.  Set parenthetical phrase flag.
      * Check that character immediately before parenthesis
      *  is an asterisk.      
      */
       if(in_paren == TRUE)
         {
        /*
         * Left parenthesis found inside a parenthetical phrase.
         * Invalid syntax, return.
         */
          free (text_string);
          return (1);
         }
         
       in_paren = TRUE;
       
       aster = strchr(field, '*' );
       if(aster[0] != *(lparen - 1))
         {
        /*
         * Asterisk not found where expected, return.
         */
          free (text_string);
          return (1);
         }
       else
         {
        /*
         * Asterisk found, separate field into multiplier and value.
         * Decode multiplier.
         */
          aster[0] = '\0';
          status = Mod_decode_single_field(field, &rparen_found,
                                           &mult_value, &multiplier);
          if(status == 1 || 
             mult_value != 1    ||
             multiplier <= 0.0)
            {
           /*
            * Problem decoding multiplier, return.
            */
             free (text_string);
             return (1);
            }
          else
            {
           /* 
            * Have multiplier, convert to int, 
            *  decode fields within parentheses.
            */
             mult_paren = (int)multiplier;
             loc_start_paren = *num_vals;
           /*
            * Find beginning of parenthetical phrase,
            *  not including opening parenthesis.
            * Note:  We already know where the left
            *  parenthesis is.  First increment to
            *  the character following the left
            *  parenthesis, then start evaluating
            *  each field inside the parentheses.
            */
             field = lparen + 1;
           /*
            * Decode field, store values if successful.
            */ 
             status = Mod_decode_single_field(field, &rparen_found,
                                              &mult_value, &value);
             
             if(status > 0 ||
                (rparen_found && !in_paren)
               )
               {
              /*
               * Error decoding field or right parenthesis unexpected, return.
               */            if(status == 1)
                free (text_string);
                return (1);
               }
             else
               {
              /*         
               * Have valid field.
               * Fill portion of values array and increment
               *  num_vals to be returned.
               */
                for (i = *num_vals; i < *num_vals + mult_value; i++)
                  {
                   values[i] = value;
                  }
                *num_vals = *num_vals + mult_value;
               }
               
             if (rparen_found)
               {
              /*
               * Field contained right parenthesis.
               * We know that we're in a parenthetical phrase or we
               *  would have been caught in error checking stanza above.
               * Fill in values array with values to be multiplied in
               *  parenthetical phrase.
               */
               in_paren = FALSE;
          
               if (mult_paren > 1)
                 {
                /*
                 * Right parenthesis found, closing parenthetical
                 *  phrase.  Fill in copies 2 to mult_paren of data
                 *  entered inside parentheses.
                 * Not much to do if parenthesis multiplier is 1.
                 */
                  num_vals_in_paren = *num_vals - loc_start_paren;
                  for (i = 0; i < mult_paren -1 ; i++)
                    {
                     for (j = 0; j < num_vals_in_paren; j++)
                       {
                        values[*num_vals + j] = values[loc_start_paren + j];
                       }
                     *num_vals = *num_vals + num_vals_in_paren;
                
                    } /* End for (... i < mult_paren - 1 ...) loop */ 
                 } /* End mult_paren > 1 stanza */
               } /* End rparen_found stanza */ 
            } /* End have valid parenthesis multiplier stanza */
         } /* End else stanza - have found asterisk */
      } /* End else stanza for left parenthesis found */
   } /* End while loop cycle through fields in string */
 free (text_string);
 return(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/Mod_text_parse.c,v $";
 static char rcs_id2[] = "$Id: Mod_text_parse.c,v 1.1 1995/11/14 12:19:02 page Exp $";}
/*  ===================================================  */

}
     
 
