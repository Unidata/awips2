#include "metar.h"                            /* dgb:09/16/01 */
/* #include <metar.h> */
 
/********************************************************************/
/*                                                                  */
/*  Title:         fracPart                                         */
/*  Organization:  W/OSO242 - GRAPHICS AND DISPLAY SECTION          */
/*  Date:          13 Jun 1995                                      */
/*  Programmer:    CARL MCCALLA                                     */
/*  Language:      C/370                                            */
/*                                                                  */
/*  Abstract:      Convert a character string fraction into a       */
/*                 decimal (floating point) number.                 */
/*                                                                  */
/*  External Functions Called:                                      */
/*                 None.                                            */
/*                                                                  */
/*  Input:         string - a pointer to a character string frac-   */
/*                          tion.                                   */
/*  Output:        A decimal (floating point) number.               */
/*                                                                  */
/*  Modification History:                                           */
/*                 09/16/01: change <metar.h> to "metar.h"          */
/*                                                                  */
/********************************************************************/
 
float fracPart( char *string )
{
 
   /***************************/
   /* DECLARE LOCAL VARIABLES */
   /***************************/
 
   char buf[ 6 ],
        *slash;
 
   float numerator,
         denominator;
 
   /*************************/
   /* START BODY OF ROUTINE */
   /*************************/

   slash = strchr(string, '/');

   memset(buf , '\0', 6);
   strncpy( buf, string, slash-string);
 
   numerator = (float) atoi(buf);
 
   memset(buf , '\0', 6);
   strcpy( buf, slash+1);
 
   denominator = (float) atoi(buf);
 
   return (numerator/denominator);
 
}
