/*******************************************************************************
* FILENAME:            TokenizeOptionString.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:          TokenizeOptionString
* DESCRIPTION:         Accepts a user-supplied string of option/value 
*                      groups and parses it.  The result is returned
*                      in a linked list of OptionValuePair structures.
*   MODULE 2:          FreeOptionValueList
*                      Frees the dynamic memory used in the creation of the
*                      linked list of OptionValuePair structures.  The caller
*                      of the TokenizeOptionString routine must call 
*                      FreeOptionValueList once done with the linked
*                      list of OptionValuePair structures.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       September 19, 2003
* ORGANIZATION:        HSEB / OHD
* MACHINE:             Developed on HP-UX, run on Redhat Linux.     
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#include <stdlib.h>
#include <string.h>

#include "GeneralUtil.h"
#include "List.h"
#include "TokenizeOptionString.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   TokenizeOptionString
* PURPOSE:       This routine parses a string of option/value groups.
*                It returns a linked list of OptionValuePair structures.
*                Each of the nodes in the linked list corresponds to
*                an option and its associated value or values are stored
*                in an array contained within the node.  Each node has
*                a member indicating how many values have been stored
*                for the option.
*
*                The input option string is expected to have the following
*                format:
*
*                option1=value1;option2=value2,value3;option4=value5
*
*                An option is separated from its value or values by an "="
*                sign.  Multiple values are separated from one another by
*                commas ",".  Option/value groups are separated from one
*                another by semicolons ";".
*                
*
* ARGUMENTS:
*   TYPE   DATA TYPE           NAME              DESCRIPTION/UNITS
*   Input  char *              option_string     Contains the user-supplied
*                                                string of option/value groups.
*   Output OptionValuePair **  pOptionValuePair  A linked list with each
*                                                of the nodes corresponding to
*                                                one option parsed from the
*                                                options/values string.  Each
*                                                node may contain zero or
*                                                more values.  That is,
*                                                an option may have zero or
*                                                more values. 
*
* RETURNS:
*   DATA TYPE                    DESCRIPTION
*   int                          Indicates whether or not the user-supplied
*                                string of options and values could be
*                                parsed. 
*
* APIs UTILIZED:
*   NAME                 HEADER FILE             DESCRIPTION
*   FreeOptionValueList  TokenizeOptionString.h  Frees the memory dynamically
*                                                allocated for the
*                                                linked list of 
*                                                OptionValuePair structures.   
*   strip_lblanks        GeneralUtil.h           Removes leading whitespace
*                                                from a character string. 
*   strip_tblanks        GeneralUtil.h           Removes trailing whitespace
*                                                from a character string.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME               DESCRIPTION
*   char *            pComma             Used to point to a comma separating
*                                        multiple values.
*   char *            pValueEnd          Used to point to the end of a value
*                                        token.
*   char *            pEqualSign         Used to point to an equal sign 
*                                        separating an option from its 
*                                        value(s).
*   char *            pOptionVal         Used to point to the successive values
*                                        belonging to an option.
*   char *            pString            Used to tokenize the user-supplied
*                                        option string based on the ";"
*                                        character. 
*   char *            temp_option_string The user-supplied option string is
*                                        copied to this array so that 
*                                        it is not mangled by the strtok
*                                        operations.
*   int               first              Used to determine if the linked list
*                                        has been created or not.
*   int               i                  A loop indexing variable.
*   int               option_string_len  The length of the user-supplied
*                                        options/values string.
*   int               value_count        The number of values associated with
*                                        a single option.
*   int               value_string_len   The length of the option/value(s) 
*                                        group being process from the user-
*                                        supplied string of options/values.
*   OptionValuePair * pOptionValueNode   Points to the current node being
*                                        created in the linked list of
*                                        OptionValuePair structures.
*
* DATA FILES AND/OR DATABASE:
* None.
*
* ERROR HANDLING:
*
*    ERROR CODE                             DESCRIPTION
*    TOKENIZE_OPTION_NULL_ARGUMENT          A null option string was passed
*                                           into this routine.
*    TOKENIZE_OPTION_MISSING_EQUAL          An equal sign separating the
*                                           option from its values is missing.
*    TOKENIZE_OPTION_MALLOC_ERROR           Could not allocate dynamic memory.
*    TOKENIZE_OPTION_OK                     This routine functioned correctly.
*
*   (These constants are defined in the TokenizeOptionString.h header file.)
********************************************************************************
*/

int TokenizeOptionString ( const char * option_string , 
                           OptionValuePair ** pOptionValuePair )
                          
{
   char * pComma = NULL ;
   char * pValueEnd = NULL ;
   char * pEqualSign = NULL ;
   char * pOptionVal = NULL ;
   char * pString = NULL ;
   char * temp_option_string = NULL ;
   int first = 1 ;
   int i ;
   int option_string_len ;
   int value_count ;
   int value_string_len ;
   OptionValuePair * pOptionValueNode = NULL ;

   * pOptionValuePair = NULL ;

   /* Check to determine if option_string is NULL. */
   if ( option_string == NULL )
   {
      return TOKENIZE_OPTION_NULL_ARGUMENT ;
   }

   /* Check to make sure that the first character is not '\0'. */
   if ( * option_string == '\0' )
   {
      return TOKENIZE_OPTION_NULL_ARGUMENT ;
   }

   /* Allocate a character array which is the length of the option_string
      plus one. */
   option_string_len = strlen ( option_string ) ;

   temp_option_string = ( char * ) malloc ( sizeof ( char ) * 
                                          ( option_string_len + 1 ) ) ;

   if ( temp_option_string == NULL )
   {
      return TOKENIZE_OPTION_MALLOC_ERROR ;
   }  

   /* Do a strcpy from option_string to the new string. */ 
   strcpy ( temp_option_string , option_string ) ;

   /* Do a strtok in the new string, using ';' as the delimeter. */
   pString = strtok ( temp_option_string , ";" ) ;

   /* While the token is not NULL */
   while ( pString != NULL )
   {
      /* Look for '='. Take the string before the equal string as
         the option name. */
      pEqualSign = strchr ( pString , '=' ) ; 

      if ( pEqualSign == NULL )
      {
         if ( temp_option_string != NULL )
         {
            free ( temp_option_string ) ;
            temp_option_string = NULL ;
         }

         FreeOptionValueList ( pOptionValuePair ) ; 
         return TOKENIZE_OPTION_MISSING_EQUAL ;
      }

      option_string_len = ( pEqualSign - pString ) + 1 ;

      if ( option_string_len > 1 )
      {
         /* Create a new value option structure. */ 
         pOptionValueNode = ( OptionValuePair * ) 
                            malloc ( sizeof ( OptionValuePair ) ) ;

         if ( pOptionValueNode == NULL )
         {
            if ( temp_option_string != NULL )
            {
               free ( temp_option_string ) ;
               temp_option_string = NULL ;
            }

            FreeOptionValueList ( pOptionValuePair ) ; 
            return TOKENIZE_OPTION_MALLOC_ERROR ;
         }

         pOptionValueNode->option_name = ( char * ) malloc
                                         ( sizeof ( char ) * 
                                           option_string_len ) ; 

         if ( pOptionValueNode->option_name == NULL )
         {
            if ( temp_option_string != NULL )
            {
               free ( temp_option_string ) ;
               temp_option_string = NULL ;
            }

            FreeOptionValueList ( pOptionValuePair ) ; 
            return TOKENIZE_OPTION_MALLOC_ERROR ;
         }

         memset ( pOptionValueNode->option_name , '\0' , option_string_len ) ;
         strncpy ( pOptionValueNode->option_name , pString , 
                   option_string_len - 1 ) ;

         strip_lblanks ( pOptionValueNode->option_name ) ;
         strip_tblanks ( pOptionValueNode->option_name ) ;

         option_string_len = strlen ( pOptionValueNode->option_name ) ; 
   
         if ( option_string_len == 0 )
         {
             free ( pOptionValueNode->option_name ) ; 
             free ( pOptionValueNode ) ;
             pOptionValueNode = NULL ;
         }
         else
         {

            pOptionVal = pEqualSign + 1 ;

            /* The portion of the string after the '=' will be the value(s). */
            /* Check in the value portion for ','. */
            /* Retrieve a count of the number of values associated with this
               option_name. */
            value_count = 1 ;

            pComma = strchr ( pOptionVal , ',' ) ; 

            while ( pComma != NULL ) 
            {
               ++ value_count ;
               pOptionVal = pComma + 1 ;
               pComma = strchr ( pOptionVal , ',' ) ; 
            }

            /* Allocate memory for the values. */
            pOptionValueNode->value_string = ( char ** ) malloc (
                                             sizeof ( char * ) * value_count ) ;                                   
            if ( pOptionValueNode->value_string == NULL )
            {
               if ( temp_option_string != NULL )
               {
                  free ( temp_option_string ) ;
                  temp_option_string = NULL ;
               }

               FreeOptionValueList ( pOptionValuePair ) ; 
               return TOKENIZE_OPTION_MALLOC_ERROR ;
            }

            for ( i = 0 ; i < value_count ; ++ i )
            {
               pOptionValueNode->value_string [ i ] = NULL ;
            }

            /* Retrieve all of the values for this option name. */
            pOptionVal = pEqualSign + 1 ;
            value_count = 0 ;

            do
            {
               pComma = strchr ( pOptionVal , ',' ) ; 

               if ( pComma != NULL )
               {
                  pValueEnd = pComma ;
               }
               else
               {
                  value_string_len = strlen ( pString ) ;
                  pValueEnd = pString + value_string_len ;
               }

               value_string_len = ( pValueEnd - pOptionVal ) + 1 ;
           
               if ( value_string_len > 1 )
               {
                  /* For each value, create a OptionValue pair structure
                     to contain the value and the option it corresponds to. */
                  pOptionValueNode->value_string [ value_count ] = ( char * ) 
                                                         malloc (
                                                         sizeof ( char ) *
                                                         value_string_len ) ;

                  if ( pOptionValueNode->value_string == NULL )
                  {
                     if ( temp_option_string != NULL )
                     {
                        free ( temp_option_string ) ;
                        temp_option_string = NULL ;
                     }

                     FreeOptionValueList ( pOptionValuePair ) ; 
                     return TOKENIZE_OPTION_MALLOC_ERROR ;
                  }

                  memset ( pOptionValueNode->value_string [ value_count ] , 
                           '\0' , value_string_len ) ;
                  strncpy ( pOptionValueNode->value_string [ value_count ] , 
                            pOptionVal , value_string_len - 1 ) ;

                  strip_lblanks ( pOptionValueNode->
                                  value_string [ value_count ] ) ;
                  strip_tblanks ( pOptionValueNode->
                                  value_string [ value_count ] ) ;

                  value_string_len = 
                         strlen ( pOptionValueNode->
                                  value_string [ value_count ] ) ;
   
                  if ( value_string_len > 0 )
                  {
                     ++ value_count ;
                  }
                  else
                  {
                     free ( pOptionValueNode->value_string [ value_count ] ) ;
                     pOptionValueNode->value_string [ value_count ] = NULL ;
                  }
               }

               if ( pComma != NULL ) pOptionVal = pComma + 1 ;

            } while ( pComma != NULL ) ; 

            pOptionValueNode->number_of_values = value_count ;

            /* Push this OptionValue pair onto the linked list. */
            if ( first )
            {
               * pOptionValuePair = pOptionValueNode ;
               ListInit ( & ( * pOptionValuePair )->list ) ; 
               first = 0 ;
            }

            ListAdd ( & ( * pOptionValuePair )->list , 
                      & pOptionValueNode->node ) ;
         }
      } 

      pString = strtok ( NULL , ";" ) ;

   }   /* Continue until the end of the option_string is reached. */

   if ( temp_option_string != NULL )
   {
      free ( temp_option_string ) ;
      temp_option_string = NULL ;
   }

   return  TOKENIZE_OPTION_OK ;
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   FreeOptionValueList 
* PURPOSE:       Frees the dynamic memory allocated in the creation
*                of the linked list of OptionValuePair structures 
*                returned from the call to TokenizeOptionString.
*
* ARGUMENTS:
*   TYPE          DATA TYPE          NAME             DESCRIPTION/UNITS
*   Input/Output  OptionValuePair ** pOptionValuePair The head pointer of the
*                                                     linked list of
*                                                     OptionValue pair 
*                                                     structures.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE         NAME     DESCRIPTION
*   int               i        Loop indexing variable
*   OptionValuePair * pNodePtr Node pointer used to walk through linked list.
*   OptionValuePair * pTempPtr Node pointer to temporarily point to next node
*                              in linked list while the current node
*                              is being freed. 
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/

void FreeOptionValueList ( OptionValuePair ** pOptionValuePair )
{
   int i ;
   OptionValuePair * pNodePtr = NULL ;
   OptionValuePair * pTempPtr = NULL ;

   if ( ( pOptionValuePair != NULL ) && ( * pOptionValuePair != NULL ) )
   {
      pNodePtr = ( OptionValuePair * ) ListFirst ( 
                                       & ( * pOptionValuePair )->list ) ;

      while ( pNodePtr != NULL )
      {
         pTempPtr = ( OptionValuePair * ) ListNext ( & pNodePtr->node ) ;  

         if ( pNodePtr->option_name != NULL )
         {
            free ( pNodePtr->option_name ) ;
            pNodePtr->option_name = NULL ;
         }

         for ( i = 0 ; i < pNodePtr->number_of_values ; ++ i )
         {
            if ( pNodePtr->value_string [ i ] != NULL )
            {
               free ( pNodePtr->value_string [ i ] ) ;
               pNodePtr->value_string [ i ] = NULL ;
            }
         }
         
         if ( pNodePtr->value_string != NULL )
         {
            free ( pNodePtr->value_string ) ;
            pNodePtr->value_string = NULL ;
         }
 
         free ( pNodePtr ) ;
         pNodePtr = pTempPtr ;
      }

      * pOptionValuePair = NULL ; 
   }
}
