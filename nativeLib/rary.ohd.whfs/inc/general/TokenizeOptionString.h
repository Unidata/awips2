
/*******************************************************************************
* FILENAME:            tokenize_option_string.h
* DESCRIPTION:         Contains the prototype for the tokenize_option_string
*                      routine.  Contains the definition of the
*                      OptionValuePair structure.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       September 22, 2003
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX, Red Hat Linux
* MODIFICATION HISTORY:
*     DATE         PROGRAMMER        DESCRIPTION/REASON
*     9/22/2003    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef TOKENIZE_OPTION_STRING_H
#define TOKENIZE_OPTION_STRING_H

#include "List.h"

#define TOKENIZE_OPTION_OK            0
#define TOKENIZE_OPTION_MALLOC_ERROR  1
#define TOKENIZE_OPTION_MISSING_EQUAL 2
#define TOKENIZE_OPTION_NULL_ARGUMENT 3

typedef struct OptionValuePair {

                  Node node ;

                  char * option_name ;
                  char ** value_string ;
                  int number_of_values ;

                  List list ; 

               } OptionValuePair ;

int TokenizeOptionString ( const char * option_string ,
                           OptionValuePair ** pOptionValuePair ) ;
void FreeOptionValueList ( OptionValuePair ** pOptionValuePair ) ;

#endif /* #ifndef TOKENIZE_OPTION_STRING_H */
