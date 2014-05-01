#include "temp_item_struct.h"

/************************************************************************
   load_tok.c
   (load_template_tokens.c)
   
   PURPOSE
   Load the information into the table for template tokens.
   This controls the tokens used in conditional expressions
   and in the phrases.
		
   NOTES
   
  ***********************************************************************/



/* define the allowable items and their attributes */
const template_token_struct TEMPLATE_TOKENS_TABLE[] = 
   {
   { "("  , LEFT_PAREN},
   { ")"  , RIGHT_PAREN},
   { "GT" , RPF_GT },
   { "GE" , RPF_GE },
   { "EQ" , RPF_EQ },
   { "LE" , RPF_LE },
   { "LT" , RPF_LT },
   { "NE" , RPF_NE },
   { "SEQ", RPF_SEQ},
   { "SNE", RPF_SNE},
   { "AND", RPF_AND},
   { "OR" , RPF_OR },
   
   };
       
/* set the number of items in the template items table */
int NUM_OF_TEMPLATE_TOKENS = 
   (sizeof(TEMPLATE_TOKENS_TABLE) / sizeof(template_token_struct));
       
