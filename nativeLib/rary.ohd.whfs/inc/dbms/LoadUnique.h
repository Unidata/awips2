#ifndef LOADUNIQUE_H
#define LOADUNIQUE_H

/* definitions */
#define LOAD_UNIQUE_CONCATENATION_STRING "||"
#define LOAD_UNIQUE_DELIMITER "|"
#define MAX_STRLEN 120 

/* structures */

#include "List.h"

typedef struct
{
   Node  node;
   char  uchar[MAX_STRLEN];
   List  list;
} UniqueList;



/* prototypes */

UniqueList *  LoadUnique(const 	char 	*field,
		         const 	char 	*table,
		         const 	char 	*where,
		    		int	*cnt);

void  FreeUnique(UniqueList *sp);

/* Prototypes of functions used for parsing Unique output. */

char ** ParseUnique ( UniqueList * sp, int * num_fields );
void FreeParseUnique ( char ** pFields );

#endif
