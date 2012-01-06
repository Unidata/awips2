/************************************************************************
   temp_name_struct.h
   
   PURPOSE
   Include file for structures on the template names.
   
   NOTES
   ***********************************************************************/

#ifndef TEMP_NAME_STRUCT_H
#define TEMP_NAME_STRUCT_H

#include "template_defs.h"


/* this structure contains the generic info related to a template;
   this includes the number of templates and their names */

typedef struct
{
   int number;
   char *name[MAX_TEMPLATES];
} temp_name_struct;


#endif
