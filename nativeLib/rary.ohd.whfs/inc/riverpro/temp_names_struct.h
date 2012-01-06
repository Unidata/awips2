/************************************************************************
   temp_names_struct.h
   
   PURPOSE
   Include file for structures on the allowable template names.
   
   NOTES
   ***********************************************************************/

#ifndef TEMP_NAMES_STRUCT_H
#define TEMP_NAMES_STRUCT_H

#include "temp_name_struct.h"


/* this structure contains the template names for the application */

typedef struct 
{
   temp_name_struct	header;
   temp_name_struct     headline;
   temp_name_struct	summary;
   temp_name_struct 	basis;
   temp_name_struct	tabular;
   temp_name_struct	roundup;
   temp_name_struct	impact;
   temp_name_struct	comparison;
   temp_name_struct	cta;
} templatenames_struct;


#endif
