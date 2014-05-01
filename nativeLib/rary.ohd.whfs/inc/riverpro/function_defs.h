/********************************************************************
   function_defs.h
   
   PURPOSE
   Include file for various functions.
   
   NOTES
   
********************************************************************/

#ifndef FUNCTION_DEFS_H
#define FUNCTION_DEFS_H


/* define a set of functions for determining the template condition types */

#define isvar(type)    ((type > BEGIN_VAR   && type < END_VAR)   ? TRUE : FALSE) 
#define islogop(type)  ((type > BEGIN_LOGOP && type < END_LOGOP) ? TRUE : FALSE) 
#define isrelop(type)  ((type > BEGIN_RELOP && type < END_RELOP) ? TRUE : FALSE) 
#define isstrop(type)  ((type > BEGIN_STROP && type < END_STROP) ? TRUE : FALSE) 
#define isstring(type) ((type == RPF_STR    || type == VAR_STR)  ? TRUE : FALSE) 
#define isnumber(type) ((type == RPF_INT    || type == RPF_FLT    || \
                         type == RPF_TIM    || type == VAR_INT  || \
			 type == VAR_FLT    || type == VAR_TIM) ? TRUE : FALSE) 

#define absdiff(x, y, diff) (diff = x > y ? x - y : y - x)

#define setmax(val1, val2, maxval) (maxval = val1 > val2 ? val1 ; val2)

#define vmatch(index, str) strcmp(TEMPLATE_VARIABLES_TABLE[index].name, str) 
   
#endif
