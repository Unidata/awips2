#include "rpf_util.h"     /* function protos */

/*******************************************************
   init_array()
   
   PURPOSE
   Initialize a given number of array elements in an
   integer array to a given value.
   
   ******************************************************/


void init_array(int *intarray,
		int newvalue,
		int num_elem)
{
   int i;
   
   for ( i = 0; i < num_elem; i++ )
      intarray[i] = newvalue;
   return;
}
