/*******************************************************************************
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Jan 01 2008  Ram Varma         New Code
*
*********************************************************************************/

#include <stdio.h>

#include "disagg.h"
#include <stdlib.h>

int compare(const void *aa, const void *bb)
{
   int *a = (int*) aa;
   int *b = (int*) bb;

   if(*a < *b)
   {
      return -1 ;
   }
   else if (*a > *b)
   {
      return 1;
   }

   return 0;
}

void sort_without_duplicates(int * numbers, int * temp, int array_size, int * cnt)
{
  int i,j, count=0, is_present = 0;
  
  qsort(numbers, array_size, sizeof(int), compare);
  
  //this code eliminates duplicates from numbers and writes it back
  //to the numbers array.
  for(i=0;i<array_size;i++)
  {  
     temp[i] = -1;
  }
  
  for(i=0;i<array_size;i++)
  {  
     for(j=0;j<array_size;j++)
     {
        if(numbers[i] == temp[j])
	{
	   is_present = 1;
	   break;
	}
     }
     if(is_present == 1)
     {
	is_present = 0;
	continue;
     }
     temp[count++] = numbers[i];
  }
  
  *cnt = count;
  for(i=0;i<array_size;i++)
  {  
     numbers[i] = temp[i];
  }

  return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

