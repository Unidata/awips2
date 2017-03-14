
/*******************************************************************************
* FILENAME:            BinarySearch.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          binary_search.c
* DESCRIPTION:         This routine performs a binary search on an ordered
*                      array of data structures.
*                 
*
* ORIGINAL AUTHOR:     Bryon Lawrence
*                      With reference to "Data Structures From Recipes to C"
*                      by Lawrence E. Turner, Jr. for the binary search
*                      algorithm.  Also, this is based on the calling
*                      structure of the qsort routine.
*
* CREATION DATE:       January 2, 2003
* ORGANIZATION:        HSEB/OHD/NWS/NOAA
* MACHINE:             HP-UX / LINUX
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        1/2/2003     Bryon Lawrence    Created
*
********************************************************************************
*/

#include <time.h> /* For size_t declaration. */
#include "BinarySearch.h" /* For binary_search routine prototype and
                             CompareFunction prototype. */ 

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   binary_search
* PURPOSE:       This routine performs a binary search on an ordered array
*                of data structures.  These data structures may be intrinsic
*                or user-defined.  An example of the use of this searching
*                routine follows this introduction.
*
*                Why use a binary search?  For an ordered array of values,
*                a binary search behaves roughly as a O (lg n ) search.  That
*                is, the number of "looks" required to find a specific value
*                is ( log base 2 ) + 1 of the number of items in the array. For 
*                example, an array of 8 items will require a maximum of 4 looks
*                to find a value.  An array of 256 items will require only 9
*                looks to find a value.  An array of 1024 items will require
*                only 11 looks.  Compare this with a linear search through
*                the array which could require up to 1024 looks for a search
*                of a single value, and the benefits of a binary search are
*                astonishing.
*
*                How does the binary search work?  The search starts by 
*                finding the midpoint of the array and comparing the value
*                at this point with the value being searched for.  If the 
*                value being searched for is greater, then the upper half of
*                the array is taken, and the midpoint of it is found and
*                compared to the search value.  If the value being searched
*                for is less, then the lower half of the array is taken, and
*                the midpoint is found and compared to the search value.
*                This process of subdividing the search array into narrower
*                search ranges continues until the value being searched for
*                is found or until the search cannot proceed any more.
*
*                The data structures in the array must be in ascending order
*                by the element the user wishes to search by.  For example, if
*                the array contains N user-defined structures each containing
*                a character and an integer data member, and the user wishes
*                to search for a specific occurence of a character, then the
*                data structures must be ordered by the value of the
*                character data member, with the lowest value character in the
*                0 position of the array and the highest value character
*                in the N-1 position of the array.  A future modification 
*                to this routine may allow binary searching on a descending
*                ordered search key.  (For information about sorting an array
*                of values see the man page on "qsort".)
*
*                The user is responsible for supplying a pointer to a
*                user-defined comparison function.  This comparison function
*                must be of type CompareFunction which is defined in the file 
*                BinarySearch.h as follows:
*
*                typedef int ( * CompareFunction ) ( void * , void * )
*
*                which reads "CompareFunction is a pointer to a function
*                which takes two pointers to void and returns an integer
*                value". 
*
*                This user-defined routine must take the first calling
*                argument to be the value being searched for, and the
*                second calling argument as the value from the array being
*                searched.  The "void *" type allows the user
*                to process any type desired as long as care is taken to cast
*                these pointers before their use.
*
*                The compare routine must return the following values based
*                on the comparison of the search value and the value from 
*                the array being searched:
*
*                Condition                      Return Value
*                =========                      ============
*                search_value > array_value     > 0
*                search_value == array_value    0
*                search_value < array_value     < 0
*
*                The binary_search routine will return a NULL value if 
*                the search fails, the pointer to the array is NULL,
*                the pointer to the search value is NULL,
*                the number of items in the array is zero or less, 
*                the size of an array element is 0 or less, or no
*                comparison function is provided.  Otherwise, the
*                binary_search routine will return a pointer
*                to the element in the array which matches the search value.
*                The returned pointer must be cast to the appropriate 
*                type before being used.
*
*  Example of useage
*
*  The following is a simple example of using the binary_search routine
*  to find an integer value in an array of integers.  This example can
*  be expanded to use complex user-defined data types.
*
*#include <stdio.h>
*#include <stdlib.h>
*
*#include "BinarySearch.h"
*
*int compare_int ( void * number_to_compare , void * number_in_array )
*{
*   int value_to_compare = * ( ( int * ) number_to_compare ) ;
*   int value_in_array = * ( ( int * ) number_in_array ) ; 
*
*   if ( value_to_compare == value_in_array )
*   {
*      return 0 ;
*   }
*   else if ( value_to_compare > value_in_array )
*   {
*      return 1 ;
*   }
*   else
*   {
*      return -1 ;
*   }
*   
*}
*
*int main ( int argc , char ** argv )
*{
*    int integer_array[] = { 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 } ;
*
*
*    int num_items = 10 ;
*    int search ;
*    int * found = NULL ;
*
*    if ( argc != 2 )
*    {
*       return 1 ; 
*    }
*
*    search = atoi ( argv [ 1 ] ) ;
*    found = ( int * ) binary_search ( integer_array ,
*                                      & search ,
*                                      num_items ,
*                                      sizeof ( int ) ,
*                                      compare_int ) ;  
*
*    if ( found == NULL )
*    {
*       fprintf ( stdout , "\nCould not find %d.\n" , search ) ;
*    }
*    else
*    {
*       fprintf ( stdout , "\nThe value is %d.\n" , * found ) ; 
*    }
*
*    return 0 ;
*}
*
*                
*
* ARGUMENTS:
*   TYPE   DATA TYPE       NAME                  DESCRIPTION/UNITS
*   input  void *          pArray                The pointer to the array of 
*                                                data elements to be searched.
*   input  void *          search_value          The value to search for in the
*                                                array. 
*   input  int             num_items             The number of items in the
*                                                array. 
*   input  size_t          size_of_array_element The size of the data type
*                                                contained within the array.
*   input  CompareFunction compare_function      The function to use in 
*                                                comparing the search value
*                                                to each element of the
*                                                array.
*
* RETURNS:
*   DATA TYPE          DESCRIPTION
*   void *             A pointer to the array element which matches the
*                      search value.
*
* APIs UTILIZED:
*   NAME                           DESCRIPTION
*   compare_function               User-supplied comparison routine.
*                                  Must be of type CompareFunction.  The
*                                  first argument is the value being searched
*                                  for.  The second argument is an element from
*                                  the array being searched.  The routine
*                                  must compare the value being searched for
*                                  to the element of the array being searched.
*                                  If the value being searched for is greater
*                                  than the search array value, this routine
*                                  must return a positive (> 0) integer value.
*                                  If the value being searched for is less than
*                                  the search array value, then this routine
*                                  must return a negative (< 0) integer value.
*                                  If the value being searched for is equal to
*                                  the value from the search array, then
*                                  the comparison routine must return 0.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME          DESCRIPTION
*   char *     pCharArray    Points to the passed in array of data elements
*                            to be searched.  Allows for pointer arithmetic
*                            on a byte by byte basis.
*   int        diff          The value returned from the comparison function.
*   int        index         The computed index in the array being searched
*                            based on the size of each array element.
*   int        lower         The lower bound of the subportion of the search
*                            array in which the search value may be.
*   int        mid_point     The current index in the search array being
*                            checked.
*   int        upper         The upper bound of the subportion of the search
*                            array in which the search value may be. 
*
* DATA FILES AND/OR DATABASE:
*   None.
*
* ERROR HANDLING:
*    ERROR CODE              DESCRIPTION
*    NULL		     The search value could not be found.
*                            The search array was NULL.
*           	             The number of items in the search array was 0
*                            or less.
*                            The size of the array element was 0 or less.	
*                            The value to search for was NULL.
*
********************************************************************************
*/

static int result_index = 0;

void * binary_search ( void * pArray ,
                       void * search_value ,
                       int num_items ,
                       size_t size_of_array_element ,
                       CompareFunction compare_function )
{
   char * pCharArray = NULL ;
   int diff ;
   int index ;
   int lower ;
   int mid_point ;
   int upper ;

   if ( pArray == NULL ||
        search_value == NULL ||
        num_items <= 0 ||
        size_of_array_element <= 0 ||
        compare_function == NULL )
   {
      return NULL ;
   }

   /* Cast the pArray to a char * array. This will allow 
      pointer arithmetic. */
   pCharArray = ( char * ) pArray ;
   
   /* Begin the binary search... */
   upper = num_items - 1 ;
   lower = 0 ;

   while ( upper >= lower )
   {

      /*  retrieve the middle point of the array defined by the
          "upper" and "lower" bounds. */
      mid_point = ( lower + upper ) / 2 ;
      result_index = mid_point;
      index = mid_point * size_of_array_element ;

      /* Call the comparison function. */
      diff = compare_function ( search_value ,   pCharArray + index ) ;

      if ( diff == 0 )
      {
         /* A match has been found.  Return the element to the user. */ 
         return pCharArray + index ;
      }
      else if ( diff < 0 )
      {
         upper = mid_point - 1 ;
      }
      else
      {
         lower = mid_point + 1 ;
      }
    }

    return NULL ;
}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    get_result_index
* PURPOSE:        Returns the position of the found item or the
*                 last position searched for the item.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         result_index                The index of the element in the
*                                           search array or the position
*                                           in the array last searched.
*
* APIs UTILIZED:
*  None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
int get_result_index ( )
{
   return result_index;
}
