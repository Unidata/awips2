/*******************************************************************************
* FILENAME:            NamedColorSetGroup.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:          addNamedColorUseSet
* DESCRIPTION:         Adds a color use set to a color set group.
*
*
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       September 6, 2006 
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*     1,2           9/6/2006     B. Lawrence       Original Coding
********************************************************************************
*/
#include <stdlib.h>

#include "NamedColorUseSet.h"
#include "NamedColorSetGroup.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   addNamedColorUseSet
* PURPOSE:       Adds a named color use set (a set of colors and thresholds
*                for a given product and duration) to a color set group and
*
* ARGUMENTS:
*   TYPE   DATA TYPE            NAME                 DESCRIPTION/UNITS
*   Input  NamedColorSetGroup * pNamedColorSetGroup  Points to the group
*                                                    of color use sets.
*   Input  NamedColorUseSet *   pNamedColorUseSet    Points to a defined
*                                                    color set to be added
*                                                    to the group of color
*                                                    use sets.
*
* RETURNS:
*   DATA TYPE             NAME                        DESCRIPTION
*   NamedColorSetGroup *  pNamedColorSetGroup         A pointer to the
*                                                     group of color use sets
*                                                     with the new color use
*                                                     set added.
*                                                    
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
P
*   None 
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   Returns a NULL pointer if a memory allocation failure occurred or if
*   a NULL color use set was applied.
********************************************************************************
*/
NamedColorSetGroup * addNamedColorUseSet ( 
                                 NamedColorSetGroup * pNamedColorSetGroup,
                                 NamedColorUseSet * pNamedColorUseSet )
{
   if ( pNamedColorUseSet == NULL )
   {
      return NULL;
   }

   if ( pNamedColorSetGroup == NULL )
   {
      /* Initialize the ColorSetGroup. */
     pNamedColorSetGroup = ( NamedColorSetGroup * ) 
                           malloc ( sizeof ( NamedColorSetGroup ) );

     if ( pNamedColorSetGroup == NULL )
     {
        return NULL;
     }

     pNamedColorSetGroup->set_count = 1;

     pNamedColorSetGroup->color_group_array = pNamedColorUseSet;
   } 
   else
   {
      /* Reallocate the ColorSet Group to be one element bigger. */
      pNamedColorSetGroup->set_count++;  

      pNamedColorSetGroup->color_group_array = ( NamedColorUseSet * )
                      realloc ( pNamedColorSetGroup->color_group_array,
                                pNamedColorSetGroup->set_count *
                                sizeof ( NamedColorUseSet ) ); 
      if ( pNamedColorSetGroup->color_group_array == NULL )
      {
         return NULL;
      }

      pNamedColorSetGroup->color_group_array[pNamedColorSetGroup->set_count-1]=
                          * pNamedColorUseSet;

      free ( pNamedColorUseSet );
   }

   return pNamedColorSetGroup;
}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    FreeColorSetGroupMemory 
* PURPOSE:        Deallocates the dynamic memory used by the group of
*                 color use sets.
*
* ARGUMENTS:
*   TYPE   DATA TYPE            NAME           DESCRIPTION/UNITS
*   Input  NamedColorSetGroup * pColorSetGroup Points to the group of color
*                                              use sets.
* RETURNS:
*   None 
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        i                            Loop index variable
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
void FreeColorSetGroupMemory ( NamedColorSetGroup * pColorSetGroup )
{
   int i;

   if ( pColorSetGroup != NULL )
   {
      for ( i = 0; i < pColorSetGroup->set_count; ++i )
      {

         free ( pColorSetGroup->color_group_array[i].color_use_db_name );
         free ( pColorSetGroup->color_group_array[i].color_use_display_string);
         free ( pColorSetGroup->color_group_array[i].threshold_array.thresholds );
      }

      free ( pColorSetGroup->color_group_array );
      pColorSetGroup->color_group_array = NULL;

      free ( pColorSetGroup );
      pColorSetGroup = NULL;
   }
}
