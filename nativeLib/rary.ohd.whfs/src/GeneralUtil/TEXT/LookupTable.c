/*******************************************************************************
* FILENAME:            LookupTable.c
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 10, 2005
* ORGANIZATION:        OHD-11, HSEB, WHFS
* MACHINE:             Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/10/2005   Bryon Lawrence    Original Coding 
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "BinarySearch.h"
#include "LookupTable.h"

static char message [ LookupMessageLength ] = {'\0'};


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   compare_keys
* PURPOSE:       The comparison routine used by the binary search for 
*                comparing the user-supplied search key with the search
*                key contained within each row of the lookup table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE  NAME               DESCRIPTION/UNITS
*   Input  void *     key_to_compare     The key to search for.
*   Input  void *     key_in_array       The key in the array currently being
*                                        inspected.
*
* RETURNS:
*   DATA TYPE         DESCRIPTION
*   int               This value indicates whether the key being compared to
*                     the array element key is lexigraphically larger
*                     (value > 0) equal (value == 0) or smaller (value < 0). 
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE            NAME             DESCRIPTION
*   char *               value_to_compare The key to search for casted
*                                         from void *.
*   int                  status           Contains the result of the string
*                                         comparison.
*   LookupTableRecord  * value_in_array   The key currently being inspected
*                                         in the array.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
*
********************************************************************************
*/
static int compare_keys ( void * key_to_compare, void * key_in_array )
{
   char * value_to_compare = ( char * ) key_to_compare;
   LookupTableRecord  * value_in_array = ( LookupTableRecord * ) key_in_array;
   int status;

   status = strcmp ( value_to_compare, value_in_array->key );

   return status;
}

/*******************************************************************************
* MODULE NUMBER:  2
* MODULE NAME:    GetItemIndex
* PURPOSE:        Returns the index in the lookup table of the key
*                 being searched for.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME     DESCRIPTION/UNITS
*   Input  LookupTable *  pTable   The lookup table.
*   Input  LookupTableKey key      The key to find the index for.
*   Input  int *          index    The index in the lookup table array
*                                  of the key.
*
* RETURNS:
*   DATA TYPE       DESCRIPTION
*   int             Has value LookupSuccess if the key was found.
*                   Has value LookupFailure if the key was not found.
*
* APIs UTILIZED:
*   NAME              HEADER FILE      DESCRIPTION
*   binary_search     BinarySearch.h   Performs a binary search on an
*                                      array of ascended ordered elements. 
*   get_result_index  BinarySearch.h   Returns the index of the binary search
*                                      element.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE          NAME      DESCRIPTION
*   int                status    Tracks the return status (LookupSuccess,
*                                LookupFailure) of this routine.
*   LookupTableRecord  found     Indicates whether or not the key was found
*                                by binary search.
*   LookupTableRecord  pRecord   Points to the lookup table search array.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE             DESCRIPTION
*    LookupSuccess          The function was successul.  The key and its
*                           index were found.
*    LookupFailure          The function failed.  The key and its index were
*                           not found.
********************************************************************************
*/
static int GetItemIndex ( const LookupTable * pTable, 
                          const LookupTableKey key, 
                          int * index )
{
   int status = LookupFailure;
   LookupTableRecord * found = NULL;
   LookupTableRecord * pRecord = NULL;
   *index = -1;

   if ( pTable != NULL )
   {
      pRecord = pTable->table; 
      
      /* If there is a lookup table and it as 1 or more elements, 
         look for the key. */
      if ( ( pRecord != NULL ) && ( pTable->num_elements_filled > 0 ) )
      {
         found = ( LookupTableRecord * )
                          binary_search ( pRecord,
                                          key,
                                          pTable->num_elements_filled,
                                          sizeof ( LookupTableRecord ),
                                          compare_keys );

         * index = get_result_index( );

         if ( found != NULL )
         {
            status = LookupSuccess; 
         } 
         else
         {
            sprintf ( message, "Key %s does not exist.", key );
         }
      }
   }
   else
   {
      sprintf ( message, "A NULL argument was passed to this function." );
   }

   return status;
}

/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   InitLookupTable
* PURPOSE:       Initializes the elements of the lookup table. Allocates
*                memory for the lookup table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME           DESCRIPTION/UNITS
*   Input  int           num_elements   The number of elements to create in
*                                       the lookup table.
*   Input  int           expansion_size The number of elements to increase
*                                       the lookup table's size by.
*   I/O    LookupTable * pTable         The lookup table.
*   Input  LookupCleanup cleanup        The name of the routine to use to
*                                       free memory used by each entry in the
*                                       lookup table.  Pass NULL if not
*                                       supplying any function.
*
* RETURNS:
*   DATA TYPE   NAME     DESCRIPTION
*   int         status   LookupSuccess - Initialization was completed.
*                        LookupFailure - Initialization was not completed.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME      DESCRIPTION
*   int        status    Contains the return status of this routine.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE          DESCRIPTION
*    LookupSuccess       Table initialization was complete.
*    LookupFailure       Table initialization was incomplete.  Either
*                        an argument was NULL or memory allocation failed.
********************************************************************************
*/
int InitLookupTable ( int num_elements,
                      int expansion_size,
                      LookupTable * pTable,
                      LookupCleanup cleanup )
{  
   register int status = LookupSuccess;

   memset ( message, '\0', LookupMessageLength );

   if ( pTable != NULL )
   {
      pTable->cleanup = cleanup;

      if ( num_elements <= 0 )
      {
         num_elements = DEFAULT_LOOKUP_TABLE_NUM_ELEMENTS;
      }
 
      if ( expansion_size <= 0 )
      {
         expansion_size = DEFAULT_LOOKUP_TABLE_EXPANSION;
      }

      /* Initialize number of table elements and the expansion increment. */
      pTable->total_elements = num_elements;
      pTable->num_elements_filled = 0;
      pTable->num_elements_to_increment_by = expansion_size; 
  
      pTable->table = ( LookupTableRecord * ) 
                      malloc ( num_elements * sizeof ( LookupTableRecord ) );

      if ( pTable->table == NULL )
      {
         sprintf ( message, "Could not allocate memory for lookup table.");
         status = LookupFailure;
      }

      memset ( pTable->table, 0, num_elements * sizeof ( LookupTableRecord ) );
   }
   else
   {
      sprintf ( message, "Table argument is NULL.");
      status = LookupFailure;
   }

   return status;
}


/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   DeleteLookupTable
* PURPOSE:       Deallocate the memory used by the lookup table.
*                Sets the total_elements and num_elements_filled
*                to 0.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME        DESCRIPTION/UNITS
*   Input  LookupTable * pTable      The lookup table.
*
* RETURNS:
*   DATA TYPE   NAME                 DESCRIPTION
*   int         status               Indicates the success or failure of
*                                    this routine.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME                  DESCRIPTION
*   int        i                     Loop index.
*   int        status                The return status of this routine.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                       DESCRIPTION
*    LookupSuccess                    This routine successfully freed the
*                                     resources used by the lookup table. 
*    LookupFailure                    This routine could not free the 
*                                     resources used by the lookup table.
********************************************************************************
*/
int DeleteLookupTable ( LookupTable * pTable )
{
   int i;
   int status = LookupSuccess;
   
   memset ( message, '\0', LookupMessageLength );

   if ( pTable != NULL )
   {

      /* Free the memory used by the search keys in the lookup table. */

      if ( pTable->table != NULL )
      {
         for ( i = 0; i < pTable->num_elements_filled; ++i )
         {
            if ( pTable->table[i].key != NULL )
            {
               free ( pTable->table[i].key );
               pTable->table[i].key = NULL;
            }

            if ( pTable->table[i].entry != NULL )
            {
               if ( pTable->cleanup != NULL )
               {
                  pTable->cleanup ( pTable->table[i].entry );
                  pTable->table[i].entry = NULL;
               }
            }

         }

         free ( pTable->table );
         pTable->table = NULL;
      }

      pTable->total_elements = 0;
      pTable->num_elements_filled = 0;
      pTable->num_elements_to_increment_by = 0;
   }
   else
   {
      sprintf ( message, "The table argument is NULL.");
      status = LookupFailure;
   }

   return status;
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   DeleteTableEntry
* PURPOSE:       Removes an entry from the lookup table based on the user
*                specified key.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME      DESCRIPTION/UNITS
*   Input  LookupTable *  pTable    The lookup table.
*   Input  LookupTableKey key       The key to be searched for.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME              HEADER FILE           DESCRIPTION
*   GetItemIndex      (static)              Returns the position in the 
*                                           lookup table array of 
*                                           the item to be deleted.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME       DESCRIPTION
*   int        i          Loop index          
*   int        index      The position of the key and entry to be deleted
*                         in the lookup table.
*   int        status     The result of trying to find the key in the lookup
*                         table.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
inline void DeleteTableEntry ( LookupTable * pTable,
                               const LookupTableKey key )
{
   int i;
   int index;
   int status;

   if ( pTable != NULL )
   {
      status = GetItemIndex ( pTable, key, &index );

      if ( status == LookupSuccess )
      {
         /* Free the memory used by the key. */
         if ( pTable->table[index].key != NULL )
         {
            free (  pTable->table[index].key );
            pTable->table[index].key = NULL;
         }

         /* Free the memory used by the entry if the user
            provided a cleanup function. */
         if ( ( pTable->table[index].entry != NULL ) &&
              ( pTable->cleanup != NULL ) )
         {
            pTable->cleanup ( pTable->table[index].entry );
            pTable->table[index].entry = NULL;
         }
         
         for ( i = index; i < pTable->num_elements_filled - 1; ++i )
         {
            pTable->table[i].key = pTable->table [i + 1].key;
            pTable->table[i].entry = pTable->table [i + 1].entry;
         }

         pTable->num_elements_filled--;
         
      }
   }
}

/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   GetTableKeys
* PURPOSE:       Returns an array of all of the keys in the lookup table.
*                It is the user's responsibility to deallocate the memory
*                used by the returned by the pKeys array.
*
* ARGUMENTS:  
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input  LookupTable * pTable    The lookup table.
*   I/O    int         * pStatus   Indicates the success or failure of this
*                                  routine.
*
* RETURNS:
*   DATA TYPE        NAME     DESCRIPTION
*   LookupTableKey * pKeys    An array containing all of the keys
*                             (in ascending order) from the lookup table.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE        NAME     DESCRIPTION
*   int              i        Loop index
*   LookupTableKey * pKeys    Dynamically allocated array of 
*                             keys in the same order as in the lookup table
*                             (in order ascending).
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE               DESCRIPTION
*    LookupSuccess            The routine worked.
*    LookupFailure            The routine failed (NULL argument or
*                             dynamic memory allocation failure)
*
********************************************************************************
*/
LookupTableKey * GetTableKeys ( const LookupTable * pTable, 
                                int * pStatus )
{
   int i;
   LookupTableKey * pKeys = NULL;
   
   memset ( message, '\0', LookupMessageLength );
   *pStatus = LookupSuccess;

   if ( ( pTable != NULL ) && ( pStatus != NULL ) )
   {
      pKeys = ( LookupTableKey * ) 
                                  malloc ( pTable->num_elements_filled *
                                  sizeof ( LookupTableKey ) );

      if ( pKeys == NULL )
      {
         * pStatus = LookupFailure;
         sprintf ( message, "Could not allocate memory for array of "
                            "lookup table Keys." );
      }
      else
      {
         memset ( pKeys, 0, pTable->num_elements_filled *
                            sizeof ( LookupTableKey ) );

         for ( i = 0; i < pTable->num_elements_filled; ++i )
         {
            pKeys [ i ] = pTable->table[ i ].key;
         }
      }
   }
   else
   {
      * pStatus = LookupFailure;
      sprintf ( message, "A NULL argument was passed into this function." );
   
   }

   return pKeys;
}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   GetTableEntryCount
* PURPOSE:       Retrieves the number of key,entry pairs stored in the
*                lookup table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME               DESCRIPTION/UNITS
*   Input  LookupTable * pTable             The lookup table. 
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         count                       The number of 
*                                           key/entry pairs in the
*                                           lookup table. 
*
* APIs UTILIZED:
*  None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   int        count                        The number of key/entry pairs in
*                                           the lookup table.
* DATA FILES AND/OR DATABASE:
*  None
*
* ERROR HANDLING:
*  None
********************************************************************************
*/
int GetTableEntryCount ( const LookupTable * pTable )
{
   int count;

   if ( pTable != NULL )
   {
      count = pTable->num_elements_filled;   
   }
   else
   {
      count = 0;
   }

   return count;
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   GetTableEntry
* PURPOSE:       Returns the entry in the lookup table associated with the
*                user-supplied key.
*
* ARGUMENTS:
*   TYPE   DATA TYPE      NAME     DESCRIPTION/UNITS
*   Input  LookupTable *  pTable   The lookup table.
*   Input  LookupTableKey key      The key the user is looking for.
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   void *      pEntry                      The entry associated with the key. 
*
* APIs UTILIZED:
*   NAME                   HEADER FILE      DESCRIPTION
*   GetItemIndex           (static)         Returns the index of a key
*                                           in the lookup table array.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME                         DESCRIPTION
*   int        index                        The index of the key/entry pair
*                                           in the lookup table array.
*   int        status                       The status of the attempt to
*                                           find the key/entry pair's index. 
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   Returns NULL if the Entry could not be found.
*
********************************************************************************
*/
inline void * GetTableEntry ( const LookupTable * pTable,
                              const LookupTableKey key )
{
   int index;
   int status;

   if ( pTable != NULL )
   {
      status = GetItemIndex ( pTable, key, &index );

      if ( status == LookupSuccess )
      {
         return pTable->table[index].entry;
      }
   }

   return NULL;
}

/*******************************************************************************
* MODULE NUMBER:  9
* MODULE NAME:   GetTableEntries
* PURPOSE:       Build an array of entries from the table.  This array
*                must be deallocated by the caller of this routine
*                when done.       
*
* ARGUMENTS:
*   TYPE   DATA TYPE     NAME      DESCRIPTION/UNITS
*   Input  LookupTable * pLookup   The lookup table.
*   Input  int         * pStatus   The success or failure of this routine
*                                  to create the array of entries.
*
* RETURNS:
*   DATA TYPE          NAME        DESCRIPTION
*   LookupTableEntry * pEntry      The array of entries ... must be
*                                  deallocated by the caller of this routine.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE          NAME        DESCRIPTION
*   int                i           Loop index.
*   LookupTableEntry * pEntries    Array of lookup table entries...The caller
*                                  of this routine must deallocate this
*                                  when done.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                     DESCRIPTION
*    LookupSuccess                  This routine worked.
*    LookupFailure                  The routine failed (due to a NULL
*                                   argument or a memory allocation failure)
*
********************************************************************************
*/
LookupTableEntry * GetTableEntries ( const LookupTable * pTable, int * pStatus )
{
   int i;
   LookupTableEntry * pEntries = NULL;
   
   memset ( message, '\0', LookupMessageLength );
   *pStatus = LookupSuccess;

   if ( ( pTable != NULL ) && ( pStatus != NULL ) )
   {
      pEntries = ( LookupTableEntry * ) 
                                      malloc ( pTable->num_elements_filled *
                                      sizeof ( LookupTableEntry ) );

      if ( pEntries == NULL )
      {
         * pStatus = LookupFailure;
         sprintf ( message, "Could not allocate memory for array of "
                            "lookup table entries." );
      }
      else
      {
         memset ( pEntries, 0, pTable->num_elements_filled *
                            sizeof ( LookupTableEntry ) );

         for ( i = 0; i < pTable->num_elements_filled; ++i )
         {
            pEntries [ i ] = pTable->table [ i ].entry;
         }
      }
   }
   else
   {
      * pStatus = LookupFailure;
      sprintf ( message, "A NULL argument was passed into this function." );
   
   }

   return pEntries;
}

/*******************************************************************************
* MODULE NUMBER: 10
* MODULE NAME:   AddTableEntry
* PURPOSE:       Inserts a new key/entry pair into the lookup table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE        NAME        DESCRIPTION/UNITS
*   I/O    LookupTable *    pTable      The lookup table.
*   Input  LookupTableKey   key         The user-supplied key.
*   Input  LookupTableEntry entry       The user-supplied entry.
*
* RETURNS:
*   DATA TYPE   NAME        DESCRIPTION
*   int         status      Indicates routine's exit state.
*
* APIs UTILIZED:
*   NAME         HEADER FILE            DESCRIPTION
*   GetItemIndex (static in this file)  Returns the index of a key in the
*                                       lookup array.  If the key doesn't
*                                       exist, then the position where it
*                                       should exist is returned.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE           NAME      DESCRIPTION
*   int                 diff      The return value of strcmp.
*   int                 found     Indicates success of binary search to
*                                 find key in the lookup array.
*   int                 i         Loop index variable.
*   int                 index     The position to insert the key/entry pair
*                                 in the lookup array.
*   int                 length    The length of the key.
*   int                 status    The return state of this function.
*   LookupTableRecord * pRecord   The array of key/entry pairs in the
*                                 lookup table.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*    LookupSuccess                          Key/entry pair inserted into
*                                           lookup table.
*    LookupFailure                          Key/entry pair not inserted into
*                                           lookup table ... check message.
*
********************************************************************************
*/

inline int AddTableEntry ( LookupTable * pTable,
                           const LookupTableKey key,
                           const LookupTableEntry entry )
{
   int diff;
   int found ;
   int i;
   int index = 0;
   int length;
   int status = LookupFailure;
   LookupTableRecord * pRecord = NULL;

   memset ( message, '\0', LookupMessageLength );

   if ( ( pTable != NULL ) && ( key != NULL ) && ( entry != NULL ) )
   {
      pRecord = pTable->table;

      if ( pRecord != NULL ) 
      {
         if ( pTable->num_elements_filled > 0 )
         {
            found = GetItemIndex ( pTable, key, &index );

            if ( found == LookupSuccess )
            {
               sprintf ( message, "Key %s already exists in the lookup table.",
                         key ); 
               return status;
            }
            else if ( index < 0 )
            {
               sprintf ( message, "Error finding key position in lookup "
                                  "table." );
               return status;
            }

            diff = strcmp ( key, pTable->table[index].key );    

            if ( diff > 0 ) index++ ;
         }

         /* Insert the new entry. */
         pTable->num_elements_filled++;

         if ( pTable->num_elements_filled > pTable->total_elements )
         {
            pTable->total_elements += pTable->num_elements_to_increment_by;
            
            pTable->table = ( LookupTableRecord * ) realloc ( pTable->table, 
                                                 pTable->total_elements *
                                                 sizeof ( LookupTableRecord ));

            if ( pTable->table == NULL )
            {
               sprintf ( message, "Could not reallocate memory for "
                                  "lookup table." );
               return status;
            }

            memset ( & pTable->table [ pTable->num_elements_filled ], 0,
                     (pTable->total_elements - pTable->num_elements_filled) *
                     sizeof ( LookupTableRecord ) ); 
         }

         for ( i = pTable->num_elements_filled - 1; i > index; --i)
         {
            pTable->table [ i ].key = pTable->table [ i - 1 ].key;
            pTable->table [ i ].entry = pTable->table [ i - 1 ].entry;
         }

         length = strlen ( key );
         length++;
        
         pTable->table [ index ].key = ( LookupTableKey ) 
                                       malloc ( length * sizeof ( char ) ); 
 
         if ( pTable->table[index].key != NULL )
         {
            strcpy ( pTable->table[index].key, key );
            pTable->table[index].entry = entry;
            status = LookupSuccess;
         }
         else
         {
            sprintf ( message, "Could not allocate memory for key %s.", key );
         }
      }
      else
      {
         sprintf ( message, "Lookup table contains NULL record array." );
      }
   }
   else
   {
      sprintf ( message, "NULL argument passed to this function." );
   }

    return status;
}

/*******************************************************************************
* MODULE NUMBER: 11
* MODULE NAME:   GetLookupMessage
* PURPOSE:       Returns the error message generated by a call to a routine.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE     NAME            DESCRIPTION
*   const char *  message         The diagnostic.
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS:
*   None
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

const char * GetLookupMessage ( )
{
   return message;
}
