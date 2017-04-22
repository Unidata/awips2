#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "DbmsDefs.h"
#include "GeneralUtil.h"
#include "Location.h"
#include "geoutil.h"
#include "db_purge.h"
#include "BinarySearch.h"

static int compare_char ( void * char_to_compare , void * char_in_array )
{
   char * value_to_compare =  ( char * ) char_to_compare ;
   lochsa * value_in_array =  ( lochsa * ) char_in_array ; 

   return(strcmp(value_to_compare, value_in_array->lid));
   
}

void get_location_hsa(char lid[LOC_ID_LEN+1], char hsa[HYD_SERV_LEN+1], int *ret)

{

/*

   this function retrieves the hsa field value for a given lid

   if first call, then linked list is created and lid and hsa are read
   from Location table into structure

   nloc = number of records in Location table

   calling routines: db_purge (main)

*/

      static int nloc, first = 1;

      Location *locHead = NULL;
      Location *locPtr = NULL;

      lochsa *found;
      static lochsa *ptloc;
      static int size_of_hsa;

      char llid[LOC_ID_LEN+1];
      int i;

      *ret = 99;
       hsa[HYD_SERV_LEN] = '\0';
       lid[LOC_ID_LEN] = '\0';
       llid[LOC_ID_LEN] = '\0';

       strcpy(llid, lid);
       strip_tblanks(llid);

/*----------------------------------------------*/
/*  if first call, then set up linked list      */
/*   and read values into structure             */
/*----------------------------------------------*/

if(first)
{

   first = 0;

   size_of_hsa = HYD_SERV_LEN+1;

   locHead = GetLocation(" ORDER BY lid ");
   nloc = ListCount(&locHead->list);

   ptloc = malloc(nloc * sizeof(lochsa));

   /*----------------------------------------------*/
   /*  set up pointer to head of linked list       */
   /*----------------------------------------------*/

   if(locHead != NULL)
      locPtr = (Location*) ListFirst(&locHead->list);
   else
   {
      locPtr = NULL;
      *ret = 98;
      return;
   }

   /*--------------------------------------------------*/
   /*  populate lochsa structure with lid,hsa values   */
   /*  strip off trailing blanks from lid              */
   /*--------------------------------------------------*/

   i = 0;
   while(locPtr)
   {

      strcpy(ptloc[i].lid , locPtr->lid);
      strcpy(ptloc[i].hsa , locPtr->hsa);

      i++;
      locPtr = (Location*) ListNext(&locPtr->node);
   }
   
   FreeLocation(locHead);

}

/*--------------------------------------------------------*/
/*  compare lid input to this routine with array values   */
/*  if a match is found, then return hsa                  */
/*--------------------------------------------------------*/

found = ( lochsa * ) binary_search ( ptloc ,
                                     llid ,
                                     nloc ,
                                     sizeof(lochsa) ,
                                     compare_char);
                                  
if( found != NULL)
   strcpy(hsa, found->hsa);

else

   /*-----------------------------------------------------------------*/
   /*   if no match found, then set hsa = 3 blanks and return         */
   /*-----------------------------------------------------------------*/

strcpy(hsa,"   ");

}
