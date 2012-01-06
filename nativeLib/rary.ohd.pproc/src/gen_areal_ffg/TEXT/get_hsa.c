#include <stdio.h>
#include <string.h>
#include "sqlca.h"
#include "gen_areal_ffg.h"
#include "Admin.h"

/*
extern long int SQLCODE;
extern struct sqlca_s sqlca;
*/

void get_hsa(int *ret)

/*

   this function returns the hsa identifier from Admin table             
  
*/

{
   Admin *adminHead, *adminPtr;

   /*-----------------------------------*/
   /*  set up Linked List               */
   /*-----------------------------------*/

   adminHead = GetAdmin("");
   if(SQLCODE != 0 )
   {
     printf("PostgreSQL error %ld attempting select from Admin table\n",
        SQLCODE);

     *ret = 1;
     return;
   }

   if(adminHead)
   {

      adminPtr = (Admin *) ListFirst(&adminHead->list);
      strcpy(hsa, adminPtr->hsa);
      *ret = 0;

   }
   else
   {
      *ret = 2;
   }

}  /*  end get_hsa function  */
