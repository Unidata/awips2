#include <stdio.h>
#include <string.h>
#include <sqlca.h>
#include "Admin.h"

/*
extern long int SQLCODE;
extern struct sqlca_s sqlca;
*/

int get_admin_hsa(char hsa[4])

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
     printf("Informix error %ld/%ld attempting select from Admin table\n",
        SQLCODE, sqlca.sqlerrd[1]);

     return 1;
   }

   if(adminHead)
   {

      hsa[3] = '\0';
      adminPtr = (Admin *) ListFirst(&adminHead->list);
      strcpy(hsa, adminPtr->hsa);
      return 0;

   }
   else
   {
      return 2;
   }

}  /*  end get_hsa function  */
