#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "db_purge.h"
#include "DAARadar.h"
#include "GeneralUtil.h"
#include "PurgeDynData.h"


void delete_daa_files(char* table,
		  char* timestring)
{

/*

   delete files from following directories

         daa_grid_dir

*/
   DAARadar 	*daaradarHead;
   DAARadar     *daaradarPtr;
   
   char where[500], DAA_GRID_DIR[100];
   
   int len ;

   long cnt, i ;
   
   
   if (0 == strcmp(table, "daaradar"))
   {
      /* get head of list, and count */
      
      sprintf(where, " WHERE obstime < '%s' AND null_product_flag = 0 ", timestring);
      daaradarHead = GetDAARadar(where);
      
      if (daaradarHead != NULL) 
      {
	 cnt = ListCount(&daaradarHead->list);
	 daaradarPtr = (DAARadar *) ListFirst(&daaradarHead->list);

         len = ( int ) strlen("daa_grid_dir");
         get_apps_defaults("daa_grid_dir",&len,DAA_GRID_DIR,&len);

	 for (i = 0; i < cnt; i++) 
	 {
	    delete_file(DAA_GRID_DIR, daaradarPtr->grid_filename, where);
	    daaradarPtr = (DAARadar *) ListNext(&daaradarPtr->node);
	 }
	 
	 FreeDAARadar(daaradarHead);
	 daaradarHead = (DAARadar*) NULL;
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9d/ohd/whfs/src/db_purge/RCS/delete_daa_files.c,v $";
 static char rcs_id2[] = "$Id: delete_daa_files.c,v 1.3 2012/02/06 17:11:50 pst Exp $";}
/*  ===================================================  */

}			
