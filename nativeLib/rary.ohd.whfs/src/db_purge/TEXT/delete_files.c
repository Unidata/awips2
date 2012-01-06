#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "db_purge.h"
#include "DPARadar.h"
#include "GeneralUtil.h"
#include "PurgeDynData.h"



/***************************************************************
   
   delete_file()
   
   ****************************************************************/


void delete_file(char	* dir_envstr,
		 char	* file,
		 char	* where)
{
   char path[500];
   char grid_dir[300];
   
   strcpy(grid_dir, dir_envstr);
   
   if (NULL != grid_dir)
   {
      strcpy(path, grid_dir);
      strcat(path, "/");
      strcat(path, file);
   }
   
   else 
   {
      printf("ERROR - Env variable %s not set!\n", dir_envstr);
      return;
   }
   
   
   if (0 != unlink(path)) 
   {
      printf("ERROR - Can't delete file %s \n",path);
      printf("   (where clause: %s)\n", where);		
   }
}


/***************************************************************
   
   delete_files()
   
   ****************************************************************/

void delete_files(char* table,
		  char* timestring)
{

/*

   delete files from following directories

         dpa_grid_dir

*/
   DPARadar 	*dparadarHead;
   DPARadar     *dparadarPtr;
   
   char where[500], STAGE1_GRID_DIR[100];
   
   int len ;

   long cnt, i ;
   
   
   if (0 == strcmp(table, "dparadar"))
   {
      /* get head of list, and count */
      
      sprintf(where, " WHERE obstime < '%s' AND maxvald > 0.0 ", timestring);
      dparadarHead = GetDPARadar(where);
      
      if (dparadarHead != NULL) 
      {
	 cnt = ListCount(&dparadarHead->list);
	 dparadarPtr = (DPARadar *) ListFirst(&dparadarHead->list);

         len = ( int ) strlen("dpa_grid_dir");
         get_apps_defaults("dpa_grid_dir",&len,STAGE1_GRID_DIR,&len);

	 for (i = 0; i < cnt; i++) 
	 {
	    delete_file(STAGE1_GRID_DIR, dparadarPtr->grid_filename, where);
	    dparadarPtr = (DPARadar *) ListNext(&dparadarPtr->node);
	 }
	 
	 FreeDPARadar(dparadarHead);
	 dparadarHead = (DPARadar*) NULL;
      }
   }
}			

/***************************************************************
   
   lower_trim()
   
   ****************************************************************/

void lower_trim(char* string)
{
   char* pIn;
   char* pOut;
   
   pIn = pOut = string;
   while(*pIn)
   {
      if (isgraph(*pIn))
      {
	 *pOut = tolower(*pIn);
	 pOut++;
      }
      pIn++;
   }
   
   *pOut = 0;
}

