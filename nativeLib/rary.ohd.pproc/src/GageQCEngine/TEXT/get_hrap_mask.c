/*******************************************************************************
* FILENAME:             get_hsa_to_grid_mask
* GENERAL INFORMATION:  Contains the routine to read and buffer
*                       the monthly mean precipitation data from
*                       the precipitation PRISM files.
*   
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        March 16, 2005
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*    DATE         PROGRAMMER        DESCRIPTION/REASON
*    3/16/2006    Bryon Lawrence    Original Coding
********************************************************************************
*/

#include <stdlib.h>
#include <stdio.h>

#include "gageqc_types.h"
#include "gageqc_defs.h"
#include "mpe_log_utils.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
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

int get_hsa_to_grid_mask (  struct hrap_grid * hrap_grid,
                            struct tag tag [ ],
                            int * wfo_all,
                            const char * hsa_fname )
{
   FILE *fp = NULL;
   int i,j,h;
   char kbuf[10000];

   int minhrapi;
   int minhrapj; 
   int maxhrapi;
   int maxhrapj;
   int ghrapi,ghrapj;
   int ii,jj;
   int ier;
   char *p = NULL;
   int gmini;
   int gminj;
   int gmaxi;
   int gmaxj;

   fp = fopen ( hsa_fname, "r");

   if ( fp == NULL )
   {
     logMessage("could not open %s\n",hsa_fname);
     logMessage("defaulting the hsa mask to all 1's\n");

      /* Initialize each grid bin in the HRAP to '1' */
      for(i=0;i<hrap_grid->maxi;i++)
      {
         for(j=0;j<hrap_grid->maxj;j++)
         {
            hrap_grid->owner[i][j] = 1;
   	     }
      }
      
      * wfo_all = 1;
      return( 1 );
   }
   
   wfo_all = 0;

   /* Initialize each grid bin in the HRAP grid to missing. */
   for(i=0;i<hrap_grid->maxi;i++)
   {
      for(j=0;j<hrap_grid->maxj;j++)
      {
          hrap_grid->owner[i][j]=-1;
   	  }
   }

   minhrapi=hrap_grid->hrap_minx;
   minhrapj=hrap_grid->hrap_miny;
   maxhrapi=hrap_grid->hrap_minx + hrap_grid->maxi;
   maxhrapj=hrap_grid->hrap_miny + hrap_grid->maxj;

   p=fgets(kbuf,100,fp);

   /* Initialize the wfo names to empty string. */
   for(i=0;i<20;i++)
   {
      tag[i].wfo[0]=0;
   }
   
   /* Read the header record of the hrap file. */
   ier=sscanf(kbuf,"%d %d %d %d %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n",
                   &gmini,&gminj,&gmaxi,&gmaxj,tag[0].wfo,tag[1].wfo,tag[2].wfo,
                   tag[3].wfo,tag[4].wfo,tag[5].wfo,tag[6].wfo,tag[7].wfo,
                   tag[8].wfo,tag[9].wfo,tag[10].wfo,tag[11].wfo,
                   tag[12].wfo,tag[13].wfo,tag[14].wfo,tag[15].wfo,
                   tag[16].wfo,tag[17].wfo,tag[18].wfo,tag[19].wfo);
      
   for(i=0;i<gmaxi;i++)
   {
     fread(kbuf,sizeof(char),gmaxj*6,fp);
     kbuf[gmaxj*6]=0; 
        
     /* get hrap coord of gridded data */
     ghrapi=gmini+i;
     ii=ghrapi-minhrapi;

     if(ghrapi >= minhrapi && ghrapi < maxhrapi)
     {  
        h=0;
           
        for(j=0;j<gmaxj;j++)
        {
           ghrapj=gminj+j;
           jj=ghrapj-minhrapj;

           if(ghrapj >= minhrapj && ghrapj < maxhrapj)
           {
              hrap_grid->owner[ii][jj]=atoi(&kbuf[h]);
           }
            
           h += 6;

        }

     }
   
   }
   
   fclose(fp);
   fp = NULL;

   return(1);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
