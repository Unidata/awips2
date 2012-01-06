#include "gen_areal_ffg.h"
#include "writeContingency.h"
#include "compute_avg_ffg.h"

#include "sqlca.h"
#include "LineSegs.h"
#include "GeoArea.h"
#include "DbmsDefs.h"

#include "time_defs.h"

/*********************************************************************
   process_areas()
   
   PURPOSE
   This routine performs all the processing for the areas
   given the single HRAP grid.
   
   calling routine: main

   ********************************************************************/

void process_areas(int logall_flag,
                     int        xor,
                     int        yor,
                     int        xsize,
                     int        ysize,
                     int        duration)
{
   long	        *rows = NULL;
   long         *beg_cols =NULL;
   long         *end_cols = NULL ;
   int          i;
   int		numrows = 0;
   int		status;
   char 	where_lseg[80], where_geo[80], area_id[LOC_ID_LEN+1], validtime[22];
   char         validt[11];
   LineSegs	*linesegPtr = NULL;
   GeoArea      *geoPtr = NULL;
   GeoArea      *geoHead = NULL;
   float 	max_val, min_val, avg_val, area_covered;
   
   /* extern long int SQLCODE; */

   sprintf(where_geo,"WHERE boundary_type = 'BASIN' AND area_id IN (SELECT area_id FROM linesegs)");

   /*--------------------------------------------------------------*/
   /*  transform validtime from int to  datetime format for use in */
   /*    posting to Contingencyvalue table                         */
   /*--------------------------------------------------------------*/
  
   sprintf(validt,"%d",latest_validtime);
   sprintf(validtime,"%c%c%c%c-%c%c-%c%c %c%c:00:00",validt[0],validt[1],
                                                     validt[2],validt[3],
                                                     validt[4],validt[5],
                                                     validt[6],validt[7],
                                                     validt[8],validt[9]);
   /* printf("ContingencyValue table: validtime set to %s\n",validtime); */

   /*--------------------------*/
   /*  begin loop on basins    */
   /*--------------------------*/
   
   geoHead = GetGeoArea(where_geo);
   if(SQLCODE != 0) 
      printf("PostgreSQL error %ld attempting select in GeoArea table\n", SQLCODE);

   if(geoHead)
   {
      geoPtr = (GeoArea*) ListFirst(&geoHead->list);
      while(geoPtr)
      {

         /* initialize variables */
   
         area_covered = 0.;
         memset(area_id, '\0', LOC_ID_LEN+1);
   
         strcpy(area_id, geoPtr->area_id);
         if(logall_flag) printf("\narea id = %s\n",area_id);

         /* read the HRAP bin coords for the area and
         extract the information from the blob fields. */
   
         sprintf(where_lseg, " WHERE area_id = '%s' ", area_id);
         linesegPtr = GetLineSegs(where_lseg);
   
         if (linesegPtr == NULL)
         {
            printf("LineSeg information not available for %s\n", area_id);
            continue;
         }
         
         numrows = ListCount(&linesegPtr->list);
        
         rows = (long *) malloc (sizeof(long) * numrows);
         if(rows == NULL)
         {
            printf("\nIn 'process_areas':\n"
                   "Could not allocate memory for the rows array\n");
            exit(1);
         }    
         beg_cols = (long *) malloc (sizeof(long) * numrows);
         
         if(beg_cols == NULL)
         {
            printf("\nIn 'process_areas':\n"
                   "Could not allocate memory for the beg_cols array\n");
            exit(1);
         }

         end_cols = (long *) malloc (sizeof(long) * numrows);         
         
         if(end_cols == NULL)
         {
            printf("\nIn 'process_areas':\n"
                   "Could not allocate memory for the end_cols array\n");
            exit(1);
         }
 
         for (i = 0; i < numrows; i++)
         {
            rows[i]      = linesegPtr->hrap_row;
            beg_cols[i]  = linesegPtr->hrap_beg_col;
            end_cols[i]  = linesegPtr->hrap_end_col;
         }
         /* do crude qc check for corrupted linesegs */

         if (numrows > 3000 || numrows <= 0)
         {
            printf("Invalid number of HRAP rows (%d) for %s.\n", numrows, area_id);
         }

         else if (rows[0]     > 10000 || rows[0]     < 0 ||
                  beg_cols[0] > 10000 || beg_cols[0] < 0 ||
                  end_cols[0] > 10000 || end_cols[0] < 0)
         {
            printf("Invalid HRAP info for %s. Check LineSegs blobspace!\n",
                  area_id);
         }
   
         else
         {

            /*-------------------------------------------------------------*/
            /*   compute average FFG value for basin and areal coverage    */
            /*-------------------------------------------------------------*/

            compute_avg_ffg(logall_flag,
                      xor, yor, xsize, ysize,
		      area_id, numrows, rows, beg_cols, end_cols,
		      &avg_val, &max_val, &min_val, &area_covered,
		      &status);

            /*-------------------------------------------------------------*/
            /*   if average FFG value for basin successfully computed      */
            /*   AND minimum areal coverage exceeded, then                 */
            /*    write to ContingencyValue table                          */
            /*-------------------------------------------------------------*/

            if(status == 0)
            {

               if(area_covered > min_coverage)
               {
                   printf("id = %s   value = %.1f inches  area covered = %.2f\n",
                   area_id, avg_val, area_covered);

                   writeContingency(area_id, duration, avg_val, validtime);
               }
               else
               {
                  printf("id = %s -- minimum areal coverage not exceeded\n",area_id);
               }
          
            }
            else if(status == -1)
            {
               printf("id = %s -- outside of site's area\n",area_id);
            }

        }
   
        geoPtr = (GeoArea*) ListNext(&geoPtr->node);
        free(rows);
        free(beg_cols);
        free(end_cols);

     }  /* end while(geoPtr)  */

   }  /* end if((geoHead = GetGeoArea(where_geo))) */
   
   else
   {
      printf("no basins found in GeoArea table\n");
   }
   return;
}
