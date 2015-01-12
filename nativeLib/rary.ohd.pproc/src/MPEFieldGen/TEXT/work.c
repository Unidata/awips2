/*******************************************************************************
* FILENAME:            work.c
*
* Purpose:
* This file has routines to read gages for a given hour from
* the global mpe structure, calculate p3 gage triangles and
* routine to write the p3 mosaic as an xmrg file.
*
* calling function:
* functions called:
*
* input variables
*
* Any of the 3 base radar mosaics which would be either
* avgrmosaic, maxrmosaic, rmosaic.
*
* output variables
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2005   Ram Varma        Original Coding
**
********************************************************************************
*/


/* This file has routines that read the radar mosaics and generates
 * the area to be eliminated in the gage triangle calculation and the same
 * areas would be used to swap values from other mosaics.
*/

#define INTSIZE 10

#include "p3.h"
#include <time.h>
#include "mpe_fieldgen.h"
#include <errno.h>

#include "draw_precip_poly_RFCW.h"
#include "List.h"
#include "polygon_RFCW.h"

extern radarLoc_table_struct *MPEFieldGen_pRadarLocTable_p3;
extern mpe_params_struct *MPEFieldGen_pMPEParams_p3;
extern run_date_struct *MPEFieldGen_pRunDate_p3;
extern geo_data_struct *MPEFieldGen_pGeoData_p3;
extern date_struct curdate;
extern date_struct p3_date;
extern p3_gage_struct gages[];
extern long int numofgages;
extern short int deletefix;
double **maxprecip;
double **precip1;
extern Widget main_draw;
//extern XPoint vertices[];

//extern p3_gage_struct*  radarpts;

static int qpe_best_estimate_p3 = 0;


int MPEFieldGen_month = 0;
int MPEFieldGen_day = 0;
int MPEFieldGen_year = 0;
int MPEFieldGen_hour = 0;
int MPEFieldGen_min = 0;
int MPEFieldGen_sec = 0;
extern int HRAP_X_p3;
extern int HRAP_Y_p3;
extern int HRAP_XOR_p3;
extern int HRAP_YOR_p3;
char mystr[100];

//a utilty function that converts the dates into a specific format.

/***********************************************************************
* Purpose:
* a utilty function that converts the dates into a specific format
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm
*
***********************************************************************/

void
MPEFieldGen_date_string ()
{
   struct tm *mpe_run_time = NULL;
   time_t t;
   size_t sz;

   if (ptrRunDate != NULL)
   {
      t = ptrRunDate->tRunTime;
      mpe_run_time = gmtime ((const time_t *) &t);
   }
   else
   {
      sprintf (message, "couldnt load time from the tRunTime structure\n");
      printMessage (message, logFile);
   }

   sz = strftime (mystr, 100, "%Y%m\%d%H", mpe_run_time);
} /* end MPEFieldGen_date_string */



//this is a routine that reads all the radar mosaics and also reads
//in the polygons created by users. these polygon regions are eliminated
//in the p3_lmosaic calculation.

/***********************************************************************
* Purpose:
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm
*
***********************************************************************/


void
MPEFieldGen_readstagei (double **AvgMosaic)
{
   int i, j, counter;

   precip1 = AvgMosaic;

   counter = 0;
   for (i = 0; i < HRAP_X_p3; i++)
   {
      for (j = 0; j < HRAP_Y_p3; j++)
      {
	 radarpts[counter].value = precip1[j][i];
	 if (radarpts[counter].value < 0)
	 {
	    radarpts[counter].value = 0.0;
	 }
	 counter++;
      }
   }
} /* end MPEFieldGen_readstagei */



/**************************************************************************/

//function that reads the gage values from the gage table structure already
//loaded by initial routines in the mpe fieldgen

/***********************************************************************
* Purpose:
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm
*
***********************************************************************/

int
MPEFieldGen_readgages (const gage_table_struct * ptrGageTablePtr)
{
   bool use_gage;
   float gage_value;
   int gage_count = 0;
   int hrap_x;
   int hrap_y;
   int i = 0;
   int z;
   rubber_poly_data *pPolyNode = NULL;

   if (ptrGageTablePtr == NULL)
   {
      sprintf (message, "oops!....No gage data exists for "
	       "this hour..choose another hour\n");
      printMessage (message, logFile);
      return -1;
   }


   /* Copy the MPE gages to the P3 gage array.  While doing this,
      check for and eliminate gages in a "Set" polygon.  These
      gages should not be included in the P3 analysis. */
   while ((ptrGageTablePtr->ptrGageRecords != NULL) &&
	  (i < ptrGageTablePtr->totalGageNum))
   {
      	 strcpy (gages[gage_count].id,
		 ptrGageTablePtr->ptrGageRecords[i].gageID);
	 gages[gage_count].lat = ptrGageTablePtr->ptrGageRecords[i].latitude;
	 gages[gage_count].lon = ptrGageTablePtr->ptrGageRecords[i].longitude;
	 gages[gage_count].value =
	    ptrGageTablePtr->ptrGageRecords[i].gageValue;
	 gage_count++;

         i++;
   }

   numofgages = ptrGageTablePtr->totalGageNum;

   if (numofgages < 3)
   {
      printf ("---------WARNING---------\n");
      printf
	 ("number of gages is less than 3. triangulated radar mosaic(p3 mosaic)\n"
	 "algorithm needs atleast 3 gages to run. this might be happening if the\n"
	 "algorithm is run on top of the hour. run the algorithm when there is a\n"
	 "possibility of having more gage data for this hour...aborting p3mosaic\n"
	 "calculation...\n");
      printf ("---------WARNING---------\n");
      sprintf (message,"number of gages is less than 3. triangulated radar\n"
      "mosaic(p3 mosaic) algorithm needs atleast 3 gages to run. this might be\n"
      "happening if the algorithm is run on top of the hour. run the algorithm\n"
      "when there is a possibility of having more gage data for this hour...\n"
      "aborting p3mosaic calculation...\n");
      printMessage (message, logFile);
      return -2;
   }

   MPEFieldGen_cleanupgages ();
   return 1;
} /* end MPEFieldGen_readgages */


/***************************************************************************************/


//routine that calls write array function to write the p3_lmosaic into
//an xmrg file also the p3_lmosaic array is preserved globally
//for possible use by mpe.

/***********************************************************************
* Purpose:
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm
*
***********************************************************************/


void
MPEFieldGen_write_p3_xmrg (double **P3Mosaic, double **QPEMosaic)
{
   long i, j;
   int len;
   double temp2;
   char filename[150], pdir[150];
/*   extern int replace_missing;*/
   long int irc;
   const int replace_missing = 0;

   len = strlen ("rfcwide_p3lmosaic_dir");
   get_apps_defaults ("rfcwide_p3lmosaic_dir", &len, pdir, &len);
   MPEFieldGen_date_string ();
   sprintf (filename, "%s%sz", "P3LMOSAIC", mystr);
   for (i = 0; i < HRAP_Y_p3; i++)
   {
      for (j = 0; j < HRAP_X_p3; j++)
      {
	 temp2 = (((radarpts[j * HRAP_Y_p3 + i]).value));
	 P3Mosaic[i][j] = temp2;
	 if (qpe_best_estimate_p3 == 1)
	 {
	    QPEMosaic[i][j] = P3Mosaic[i][j];
	 }
      }
   }

   MPEFieldGen_writeArray (MPEFieldGen_pGeoData_p3, pdir, filename, FACTOR_PRECIP, replace_missing,
	       MPEFieldGen_pMPEParams_p3->user, MPEFieldGen_pRunDate_p3->tRunTime, PROC_FLAG,
	       P3Mosaic, &irc);
} /* MPEFieldGen_write_p3_xmrg */


/***********************************************************************
* Purpose:
* writes the gage triangle coordinates into a text file
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm
*
***********************************************************************/


void
MPEFieldGen_write_gage_triangles ()
{
   FILE *fd = NULL;
   int i, j;
   char pdir[150], filename[150];
   int len = strlen ("rfcwide_gagetriangles_dir");
   len = get_apps_defaults ("rfcwide_gagetriangles_dir", &len, pdir, &len);
   //printf("pdir = %s\n",pdir);
   MPEFieldGen_date_string ();
   sprintf (filename, "%s%s%sz", pdir, "/GAGETRIANGLES", mystr);
   //printf("writing gage triangle coordinates to %s....\n",filename);
   fd = fopen (filename, "w");
   if (fd != NULL)
   {
      for (i = 0; i < numofgages; i++)
      {
	 fwrite (&gages[i].id, (9 * sizeof (char)), 1, fd);
	 fwrite (&gages[i].n_contig, sizeof (long), 1, fd);
	 fwrite (&gages[i].lat, sizeof (double), 1, fd);
	 fwrite (&gages[i].lon, sizeof (double), 1, fd);
	 for (j = 0; j < gages[i].n_contig; j++)
	 {
	    fwrite (&gages[gages[i].contig[j]].lat, sizeof (double), 1, fd);
	    fwrite (&gages[gages[i].contig[j]].lon, sizeof (double), 1, fd);
	 }
      }
      fclose (fd);
   }
   else
   {
      sprintf (message, "error opening file %s but continuing..\n", filename);
      printMessage (message, logFile);
   }
} /* MPEFieldGen_write_gage_triangles */


void
MPEFieldGen_set_best_estimate_p3 ()
{
   qpe_best_estimate_p3 = 1;
}

void
MPEFieldGen_unset_best_estimate_p3 ()
{
   qpe_best_estimate_p3 = 0;
}

//this function removes all the missing gages from the gage array.

/***********************************************************************
* Purpose:
* this function removes all the missing gages from the gage array
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm
*
***********************************************************************/


void
MPEFieldGen_cleanupgages ()
{
   /*  This function removes all "-901.0" or missing gages from the
      gage array, so that they are not used in the calculations for
      P1  */

   int i, j;

   for (i = 0; i < numofgages; i++)
   {
      if (gages[i].value == MISSING_VALUE)
      {
	 //printf("deleting %s\n", gages[i].id);
	 for (j = i + 1; j < numofgages; j++)
	 {
	    gages[j - 1] = gages[j];
	 }
	 numofgages--;
	 i--;
      }
   }

} /* end MPEFieldGen_cleanupgages */

/*********************************************************************************/
