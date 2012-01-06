/*******************************************************************************
* FILENAME:            p3_lmosaic.c
*
* Purpose:
* This is the main function/call to calculate the local bias based on the p3 way.
* This function calls a series of functions that calculate the local bias and 
* also writes the results into an xmrg.


* calling function: main calls p3_lmosaic
* functions called: readstagei, readgages, triangulate_gage, calibrate_radar
* write_gage_triangles
*
* input variables
*
* Any of the 3 base radar mosaics which would be either
* avgrmosaic, maxrmosaic, rmosaic.
* 
* output variables
*
* P3 local bias corrected mosaic
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2005   Ram Varma         ABRFC P3 mosaic algorithm 
**
********************************************************************************
*/

#include "delete_polygons_show.h"
#include "mpe_fieldgen.h"
#include "mpe_db_tables.h"
#include "polygon_RFCW.h"
#include "p3.h"

extern process1_struct process_data;
extern char mystr[100];
/*************************************************************************************/

/* This is the main function/call to calculate the local bias based on the p3 way.
   This function calls a series of functions that calculate the local bias and 
   also writes the results into an xmrg.
*/

/***********************************************************************
* Purpose:
* main function call to p3_lmosaic calculation
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Dec 2004     ABRFC             Original code  
*   August 2005  Ram Varma         ABRFC P3 mosaic algorithm 
*   April 2007   P Tilles          added check on return code from 
*                                   readradartriangles
*
***********************************************************************/


int
MPEFieldGen_runP3LMosaic (const run_date_struct * pRunDate,
	      const mpe_params_struct * pMPEParams,
	      const radarLoc_table_struct * pRadarLocTable,
	      const gage_table_struct * ptrGageTableP3,
	      const geo_data_struct * ptrGeoData,
	      enum DisplayFieldData radar_display_type,
	      double **P3Mosaic, double **AvgMosaic, double **QPEMosaic)
{
   char file_time_string[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
   int return_from_read_gages = -1;
   List PolyList;
   struct tm *pRunTime = NULL;
   static int readradartriangles_once_for_all_runs = 0;
   static int readradartri_error;
   int len = 0;
   char filename[150], pdir[150], fname[150];
   int ret = 1;


    //getCurrentTime(currTime);
    //sprintf ( message, "%s = time begin P3LMOSAIC calc\n", currTime );
    //printMessage ( message, logFile );

   // The following checks are being put as these fields are supposed to be
   // already calculated at this point.
   if (pRunDate == NULL)
   {
      //fprintf(stderr,"pRunDate is NULL\n");
      sprintf (message, "pRunDate is NULL\n");
      printMessage (message, logFile);
      ret = -1;
   }
   if (pMPEParams == NULL)
   {
      //fprintf(stderr,"pMPEParams is NULL\n");
      sprintf (message, "pMPEParams is NULL\n");
      printMessage (message, logFile);
      ret = -1;
   }
   if (pRadarLocTable == NULL)
   {
      //fprintf(stderr,"pRadarLocTable is NULL\n");
      sprintf (message, "pRadarLocTable is NULL\n");
      printMessage (message, logFile);
      ret = -1;
   }

   if (ptrGageTableP3 == NULL)
   {
      //fprintf(stderr,"ptrGageTableP3 is NULL\n");
      sprintf (message, "ptrGageTableP3 is NULL\n");
      printMessage (message, logFile);
      ret = -1;
   }
   if (ptrGeoData == NULL)
   {
      //fprintf(stderr,"ptrGeoData is NULL\n");
      sprintf (message, "ptrGeoData is NULL\n");
      printMessage (message, logFile);
      ret = -1;
   }
   if (QPEMosaic == NULL)
   {
      //fprintf(stderr,"QPEMosaic is NULL\n");
      sprintf (message, "QPEMosaic is NULL\n");
      printMessage (message, logFile);
      ret = -1;
   }
   if (P3Mosaic == NULL || AvgMosaic == NULL)
   {
      sprintf (message,
"one of P3Mosaic or AvgMosaic is NULL which shouldnt be...so exiting...\n");
      printMessage (message, logFile);
      ret = -1;
   }

   //set the global p3 pointers to the values obtained from mpe fieldgen. 
   //These values are "extern"ed from other files to access the data.
   MPEFieldGen_mpe_values (pRunDate, pMPEParams, pRadarLocTable, ptrGeoData, 1);

   getCurrentTime ( currTime );
   sprintf ( message, "%s = time begin P3LMOSAIC calc\n", currTime );
   
   // Read in the polygons which apply to this product.
   //pRunTime = gmtime (&pRunDate->tRunTime);

   //strftime (file_time_string, ANSI_YEARSEC_TIME_LEN + 1,
//	     "%Y%m%d%H", pRunTime);
   /* Since the user is allowed to choose the base radar mosaic,
      we cannot assume that the average radar mosaic is to be 
      used for the p3 lmosaic. */
   //get_snow_polygons (&PolyList, file_time_string);

   //the following function has been commented as the function call following
   //this call incorporates gages outside the HRAP grid bin rectangle boundary
   //as per the ABRFC request following the visit to their office  
   //reads the gage info from the gage pointer obtained from the mpe
   //return_from_read_gages = readgages(ptrGageTable, & PolyList);

   //read the radar triangles and the hrap grid bin radar mosaicked values.
   if (readradartriangles_once_for_all_runs != 1)
   {
      readradartri_error = 0;
      ret = MPEFieldGen_readradartriangles ();
      if(ret == -1) readradartri_error = 1;

      readradartriangles_once_for_all_runs = 1;
   }

   /* if ret = -1, means that files in utiltriangles dir not read */

   if(readradartri_error == 1)
   {
      ret = -1;
      return ret;
   }

   //reads the gage info from the gage pointer obtained from the mpe
   return_from_read_gages = MPEFieldGen_readgages (ptrGageTableP3);

   if (return_from_read_gages == -1)
   {
      ret = -1;
      return ret;
   }
   if (return_from_read_gages == -2)
   {
      len = strlen ("rfcwide_p3lmosaic_dir");
      get_apps_defaults ("rfcwide_p3lmosaic_dir", &len, pdir, &len);
      MPEFieldGen_date_string ();
      sprintf (filename, "%s%s%sz", pdir, "/P3LMOSAIC", mystr);
      sprintf (fname, "%s %s", "rm -rf", filename);

      sprintf (message, "executing command %s\n", fname);
      system (fname);

      len = strlen ("rfcwide_gagetriangles_dir");
      get_apps_defaults ("rfcwide_gagetriangles_dir", &len, pdir, &len);
      MPEFieldGen_date_string ();
      sprintf (filename, "%s%s%sz", pdir, "/GAGETRIANGLES", mystr);
      sprintf (fname, "%s %s", "rm -rf", filename);

      sprintf (message, "executing command %s\n", fname);
      system (fname);

      ret = -1;
      return ret;
   }

   //reads the radar mosaics from the corresponding xmrg's.
   MPEFieldGen_readstagei (AvgMosaic);

   //calculates the gage triangles using the information of grid triangles 
   //created from the p3_util library
   MPEFieldGen_triangulategage ();

   //function that calculates the p3 lmosaic.
   MPEFieldGen_calibrate_radar ();

   //write the local bias values into the xmrg which is also there as an array
   //for further use in mpe if necessary.
   MPEFieldGen_write_p3_xmrg (P3Mosaic, QPEMosaic);

   //this routine writes the gage triangles in a particular(spiderweb) 
   //format into a file which is later on read by the mpe_editor that 
   //displays it. in the spiderweb format, there is a center point and 
   //some subsequent points that are to be connected from that point around it.
   MPEFieldGen_write_gage_triangles ();

   MPEFieldGen_free_gages ();


   getCurrentTime ( currTime );
   sprintf ( message, "%s = time end P3LMOSAIC calc\n", currTime );
   printMessage ( message, logFile ); 

   return ret;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/p3_lmosaic.c,v $";
 static char rcs_id2[] = "$Id: p3_lmosaic.c,v 1.1 2007/10/15 12:25:38 dsa Exp $";}
/*  ===================================================  */

}
