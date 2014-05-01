/* This is the main function/call to calculate the local bias based on the p3 way.
   This function calls a series of functions that calculate the local bias and 
   also writes the results into an xmrg.
*/


#include "delete_polygons_show.h"
#include "empe_fieldgen.h"
#include "empe_db_tables.h"
#include "polygon_RFCW.h"
#include "p3.h"

extern process1_struct process_data;
extern void free_gages();
extern int  readgages(const gage_table_struct * ptrGageTable,List * pPolyList );
extern void readstagei(double ** AvgMosaic);
extern void triangulategage();
extern int calibrate_radar();
extern void date_string(void);
extern void  write_p3_xmrg(double ** P3Mosaic, double ** QPEMosaic);
extern int mpe_values(const run_date_struct *,const empe_params_struct *,const radarLoc_table_struct *,const geo_data_struct *, const int);

extern void write_gage_triangles(void);
extern char mystr[100];
/*************************************************************************************/
int runP3LMosaic (	const run_date_struct * pRunDate,
			const empe_params_struct * pMPEParams ,
			const radarLoc_table_struct * pRadarLocTable ,
			const gage_table_struct * ptrGageTableP3,
			const geo_data_struct * ptrGeoData,
                        enum DisplayFieldData radar_display_type,
			double ** P3Mosaic,
			double ** AvgMosaic,
			double ** QPEMosaic
	           )
{
        char file_time_string [ ANSI_YEARSEC_TIME_LEN + 1 ] = {'\0'};
	int return_from_read_gages = -1;
        List PolyList;
        struct tm * pRunTime = NULL;
	extern int readradartriangles_once_for_all_hours;
	int len = 0;
	char filename[150], pdir[150], fname[150];

	
	// The following checks are being put as these fields are supposed to be
	// already calculated at this point.
	if(pRunDate == NULL)
	{
		//fprintf(stderr,"pRunDate is NULL\n");
		sprintf(message,"pRunDate is NULL\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}
	if(pMPEParams == NULL)
	{
		//fprintf(stderr,"pMPEParams is NULL\n");
		sprintf(message,"pMPEParams is NULL\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}
	if(pRadarLocTable == NULL)
	{
		//fprintf(stderr,"pRadarLocTable is NULL\n");
		sprintf(message,"pRadarLocTable is NULL\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}
/*	if(ptrGageTable == NULL)
	{
		//fprintf(stderr,"ptrGageTable is NULL\n");
		sprintf(message,"ptrGageTable is NULL\n");
		printMessage( message);
		return -1;
	}*/
	if(ptrGageTableP3 == NULL)
	{
		//fprintf(stderr,"ptrGageTableP3 is NULL\n");
		sprintf(message,"ptrGageTableP3 is NULL\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}
	if(ptrGeoData== NULL)
	{
		//fprintf(stderr,"ptrGeoData is NULL\n");
		sprintf(message,"ptrGeoData is NULL\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}
	if(QPEMosaic == NULL)
	{
		//fprintf(stderr,"QPEMosaic is NULL\n");
		sprintf(message,"QPEMosaic is NULL\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}
	if(P3Mosaic == NULL || AvgMosaic == NULL )
	{
		sprintf(message,"one of P3Mosaic or AvgMosaic is NULL"
		                " which shouldnt be...so exiting...\n");
		hpe_fieldgen_printMessage( message);
		return -1;
	}

	//set the global p3 pointers to the values obtained from the mpe. These values
	//are "extern"ed from other files to access the data.
	mpe_values(pRunDate,pMPEParams,pRadarLocTable,ptrGeoData,1);

        // Read in the polygons which apply to this product.
        pRunTime = gmtime ( & pRunDate->tRunTime );

        strftime ( file_time_string, ANSI_YEARSEC_TIME_LEN + 1,
                   "%Y%m%d%H", pRunTime );
        /* Since the user is allowed to choose the base radar mosaic,
           we cannot assume that the average radar mosaic is to be 
           used for the p3 lmosaic. */ 
        get_polygons ( radar_display_type, & PolyList, file_time_string );
	
	//the following function has been commented as the function call following
	//this call incorporates gages outside the HRAP grid bin rectangle boundary
	//as per the ABRFC request following the visit to their office	
	//reads the gage info from the gage pointer obtained from the mpe
	//return_from_read_gages = readgages(ptrGageTable, & PolyList);
	
	//read the radar triangles and the hrap grid bin radar mosaicked values.
	if(readradartriangles_once_for_all_hours != 1)
	{
		readradartriangles();
		readradartriangles_once_for_all_hours = 1;
	}
	
	//reads the gage info from the gage pointer obtained from the mpe
	return_from_read_gages = readgages(ptrGageTableP3, & PolyList);

	if(return_from_read_gages == -1)
	{
		return -1;
	}
	if(return_from_read_gages == -2)
	{
		len = strlen("rfcwide_p3lmosaic_dir");
		get_apps_defaults("rfcwide_p3lmosaic_dir",&len,pdir,&len);
		date_string();
		sprintf(filename, "%s%s%sz",pdir,"/P3LMOSAIC",mystr);
		sprintf(fname,"%s %s","rm -rf",filename);

		sprintf(message,"executing command %s\n",fname);	
		system(fname);

		len = strlen("rfcwide_gagetriangles_dir");
		get_apps_defaults("rfcwide_gagetriangles_dir",&len,pdir,&len);
		date_string();
		sprintf(filename, "%s%s%sz",pdir,"/GAGETRIANGLES",mystr);
		sprintf(fname,"%s %s","rm -rf",filename);
			
		sprintf(message,"executing command %s\n",fname);
		system(fname);

		return -1;
	}
	
	//reads the radar mosaics from the corresponding xmrg's.
	readstagei(AvgMosaic);
	
	//calculates the gage triangles using the information of little triangles created
	//from the p3_util library
	triangulategage();

	//function that calculates the p3 lmosaic.
	calibrate_radar();
	
	//write the local bias valuse into the xmrg which is also there as an array
	//for further use in mpe if necessary.
	write_p3_xmrg(P3Mosaic,QPEMosaic);
	
	//this routine writes the gage triangles in a particular(spiderweb) format into a file
	//which is later on read by the hydroview that displays it. in the spiderweb format, there
	//is a center point and some subsequent points that are to be connected from that point
	//around it.
	write_gage_triangles();
	
	free_gages();

	sprintf(message,"exiting runP3LMosaic routine to calculate the p3 local bias\n");
	hpe_fieldgen_printMessage( message );
	
	return 1;
}
