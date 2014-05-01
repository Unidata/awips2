/**********************************************************************
 * readGeoData ( )
 * 
 * This function reads geographic data. 
 *********************************************************************/

#include "dqc_preproc_setup.h"

extern HydroStatus read_mpe_coordinate_file ( int * xor, int * yor, int * maxx,
                                    int * maxy ); 

void qdc_preproc_setup_readGeoData()
{
	/*
	 * allocate memory for geo struct data.
	 */
	ptrGeoData = (geo_data_struct *)malloc(sizeof(geo_data_struct)); 
	if(ptrGeoData == NULL)
	{
		fprintf ( stderr , "ERROR: memory allocation failure"
					" in main function.\n\tProgram exit.\n") ;
		exit(-1);
	}

    int xor, yor, maxx, maxy;
    HydroStatus hydroStatus ;

    hydroStatus = read_mpe_coordinate_file(&xor, &yor, &maxx, &maxy);
    if(hydroStatus == HydroStatus_OK)
    {
        ptrGeoData->hrap_x = xor;
        ptrGeoData->hrap_y = yor;
        ptrGeoData->num_cols = maxx;
        ptrGeoData->num_rows = maxy;
    }
    else
    {
        fprintf ( stderr , "ERROR: loading geo data failure.\n") ;
    }
}

