/* This function takes in run date structure, mpe parameter structure, radar location structure, and  	 
   geo data structure as 4 arguements and also the mode as the fifth arguement. The mode indicates
   weather this function is acting as a getter or a setter. The main p3_lmosaic routine gets the 
   4 values from the caller and sets the global variables for p3 use. only p3_lmosaic routine can 
   set the global values. all the other routiunes use this function as a getter and retrieve the 
   values for their use.
*/


#include "mpe_fieldgen.h"
#include "mpe_params.h"
#include "mpe_db_tables.h"

//structure in the mpe that has all the application run date info
run_date_struct * MPEFieldGen_pRunDate_p3 = NULL;

//structure that has rfc name, db_name,os,user,and many other fields. it is declared in the 
//mpe_params.h
mpe_params_struct * MPEFieldGen_pMPEParams_p3 = NULL;

//this has all the radar information like the location,latitude,longitude etc. it is declared
//in the mpe_db_tables.h
radarLoc_table_struct * MPEFieldGen_pRadarLocTable_p3 = NULL;

//this structure has the rfc xor,yor coordinates relative to the national grid and the number
//of rows and columns
geo_data_struct * MPEFieldGen_pGeoData_p3 = NULL;

#define SET 1
#define GET 0

int HRAP_Y_p3;
int HRAP_X_p3;
int HRAP_XOR_p3;
int HRAP_YOR_p3;

//int mpe_values(run_date_struct * ptrRunDate,mpe_params_struct * pMPEParams,radarLoc_table_struct * pRadarLocTable,geo_data_struct * ptrGeoData,int set_or_get);


int MPEFieldGen_mpe_values(run_date_struct * ptrRunDate,mpe_params_struct * pMPEParams,radarLoc_table_struct * pRadarLocTable,geo_data_struct * ptrGeoData,int set_or_get)
{
	//setter -> sets the global pointer values declared above.
	if(set_or_get == SET)
	{
		if(ptrRunDate != NULL)
		{
			MPEFieldGen_pRunDate_p3 = ptrRunDate;
		}
		if(pMPEParams != NULL)
		{
			MPEFieldGen_pMPEParams_p3 = pMPEParams;
		}
		if(pRadarLocTable != NULL)
		{
			MPEFieldGen_pRadarLocTable_p3 = pRadarLocTable;
		}
		if(ptrGeoData != NULL)
		{
			MPEFieldGen_pGeoData_p3 = ptrGeoData;
			HRAP_X_p3 = MPEFieldGen_pGeoData_p3->num_cols;
			HRAP_Y_p3 = MPEFieldGen_pGeoData_p3->num_rows;
			HRAP_XOR_p3 = MPEFieldGen_pGeoData_p3->hrap_x;
			HRAP_YOR_p3 = MPEFieldGen_pGeoData_p3->hrap_y;
		}
		return 0;
	}
	return -1;
}

