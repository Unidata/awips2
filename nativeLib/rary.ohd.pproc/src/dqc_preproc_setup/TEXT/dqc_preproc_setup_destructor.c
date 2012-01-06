#include "dqc_preproc_setup.h"

void memoryRelease()
{
	if(ptrGeoData != NULL)
	{
		free(ptrGeoData);
		ptrGeoData = NULL;
	}

	if(ptrPrecipInfo != NULL)
	{
		free(ptrPrecipInfo);
		ptrPrecipInfo = NULL;
	}

	if(ptrTempInfo != NULL)
	{
		free(ptrTempInfo);
		ptrTempInfo = NULL;
	}

	if(ptrClimoFile != NULL)
	{
		fclose ( ptrClimoFile );
		ptrClimoFile = NULL;
	}
}
