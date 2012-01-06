/*
 ============================================================================
 Name        : edex_pdc.c
 Author      :
 Version     :
 Copyright   : Your copyright notice
 Description : Hello World in C, Ansi-style
 ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "PointDataRequest.h"
#include "GridDataRequest.h"

int main(void) {
	CPointDataRequest * request = NULL;
	request = get_pdr_instance("awips-dev1", 9581);

	pdr_set_plugin(request, "sfcobs");
	pdr_add_parameter(request, "stationId");
	pdr_add_parameter(request, "timeObs");
	pdr_add_parameter(request, "latitude");
	pdr_add_parameter(request, "longitude");
	pdr_add_parameter(request, "temperature");
	pdr_add_parameter(request, "dewpoint");
	pdr_add_parameter(request, "seaLevelPress");
	pdr_add_parameter(request, "windDir");
	pdr_add_parameter(request, "windSpeed");
	pdr_add_parameter(request, "windGust");
	pdr_add_parameter(request, "visibility");
	pdr_add_parameter(request, "totalCloudCover");
	pdr_add_parameter(request, "presWeather");
	pdr_add_parameter(request, "seaSurfaceTemp");
	pdr_add_parameter(request, "wavePeriod");
	pdr_add_parameter(request, "waveHeight");
	pdr_add_parameter(request, "highResWaveHeight");
	pdr_add_parameter(request, "primarySwellWaveDir");
	pdr_add_parameter(request, "primarySwellWaveHeight");
	pdr_add_parameter(request, "primarySwellWavePeriod");
	pdr_add_parameter(request, "maxWindSpeed");
	pdr_add_parameter(request, "windWavePeriod");
	pdr_add_parameter(request, "windWaveHeight");
	pdr_add_parameter(request, "rawReport");
	int reportsFound = pdr_request_data(request,"ARPF1");
	int visType = pdr_get_parameter_type(request, "visibility");
	printf("The type for vis is %d\n", visType);
	printf("For station ARPF1, found %d reports.\n", reportsFound);
	int counter = 0;
	for(counter = 0;counter < reportsFound; counter++) {
		const char * stationId = pdr_get_string_data(request, "stationId", counter);
		int64_t timeObs = pdr_get_long_data(request, "timeObs", counter);
		int32_t vis = pdr_get_int_data(request, "visibility", counter);
		float temp = pdr_get_float_data(request, "temperature", counter);
		printf("Temp and vis for station %s: %f %d %lld\n", stationId, temp, vis, timeObs);
	}

	pdr_set_plugin(request, "obs");
	pdr_clear_parameters(request);
	pdr_add_parameter(request, "tempFromTenths");
	pdr_set_datatime(request, "2010-01-28 17:55:00.0");
	reportsFound = pdr_request_data(request, "KOFF");
	printf("Found %d report for KOFF\n", reportsFound);
	float temp = pdr_get_float_data(request, "tempFromTenths", 0);
	printf("Temp for KOFF is %f\n", temp);
	printf("The units for tempFromTenths are %s\n", pdr_get_parameter_unit(request, "tempFromTenths"));

	delete_pdr_instance(request);


	CGridDataRequest * gridRequest = get_gdr_instance("localhost", 9581);
	gdr_set_modelname(gridRequest, "RUC");
	gdr_request_data(gridRequest);
	CGribThriftContainer * gridResults = gdr_get_results(gridRequest);
	printf("Retrieved %i records from database.\n", gtc_get_num_records(gridResults));
	delete_gdr_instance(gridRequest);
	return EXIT_SUCCESS;
}
