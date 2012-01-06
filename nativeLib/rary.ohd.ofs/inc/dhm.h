#ifndef DHM_H
#define DHM_H

#include <ctype.h>
#include <jni.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAXARRAY 2048
#define MAXCARRYOVERDATES 20
#define MAX_DHM_TS_LEN 744
#define MAX_MSG_LEN 2000
#define NWSRFS_ID_LENGTH 8
#define NWSRFS_DATA_TYPE_LENGTH 4
#define DATE_STRING_LENGTH 20
#define MAX_DIRECTORY_NAME_LENGTH 120
#define STRING_CLASS                 "java/lang/String"
#define SIMPLE_DATE_AND_TIME_CLASS   "ohd/hseb/dhm/time/SimpleDateAndTime"
#define RESULT_CLASS                 "ohd/hseb/ofs/DhmResult"
#define RUNNER_CLASS                 "ohd/hseb/ofs/Runner" 
#define RUNNER_PATHS_CLASS           "ohd/hseb/ofs/DhmRunnerPaths"
#define MODS_MANAGER_CLASS           "ohd/hseb/dhmGuis/modsGui/ModsManager"

extern JNIEnv *env; 
extern JavaVM *jvm; 
int jvmTokenNotExists;
jobject runner;
jclass runnerClass;
jstring getJavaErrorMessage(jthrowable error);


int *use_snow17_input;

typedef enum {SUCCEEDED, FAILED} DhmStatus;

typedef struct {
	DhmStatus code;
	char * message;
	float timeSeries[MAX_DHM_TS_LEN];
	int timeSeriesLength;
} DhmResult;

DhmResult dhmfailed(char * message) ;

DhmResult dhmsucceeded(float timeSeries[MAX_DHM_TS_LEN], int length);

void remove_trailing_space(char *inputString);

int get_apps_defaults(char *request, int *request_len, char *reply, int *reply_len);

jfloatArray buildUpstreamFlowArray(float tsData[], int tsDataSize);

jclass getClass(const char* javaClassName);

char *getErrorMsg( const char detailedMsg[] );

jobject buildDhmRunnerPaths(jstring jprecipDataPath, 
                            jstring jDHMModelDataPath,
                            jstring jd2dDataPath, 
                            jstring jdhmNotifyDataPath, 
                            jstring jgeoCoordDataPathAndFileName,
			    jstring jprecipXmrgSrcPath,
			    jstring jDHMModelDataSrcPath,
			    jstring jgridOutputConfigurationPath);
                            
jobject buildDateTime(int day, int month, int year, int hour);

jobjectArray createCarryOverDateList(int numberOfCarryoverToBeSaved, 
                                         int carryoverMonthList[MAXCARRYOVERDATES], 
                                         int carryoverDayList[MAXCARRYOVERDATES],    
                                         int carryoverYearList[MAXCARRYOVERDATES],
                                         int carryoverHourList[MAXCARRYOVERDATES]);

DhmResult run_dhm(
		 int startMonthOfRunPeriod, 
		 int startDayOfRunPeriod, 
		 int startYearOfRunPeriod, 
		 int startHourOfRunPeriod,
		 int startMonthOfForecastPeriod, 
		 int startDayOfForecastPeriod,
		 int startYearOfForecasPeriod,
		 int startHourOfForecastPeriod,
		 int endMonthOfRunPeriod,
		 int endDateOfRunPeriod,
		 int endYearOfRunPeriod,
		 int endHourOfRunPeriod,
		 char basinID[], 
		 int saveCO, 
		 int carryoverMonthList[MAXCARRYOVERDATES], 
		 int carryoverDayList[MAXCARRYOVERDATES],
		 int carryoverYearList[MAXCARRYOVERDATES], 
		 int carryoverHourList[MAXCARRYOVERDATES],
		 int futurePrecipitation,
		 char ModString[],
		 int useRainPlusMeltPrecip
);

void get_formatted_output_for_dhm(int *tmpStartObsMonth,
			      int *tmpStartObsDate,
			      int *tmpStartObsYear,
			      int *tmpStartObsHour,
			      int *tmpStartForecastMonth,
			      int *tmpStartForecastDate,
			      int *tmpStartForecastYear,
			      int *tmpStartForecastHour,
			      int *tmpEndMonth,
			      int *tmpEndDate,
			      int *tmpEndYear,
			      int *tmpEndHour,
			      char *basinID,	
			      char upBasinIDs[],
			      int *numOfUpBasinIds,
			      char *precipDataPath, 
			      char *DHMModelDataPath,
                              char *d2dDataPath,
                              char *dhmNotifyDataPath,
			      int *shouldSaveCarryOver, 
			      int carryoverMonthList[MAXCARRYOVERDATES],
			      int carryoverDayList[MAXCARRYOVERDATES], 
			      int carryoverYearList[MAXCARRYOVERDATES],
			      int carryoverHourList[MAXCARRYOVERDATES],
			      int *futurePrecipitation,                 
			      int *useRainPlusMeltFlag,
			      char formattedOutput[MAXARRAY],	
			      int *formattedOutputLength);


void create_runner_with_args_named(
                                  const char * runnerClassName, 
                                  char precipDataPath[],
                                  char DHMModelDataPath[],
                                  char d2dDataPath[],
                                  char dhmNotifyDataPath[],
                                  char geoCoordDataPathAndFileName[],
                                  char precipXmrgSrcPath[],
                                  char DHMModelDataSrcPath[],
                                  char gridOutputConfigurationPath[],
                                  int* startMonth,
                                  int* startDay,
                                  int* startYear,
                                  int* startHour,
                                 char returnMessage[] );

void send_inflows_to_runner(
                           char basinID[],
                           float upstreamFlowTimeSeries[], 
                           int indicesToInputData[], 
                           char upBasinIds[],
                           int *startOfTSindex,
                           int *endOfTSindex, 
                           int *nInflow, 
                           char errorMessage[MAX_DHM_TS_LEN]);
#endif

