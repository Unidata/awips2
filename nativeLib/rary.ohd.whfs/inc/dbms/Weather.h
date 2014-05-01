/*
    File: Weather.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Weather_h
#define Weather_h


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include "DbmsAccess.h"
#include "DbmsUtils.h"
#include "List.h"
#include "GeneralUtil.h"
#include "dbmserrs.h"
#include "datetime.h"
#include "time_convert.h"



typedef struct _Weather
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    dtime_t		obstime;
    double		value;
    char		shef_qual_code[2];
    long		quality_code;
    short		revision;
    char		product_id[11];
    dtime_t		producttime;
    dtime_t		postingtime;
    List		list;
} Weather;
/*
    Function Prototypes
*/
    Weather* GetWeather(const char * where);
    Weather* SelectWeather(const char * where);
    int SelectWeatherCount(const char * where);
    int PutWeather(const Weather * structPtr);
    int InsertWeather(const Weather * structPtr);
    int UpdateWeather(const Weather* structPtr, const char *where);
    int DeleteWeather(const char *where);
    int UpdateWeatherByRecord (const Weather * newStructPtr, const Weather * oldStructPtr);
    int InsertOrUpdateWeather(const Weather * structPtr);
    int InsertIfUniqueWeather(const Weather * structPtr, bool *isUnique);
    bool WeatherExists(const Weather * structPtr);
    int DeleteWeatherByRecord(const Weather * structPtr);
    void GetWeatherPrimaryKeyWhereString (const Weather * structPtr, char returnWhereString[] );
    void FreeWeather(Weather * structPtr);
    DbStatus * GetWeatherDbStatus();
    void SetWeatherErrorLogging(int value);
#endif
