/*
    File: City.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef City_h
#define City_h


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



typedef struct _City
{
    Node		node;
    char		name[21];
    char		state[3];
    double		lat;
    double		lon;
    long		disp_precedence;
    long		population;
    List		list;
} City;
/*
    Function Prototypes
*/
    City* GetCity(const char * where);
    City* SelectCity(const char * where);
    int SelectCityCount(const char * where);
    int PutCity(const City * structPtr);
    int InsertCity(const City * structPtr);
    int UpdateCity(const City* structPtr, const char *where);
    int DeleteCity(const char *where);
    void FreeCity(City * structPtr);
    DbStatus * GetCityDbStatus();
    void SetCityErrorLogging(int value);
#endif
