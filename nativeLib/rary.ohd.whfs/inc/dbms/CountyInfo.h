// This is a view record !
/*
    File: CountyInfo.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef CountyInfo_h
#define CountyInfo_h


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



typedef struct _CountyInfo
{
    Node		node;
    char		lid[9];
    char		state[3];
    char		county[21];
    char		countynum[5];
    List		list;
} CountyInfo;
/*
    Function Prototypes
*/
    CountyInfo* GetCountyInfo(const char * where);
    CountyInfo* SelectCountyInfo(const char * where);
    int SelectCountyInfoCount(const char * where);
    void FreeCountyInfo(CountyInfo * structPtr);
    DbStatus * GetCountyInfoDbStatus();
    void SetCountyInfoErrorLogging(int value);
#endif
