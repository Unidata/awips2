/*
    File: Counties.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Counties_h
#define Counties_h


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



typedef struct _Counties
{
    Node		node;
    char		county[21];
    char		state[3];
    char		countynum[5];
    char		wfo[4];
    char		primary_back[4];
    char		secondary_back[4];
    List		list;
} Counties;
/*
    Function Prototypes
*/
    Counties* GetCounties(const char * where);
    Counties* SelectCounties(const char * where);
    int SelectCountiesCount(const char * where);
    int PutCounties(const Counties * structPtr);
    int InsertCounties(const Counties * structPtr);
    int UpdateCounties(const Counties* structPtr, const char *where);
    int DeleteCounties(const char *where);
    int UpdateCountiesByRecord (const Counties * newStructPtr, const Counties * oldStructPtr);
    int InsertOrUpdateCounties(const Counties * structPtr);
    int InsertIfUniqueCounties(const Counties * structPtr, bool *isUnique);
    bool CountiesExists(const Counties * structPtr);
    int DeleteCountiesByRecord(const Counties * structPtr);
    void GetCountiesPrimaryKeyWhereString (const Counties * structPtr, char returnWhereString[] );
    void FreeCounties(Counties * structPtr);
    DbStatus * GetCountiesDbStatus();
    void SetCountiesErrorLogging(int value);
#endif
