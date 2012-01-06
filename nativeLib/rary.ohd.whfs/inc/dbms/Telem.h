/*
    File: Telem.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Telem_h
#define Telem_h


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



typedef struct _Telem
{
    Node		node;
    char		lid[9];
    char		type[11];
    char		payor[11];
    double		cost;
    char		criteria[51];
    char		owner[11];
    char		phone[13];
    char		sensorid[11];
    char		rptfreq[5];
    char		notify[2];
    List		list;
} Telem;
/*
    Function Prototypes
*/
    Telem* GetTelem(const char * where);
    Telem* SelectTelem(const char * where);
    int SelectTelemCount(const char * where);
    int PutTelem(const Telem * structPtr);
    int InsertTelem(const Telem * structPtr);
    int UpdateTelem(const Telem* structPtr, const char *where);
    int DeleteTelem(const char *where);
    int UpdateTelemByRecord (const Telem * newStructPtr, const Telem * oldStructPtr);
    int InsertOrUpdateTelem(const Telem * structPtr);
    int InsertIfUniqueTelem(const Telem * structPtr, bool *isUnique);
    bool TelemExists(const Telem * structPtr);
    int DeleteTelemByRecord(const Telem * structPtr);
    void GetTelemPrimaryKeyWhereString (const Telem * structPtr, char returnWhereString[] );
    void FreeTelem(Telem * structPtr);
    DbStatus * GetTelemDbStatus();
    void SetTelemErrorLogging(int value);
#endif
