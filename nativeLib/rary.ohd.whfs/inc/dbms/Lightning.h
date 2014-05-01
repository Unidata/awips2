/*
    File: Lightning.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Lightning_h
#define Lightning_h


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



typedef struct _Lightning
{
    Node		node;
    short		x_hgrid;
    short		y_hgrid;
    dtime_t		obstime;
    short		no_of_strike;
    List		list;
} Lightning;
/*
    Function Prototypes
*/
    Lightning* GetLightning(const char * where);
    Lightning* SelectLightning(const char * where);
    int SelectLightningCount(const char * where);
    int PutLightning(const Lightning * structPtr);
    int InsertLightning(const Lightning * structPtr);
    int UpdateLightning(const Lightning* structPtr, const char *where);
    int DeleteLightning(const char *where);
    int UpdateLightningByRecord (const Lightning * newStructPtr, const Lightning * oldStructPtr);
    int InsertOrUpdateLightning(const Lightning * structPtr);
    int InsertIfUniqueLightning(const Lightning * structPtr, bool *isUnique);
    bool LightningExists(const Lightning * structPtr);
    int DeleteLightningByRecord(const Lightning * structPtr);
    void GetLightningPrimaryKeyWhereString (const Lightning * structPtr, char returnWhereString[] );
    void FreeLightning(Lightning * structPtr);
    DbStatus * GetLightningDbStatus();
    void SetLightningErrorLogging(int value);
#endif
