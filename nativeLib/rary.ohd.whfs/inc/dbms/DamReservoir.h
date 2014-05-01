/*
    File: DamReservoir.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DamReservoir_h
#define DamReservoir_h


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



typedef struct _DamReservoir
{
    Node		node;
    char		nidid[11];
    char		type[2];
    double		elevation;
    double		stordis;
    double		surface;
    dtime_t		updated;
    List		list;
} DamReservoir;
/*
    Function Prototypes
*/
    DamReservoir* GetDamReservoir(const char * where);
    DamReservoir* SelectDamReservoir(const char * where);
    int SelectDamReservoirCount(const char * where);
    int PutDamReservoir(const DamReservoir * structPtr);
    int InsertDamReservoir(const DamReservoir * structPtr);
    int UpdateDamReservoir(const DamReservoir* structPtr, const char *where);
    int DeleteDamReservoir(const char *where);
    int UpdateDamReservoirByRecord (const DamReservoir * newStructPtr, const DamReservoir * oldStructPtr);
    int InsertOrUpdateDamReservoir(const DamReservoir * structPtr);
    int InsertIfUniqueDamReservoir(const DamReservoir * structPtr, bool *isUnique);
    bool DamReservoirExists(const DamReservoir * structPtr);
    int DeleteDamReservoirByRecord(const DamReservoir * structPtr);
    void GetDamReservoirPrimaryKeyWhereString (const DamReservoir * structPtr, char returnWhereString[] );
    void FreeDamReservoir(DamReservoir * structPtr);
    DbStatus * GetDamReservoirDbStatus();
    void SetDamReservoirErrorLogging(int value);
#endif
