/*
    File: DamMaster.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DamMaster_h
#define DamMaster_h


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



typedef struct _DamMaster
{
    Node		node;
    char		nidid[11];
    char		dam_name[66];
    char		county[31];
    char		river[31];
    char		downstream_hazard[12];
    double		max_storage;
    char		hsa[4];
    char		rfc[6];
    double		latitude_dam;
    double		longitude_dam;
    List		list;
} DamMaster;
/*
    Function Prototypes
*/
    DamMaster* GetDamMaster(const char * where);
    DamMaster* SelectDamMaster(const char * where);
    int SelectDamMasterCount(const char * where);
    int PutDamMaster(const DamMaster * structPtr);
    int InsertDamMaster(const DamMaster * structPtr);
    int UpdateDamMaster(const DamMaster* structPtr, const char *where);
    int DeleteDamMaster(const char *where);
    int UpdateDamMasterByRecord (const DamMaster * newStructPtr, const DamMaster * oldStructPtr);
    int InsertOrUpdateDamMaster(const DamMaster * structPtr);
    int InsertIfUniqueDamMaster(const DamMaster * structPtr, bool *isUnique);
    bool DamMasterExists(const DamMaster * structPtr);
    int DeleteDamMasterByRecord(const DamMaster * structPtr);
    void GetDamMasterPrimaryKeyWhereString (const DamMaster * structPtr, char returnWhereString[] );
    void FreeDamMaster(DamMaster * structPtr);
    DbStatus * GetDamMasterDbStatus();
    void SetDamMasterErrorLogging(int value);
#endif
