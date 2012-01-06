/*
    File: Velocity.h
    Author  : CDBGEN
    Created : Mon Nov 30 21:12:47 EST 2009 using database hd_ob92empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Velocity_h
#define Velocity_h


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



typedef struct _Velocity
{
    Node		node;
    char		mosaicid[21];
    dtime_t		createtime;
    double		umean;
    double		vmean;
    short		count;
    List		list;
} Velocity;
/*
    Function Prototypes
*/
    Velocity* GetVelocity(const char * where);
    Velocity* SelectVelocity(const char * where);
    int SelectVelocityCount(const char * where);
    int PutVelocity(const Velocity * structPtr);
    int InsertVelocity(const Velocity * structPtr);
    int UpdateVelocity(const Velocity* structPtr, const char *where);
    int DeleteVelocity(const char *where);
    int UpdateVelocityByRecord (const Velocity * newStructPtr, const Velocity * oldStructPtr);
    int InsertOrUpdateVelocity(const Velocity * structPtr);
    int InsertIfUniqueVelocity(const Velocity * structPtr, bool *isUnique);
    bool VelocityExists(const Velocity * structPtr);
    int DeleteVelocityByRecord(const Velocity * structPtr);
    void GetVelocityPrimaryKeyWhereString (const Velocity * structPtr, char returnWhereString[] );
    void FreeVelocity(Velocity * structPtr);
    DbStatus * GetVelocityDbStatus();
    void SetVelocityErrorLogging(int value);
#endif
