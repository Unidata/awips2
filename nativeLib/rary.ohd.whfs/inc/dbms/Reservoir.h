/*
    File: Reservoir.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Reservoir_h
#define Reservoir_h


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



typedef struct _Reservoir
{
    Node		node;
    char		lid[9];
    char		name[21];
    char		type[11];
    char		owner[11];
    double		deadpool;
    double		conserpool;
    double		floodpool;
    double		spillway;
    double		sill;
    double		top;
    double		surchg;
    double		elev;
    long		gates;
    date_t		impounded;
    char		uses[9];
    char		damids[3];
    char		damidn[6];
    List		list;
} Reservoir;
/*
    Function Prototypes
*/
    Reservoir* GetReservoir(const char * where);
    Reservoir* SelectReservoir(const char * where);
    int SelectReservoirCount(const char * where);
    int PutReservoir(const Reservoir * structPtr);
    int InsertReservoir(const Reservoir * structPtr);
    int UpdateReservoir(const Reservoir* structPtr, const char *where);
    int DeleteReservoir(const char *where);
    int UpdateReservoirByRecord (const Reservoir * newStructPtr, const Reservoir * oldStructPtr);
    int InsertOrUpdateReservoir(const Reservoir * structPtr);
    int InsertIfUniqueReservoir(const Reservoir * structPtr, bool *isUnique);
    bool ReservoirExists(const Reservoir * structPtr);
    int DeleteReservoirByRecord(const Reservoir * structPtr);
    void GetReservoirPrimaryKeyWhereString (const Reservoir * structPtr, char returnWhereString[] );
    void FreeReservoir(Reservoir * structPtr);
    DbStatus * GetReservoirDbStatus();
    void SetReservoirErrorLogging(int value);
#endif
