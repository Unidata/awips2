/*
    File: ReservoirModel.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ReservoirModel_h
#define ReservoirModel_h


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



typedef struct _ReservoirModel
{
    Node		node;
    char		reservoir_model[31];
    List		list;
} ReservoirModel;
/*
    Function Prototypes
*/
    ReservoirModel* GetReservoirModel(const char * where);
    ReservoirModel* SelectReservoirModel(const char * where);
    int SelectReservoirModelCount(const char * where);
    int PutReservoirModel(const ReservoirModel * structPtr);
    int InsertReservoirModel(const ReservoirModel * structPtr);
    int UpdateReservoirModel(const ReservoirModel* structPtr, const char *where);
    int DeleteReservoirModel(const char *where);
    int UpdateReservoirModelByRecord (const ReservoirModel * newStructPtr, const ReservoirModel * oldStructPtr);
    int InsertOrUpdateReservoirModel(const ReservoirModel * structPtr);
    int InsertIfUniqueReservoirModel(const ReservoirModel * structPtr, bool *isUnique);
    bool ReservoirModelExists(const ReservoirModel * structPtr);
    int DeleteReservoirModelByRecord(const ReservoirModel * structPtr);
    void GetReservoirModelPrimaryKeyWhereString (const ReservoirModel * structPtr, char returnWhereString[] );
    void FreeReservoirModel(ReservoirModel * structPtr);
    DbStatus * GetReservoirModelDbStatus();
    void SetReservoirModelErrorLogging(int value);
#endif
