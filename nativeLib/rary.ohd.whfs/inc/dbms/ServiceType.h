/*
    File: ServiceType.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef ServiceType_h
#define ServiceType_h


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



typedef struct _ServiceType
{
    Node		node;
    char		service_type[21];
    List		list;
} ServiceType;
/*
    Function Prototypes
*/
    ServiceType* GetServiceType(const char * where);
    ServiceType* SelectServiceType(const char * where);
    int SelectServiceTypeCount(const char * where);
    int PutServiceType(const ServiceType * structPtr);
    int InsertServiceType(const ServiceType * structPtr);
    int UpdateServiceType(const ServiceType* structPtr, const char *where);
    int DeleteServiceType(const char *where);
    int UpdateServiceTypeByRecord (const ServiceType * newStructPtr, const ServiceType * oldStructPtr);
    int InsertOrUpdateServiceType(const ServiceType * structPtr);
    int InsertIfUniqueServiceType(const ServiceType * structPtr, bool *isUnique);
    bool ServiceTypeExists(const ServiceType * structPtr);
    int DeleteServiceTypeByRecord(const ServiceType * structPtr);
    void GetServiceTypePrimaryKeyWhereString (const ServiceType * structPtr, char returnWhereString[] );
    void FreeServiceType(ServiceType * structPtr);
    DbStatus * GetServiceTypeDbStatus();
    void SetServiceTypeErrorLogging(int value);
#endif
