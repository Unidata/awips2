/*
    File: Network.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Network_h
#define Network_h


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



typedef struct _Network
{
    Node		node;
    char		network[4];
    List		list;
} Network;
/*
    Function Prototypes
*/
    Network* GetNetwork(const char * where);
    Network* SelectNetwork(const char * where);
    int SelectNetworkCount(const char * where);
    int PutNetwork(const Network * structPtr);
    int InsertNetwork(const Network * structPtr);
    int UpdateNetwork(const Network* structPtr, const char *where);
    int DeleteNetwork(const char *where);
    int UpdateNetworkByRecord (const Network * newStructPtr, const Network * oldStructPtr);
    int InsertOrUpdateNetwork(const Network * structPtr);
    int InsertIfUniqueNetwork(const Network * structPtr, bool *isUnique);
    bool NetworkExists(const Network * structPtr);
    int DeleteNetworkByRecord(const Network * structPtr);
    void GetNetworkPrimaryKeyWhereString (const Network * structPtr, char returnWhereString[] );
    void FreeNetwork(Network * structPtr);
    DbStatus * GetNetworkDbStatus();
    void SetNetworkErrorLogging(int value);
#endif
