/*
    File: FlowType.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef FlowType_h
#define FlowType_h


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



typedef struct _FlowType
{
    Node		node;
    char		flowtype[41];
    List		list;
} FlowType;
/*
    Function Prototypes
*/
    FlowType* GetFlowType(const char * where);
    FlowType* SelectFlowType(const char * where);
    int SelectFlowTypeCount(const char * where);
    int PutFlowType(const FlowType * structPtr);
    int InsertFlowType(const FlowType * structPtr);
    int UpdateFlowType(const FlowType* structPtr, const char *where);
    int DeleteFlowType(const char *where);
    int UpdateFlowTypeByRecord (const FlowType * newStructPtr, const FlowType * oldStructPtr);
    int InsertOrUpdateFlowType(const FlowType * structPtr);
    int InsertIfUniqueFlowType(const FlowType * structPtr, bool *isUnique);
    bool FlowTypeExists(const FlowType * structPtr);
    int DeleteFlowTypeByRecord(const FlowType * structPtr);
    void GetFlowTypePrimaryKeyWhereString (const FlowType * structPtr, char returnWhereString[] );
    void FreeFlowType(FlowType * structPtr);
    DbStatus * GetFlowTypeDbStatus();
    void SetFlowTypeErrorLogging(int value);
#endif
