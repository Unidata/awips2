/*
    File: State.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef State_h
#define State_h


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



typedef struct _State
{
    Node		node;
    char		state[3];
    char		name[21];
    List		list;
} State;
/*
    Function Prototypes
*/
    State* GetState(const char * where);
    State* SelectState(const char * where);
    int SelectStateCount(const char * where);
    int PutState(const State * structPtr);
    int InsertState(const State * structPtr);
    int UpdateState(const State* structPtr, const char *where);
    int DeleteState(const char *where);
    int UpdateStateByRecord (const State * newStructPtr, const State * oldStructPtr);
    int InsertOrUpdateState(const State * structPtr);
    int InsertIfUniqueState(const State * structPtr, bool *isUnique);
    bool StateExists(const State * structPtr);
    int DeleteStateByRecord(const State * structPtr);
    void GetStatePrimaryKeyWhereString (const State * structPtr, char returnWhereString[] );
    void FreeState(State * structPtr);
    DbStatus * GetStateDbStatus();
    void SetStateErrorLogging(int value);
#endif
