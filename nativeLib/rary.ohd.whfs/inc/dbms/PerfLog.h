/*
    File: PerfLog.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef PerfLog_h
#define PerfLog_h


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



typedef struct _PerfLog
{
    Node		node;
    char		process[11];
    dtime_t		start_time;
    long		num_processed;
    long		num_reads;
    long		num_inserts;
    long		num_updates;
    long		num_deletes;
    double		elapsed_time;
    double		cpu_time;
    double		io_time;
    List		list;
} PerfLog;
/*
    Function Prototypes
*/
    PerfLog* GetPerfLog(const char * where);
    PerfLog* SelectPerfLog(const char * where);
    int SelectPerfLogCount(const char * where);
    int PutPerfLog(const PerfLog * structPtr);
    int InsertPerfLog(const PerfLog * structPtr);
    int UpdatePerfLog(const PerfLog* structPtr, const char *where);
    int DeletePerfLog(const char *where);
    int UpdatePerfLogByRecord (const PerfLog * newStructPtr, const PerfLog * oldStructPtr);
    int InsertOrUpdatePerfLog(const PerfLog * structPtr);
    int InsertIfUniquePerfLog(const PerfLog * structPtr, bool *isUnique);
    bool PerfLogExists(const PerfLog * structPtr);
    int DeletePerfLogByRecord(const PerfLog * structPtr);
    void GetPerfLogPrimaryKeyWhereString (const PerfLog * structPtr, char returnWhereString[] );
    void FreePerfLog(PerfLog * structPtr);
    DbStatus * GetPerfLogDbStatus();
    void SetPerfLogErrorLogging(int value);
#endif
