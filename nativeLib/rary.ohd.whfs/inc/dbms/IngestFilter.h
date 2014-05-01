/*
    File: IngestFilter.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef IngestFilter_h
#define IngestFilter_h


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



typedef struct _IngestFilter
{
    Node		node;
    char		lid[9];
    char		pe[3];
    short		dur;
    char		ts[3];
    char		extremum[2];
    short		ts_rank;
    char		ingest[2];
    char		ofs_input[2];
    char		stg2_input[2];
    List		list;
} IngestFilter;
/*
    Function Prototypes
*/
    IngestFilter* GetIngestFilter(const char * where);
    IngestFilter* SelectIngestFilter(const char * where);
    int SelectIngestFilterCount(const char * where);
    int PutIngestFilter(const IngestFilter * structPtr);
    int InsertIngestFilter(const IngestFilter * structPtr);
    int UpdateIngestFilter(const IngestFilter* structPtr, const char *where);
    int DeleteIngestFilter(const char *where);
    int UpdateIngestFilterByRecord (const IngestFilter * newStructPtr, const IngestFilter * oldStructPtr);
    int InsertOrUpdateIngestFilter(const IngestFilter * structPtr);
    int InsertIfUniqueIngestFilter(const IngestFilter * structPtr, bool *isUnique);
    bool IngestFilterExists(const IngestFilter * structPtr);
    int DeleteIngestFilterByRecord(const IngestFilter * structPtr);
    void GetIngestFilterPrimaryKeyWhereString (const IngestFilter * structPtr, char returnWhereString[] );
    void FreeIngestFilter(IngestFilter * structPtr);
    DbStatus * GetIngestFilterDbStatus();
    void SetIngestFilterErrorLogging(int value);
#endif
