/*
    File: DownStream.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DownStream_h
#define DownStream_h


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



typedef struct _DownStream
{
    Node		node;
    char		nidid[11];
    char		down_name[26];
    double		longitude;
    double		latitude;
    double		elevation;
    double		distance_from_dam;
    double		flood_flow;
    double		flood_depth;
    double		flood_width;
    double		mann_oc;
    char		comments[31];
    char		xsec_best_type[3];
    dtime_t		updated;
    List		list;
} DownStream;
/*
    Function Prototypes
*/
    DownStream* GetDownStream(const char * where);
    DownStream* SelectDownStream(const char * where);
    int SelectDownStreamCount(const char * where);
    int PutDownStream(const DownStream * structPtr);
    int InsertDownStream(const DownStream * structPtr);
    int UpdateDownStream(const DownStream* structPtr, const char *where);
    int DeleteDownStream(const char *where);
    int UpdateDownStreamByRecord (const DownStream * newStructPtr, const DownStream * oldStructPtr);
    int InsertOrUpdateDownStream(const DownStream * structPtr);
    int InsertIfUniqueDownStream(const DownStream * structPtr, bool *isUnique);
    bool DownStreamExists(const DownStream * structPtr);
    int DeleteDownStreamByRecord(const DownStream * structPtr);
    void GetDownStreamPrimaryKeyWhereString (const DownStream * structPtr, char returnWhereString[] );
    void FreeDownStream(DownStream * structPtr);
    DbStatus * GetDownStreamDbStatus();
    void SetDownStreamErrorLogging(int value);
#endif
