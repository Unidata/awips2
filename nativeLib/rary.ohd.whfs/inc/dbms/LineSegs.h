/*
    File: LineSegs.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:15 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef LineSegs_h
#define LineSegs_h


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



typedef struct _LineSegs
{
    Node		node;
    char		area_id[9];
    long		hrap_row;
    long		hrap_beg_col;
    long		hrap_end_col;
    double		area;
    List		list;
} LineSegs;
/*
    Function Prototypes
*/
    LineSegs* GetLineSegs(const char * where);
    LineSegs* SelectLineSegs(const char * where);
    int SelectLineSegsCount(const char * where);
    int PutLineSegs(const LineSegs * structPtr);
    int InsertLineSegs(const LineSegs * structPtr);
    int UpdateLineSegs(const LineSegs* structPtr, const char *where);
    int DeleteLineSegs(const char *where);
    int UpdateLineSegsByRecord (const LineSegs * newStructPtr, const LineSegs * oldStructPtr);
    int InsertOrUpdateLineSegs(const LineSegs * structPtr);
    int InsertIfUniqueLineSegs(const LineSegs * structPtr, bool *isUnique);
    bool LineSegsExists(const LineSegs * structPtr);
    int DeleteLineSegsByRecord(const LineSegs * structPtr);
    void GetLineSegsPrimaryKeyWhereString (const LineSegs * structPtr, char returnWhereString[] );
    void FreeLineSegs(LineSegs * structPtr);
    DbStatus * GetLineSegsDbStatus();
    void SetLineSegsErrorLogging(int value);
#endif
