/*
    File: SectionPair.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef SectionPair_h
#define SectionPair_h


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



typedef struct _SectionPair
{
    Node		node;
    char		nidid[11];
    char		down_name[26];
    long		pair_num;
    char		xsec_type[3];
    double		elev;
    double		tw;
    double		mann_n;
    double		inactive_width;
    dtime_t		updated;
    List		list;
} SectionPair;
/*
    Function Prototypes
*/
    SectionPair* GetSectionPair(const char * where);
    SectionPair* SelectSectionPair(const char * where);
    int SelectSectionPairCount(const char * where);
    int PutSectionPair(const SectionPair * structPtr);
    int InsertSectionPair(const SectionPair * structPtr);
    int UpdateSectionPair(const SectionPair* structPtr, const char *where);
    int DeleteSectionPair(const char *where);
    int UpdateSectionPairByRecord (const SectionPair * newStructPtr, const SectionPair * oldStructPtr);
    int InsertOrUpdateSectionPair(const SectionPair * structPtr);
    int InsertIfUniqueSectionPair(const SectionPair * structPtr, bool *isUnique);
    bool SectionPairExists(const SectionPair * structPtr);
    int DeleteSectionPairByRecord(const SectionPair * structPtr);
    void GetSectionPairPrimaryKeyWhereString (const SectionPair * structPtr, char returnWhereString[] );
    void FreeSectionPair(SectionPair * structPtr);
    DbStatus * GetSectionPairDbStatus();
    void SetSectionPairErrorLogging(int value);
#endif
