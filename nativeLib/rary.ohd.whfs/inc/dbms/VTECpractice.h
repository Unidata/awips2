/*
    File: VTECpractice.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECpractice_h
#define VTECpractice_h


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



typedef struct _VTECpractice
{
    Node		node;
    char		geoid[25];
    char		product_id[11];
    dtime_t		producttime;
    char		productmode[2];
    char		action[4];
    char		office_id[6];
    char		phenom[3];
    char		signif[2];
    short		etn;
    dtime_t		begintime;
    dtime_t		endtime;
    char		severity[2];
    char		immed_cause[3];
    dtime_t		risetime;
    dtime_t		cresttime;
    dtime_t		falltime;
    char		record[3];
    char		risets[3];
    char		crests[3];
    char		fallts[3];
    double		crest_value;
    dtime_t		expiretime;
    List		list;
} VTECpractice;
/*
    Function Prototypes
*/
    VTECpractice* GetVTECpractice(const char * where);
    VTECpractice* SelectVTECpractice(const char * where);
    int SelectVTECpracticeCount(const char * where);
    int PutVTECpractice(const VTECpractice * structPtr);
    int InsertVTECpractice(const VTECpractice * structPtr);
    int UpdateVTECpractice(const VTECpractice* structPtr, const char *where);
    int DeleteVTECpractice(const char *where);
    int UpdateVTECpracticeByRecord (const VTECpractice * newStructPtr, const VTECpractice * oldStructPtr);
    int InsertOrUpdateVTECpractice(const VTECpractice * structPtr);
    int InsertIfUniqueVTECpractice(const VTECpractice * structPtr, bool *isUnique);
    bool VTECpracticeExists(const VTECpractice * structPtr);
    int DeleteVTECpracticeByRecord(const VTECpractice * structPtr);
    void GetVTECpracticePrimaryKeyWhereString (const VTECpractice * structPtr, char returnWhereString[] );
    void FreeVTECpractice(VTECpractice * structPtr);
    DbStatus * GetVTECpracticeDbStatus();
    void SetVTECpracticeErrorLogging(int value);
#endif
