/*
    File: VTECevent.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef VTECevent_h
#define VTECevent_h


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



typedef struct _VTECevent
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
} VTECevent;
/*
    Function Prototypes
*/
    VTECevent* GetVTECevent(const char * where);
    VTECevent* SelectVTECevent(const char * where);
    int SelectVTECeventCount(const char * where);
    int PutVTECevent(const VTECevent * structPtr);
    int InsertVTECevent(const VTECevent * structPtr);
    int UpdateVTECevent(const VTECevent* structPtr, const char *where);
    int DeleteVTECevent(const char *where);
    int UpdateVTECeventByRecord (const VTECevent * newStructPtr, const VTECevent * oldStructPtr);
    int InsertOrUpdateVTECevent(const VTECevent * structPtr);
    int InsertIfUniqueVTECevent(const VTECevent * structPtr, bool *isUnique);
    bool VTECeventExists(const VTECevent * structPtr);
    int DeleteVTECeventByRecord(const VTECevent * structPtr);
    void GetVTECeventPrimaryKeyWhereString (const VTECevent * structPtr, char returnWhereString[] );
    void FreeVTECevent(VTECevent * structPtr);
    DbStatus * GetVTECeventDbStatus();
    void SetVTECeventErrorLogging(int value);
#endif
