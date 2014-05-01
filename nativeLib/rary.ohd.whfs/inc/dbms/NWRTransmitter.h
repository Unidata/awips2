/*
    File: NWRTransmitter.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef NWRTransmitter_h
#define NWRTransmitter_h


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



typedef struct _NWRTransmitter
{
    Node		node;
    char		call_sign[7];
    char		wfo[4];
    char		city[21];
    char		county[21];
    char		state[3];
    char		coverage_area[26];
    double		lat;
    double		lon;
    double		transmit_freq;
    long		transmit_power;
    char		transmit_prod_code[4];
    char		transmit_countynum[5];
    char		use_transmitter[2];
    List		list;
} NWRTransmitter;
/*
    Function Prototypes
*/
    NWRTransmitter* GetNWRTransmitter(const char * where);
    NWRTransmitter* SelectNWRTransmitter(const char * where);
    int SelectNWRTransmitterCount(const char * where);
    int PutNWRTransmitter(const NWRTransmitter * structPtr);
    int InsertNWRTransmitter(const NWRTransmitter * structPtr);
    int UpdateNWRTransmitter(const NWRTransmitter* structPtr, const char *where);
    int DeleteNWRTransmitter(const char *where);
    int UpdateNWRTransmitterByRecord (const NWRTransmitter * newStructPtr, const NWRTransmitter * oldStructPtr);
    int InsertOrUpdateNWRTransmitter(const NWRTransmitter * structPtr);
    int InsertIfUniqueNWRTransmitter(const NWRTransmitter * structPtr, bool *isUnique);
    bool NWRTransmitterExists(const NWRTransmitter * structPtr);
    int DeleteNWRTransmitterByRecord(const NWRTransmitter * structPtr);
    void GetNWRTransmitterPrimaryKeyWhereString (const NWRTransmitter * structPtr, char returnWhereString[] );
    void FreeNWRTransmitter(NWRTransmitter * structPtr);
    DbStatus * GetNWRTransmitterDbStatus();
    void SetNWRTransmitterErrorLogging(int value);
#endif
