/*
    File: Observer.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:16 EDT 2008 using database hd_ob83empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef Observer_h
#define Observer_h


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



typedef struct _Observer
{
    Node		node;
    char		lid[9];
    char		a1[31];
    char		a2[31];
    char		a3[31];
    char		city[31];
    char		state[3];
    char		zip[11];
    char		comm[11];
    date_t		dos;
    char		gn[2];
    char		hphone[19];
    char		firstname[13];
    char		lastname[29];
    char		phone[19];
    char		email[61];
    char		ornr[5];
    double		rate;
    char		recip[16];
    char		rprt[61];
    char		spons[8];
    char		ssn[12];
    char		tsk[14];
    List		list;
} Observer;
/*
    Function Prototypes
*/
    Observer* GetObserver(const char * where);
    Observer* SelectObserver(const char * where);
    int SelectObserverCount(const char * where);
    int PutObserver(const Observer * structPtr);
    int InsertObserver(const Observer * structPtr);
    int UpdateObserver(const Observer* structPtr, const char *where);
    int DeleteObserver(const char *where);
    int UpdateObserverByRecord (const Observer * newStructPtr, const Observer * oldStructPtr);
    int InsertOrUpdateObserver(const Observer * structPtr);
    int InsertIfUniqueObserver(const Observer * structPtr, bool *isUnique);
    bool ObserverExists(const Observer * structPtr);
    int DeleteObserverByRecord(const Observer * structPtr);
    void GetObserverPrimaryKeyWhereString (const Observer * structPtr, char returnWhereString[] );
    void FreeObserver(Observer * structPtr);
    DbStatus * GetObserverDbStatus();
    void SetObserverErrorLogging(int value);
#endif
