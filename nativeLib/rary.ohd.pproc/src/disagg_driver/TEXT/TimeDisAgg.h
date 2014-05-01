/* ***************************************** */
/*      File:           ProcessDb.h         */
/*      Date:           June2002             */
/*      Author:         Ai Vo                */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
#include "CurPP.h" 
#include "time_convert.h"
#ifndef _TimeDisAgg_H
#define _TimeDisAgg_H


struct   PosStruct{

float  lat;
float  lon;

};

struct   InputTimeStruct{

time_t  startT,endT;
time_t  lastrunT;
int     durTst;

};


typedef struct _LIDPOS
{
   char lid[10];
   struct PosStruct pos;
   
}LIDPOS;

/*typedef struct _PROCPRECIP_TYPE
{
   time_t   *timePtr;
   int      kount;

}PROCPRECIP_TYPE; PROCPRECIP_TYPE g_procPrecip;
*/
struct PosStruct getPosbyLid(char lid[]);

/*int CheckProcPrecip(time_t thisTime);*/
int OpenXMRGFiles (time_t StartTime, time_t EndTime, int Radius, char XMRGDir[]);
int getLocTable();
int durinHrs(int dur);

/*char  *versNumb = "OB1";*/

float  GetXMRGVal (time_t StartTime, time_t CurrTime, 
                              struct PosStruct Pos, int Radius);
/*void  StoreProcPrecipTD(char *lid,  char *ts, time_t startTime, time_t endTime);
void  fillPostProcPrecip (int iretDur , int *updknt, float radVal, 
               float gageVal,float sumation, time_t ct, CurPP *FprecipPtr );
*/
void  CloseXMRGFiles ();

int GetRadiusVal();
int getLocTbl();

#endif
