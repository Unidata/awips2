/* ***************************************** */
/*      File:           SaveTSMod.h          */
/*      Date:           March 2002           */
/*      Author:         KWZ                  */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
#ifndef SaveTSMod_H
#define SaveTSMod_H

#include "Mods_info.h"

typedef struct _ModKeywords
{
  char TSID[9] ;  /*Time series operation ID*/
  char TSType[5]; /*Time series tarce type*/
  int  TimeInterval ; /*Time series time interval*/
  
  int  NumOps; /*length of OpTypes and OpNames*/
  char **OpTypes;  /*An array of strings operation type*/
  char **OpNames;  /*An array of strings operation name*/

  int		ModIndex;		/* Index identifying current ModArray	*/
					/* position				*/
  ModInfo	*ModArray[MAX_MODS];	/* Array of pointers to structures for	*/
  
} ModKeywords;



void Save_TSChange(int GraphIndex,int TraceIndex, ModKeywords TSMods[],int *NumTS);

#endif
