
#ifndef DBMS_CODE_UTIL_H
#define DBMS_CODE_UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>


/*
	Processed State
*/
typedef enum _PreProcessState
{

    NOT_PROCESSED,
    PROCESSED,
    NUM_PRE_PROCESS_STATES
   
} PreProcessState;


typedef enum _PreProcessor
{

     OFSDE,
     SIIPP,
     NUM_PRE_PROCESSORS

} PreProcessor;


/**************************************************************************/


void  setPreProcessCode(short *code,
                        PreProcessor processId,
			PreProcessState newState);


PreProcessState getPreProcessState(short code,
                                   PreProcessor processId); 

short getStateFromCode(short code,
                       short base,
		       short place,
		       short numPlaces);

#endif
