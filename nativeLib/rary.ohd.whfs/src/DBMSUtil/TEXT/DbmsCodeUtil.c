#include "DbmsCodeUtil.h"

/**************************************************************************/


void  setPreProcessCode(short           *code,
		        PreProcessor    processId,
			PreProcessState newState)
{

     
     PreProcessState oldState;
     long powerValue;
     
     if ( (processId < 0) || (processId >= NUM_PRE_PROCESSORS)) 
     {
	  fprintf(stderr, "ERROR processId = %d. "
		  "It should be >= 0 and < %d\n",
		  processId, NUM_PRE_PROCESSORS);
     }

     else if ( ( newState < 0) || (newState >= NUM_PRE_PROCESS_STATES) )
     {
	  fprintf(stderr, "ERROR newState = %d. "
		  "It should be >= 0 and < %d\n",
		  newState, NUM_PRE_PROCESS_STATES);	  
     }
     
     else
     {
	  oldState = getPreProcessState(*code, processId);
	  
	  powerValue =  pow(NUM_PRE_PROCESS_STATES, processId);
	  *code -=  powerValue * oldState;
	  *code +=  powerValue * newState;
     }
     
     return;
}

/**************************************************************************/


PreProcessState getPreProcessState(short        code,
                                   PreProcessor processId) 
			 
{
     PreProcessState state;
     int  base = NUM_PRE_PROCESS_STATES;
     int  numPlaces = NUM_PRE_PROCESSORS;
     int place = processId;
     
     
     state = (PreProcessState) getStateFromCode(code, base, place, numPlaces);
  
     
     return state;  
}



/**************************************************************************/

short getStateFromCode(short code,
                      short base,
		      short place,
		      short numPlaces)
{
     short done = 0;
     short i, j;
     short value = 0;
     short state = 0;
     
     state = code;
     
     done = 0;
     
     for (i = numPlaces - 1; ( i >= 0 && (! done) ); i--)
     {
	  for ( j = base - 1; j >= 1; j--)
	  {
	       value = pow(base, i) * j;
	       
	       if  (state - value >= 0) 
	       {
		    if (i == place)
		    {
			 state = j;
			 
			 done = 1;
		    }
		    else
		    {
			 state -= value;   
		    }
		    
		    break;
	       }
	       
	  }
	  
     }	         
     
  
     return state;
     
}
