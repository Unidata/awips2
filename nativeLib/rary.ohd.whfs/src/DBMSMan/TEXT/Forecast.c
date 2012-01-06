#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>

#include "Forecast.h"

#include "List.h"

#include "FcstHeight.h"
#include "FcstPrecip.h"
#include "FcstDischarge.h"
#include "FcstTemperature.h"
#include "FcstOther.h"

#include "ContingencyValue.h"

typedef struct FcstFunctions
{
	char tablename[32];

	void *  (* get)(char *);
 	int	(* put )(void *);
	int 	(* delete)( const char *);
	int	(* update)(void *, char *);
	void	(* free)(void *);
 
} FcstFunctions;


const FcstFunctions _fcst_function_set[] = 
{
   
   { "fcstheight",
      ( void * ) GetFcstHeight, 
      ( void * ) PutFcstHeight, 
      DeleteFcstHeight,
      ( void * ) UpdateFcstHeight,
      ( void * ) FreeFcstHeight },
			
   { "fcstprecip",
      ( void * ) GetFcstPrecip,
      ( void * ) PutFcstPrecip, 
      DeleteFcstPrecip,
      ( void * ) UpdateFcstPrecip,
      ( void * ) FreeFcstPrecip },
			
   { "fcstdischarge",
      ( void * ) GetFcstDischarge,
      ( void * ) PutFcstDischarge,
      DeleteFcstDischarge,
      ( void * ) UpdateFcstDischarge,
      ( void * ) FreeFcstDischarge },
			
   { "fcsttemperature", 
     ( void * ) GetFcstTemperature,
     ( void * ) PutFcstTemperature, 
     DeleteFcstTemperature,
     ( void * ) UpdateFcstTemperature, 
     ( void * ) FreeFcstTemperature },

   { "fcstother",
     ( void * ) GetFcstOther, 
     ( void * ) PutFcstOther, 
     DeleteFcstOther,
     ( void * ) UpdateFcstOther, 
     ( void * ) FreeFcstOther },
			
   { "contingencyvalue",
     ( void * ) GetContingencyValue, 
     ( void * ) PutContingencyValue, 
     DeleteContingencyValue,
     ( void * ) UpdateContingencyValue, 
     ( void * ) FreeContingencyValue },
			
};

const long _num_fcst_function_sets = 
   		(sizeof _fcst_function_set) / sizeof(FcstFunctions);



long	find_fcst_set_index(const char *table)
{
   	/*
   		This functions looks through the function set C table
		to find the matching SQL table name.  It returns the
		index to the table.  If the index is not found, it
		returns -1.
   	*/
   
   	long	index = -1L;
   	int	i;
	char	tablename[BUFSIZ];
	
	
	/*
		Copy to tablename and convert to lower case
	*/
	for ( i = 0; ( tablename[i] = tolower(table[i] ) ) ; i++ ) ;
   

	/*
		Search through the table
	*/
	for (i = 0; i < _num_fcst_function_sets; i++)
	{
		if (strcmp(tablename, _fcst_function_set[i].tablename) == 0)
		{
			index = i;
			break;
		}   
	}
   	return index;
}   


Forecast * GetForecast(const char *where, const char *table)
{
     long   	index;
     Forecast	*fcstHead = NULL;
   
     index = find_fcst_set_index(table);   
     
     if (index > -1)
     {   
          fcstHead = (Forecast *) _fcst_function_set[index].get((char *)where);
     }
	
     else
          fcstHead = (Forecast *) NULL;	   
	
     return fcstHead;
}   


int	PutForecast(Forecast *fcstPtr, const char *table)
{
   
   	long 	index;
	int	rv = -1;
	
	
	index = find_fcst_set_index(table);  
	
	if (index > -1)	   
		rv = _fcst_function_set[index].put(fcstPtr);
 	

	return rv;
}   




int	UpdateForecast(Forecast *fcstPtr, const char *where,
			  const char *table)
{   
	long 	index;
	int	rv = -1;
	
	
	index = find_fcst_set_index(table);  
	
	if (index > -1)	   
		rv = _fcst_function_set[index].update(fcstPtr, (char *) where);	   
   
	return rv;
}




int	DeleteForecast(const char *where, const char* table)
{
		   
   	long 	index;
	int	rv = -1;
	
	
	index = find_fcst_set_index(table);  
	
	if (index > -1)	   
		rv = _fcst_function_set[index].delete((char *) where);	
	
	return rv;
   
}


void	FreeForecast(Forecast *fcstPtr)
{
   
	long 	index = 0;
	
	_fcst_function_set[index].free(fcstPtr);	
	
	return;
}



