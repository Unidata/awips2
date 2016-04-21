#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>

#include "DbmsAccess.h"
#include "dbmserrs.h"
#include "List.h"

#include "Observation.h"

#include "Agricultural.h"
#include "CurPC.h"
#include "CurPP.h"
#include "Discharge.h"
#include "Evaporation.h"
#include "FishCount.h"
#include "GateDam.h"
#include "Ground.h"
#include "Height.h"
#include "Ice.h"
#include "Lake.h"
#include "Moisture.h"
#include "Power.h"
#include "Pressure.h"
#include "ProcValue.h"
#include "Radiation.h"
#include "RawPC.h"
#include "RawPP.h"
#include "RawPother.h"
#include "Snow.h"
#include "Temperature.h"
#include "WaterQuality.h"
#include "Weather.h"
#include "Wind.h"
#include "YUnique.h"

#define QUERY_LEN               1024
#include "DbmsAccess.h"
#include "dbmserrs.h"
#include "List.h"
   
typedef struct ObsFunctions
{
	char tablename[32];
	
	void *  (* get)(char *);
 	int	(* put )(void *);
	int 	(* delete)(const char *);
	int	(* update)(void *, char *);
	void	(* free)(void *);
   
} ObsFunctions; 



const ObsFunctions _obs_function_set[] = 
{
   
   { "agricultural", 
     ( void * ) GetAgricultural, 
     ( void * ) PutAgricultural, 
     DeleteAgricultural,
     ( void * ) UpdateAgricultural, 
     ( void * ) FreeAgricultural },

   { "evaporation", 
     ( void * ) GetEvaporation, 
     ( void * ) PutEvaporation, 
     DeleteEvaporation,
     ( void * ) UpdateEvaporation, 
     ( void * )  FreeEvaporation },

   { "ground",
     ( void * ) GetGround, 
     ( void * ) PutGround, 
     DeleteGround,
     ( void * ) UpdateGround, 
     ( void * ) FreeGround },

   { "discharge",   
     ( void * ) GetDischarge, 
     ( void * ) PutDischarge, 
     DeleteDischarge,
     ( void * ) UpdateDischarge, 
     ( void * ) FreeDischarge },

   { "fishcount",   
     ( void * ) GetFishCount, 
     ( void * ) PutFishCount, 
     DeleteFishCount,
     ( void * ) UpdateFishCount, 
     ( void * ) FreeFishCount },

   { "gatedam",     
     ( void * ) GetGateDam, 
     ( void * ) PutGateDam, 
     DeleteGateDam, 
     ( void * ) UpdateGateDam, 
     ( void * ) FreeGateDam },

   { "height",      
     ( void * ) GetHeight, 
     ( void * ) PutHeight, 
     DeleteHeight, 
     ( void * ) UpdateHeight, 
     ( void * ) FreeHeight },

   { "ice",         
     ( void * ) GetIce, 
     ( void * ) PutIce, 
     DeleteIce, 
     ( void * ) UpdateIce, 
     ( void * ) FreeIce },

   { "lake",
     ( void * ) GetLake, 
     ( void * ) PutLake, 
     DeleteLake, 
     ( void * ) UpdateLake, 
     ( void * ) FreeLake },

   { "moisture",    
     ( void * ) GetMoisture, 
     ( void * ) PutMoisture, 
     DeleteMoisture,
     ( void * ) UpdateMoisture, 
     ( void * ) FreeMoisture },

   { "power",       
     ( void * ) GetPower, 
     ( void * ) PutPower, 
     DeletePower, 
     ( void * ) UpdatePower, 
     ( void * ) FreePower },

   { "curpc",   
     ( void * ) GetCurPC ,
     ( void * ) PutCurPC ,
     DeleteCurPC,
     ( void * ) UpdateCurPC ,
     ( void * ) FreeCurPC },

   { "curpp",   
     ( void * ) GetCurPP ,
     ( void * ) PutCurPP ,
     DeleteCurPP,
     ( void * ) UpdateCurPP ,
     ( void * ) FreeCurPP },

   { "pressure",
     ( void * ) GetPressure, 
     ( void * ) PutPressure, 
     DeletePressure,
     ( void * ) UpdatePressure, 
     ( void * ) FreePressure },

   { "procvalue",   
     ( void * )  GetProcValue, 
     ( void * ) PutProcValue, 
     DeleteProcValue, 
     ( void * ) UpdateProcValue, 
     ( void * ) FreeProcValue },

   { "radiation",   
     ( void * ) GetRadiation, 
     ( void * ) PutRadiation, 
     DeleteRadiation,
     ( void * ) UpdateRadiation, 
     ( void * ) FreeRadiation },

   { "rawpc",   
     ( void * ) GetRawPC ,
     ( void * ) PutRawPC ,
     DeleteRawPC,
     ( void * ) UpdateRawPC ,
     ( void * ) FreeRawPC },
     
   { "rawpp",   
     ( void * ) GetRawPP ,
     ( void * ) PutRawPP ,
     DeleteRawPP ,
     ( void * ) UpdateRawPP ,
     ( void * ) FreeRawPP} ,
    
   { "rawpother",   
     ( void * ) GetRawPother ,
     ( void * ) PutRawPother ,
     DeleteRawPother ,
     ( void * ) UpdateRawPother ,
     ( void * ) FreeRawPother} ,
     
   { "snow",   	    
     ( void * ) GetSnow, 
     ( void * ) PutSnow, 
     DeleteSnow, 
     ( void * ) UpdateSnow, 
     ( void * ) FreeSnow },   

   { "temperature", 
     ( void * ) GetTemperature, 
     ( void * ) PutTemperature, 
     DeleteTemperature,
     ( void * ) UpdateTemperature, 
     ( void * ) FreeTemperature },

   { "waterquality",
     ( void * ) GetWaterQuality, 
     ( void * ) PutWaterQuality, 
     DeleteWaterQuality,
     ( void * ) UpdateWaterQuality, 
     ( void * ) FreeWaterQuality },   	

   { "wind",
     ( void * ) GetWind, 
     ( void * ) PutWind, 
     DeleteWind, 
     ( void * ) UpdateWind, 
     ( void * ) FreeWind },   	

   { "weather",     
     ( void * ) GetWeather, 
     ( void * ) PutWeather, 
     DeleteWeather,
     ( void * ) UpdateWeather, 
     ( void * ) FreeWeather },

   { "yunique",
      ( void * ) GetYUnique, 
      ( void * ) PutYUnique, 
      DeleteYUnique,
      ( void * ) UpdateYUnique, 
      ( void * ) FreeYUnique },
};


const long _num_obs_function_sets = 
   		(sizeof _obs_function_set) / sizeof(ObsFunctions);



long	find_function_set_index(const char *table)
{
 
   	/*
   		This functions looks through the function set C table
		to find the matching SQL table name.  It returns the
		index to the table.  If the index is not found, it
		returns -1.
   	*/
   
   	long	index = -1;
   	int	i;
	char	tablename[BUFSIZ];
	
	
	/*
		Copy to tablename and convert to lower case
	*/
	for (i = 0; ( tablename[i] = tolower(table[i]) ) ; i++);
   

	/*
		Search through the table
	*/
	for (i = 0; i < _num_obs_function_sets; i++)
	{
		if (strcmp(tablename, _obs_function_set[i].tablename) == 0)
		{
			index = i;
			break;
		}   
	}
   
	return index;
}   


Observation * GetObservation(const char *where, const char *table)
{
   	long 		index;
	Observation	*obsHead = NULL;
   
	index = find_function_set_index(table);   
 	
	if (index > -1)
		obsHead = 
		   (Observation *) _obs_function_set[index].get( (char *) where);

	
	return obsHead;
}   


int	PutObservation(Observation *obsPtr, const char *table)
{
	long 	index;
	int	rv = -1;
	
	
	index = find_function_set_index(table);  
	
	if (index > -1)	   
		rv = _obs_function_set[index].put((void *) obsPtr);
 	

	return rv;
}   




int	UpdateObservation(Observation *obsPtr, const char *where,
			  const char *table)
{   
	long 	index;
	int	rv = -1;
	
	
	index = find_function_set_index(table);  
	
	if (index > -1)	   
		rv = _obs_function_set[index].update(obsPtr, (char *) where);	   
   
	return rv;
}




int	DeleteObservation(const char *where, const char* table)
{
		   
   	long 	index;
	int	rv = -1;
	
	
	index = find_function_set_index(table);  
	
	if (index > -1)	   
		rv = _obs_function_set[index].delete((char *) where);	
	
	return rv;
   
}


void	FreeObservation(Observation *obsPtr)
{
   
	long 	index = 0;
	
	_obs_function_set[index].free(obsPtr);	
	
	return;
}



