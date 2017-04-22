/*
	File:		rating_util.h
	Date:		December 1999
	Author:		Russell Erb
			Sung Vo
	
	Purpose:	
*/

#ifndef rating_util_h
#define rating_util_h


/*	Defines.	*/

#define RATING_CONVERT_FAILED -9999.


/*	Includes.	*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "Forecast.h"
#include "List.h"
#include "Observation.h"
#include "Rating.h"
#include "RejectedData.h"
#include "time_convert.h"


/*	type definitions	*/

/* This structure is used by the load_rating_curve routine. It buffers
   all of the rating curve information from the Rating table into
   memory. */
typedef struct RatingInfo
{
   Node node ;
   char lid [ 9 ] ;
   int count ;
   Rating * pRatingHead ;
   List list ;
} RatingInfo ; 

/*	Function prototypes.	*/

float stage2discharge(char *lid, float stage);
float discharge2stage(char *lid, float discharge);
float stage2discharge_buff(char *lid, float stage);
float discharge2stage_buff(char *lid, float discharge);
void free_rating_curve ( ) ;
Rating * load_rating_curve ( char * lid ) ;

void    setRejectedDataObs(Observation *obs, RejectedData *rejectObs,
                           float old_value);
void    setRejectedDataFcst(Forecast *fcst,  RejectedData *rejectObs,
                           float old_value);
void	createUpdDelWhereObs(char *where, Observation *obsPtr);
void	createUpdDelWhereFcst(char *fcst_where, Forecast *fcstPtr);

#endif
