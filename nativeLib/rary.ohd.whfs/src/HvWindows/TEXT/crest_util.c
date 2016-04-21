/*
	File:		crest_util.c
	Date:		August 1995
	Author:		Dale Shelton
	
	Purpose:	Utility routines to calculate the 
			max/min stage/date.
			
*/


#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "Crest.h"
#include "crest_show.h"
#include "ToolDefs.h"
#include "DbmsUtils.h"


void	crest_stage_range(Crest *crest, double *max, double *min)
{
 	Crest		*crestPtr = NULL;
	double		maxstg,
	   		minstg;
	long		round,
	   		mod;
	

	
	/*
		Initialize values.
	*/
	if ( (crest)  && (crestPtr = (Crest *) ListFirst(&crest->list)) )
	{
		if (! IsNull(DOUBLE, (void*) &crestPtr->stage))
		{
		   	maxstg = crestPtr->stage;
	   		minstg = crestPtr->stage;
	   	}
	   	else
	   	{
   			maxstg = 10;
			minstg = 0;
		}
	}
	else
	{
		maxstg = 10;
		minstg = 0;
	}
		
	
	/*
		Iterate through and determine the max crest.
	*/
	while (crestPtr)
	{
	   	if (crestPtr->stage > maxstg)
		   	maxstg = crestPtr->stage;
		
		if (crestPtr->stage < minstg)
		   	minstg = crestPtr->stage;
		
		crestPtr = (Crest *) ListNext(&crestPtr->node);
	}
	

	/*
		Round off ranges to nice even values.
	*/
	round  = ceil(maxstg);
	if ( (mod = (round % 10)) )
	   	round += (10 - mod);
	maxstg = round + 10;
	
	round  = floor(minstg);
	if ( (mod = (round % 10)) )
	   	round -= mod;
	minstg = round - 10;
	

	/*
		Set return values.
	*/
	*max = maxstg;
	*min = minstg;
	return;
}



void	crest_date_range(Crest *crest, long *max, long *min)
{
   	Crest		*crestPtr = NULL;
 	long		maxyear,
	   		minyear,
			curyear,
			round,
			mod;
	
	
	/*
		Initialize local values.
	*/
	if (crest)
		crestPtr = (Crest *) ListFirst(&crest->list);
	maxyear  = 1000;
	minyear  = 3000;
	
	
	/*
		Iterate through and determine the max/min 
		date range in years.
	*/
	while (crestPtr)
	{
	   	curyear = get_year(crestPtr->datcrst);
			
		if (curyear > maxyear)
		   	maxyear = curyear;
		
		if (curyear < minyear)
		   	minyear = curyear;
		
	 	crestPtr = (Crest *) ListNext(&crestPtr->node);  
	}
	

	/*
		Round off years to the nearest decade.
	*/
	round   = ceil(maxyear);
	if ( (mod = (round % 10)) )
	   	round += (10 - mod);
	maxyear = round;
	
	
	round   = floor(minyear);
	if ( (mod = (round % 10)) )
	   	round -= mod;
	minyear = round;
	
	
	/*
		Set the return values.
	*/
	*max = maxyear;
	*min = minyear;

  	return;
	
}

long	get_year(long	datecrest)
{
   long yyyy = 0;
   
   yyyy = datecrest / 10000;
	
  	return yyyy;
}   


void	map_coord_bank(int action, double *min_data, double *max_data,
			   Position *min_win, Position *max_win)
{
   
   	static double		MinDataX,
	   			MaxDataX,
				MinDataY,
				MaxDataY;
	
	static Position 	MinWinX,
	   			MaxWinX,
				MinWinY,
				MaxWinY;
	
   
   
   	if (action == SET_X)
   	{
		MinDataX = *min_data;
		MaxDataX = *max_data;
		
		MinWinX = *min_win;
		MaxWinX = *max_win;
	}
	
	else if (action == GET_X)
	{
		*min_data = MinDataX;
		*max_data = MaxDataX;
		
		*min_win = MinWinX;
		*max_win = MaxWinX;
	}   
	
	
	else if (action == SET_Y)
   	{
		MinDataY = *min_data;
		MaxDataY = *max_data;
		
		MinWinY = *min_win;
		MaxWinY = *max_win;
	}
	
	else if (action == GET_Y)
	{
		*min_data = MinDataY;
		*max_data = MaxDataY;
		
		*min_win = MinWinY;
		*max_win = MaxWinY;
	}   
	   
	return;   
}   

void    set_x_coords(double min_data, double max_data,
		     Position min_win,	Position max_win)
{
	map_coord_bank(SET_X, &min_data, &max_data, &min_win, &max_win);   
   
}   

void    set_y_coords(double min_data, double max_data,
		     Position min_win,	Position max_win)
{
  	map_coord_bank(SET_Y, &min_data, &max_data, &min_win, &max_win); 
}   

void    get_x_coords(double *min_data, double *max_data,
		     Position *min_win,	Position *max_win)
{
   	map_coord_bank(GET_X, min_data, max_data, min_win, max_win);  
   	
}   

void    get_y_coords(double *min_data, double *max_data,
		     Position *min_win,	Position *max_win)
{
   	map_coord_bank(GET_Y, min_data, max_data, min_win, max_win); 
}   
