/*

        File:   Forecast.h

        Purpose:        Provides methods for accessing
                INFORMIX database for the purpose of
                selecting, inserting, updating, and
                deleting fields from the table.

        Note:   When using variables of type locator
                as structure members, the programmer is
                is responsible for setting the loc_size
                member of that structure when performing
                UPDATE and INSERT operations.
                Failure to set the size of this variable
                will provide unknown results.
*/


#ifndef Forecast_h
#define Forecast_h


/*
	Local library includes.
*/
#include "List.h"
#include "FcstHeight.h"

typedef FcstHeight Forecast;


/*
	Function prototypes.
*/
Forecast	*GetForecast(const char *where, const char *table);
void		FreeForecast(Forecast *);
int		PutForecast(Forecast *sp, const char *table);
int		UpdateForecast(Forecast *sp, const char *where, const char *table);
int		DeleteForecast(const char *where, const char* table);
long	        find_fcst_set_index(const char *table);
#endif
