/*

        File:

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


#ifndef Observation_h
#define Observation_h


/*
	Local library includes.
*/
#include "List.h"
#include "Height.h"

typedef Height Observation;


/*
	Function prototypes.
*/
Observation	*GetObservation(const char *where, const char *table);
void		FreeObservation(Observation *);
int		PutObservation(Observation *sp, const char *table);
int		UpdateObservation(Observation *sp, const char *where, const char *table);
int		DeleteObservation(const char *where, const char* table);
long	        find_function_set_index(const char *table);

#endif
