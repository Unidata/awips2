/*
	File:		agencyoffice_typedefs.h
	Date:		March 1998
	Author:		Paul Taylor
	
	Purpose:	Provides support for the
			Cooperating Agencies/Offices DS.
	
*/


#ifndef agencyoffice_typedefs_h
#define agencyoffice_typedefs_h


#include "LocExtAgency.h"
#include "AgencyOfficeUnique.h"


/*
	Structures.
*/
typedef struct AgencyOfficeRec
{
   
    AgencyOfficeUnique *availableHead;
    LocExtAgency *selectedHead;
    
    char lid[30];
 
} AgencyOfficeRec;




#endif



