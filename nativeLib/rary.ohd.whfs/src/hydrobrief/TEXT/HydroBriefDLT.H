#ifndef HYDRO_BRIEF_DLT_HXX
#define HYDRO_BRIEF_DLT_HXX 


#include "RiverNetwork.H"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C"
{
     
#include "DbmsAccess.h"
}



class HydroBriefDLT
{
   
   
public:
   
     HydroBriefDLT(char *initDatabaseName);
     virtual ~HydroBriefDLT(); 
     
     RiverNetwork * loadRiverNetwork();
   
private:
   
     virtual int openDatabase();
     virtual int closeDatabase();
     int isDbConnectionOpen(); 
     
     int isDbOpen;
     
     char databaseName[80];
    
};

//**************************************************************************

#endif
