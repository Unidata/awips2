#include "HydroBriefDLT.H"


//********************************************************************

HydroBriefDLT::HydroBriefDLT(char *initDatabaseName)
{
     //printf("inside HydroBriefDLT constructor\n");
     
     strcpy(databaseName, initDatabaseName);
   
     isDbOpen = 0;
   
     if (! isDbConnectionOpen())
          openDatabase();
     
     return;   
}

//********************************************************************

HydroBriefDLT::~HydroBriefDLT()
{
   
    
     closeDatabase();
     isDbOpen=0;
     // printf("inside HydroBriefDLT destructor\n");
   
   
     return;   
}

//********************************************************************

int HydroBriefDLT::openDatabase()
{
        
     //printf("HydroBriefDLT::openDatabase()\n");
     if (OpenDbms(databaseName) == Ok)
     { 
          printf("Opened database %s\n", databaseName);
	  isDbOpen = 1;
     }
     
     // if failed to open database, there is no reason to continue
     
     else
     {
          printf("HydroBriefDLT: Unable to open database %s\n", databaseName); 
	  isDbOpen = 0;
	  exit(-4);
     }
     
     return isDbOpen;   
}

//********************************************************************

int HydroBriefDLT::closeDatabase()
{
     if (isDbOpen)
     {
          CloseDbms();
          isDbOpen = 0;  
     }
     else
     {
 	  //printf("the database was already closed\n");	
     }
     
     return 1;   
}

//********************************************************************

RiverNetwork * HydroBriefDLT::loadRiverNetwork()
{
     RiverNetwork *network = NULL;
     RiverNetworkLoader *loader = NULL;
     
     // printf("HydroBriefDLT::loadRiverNetwork()\n");  
   
     if (! isDbConnectionOpen())
     {
     	   openDatabase(); 
     }
     
     if ( isDbConnectionOpen())
     {
	   loader = new RiverNetworkLoader();
	   network = new RiverNetwork(loader);
	   delete loader;
     }
     else
     {
 	   fprintf(stderr, "Error opening database %s\n", databaseName);	
     }
     
     return network;  
}

//********************************************************************
// use the State table to see if the database is open
int HydroBriefDLT::isDbConnectionOpen()
{
     int connected = 0;
     long count = recordCount("state"," ");
   
     if (count > 0)
	   connected = 1;
     
     return connected;
}
//********************************************************************
