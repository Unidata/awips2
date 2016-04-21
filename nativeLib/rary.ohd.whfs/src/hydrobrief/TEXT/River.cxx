#include "River.H"

/******************************************************************/

River::River()
{
     riverStationArray = NULL;
     numRiverStations = 0;
     
     return;
}   

/******************************************************************/

River::River(RiverLoader *loader)
{
   
     setName(loader->getName());
     
     riverStationArray = loader->getRiverStationArray();
   
     numRiverStations = loader->getNumRiverStations();
 
     
     return;
}

/******************************************************************/

River::~River()
{
     
     return;
} 


/******************************************************************/

void River::print()
{
     int i;
     
     printf("inside River::print()\n");
     printf("River :%s: \n", name);
     
     for (i = 0; i < numRiverStations; i++)
     {
          riverStationArray[i].print();	  
     }
     
     return;
}   

/******************************************************************/
