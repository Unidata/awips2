
#include "RiverNetwork.H"


//******************************************************************************


RiverNetwork::RiverNetwork(RiverNetworkLoader *loader)
{
   
     riverArray = loader->getRiverArray();
     
     numRivers = loader->getNumRivers();
     
     return;
}   


//******************************************************************************

RiverNetwork::~RiverNetwork()
{
     
     return;
} 

//******************************************************************************

River * RiverNetwork::getRiver(int riverIndex)
{
  
      return &riverArray[riverIndex];
}

//****************************************************************************

void RiverNetwork::print()
{
     long i;
     
     //    printf("inside RiverNetwork::print()\n");
     
     //  printf("Number of Rivers = %ld\n", numRivers);
      
     for (i = 0; i < numRivers; i++)
     { 
	riverArray[i].print();
	  printf("\n\n");
     }
     
     return;
}   

//******************************************************************************
long RiverNetwork::getMaxNumStations()
{
     long max = 0;
     long num = 0;
     long i;
     
      
     for (i = 0; i < numRivers; i++)
     {
	  num = riverArray[i].getNumRiverStations();
	  
          if (num > max)     	
	  {
	       max = num;   
	  }
     }
     
     return max;
}
//******************************************************************************
