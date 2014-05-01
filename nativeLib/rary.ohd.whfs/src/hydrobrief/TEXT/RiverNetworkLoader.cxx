
#include "RiverNetworkLoader.H"


//******************************************************************************


RiverNetworkLoader::RiverNetworkLoader()
{
   
     char where[BUFSIZ];
     char prevRiverName[80];
     Riverstat *rHead = NULL;
     Riverstat *rPtr = NULL;
     HvStation *hHead = NULL;
     RiverLoader *riverLoader;
     
     long i = 0;    
     
     
     //printf("inside RiverNetworkLoader constructor()\n");
     
     //sprintf(where, " WHERE stream like '%%NORTH%%' ORDER BY stream, mile ");
     sprintf(where, " ORDER BY stream, mile DESC ");
     
  
     if ( ( rHead = GetRiverstat(where) ) != NULL )
     {
	  
	  if ( ( hHead = GetHvStation(" ORDER by lid ") ) != NULL )
	  {
	       
	       numRivers = getRiverCount(rHead);       
	       riverArray = new River[numRivers];
	       
	       
	       rPtr = (Riverstat * ) ListFirst(&rHead->list);
	       i = 0;
	       
	       
	       strcpy(prevRiverName, "");
	       
	       while (rPtr)
	       {
		    
		    if (strcmp(prevRiverName, rPtr->stream) != 0)
		    {
			 strcpy(prevRiverName, rPtr->stream);
			 
			 riverLoader = new RiverLoader(rPtr, hHead);
			 riverArray[i] = River(riverLoader);
			 delete riverLoader;
			 
			 //riverArray[i] = River();
			 //riverLoader.load(&riverArray[i], rPtr, hHead);
			 
			 i++;
			 
		    }
		    rPtr = (Riverstat *) ListNext(&rPtr->node);   
	       }
	       
	       
	       FreeHvStation(hHead);
	       
	  }
	  
	  FreeRiverstat(rHead);
     }
     
     return;  
 
}   


//******************************************************************************


RiverNetworkLoader::~RiverNetworkLoader()
{
     
     return;
} 


//******************************************************************************


/*
void RiverNetworkLoader::print()
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
*/

//******************************************************************************

int RiverNetworkLoader::getRiverCount(Riverstat *rHead)
{
     
       Riverstat *rPtr = NULL;
       char prevRiverName[BUFSIZ];
       int count = 0;
       
       rPtr = (Riverstat *) ListFirst(&rHead->list);
       
       strcpy(prevRiverName,"");
       
       count = 0;
       while (rPtr)
       {
	    if (strcmp(rPtr->stream, prevRiverName) != 0)
	    {
	         strcpy(prevRiverName, rPtr->stream);
		 count++;
	    }
	    
            rPtr = (Riverstat *) ListNext(&rPtr->node);		    
       }
     
       return count;
}

//******************************************************************************
