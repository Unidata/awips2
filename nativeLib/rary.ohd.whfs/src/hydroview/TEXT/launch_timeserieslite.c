#include "launch_timeserieslite.h"

void callTimeSeriesLiteThroughJNI();

char getShefDurCodeFromIhfsDurCode(int intDuration);

// --------------------------------------------------------------------------
void launch_timeserieslite ( Widget map_widget , clicks  * mouse_clicks )
{
    
   char header[] = "launch_timeserieslite(): ";
    
   int  x;
   int  y;
   char * lid = NULL;
   char paramCode[10];
   char otherParamCode[10];

  // get point of clicking
   x = mouse_clicks->x ;
   y = mouse_clicks->y ;
   
   int isNewStation = 0;
   
   printf("Before startJVM()\n");
   startjvm() ;
   printf("After startJVM()\n");
      
   Station * stationPtr = locateStationWithShifting(x, y,  &isNewStation);

   if (stationPtr == NULL)
   {
        char message[BUFSIZ];
        sprintf(message, "No location has been selected. \n" 
                         "TimeSeriesLite cannot be launched. \n"
                         "Position the cursor closer to the station you would like displayed. ");
        InfoDialog(map_widget, message);
        return; 
   }
  
   
   lid = stationPtr->lid;
   strcpy(paramCode, stationPtr->paramCode);
    
   //determine the locationId (lid)
   //get the paramCode information for that currently displayed lid
   //getParamCode(stationPtr->lid, paramCode);
     
   if ((strchr(paramCode, '-')) != NULL) //is there a '-' in the paramCode ?
   {
        char message[BUFSIZ];
        sprintf(message, "This location's paramCode, %s, is incomplete.\n" 
                         "TimeSeriesLite cannot be launched for it. \n"
                         "If you are in The Point Data Control's Ad Hoc Mode,\n" 
                         "try switching to Time-Step Mode for this feature. ", paramCode);
        InfoDialog(map_widget, message);
        return;
   }

   if ( lid != NULL )
   {
      printf("%s paramCode = :%s: \n", header, paramCode);
    
      if ( strncmp(paramCode, "PP", 2) == 0)
      {
          strcpy(otherParamCode, "PCIRGZ"); //show 2 graphs if one is PP
      }
      else if (strncmp(paramCode, "PC", 2) == 0)
      {
           paramCode[2] = 'I';
      }
      else
      {
          strcpy(otherParamCode, "");
      }
      
      getFcstParamCode(lid, paramCode, otherParamCode);
      
      printf("%s paramCode = :%s: otherParamCode = :%s \n", 
             header, paramCode, otherParamCode); 
      
      displayTimeSeriesLite(lid, paramCode, otherParamCode);
   
   }
   else
   {
     
      /* Error message */
      ErrorDialog ( map_widget , "To launch TimeSeriesLite, you must first click near the station you are concerned with." ) ;
   
       printf("%s after ErrorDialog() clicked station lid = :%s: paramCode = :%s: \n", 
                  header, stationPtr->lid, stationPtr->paramCode);
  
   }
   
  
   return;
}

// --------------------------------------------------------------------------
void displayTimeSeriesLite(const char *lid, const char *paramCode, const char *fcstParamCode)
{
    
    
    // Launch TimeSeriesLite for PDC 
//   sprintf( commandString, "run_pdc_tsl %s %s %s &", lid, paramCode, otherParamCode);
 //   printf("launch_timeserieslite(): commandString =  :%s: \n", commandString);
 //     system ( commandString ) ;
 
    const char * jdbcUrlString = getenv("JDBCURL");
 
    callTimeSeriesLiteThroughJNI(lid, paramCode, fcstParamCode, jdbcUrlString);
    

    return;
}
// --------------------------------------------------------------------------

void getParamCode(const char * lid, char * paramCode)
{

    ReportList * reportlistHead = getReportListHead();
    ReportList * node = NULL;
    char shefDurCode = '\0';

    node = ( ReportList  * ) ListFirst( & reportlistHead->list ) ;

    while ( node != NULL )
    {
        if ( strcmp(node->lid, lid) == 0)
        {
            shefDurCode = getShefDurCodeFromIhfsDurCode(node->dur);
            sprintf(paramCode, "%s%c%s%s",
                             node->pe, shefDurCode,
                             node->ts, node->extremum);
            break;
                
        } 
  
        node = ( ReportList * ) ListNext (& node->node ) ;
    }

    return;
} 


// --------------------------------------------------------------------------
void getFcstParamCode(const char * lid, const char * paramCode, char * fcstParamCode)
{
    
    char pe[SHEF_PE_LEN + 1];
 //   char where[BUFSIZ];
    
    strncpy(pe, paramCode, 2);
    pe[2]='\0';
 /*   
    sprintf(where, "WHERE lid = '%s' and pe = '%s' ORDER BY ts_rank ASC", lid, pe);
    
    IngestFilter * ingestFilterHead = getIngestFilter(where);
    IngestFilter * node  = NULL;
    
    if (ingestFilterHead != NULL)
    {
    
        node = (IngestFilter *) ListFirst (ingestFilterHead->list);
   
        while ( node != NULL )
        {
           
            node = ( IngestFilter * ) ListNext (& node->node ) ;
        }
        
    }
 */
    sprintf(fcstParamCode, "%sIF-Z", pe);  

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

// --------------------------------------------------------------------------

