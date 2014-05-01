#include "pointcontrol_loc_shift.h"

char getShefDurCodeFromIhfsDurCode(int intDuration); //from drawStations.h, which I don't want to include here

/*  hv_config_dir  */

/**************************************************************************************/

static PdcLocationShift *shiftArray = NULL;
static int shiftArraySize;
/**************************************************************************************/

PdcLocationShift * getPdcLocationShiftArray()
{
    return shiftArray;    
}
/**************************************************************************************/

void loadShiftValues()
{
   //char header[] ="loadShiftValues(): ";
   static int   first_time = 1;
   static char  hv_config_dir[BUFSIZ];
   static char  file_path[BUFSIZ];
   int          gad_token_len = 0;
   int          gad_value_len = 0;   
   
   /* get the location shift info the first time through here */
   
   if (first_time)
   {
      first_time = 0;
      
      gad_token_len = strlen("hv_config_dir");
      get_apps_defaults("hv_config_dir", &gad_token_len, hv_config_dir, &gad_value_len);
      if (strlen(hv_config_dir) <= 0)
      {
          strcpy(hv_config_dir, "/awips/hydroapps/whfs/local/data/app/hydroview");
          fprintf(stderr, "hv_config_dir undefined, using %s\n", hv_config_dir);
      }
      
      sprintf(file_path, "%s/pdc_loc_shift.txt", hv_config_dir);
    
   }    
     
  
   if (shiftArray != NULL)
   {
       free(shiftArray);
       shiftArray = NULL;  
       shiftArraySize = 0;
   }
  
   shiftArray = readShiftFile(file_path, &shiftArraySize);
   
   sortShiftArray(shiftArray, shiftArraySize);
    
   return;
    
}

/**************************************************************************************/

void applyShiftValues()
{
    //char header[] = " ------------------------------------- applyShiftValues():  ";
    int array_slot = 0;
    ReportList * rHead = getReportListHead(); 
    ReportList *rPtr = NULL;
    PdcLocationShift * shift = NULL;
   
    if (rHead == NULL)
    {
       return;    
    }
   
    rPtr = (ReportList *) ListFirst(& rHead->list);
    
    //for each Report, find the matching location Shift Value, 
    //if found, assign the x_shift and y_shift values to the ReportList node.
    while (rPtr != NULL)
    {
       
        shift = findPdcLocationShift(shiftArray, shiftArraySize, &array_slot, rPtr);
        
        if (shift != NULL)
        {
            rPtr->x_shift = shift->x_shift;
            rPtr->y_shift = shift->y_shift;           
        }
        else //shift == NULL
        {
            rPtr->x_shift = 0;
            rPtr->y_shift = 0;
        }
        
        rPtr = (ReportList *) ListNext(& rPtr->node);    
    } 
    
   
    return;    
}

/**************************************************************************************/
PdcLocationShift * findPdcLocationShift(PdcLocationShift * shift_array,
                                      int location_shift_count, 
                                      int *array_slot,
                                      ReportList * report)
{
   // char header[] = "findPdcLocationShift(): ";
    PdcLocationShift * foundShift = NULL;
    PdcLocationShift * candidateShift = NULL;
    char param_code[PARAM_CODE_LEN + 1];
    char search_key[LOC_ID_LEN + 1 + PARAM_CODE_LEN + 1];
    char candidate_key[LOC_ID_LEN + 1 + PARAM_CODE_LEN + 1];
    int compare_result = 0;
    int i = 0;
 
    getParamCodeFromReport(report, param_code);
    create_shift_key(report->lid, param_code, search_key);
    
 //   printf("%s report->lid  = :%s: param_code = :%s \n", header, report->lid, param_code);
 
      
    for (i = *array_slot; i < location_shift_count; i++)
    {
         candidateShift =  &(shift_array[i]);  
         create_shift_key(candidateShift->lid, candidateShift->param_code, candidate_key);
           
        // printf("%s candidate_key = :%s:\n", header, candidate_key); 
           
         compare_result = strcmp(search_key, candidate_key);
         if (compare_result > 0) //you haven't gotten to it yet
         {
            // continue to the next iteration, you might find it up ahead
         }
         else if (compare_result == 0)
         {
            *array_slot = i;  
            foundShift = candidateShift;
       
        //   printf("%s found a match for :%s:\n", header, search_key);
            break; 
         }
         else //compare_result < 0), the result is not in this list
         {
             foundShift = NULL;
      //       printf("%s did NOT find a match for :%s:\n", header, search_key); 
             break;
         }
        
    }
    
    return foundShift;
}
/**************************************************************************************/
void create_shift_key(const char * lid, const char * param_code, char * returned_key)
{
    sprintf(returned_key, "%s|%s", lid, param_code);
    
    return;    
}
/**************************************************************************************/
PdcLocationShift * readShiftFile(char * filePath, int *arraySizeParam)
{
    
    FILE *inFile = fopen(filePath, "r");
    char * line = NULL;
    int len = 0;
    int byteCount= 0;
    int arraySlots = INITIAL_SHIFTS;
    PdcLocationShift shift;
    int lineCount = 0;
    int scannedCount = 0;
    
    //shiftArray is file static   
    shiftArray = (PdcLocationShift *) malloc (sizeof(PdcLocationShift) * arraySlots  );
    
    if (inFile != NULL)
    {
        while ((byteCount = getline(&line, &len, inFile)) != -1) 
        {  
            if( line == NULL || line[0] == '\n') /*** Means no more entries  ***/
            {
               break;
            }
         
         
            scannedCount = sscanf(line, "%s %s %d %d", shift.lid, shift.param_code,
                                                           &shift.x_shift, &shift.y_shift);
            if (scannedCount == 4)
            {
                shiftArray[lineCount] = shift;   
            }
            
            if (line)
            {
                free(line);
                line = NULL;
            }
            
          
           lineCount++;
           
           if (lineCount >= arraySlots)
           {
                arraySlots = arraySlots * 2;
                shiftArray = (PdcLocationShift *) realloc (shiftArray, sizeof(PdcLocationShift) * arraySlots  );
           }
        } // end while
    }    
    
     if (lineCount != arraySlots)
     {
           arraySlots = lineCount;
           shiftArray = (PdcLocationShift *) realloc (shiftArray, sizeof(PdcLocationShift) * arraySlots  );
     }
     *arraySizeParam = arraySlots;
    
    if (inFile != NULL)
    {
        fclose(inFile);    
    }
    
    return shiftArray;
}

/**************************************************************************************/
void sortShiftArray(PdcLocationShift * shiftArrayToBeSorted, int arrayLength)
{
    
    char header[] = "sortShiftArray(): ";
    
      qsort((void *)shiftArrayToBeSorted, arrayLength,
            sizeof(PdcLocationShift),
            compareLocationShifts);
            
    int i = 0;   
    
    printf("%s arrayLength = %d\n", header, arrayLength);
    for (i = 0; i < arrayLength; i++)
    {
        printf("%s shiftArrayToBeSorted[i] = %s|%s x = %d y = %d\n",
             header, 
             shiftArrayToBeSorted[i].lid,
             shiftArrayToBeSorted[i].param_code,
             shiftArrayToBeSorted[i].x_shift,
             shiftArrayToBeSorted[i].y_shift);
        
    }        
    
    return;    
}

/**************************************************************************************/
int compareLocationShifts(const void * s1, const void * s2)
{
    PdcLocationShift * shift1  = (PdcLocationShift *) s1;    
    PdcLocationShift * shift2  = (PdcLocationShift *) s2;    
    
    char string1[LOC_ID_LEN + 1 + PARAM_CODE_LEN + 1];
    char string2[LOC_ID_LEN + 1 + PARAM_CODE_LEN + 1];
  
    sprintf(string1, "%s|%s", shift1->lid, shift1->param_code);
    sprintf(string2, "%s|%s", shift2->lid, shift2->param_code);
    
    int result = strcmp(string1, string2);
    
    return result;
}

/**************************************************************************************/


void printShiftArray(PdcLocationShift * shiftArray, int arraySize)
{
    int i = 0;
    PdcLocationShift shift;
    
    for (i = 0; i < arraySize; i++)
    {
        shift = shiftArray[i];
        
        printf("%s %s %d %d\n", shift.lid, shift.param_code, shift.x_shift, shift.y_shift);     
    }
    
     
}


/**************************************************************************************/
void getParamCodeFromReport(ReportList * report, char *paramCode)
{
    // paramcode must have already been allocated
    char     shefDurCode;
       
         
    if (strcmp(report->pe, "PC")  == 0 )
    {
        shefDurCode = 'I';    //PC is always "I", but sometimes the duration might have been screwed up
    } 
    else
    {
        shefDurCode = getShefDurCodeFromIhfsDurCode(report->dur);
    }
        
    sprintf(paramCode, "%s%c%s%s",
                     report->pe, shefDurCode,
                     report->ts, report->extremum);
    
    
    return;    
}
/**************************************************************************************/

char getShefDurCodeFromIhfsDurCode(int intDuration)
{
    
    /*
  0 | I       | Instantaneous
    1 | U       | 1 Minute
   15 | C       | 15 Minutes
   30 | J       | 30 Minutes
 1001 | H       | 1 Hour
 1002 | B       | 2 Hour
 1003 | T       | 3 Hour
 1004 | F       | 4 Hour
 1006 | Q       | 6 Hour
 1008 | A       | 8 Hour
 1012 | K       | 12 Hour
 1018 | L       | 18 Hour
 2001 | D       | 1 Day
 2007 | W       | 1 Week
 3001 | M       | 1 Month
 4001 | Y       | 1 Year
 5000 | Z       | Unspecified
 5001 | S       | Seasonal
 5002 | R       | Period of Record
 5004 | P       | Total Since 7 AM
 5005 | X       | Unknown
    
*/  
    
    
    char code = '?';
    
    
    int length = 22;
    
    char charCodeArray[22] = "IUCJHBTFQAKLDWMYZSRPX"; //21 characters that I care about
    
    int intCodeArray[22] = { 0, 1, 15, 30,
               1001, 1002, 1003, 1004, 1006, 1008, 1012, 1018, 
               2001, 2007,
               3001,
               4001,
               5000, 5001, 5002, 5004, 5005  };
               
              
    int i = 0;
    
    for (i = 0; i < length; i++)
    {
       if (intDuration == intCodeArray[i])
       {
           code = charCodeArray[i];
           break;   
       }        
    }
    
    if (code == '?')
    {
        printf("getShefDurCodeFromIhfsDurCode(): unidentified dur code = %d\n", intDuration);   
    }
    
    return code;

}
/**************************************************************************************/
