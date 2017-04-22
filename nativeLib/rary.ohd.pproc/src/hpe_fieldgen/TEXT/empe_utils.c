#include <sys/stat.h>
#include <sys/types.h>

#include "empe_fieldgen.h"

char currTime[HHMMSS_LEN + 1] = {'\0'} ;
char message[MESSAGE_LEN] = {'\0'} ;

FILE * logFile = NULL;

static char logFilePath[PATH_LEN] = {'\0'} ;

short  * ptr1DShort  = NULL;
int    * ptr1DInt    = NULL;
float  * ptr1DFloat  = NULL;
double * ptr1DDouble = NULL;

short  ** ptr2DShort  = NULL;
int    ** ptr2DInt    = NULL;
float  ** ptr2DFloat  = NULL;
double ** ptr2DDouble = NULL;

const char * HPE_LOG_DIR_TOKEN   = "hpe_log_dir";
const char * HPE_DEBUG_LOG_TOKEN = "hpe_debug_log";

static int hpe_debug_log = -1;
 
/************************************************************************
 * This function loads token value.
 * If token is not available, return -1; otherwise return 0.
 ************************************************************************/

int hpe_fieldgen_getAppsDefaults(const char* strToken, char* strTokenValue)
{
    int tokenLen, tokenValueLen;

    tokenLen = strlen(strToken);

    get_apps_defaults((char *)strToken, &tokenLen,
                      strTokenValue, &tokenValueLen);

    if (tokenValueLen == 0)
    {
    	strcpy(strTokenValue, "");
        return -1;
    }

    return 0;
}

/************************************************************************
 * This function checks if the string only contains digit data [0,9].
 * If yes, return 1, otherwise return 0.
 ************************************************************************/

int hpe_fieldgen_isDigits(const char* str)
{
    char  * tmpString = NULL;
    int i, size, status;

    size = strlen(str);
    if (size < 1)
    {
        return 0;
    }

    tmpString = (char *)malloc((size + 1) * sizeof(char));
    if(tmpString == NULL)
    {
        sprintf ( message , "ERROR: Malloc memory failure in \"isDigits\""
                  " function.\n\tProgram exit.");
        shutdown( message );    	
    }

    status = 1; 

    memset(tmpString, '\0', size + 1);
    strcpy(tmpString, str);

    for(i = 0; i < size; i++)
    {
        if(!isdigit(tmpString[i]))
        {
        	status = 0;
            break;
        }        
    }    
    
    if(tmpString != NULL)
    {
        free(tmpString);    	
    }
    return status;
}

/************************************************************************
 * This function returns the current time in format: HH:MM:SS
 ************************************************************************/

void hpe_fieldgen_getCurrentTime(char* strCurrTime)
{
    struct tm *tmCurrTime;
    time_t t_time;
    
    memset(strCurrTime, '\0', HHMMSS_LEN + 1) ;
    time(&t_time);

    tmCurrTime = gmtime(&t_time);
    strftime(strCurrTime, HHMMSS_LEN + 1, "%H:%M:%S", tmCurrTime);
}

/************************************************************************
 * This function converts a string to lower case.
 ************************************************************************/

char* hpe_fieldgen_toLowerCase(char* str)
{
    int i;
    for(i = 0; i < strlen(str); i++)
    {
        str[i] = tolower(str[i]);
    }
    return str;    
}


/************************************************************************
 * This function opens the log file for debug version.
 * The file name is based on the token "hpe_log_dir"
 * and the datetime string from input struct tm data.
 ************************************************************************/

void hpeOpenDebugLogFile(const time_t tDate)
{
    static char strFileName[FNAME_LEN] = {'\0'};
    static char tmpTime[20] = {'\0'};
    struct tm * pDate = NULL ;

    if(hpe_fieldgen_getAppsDefaults(HPE_LOG_DIR_TOKEN, logFilePath) == -1)
    {
        sprintf ( message , "ERROR: Token value for token \"%s\""
                  " is not available.\n\tProgram exit.", HPE_LOG_DIR_TOKEN);
        shutdown( message );
    }
    
    pDate = gmtime(&tDate) ;

    strftime(tmpTime, 13, "%Y%m%d%H%M", pDate);    

    sprintf(strFileName, "%s/HPE%sZ", logFilePath, tmpTime);

    if((logFile = fopen(strFileName, "w")) == NULL)
    {
        sprintf ( message , "ERROR: can't open log file: %s"
                    "\n\tProgram exit.", strFileName) ;
        shutdown( message );
    }
}


/************************************************************************
 * This function opens the log file for operational version 
 * which has minimum log message.
 * The file name is based on the token "hpe_log_dir"
 * and the date string from input struct tm data.
 ************************************************************************/

void hpeOpenOperateLogFile(const time_t tDate)
{
    static char strFileName[FNAME_LEN] = {'\0'};
    static char tmpTime[20] = {'\0'};
    struct tm * pDate = NULL ;

    if(hpe_fieldgen_getAppsDefaults(HPE_LOG_DIR_TOKEN, logFilePath) == -1)
    {
        sprintf ( message , "ERROR: Token value for token \"%s\""
                  " is not available.\n\tProgram exit.", HPE_LOG_DIR_TOKEN) ;
        shutdown( message );
    }
    
    pDate = gmtime(&tDate) ;

    strftime(tmpTime, 9, "%Y%m%d", pDate);    

    sprintf(strFileName, "%s/HPE%sZ", logFilePath, tmpTime);

    if((logFile = fopen(strFileName, "a+")) == NULL)
    {
        sprintf ( message , "ERROR: can't open log file: %s"
                    "\n\tProgram exit.", strFileName) ;
        shutdown( message );
    }
}

/************************************************************************
 * This function opens the log file.
 ************************************************************************/

void hpeOpenLogFile(const time_t tDate)
{
    static char strLogFlag[10] = {'\0'};

    hpe_debug_log = 0;

    if(hpe_fieldgen_getAppsDefaults(HPE_DEBUG_LOG_TOKEN, strLogFlag) != -1)
    {
        if(strcmp(hpe_fieldgen_toLowerCase(strLogFlag), "on") == 0)
        {
            hpe_debug_log = 1;
        }
    }

    if(hpe_debug_log == 1)
    {
        hpeOpenDebugLogFile(tDate);
    }
    else
    {
        hpeOpenOperateLogFile(tDate);
    }
}



/************************************************************************
 * This function deletes the temporary log files
 * and close the main log file.
 ************************************************************************/

void hpeDeleteLogFile()
{
    if(logFile != NULL)
    {
        fclose(logFile);
        logFile = NULL;
    }

}

/************************************************************************
 * This function prints out the program usage message.
 ************************************************************************/

void hpe_fieldgen_printUsage()
{
    printf("\n\t********************************************************");
    printf("\n\tUSAGE:");
    printf("\n\t       (1) hpe_fieldgen -n NN -t HHMM -d MMDDYYYY -l MM");
    printf("\n\twhere\n");
    printf("\n\t             NN = number to be run.");
    printf("\n\t           HHMM = ending hour/minute of runs (Z time).");
    printf("\n\t       MMDDYYYY = ending date of runs (Z time).");
    printf("\n\t             MM = lag minutes of runs (Z time).");
    printf("\n\t********************************************************\n\n");
}

/************************************************************************
 * This function prints out a log message.
 ************************************************************************/

void printLogMessage(const char* message)
{
    if(logFile != NULL)
    {
        fprintf(logFile, "%s\n", message);
    }
    else
    {
        printf("%s\n", message);
    }
}

/************************************************************************
 * This function prints out a message.
 ************************************************************************/

void hpe_fieldgen_printMessage(const char* message)
{
    if(hpe_debug_log == 1)
    {
        printLogMessage(message);
    }
}

/************************************************************************
 * This function shuts down the program and releases resources.
 ************************************************************************/

void shutdown(const char* message)
{
    printLogMessage(message) ;
    
    hpeDeleteLogFile();

    CloseDbms();

    exit(-1);
}


/************************************************************************
* Purpose:
* This function builds the category name based on 
* the given time value.
*
* calling function: 
* functions called: none
*
* input variables
*
* timeValue - time value
*             unit: minute
*             Not greater than 99 days 
*
* output variables
*
* categoryName - category name for the given time value
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   09/19/2006   Guoxian Zhou      first version 
*
************************************************************************/

void buildCategoryName(const int timeValue, char categoryName[4])
{

    /*      
     * build the category string for the given time
     */

    if(timeValue <= 99)
    {
        /*
         * the category is minute if
         * the duration is less than 99 minutes.
         */

         sprintf(categoryName, "M%02d", timeValue);
    }
    else
    {
        /*
         * the category is hour if
         * the duration is great than 99 minutes,
         * but less than 99 hours.
         */

         int catHours = timeValue / 60 ;
         if( catHours <= 99 )
         {
             sprintf(categoryName, "H%02d", catHours);
         }
         else
         {
            /*
             * the category is day if
             * the duration is great than 99 hour,
             * but less than 99 days.
             */

            int catDays = catHours / 24 ;
            if( catDays <= 99 )
            {
                 sprintf(categoryName, "D%02d", catDays);
            }
            else
            {
                sprintf( message , "INVALID time value: %d.\nProgram exit.", 
                                timeValue) ;
                shutdown( message );
             }
         }
    }
}

/************************************************************************
* Purpose:
* This function computes the size of the HRAP box in km 
* given latitude in fractinal degrees.
*
* calling function: gage_only
* functions called: none
*
* input variables
*
* flat - latitude in fractional degrees
*
* output variables
*
* rmesh - side of the HRAP bin in km
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   June 2005    Guoxian Zhou      finish conversion to C Language 
*
************************************************************************/

void hpe_fieldgen_hrapsize(const double flat , double * rmesh)
{
    const double pi = 3.141592654 ;
    const double dtorad = pi / 180.0 ;
    
    *rmesh = 4.7625 * (1.0 + sin(flat * dtorad))
             / (1.0 + sin(60.0 * dtorad)) ;
    return ;
}


/************************************************************************
* Purpose:
* converts HRAP to lat-lon.
*
* calling function: 
* functions called: none
*
* input variables
*
* hrap_x - global HRAP x-coordinate
* hrap_y - global HRAP y-coordinate
*
* output variables
*
* flon   - longitude in fractional degrees
* flat   - lattitude in fractional degrees
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   June 2005    Guoxian Zhou      finish conversion to C Language 
*
************************************************************************/

void hpe_fieldgen_hrap_to_latlon(const double hrap_x ,
                const double hrap_y ,
                double * flon ,
                double * flat )
{
    double earthr = 6371.2 ;
    double stlon = 105.0 ;
    double pi = 3.141592654 ;
    double raddeg = 180.0/pi ;
    double xmesh = 4.7625 ;
    double tlat = 60./raddeg ;
    double x = hrap_x - 401.0 ;
    double y = hrap_y - 1601.0 ;
    double rr = x * x + y * y ;
    double gi = ((earthr * (1.0 + sin(tlat)))/xmesh) ;
    gi = gi * gi ;
    *flat = asin((gi - rr) / (gi + rr)) * raddeg ;
    double ang = atan2(y,x) * raddeg ;

    if(ang < 0.0)
    {
        ang += 360.0 ;
    }

    *flon = 270.0 + stlon - ang ;

    if(*flon < 0.0)
    {
        *flon += 360.0 ;
    }

    if(*flon > 360.0)
    {
        *flon -= 360.0 ;
    }
}


/************************************************************************
* Purpose:
* converts lat-lon to quarter HRAP.
*
* calling function: 
* functions called: none
*
* input variables
*
* lon   - longitude in fractional degrees
* lat   - latitude in fractional degrees
*
* output variables
*
* scale_row - global scaled HRAP x-coordinate
* scale_col - global scaled HRAP y-coordinate
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*  08/01/2006    Guoxian Zhou      first version
*
************************************************************************/

void LatLongToScaledHrap(const double lat ,
                         const double lon ,
                         const double factor,
                         double * scale_row ,
                         double * scale_col )
{
    double row, col;
    LatLongToHrapByReference(lat, lon, &row, &col);
    *scale_row = row * factor ;
    *scale_col = col * factor ;
}


/************************************************************************
* Purpose:
* converts HRAP/quarter HRAP to lat-lon.
*
* calling function: 
* functions called: none
*
* input variables
* scale_row - global HRAP/quarter HRAP x-coordinate
* scale_col - global HRAP/quarter HRAP y-coordinate
*
* output variables
*
* lon   - longitude in fractional degrees
* lat   - latitude in fractional degrees
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*  11/22/2006    Guoxian Zhou      first version
*
************************************************************************/

void ScaledHrapToLatLong(const double scale_row,
                         const double scale_col,
                         const double factor,
                         double * lat ,
                         double * lon )
{
    double row = scale_row;
    double col = scale_col;
    
    if(factor > 1.0)
    {
        row /= factor ;
        col /= factor ;
    }

    HrapToLatLongByReference(row, col, lat, lon );
}


/************************************************************************
 * compare_radar_id()
 * 
 * Compare function used for the binary search of the id list
 * done when checking if the location is a valid one.
 ************************************************************************/

int compare_radar_id ( void * search_value,
                       void * array_value )
{

   /* declare and define the values for two variables mapped
      against the input arguments */

   char * pSearchValue = ( char * ) search_value ;
   radar_result_struct * pArrayValue = ( radar_result_struct * ) array_value ;

   /* return the result of the simple string comparison */

   return ( strcmp ( pSearchValue , pArrayValue->radID) ) ;
}



/************************************************************************
* Purpose:
* fill in a value for two-dimensional float array.
*
* calling function: 
*
* input variables
*
* rows - row size of the array
* cols - column size of the array
* value - the value for setting the whole array
* pArray - two-dimensional float array.
*
* output variables
* pArray - two-dimensional float array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/08/2006   Guoxian Zhou      first version
*
************************************************************************/

void fill2DFloatArray(float ** pArray, 
                      const float value,
                      const int rows,
                      const int cols )
{
    int i, j;
    for(i = 0; i < rows; i++)
    {
        for( j = 0; j < cols; j++)
        {
            pArray[i][j] = value ;
        }
    }
}


/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for one-dimensional short int array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr1DShort - one-dimension short int array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

short * init1DShortArray(const short defaultValue,
                         const int rowSize)
{
    int i;

    ptr1DShort = NULL;
    ptr1DShort = (short *)malloc(rowSize * sizeof(short)); 
    if(ptr1DShort == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in Init1DShortArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr1DShort[i] = defaultValue ;
    }

    return ptr1DShort;

}


/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for two-dimensional short int array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* colSize - column size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr2DShort - two-dimension short int array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

short ** init2DShortArray(const short defaultValue,
                          const int rowSize,
                          const int colSize )
{
    int i, j;

    ptr2DShort = NULL;
    ptr2DShort = (short **)malloc(rowSize * sizeof(short *)); 
    if(ptr2DShort == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in Init2DShortArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr2DShort[i] = (short *)malloc(colSize * sizeof(short)); 
        if(ptr2DShort[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in Init2DShortArray function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        for( j = 0; j < colSize; j++)
        {
            ptr2DShort[i][j] = defaultValue ;
        }
    }

    return ptr2DShort;

}

/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for one-dimensional int array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr1DInt - one-dimension int array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

int * init1DIntArray(const int defaultValue,
                     const int rowSize)
{
    int i;

    ptr1DInt = NULL;
    ptr1DInt = (int *)malloc(rowSize * sizeof(int)); 
    if(ptr1DInt == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in Init1DIntArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr1DInt[i] = defaultValue ;
    }

    return ptr1DInt;

}



/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for two-dimensional int array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* colSize - column size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr2DInt - two-dimension int array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

int ** init2DIntArray(const int defaultValue,
                      const int rowSize,
                      const int colSize )
{
    int i, j;

    ptr2DInt = NULL;
    ptr2DInt = (int **)malloc(rowSize * sizeof(int *)); 
    if(ptr2DInt == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in init2DIntArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr2DInt[i] = (int *)malloc(colSize * sizeof(int)); 
        if(ptr2DInt[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in init2DIntArray function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        for( j = 0; j < colSize; j++)
        {
            ptr2DInt[i][j] = defaultValue ;
        }
    }
    return ptr2DInt;

}


/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for one-dimensional float array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr1DFloat - one-dimension float array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

float * init1DFloatArray(const float defaultValue,
                         const int rowSize)
{
    int i;

    ptr1DFloat = NULL;
    ptr1DFloat = (float *)malloc(rowSize * sizeof(float)); 
    if(ptr1DFloat == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in Init1DFloatArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr1DFloat[i] = defaultValue ;
    }

    return ptr1DFloat;

}



/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for two-dimensional float array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* colSize - column size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr2DFloat - two-dimension float array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

float ** init2DFloatArray(const float defaultValue,
                          const int rowSize,
                          const int colSize )
{
    int i, j;

    ptr2DFloat = NULL;
    ptr2DFloat = (float **)malloc(rowSize * sizeof(float *)); 
    if(ptr2DFloat == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in init2DFloatArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr2DFloat[i] = (float *)malloc(colSize * sizeof(float)); 
        if(ptr2DFloat[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in init2DFloatArray function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        for( j = 0; j < colSize; j++)
        {
            ptr2DFloat[i][j] = defaultValue ;
        }
    }
    return ptr2DFloat;

}


/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for one-dimensional double array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr1DDouble - one-dimension double array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

double * init1DDoubleArray(const double defaultValue,
                           const int rowSize)
{
    int i;

    ptr1DDouble = NULL;
    ptr1DDouble = (double *)malloc(rowSize * sizeof(double)); 
    if(ptr1DDouble == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in Init1DDoubleArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr1DDouble[i] = defaultValue ;
    }

    return ptr1DDouble;

}


/************************************************************************
* Purpose:
* dynamically allocate memory and fill in default value
* for two-dimensional double array.
*
* calling function: 
*
* input variables
*
* rowSize - row size of the array
* colSize - column size of the array
* defaultValue - default value for array initialization
*
* output variables
* ptr2DDouble - two-dimension double array.
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

double ** init2DDoubleArray(const double defaultValue,
                            const int rowSize,
                            const int colSize )
{
    int i, j;

    ptr2DDouble = NULL;
    ptr2DDouble = (double **)malloc(rowSize * sizeof(double *)); 
    if(ptr2DDouble == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in init2DFloatArray function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < rowSize; i++)
    {
        ptr2DDouble[i] = (double *)malloc(colSize * sizeof(double)); 
        if(ptr2DDouble[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in init2DFloatArray function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        for( j = 0; j < colSize; j++)
        {
            ptr2DDouble[i][j] = defaultValue ;
        }
    }
    return ptr2DDouble;

}


/************************************************************************
* Purpose:
* free memory for one-dimensional short int array.
*
* calling function: 
*
* input variables
*
* array   - one-dimension short int array.
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

void free1DShortArray(short * array )
{

    if(array != NULL)
    {
        free(array);
        array = NULL;
    }
}



/************************************************************************
* Purpose:
* free memory for two-dimensional short int array.
*
* calling function: 
*
* input variables
*
* array   - two-dimension short int array.
* rowSize - row size of the array
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

void free2DShortArray(short ** array, 
                      const int rowSize )
{
    int i;

    if(array != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(array[i] != NULL)
            {
                free(array[i]);
                array[i] = NULL;
            }
        }

        free(array);
        array = NULL;
    }
}


/************************************************************************
* Purpose:
* free memory for one-dimensional int array.
*
* calling function: 
*
* input variables
*
* array   - one-dimension int array.
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

void free1DIntArray(int * array )
{

    if(array != NULL)
    {
        free(array);
        array = NULL;
    }
}

/************************************************************************
* Purpose:
* free memory for two-dimensional int array.
*
* calling function: 
*
* input variables
*
* array   - two-dimension int array.
* rowSize - row size of the array
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

void free2DIntArray(int ** array, 
                    const int rowSize )
{
    int i;
    
    if(array != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(array[i] != NULL)
            {
                free(array[i]);
                array[i] = NULL;
            }
        }

        free(array);
        array = NULL;
    }

}


/************************************************************************
* Purpose:
* free memory for one-dimensional float array.
*
* calling function: 
*
* input variables
*
* array   - one-dimension float array.
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

void free1DFloatArray(float * array )
{

    if(array != NULL)
    {
        free(array);
        array = NULL;
    }
}



/************************************************************************
* Purpose:
* free memory for two-dimensional float array.
*
* calling function: 
*
* input variables
*
* array   - two-dimension float array.
* rowSize - row size of the array
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

void free2DFloatArray(float ** array, 
                      const int rowSize )
{
    int i;

    if(array != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(array[i] != NULL)
            {
                free(array[i]);
                array[i] = NULL;
            }
        }

        free(array);
        array = NULL;
    }

}


/************************************************************************
* Purpose:
* free memory for one-dimensional double array.
*
* calling function: 
*
* input variables
*
* array   - one-dimension double array.
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   12/06/2006   Guoxian Zhou      first version
*
************************************************************************/

void free1DDoubleArray(double * array )
{

    if(array != NULL)
    {
        free(array);
        array = NULL;
    }
}



/************************************************************************
* Purpose:
* free memory for two-dimensional double array.
*
* calling function: 
*
* input variables
*
* array   - two-dimension double array.
* rowSize - row size of the array
*
* output variables
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   11/28/2006   Guoxian Zhou      first version
*
************************************************************************/

void free2DDoubleArray(double ** array, 
                       const int rowSize )
{
    int i;

    if(array != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(array[i] != NULL)
            {
                free(array[i]);
                array[i] = NULL;
            }
        }

        free(array);
        array = NULL;
    }
}



/************************************************************************
* Purpose:
* expands/diminishs array.
*
* calling function: 
*
* input variables
*
* orig   - original two_dimension short-value array.
* orig_x - x-coordinate of the original array
* orig_y - y-coordinate of the original array
* target_x - x-coordinate of the target array
* target_y - y-coordinate of the target array
* missingValue - missing value in the array
*
* output variables
*
* target - target two_dimension short-value array.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   7/31/2006    Guoxian Zhou      first version
*
************************************************************************/

void convertShortArray(const int orig_x ,
                       const int orig_y ,
                       short ** source,
                       const short defaultValue ,
                       const int target_x ,
                       const int target_y ,
                       short ** target )
{
    int i, j, k, m;
    int row_factor, col_factor;
    int index_x, index_y;

    /*
     * expand the array
     */

    if( (orig_x <= target_x ) && (orig_y <= target_y ) )
    {
        row_factor = target_x / orig_x;
        col_factor = target_y / orig_y;

        for(i = 0; i < orig_x; i++)
        {
            for(j = 0; j < orig_y; j++)
            {
                for(k = 0; k < row_factor; k++)
                {
                    for(m = 0; m < col_factor; m++)
                    {
                        index_x = i * row_factor + k;
                        index_y = j * col_factor + m;
                        target[index_x][index_y] = source[i][j];                        
                    }
                }
            }
        }        
    }

    /*
     * diminish the array
     */

    else if( (orig_x >= target_x ) && (orig_y >= target_y ) )
    {
        int count[target_x][target_y];
        row_factor = orig_x / target_x;
        col_factor = orig_y / target_y;

        /*
         * initialize the target array and count array.
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                target[i][j] = 0;                        
                 count[i][j] = 0;                        
            }
        }

        /*
         * summarize for the target array
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                for(k = 0; k < row_factor; k++)
                {
                    for(m = 0; m < col_factor; m++)
                    {
                        index_x = i * row_factor + k;
                        index_y = j * col_factor + m;

                        if( source[index_x][index_y] != defaultValue)
                        {
                            target[i][j] += source[index_x][index_y];
                            count[i][j] ++;
                        }
                    }
                }
            }
        }

        /*
         * Average for the target array
         * if the count value great than 0,
         * otherwise, set it to missing.
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                if(count[i][j] > 0)
                {
                    target[i][j] /= count[i][j];
                }
                else
                {
                    target[i][j] = defaultValue;
                }        
            }
        }        
    }
}

void convertDoubleArray(const int orig_x ,
                        const int orig_y ,
                        double ** source ,
                        const double defaultValue ,
                        const int target_x ,
                        const int target_y ,
                        double ** target )
{
    int i, j, k, m;
    int row_factor, col_factor;
    int index_x, index_y;

    /*
     * expand the array
     */

    if( (orig_x <= target_x ) && (orig_y <= target_y ) )
    {
        row_factor = target_x / orig_x;
        col_factor = target_y / orig_y;

        for(i = 0; i < orig_x; i++)
        {
            for(j = 0; j < orig_y; j++)
            {
                for(k = 0; k < row_factor; k++)
                {
                    for(m = 0; m < col_factor; m++)
                    {
                        index_x = i * row_factor + k;
                        index_y = j * col_factor + m;
                        target[index_x][index_y] = source[i][j];                        
                    }
                }
            }
        }        
    }

    /*
     * diminish the array
     */

    else if( (orig_x > target_x ) && (orig_y > target_y ) )
    {
        int count[target_x][target_y];
        row_factor = orig_x / target_x;
        col_factor = orig_y / target_y;

        /*
         * initialize the target array and count array.
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                target[i][j] = 0;                        
                 count[i][j] = 0;                        
            }
        }

        /*
         * summarize for the target array
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                for(k = 0; k < row_factor; k++)
                {
                    for(m = 0; m < col_factor; m++)
                    {
                        index_x = i * row_factor + k;
                        index_y = j * col_factor + m;

                        if( fabs(source[index_x][index_y] - defaultValue)
                             > 0.001)
                        {
                            target[i][j] += source[index_x][index_y];
                             count[i][j] ++;
                        }
                    }
                }
            }
        }

        /*
         * Average for the target array
         * if the count value great than 0,
         * otherwise, set it to missing.
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                if(count[i][j] > 0)
                {
                    target[i][j] /= count[i][j];
                }
                else
                {
                    target[i][j] = defaultValue;
                }        
            }
        }        
    }
}


void convertFloatArray(const int orig_x ,
                       const int orig_y ,
                       float ** source ,
                       const float defaultValue ,
                       const int target_x ,
                       const int target_y ,
                       float ** target )
{
    int i, j, k, m;
    int row_factor, col_factor;
    int index_x, index_y;

    /*
     * expand the array
     */

    if( (orig_x <= target_x ) && (orig_y <= target_y ) )
    {
        row_factor = target_x / orig_x;
        col_factor = target_y / orig_y;

        for(i = 0; i < orig_x; i++)
        {
            for(j = 0; j < orig_y; j++)
            {
                for(k = 0; k < row_factor; k++)
                {
                    for(m = 0; m < col_factor; m++)
                    {
                        index_x = i * row_factor + k;
                        index_y = j * col_factor + m;
                        target[index_x][index_y] = source[i][j];                        
                    }
                }
            }
        }        
    }

    /*
     * diminish the array
     */

    else if( (orig_x > target_x ) && (orig_y > target_y ) )
    {
        int count[target_x][target_y];
        row_factor = orig_x / target_x;
        col_factor = orig_y / target_y;

        /*
         * initialize the target array and count array.
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                target[i][j] = 0;                        
                 count[i][j] = 0;                        
            }
        }

        /*
         * summarize for the target array
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                for(k = 0; k < row_factor; k++)
                {
                    for(m = 0; m < col_factor; m++)
                    {
                        index_x = i * row_factor + k;
                        index_y = j * col_factor + m;

                        if( fabs(source[index_x][index_y] - defaultValue)
                            > 0.001)
                        {
                            target[i][j] += source[index_x][index_y];
                             count[i][j] ++;
                        }
                    }
                }
            }
        }

        /*
         * Average for the target array
         * if the count value great than 0,
         * otherwise, set it to missing.
         */

        for(i = 0; i < target_x; i++)
        {
            for(j = 0; j < target_y; j++)
            {
                if(count[i][j] > 0)
                {
                    target[i][j] /= count[i][j];
                }
                else
                {
                    target[i][j] = defaultValue;
                }        
            }
        }        
    }
}


/************************************************************************
* Purpose:
* Build an array by weight-average two arrays.
*
* calling function: 
*
* input variables
*
* prev_array - original two-dimensional array.
* post_array - original two-dimensional array.
* rows   - row value of the original array
* cols   - column value of the original array
* prev_weight - weight value of the first original array
* post_weight - weight value of the second original array
* missingValue - missing value in the array
*
* output variables
*
* target - target two-dimensional array.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   8/14/2006    Guoxian Zhou      first version
*
************************************************************************/

void interpolateArray(const int rows ,
                      const int cols ,
                      float ** prev_array ,
                      float ** post_array ,
                      const float missingValue ,
                      const float prev_weight ,
                      const float post_weight ,
                      float ** target )
{
    int i, j;
    float factor1, factor2;

    if(prev_weight + post_weight < 0.001)
    {
        return;
    }

    factor1 = prev_weight / (prev_weight + post_weight);
    factor2 = post_weight / (prev_weight + post_weight);

    for(i = 0; i < rows; i++)
    {
        for(j = 0; j < cols; j++)
        {
            if( (fabs(prev_array[i][j] - missingValue) > 0.001) &&
                (fabs(post_array[i][j] - missingValue) > 0.001) )
            {
                target[i][j] = prev_array[i][j] * factor1
                             + post_array[i][j] * factor2 ;
            }
            else
            {
                target[i][j] = missingValue ;
            }
        }        
    }
}

