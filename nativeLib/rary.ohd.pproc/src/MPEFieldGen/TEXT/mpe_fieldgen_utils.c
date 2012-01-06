#include "mpe_fieldgen.h"
#include "GeneralUtil.h"

#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>

char currTime[HHMMSS_LEN + 1];
char message[MESSAGE_LEN] ;

FILE * logFile ;

static char logFilePath[PATH_LEN] = {'\0'} ;

/********************************************************************
 * This function loads token value.
 * If token is not available, return -1; otherwise return 0.
 **/
int getAppsDefaults(const char* strToken, char* strTokenValue)
{
    int tokenLen, tokenValueLen;

    tokenLen=strlen(strToken);
    get_apps_defaults( ( char *) strToken,&tokenLen,strTokenValue,&tokenValueLen);

    if (tokenValueLen == 0)
        return -1;

    return 0;
}

/********************************************************************
 * This function checks if the string only contains digit data [0,9].
 * If yes, return 1, otherwise return 0.
 **/
int isDigits(const char* str)
{
    char string[128];
    int i, length;

    memset(string, '\0', 128);
    strcpy(string, str);
    length = strlen(string);
    if (length < 1)
        return 0;
    for(i = 0; i < length; i++)
    {
        if(! isdigit(string[i]))
            return 0;        
    }    
    return 1;
}

/********************************************************************
 * This function returns the current time in format: HH:MM:SS
 **/
void getCurrentTime(char* strCurrTime)
{
    struct tm *tmCurrTime;
    time_t t_time;
    
    memset(strCurrTime, '\0', HHMMSS_LEN + 1) ;
    time(&t_time);

    tmCurrTime = gmtime(&t_time);
    strftime(strCurrTime, HHMMSS_LEN + 1, "%H:%M:%S", tmCurrTime);
}

/********************************************************************
 * This function converts a string to lower case.
 **/
char* toLowerCase(char* str)
{
    int i;
    for(i = 0; i < strlen(str); i++)
    {
        str[i] = tolower(str[i]);
    }
    return str;    
}

/********************************************************************
 * This function opens the log files.
 * The file name is based on the token "rfcwide_logs_dir"
 * and the time string from input struct tm data.
 **/
void mpe_fg_openLogFile(const time_t tDate, int num_hours)
{
    static const char*    logToken = "rfcwide_logs_dir";
    static char    strFileName[FNAME_LEN];
    static char    tmpTime[12];
    struct tm * pDate = NULL ;

    if(getAppsDefaults(logToken, logFilePath) == -1)
    {
        sprintf ( message , "ERROR: Token value for token \"%s\" is not available."
                    "\n\tProgram exit.", logToken) ;
        shutDownMPE( message, logFile );
    }
    
    pDate = gmtime(&tDate) ;

    strftime(tmpTime, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pDate);    

    //sprintf(strFileName, "%s/RFCW%sZ", logFilePath, tmpTime);

/*
    if(num_hours == 1)
       sprintf(strFileName, "%s/mpe_rerun%sZ", logFilePath, tmpTime);
    else
       sprintf(strFileName, "%s/mpe_auto%sZ", logFilePath, tmpTime);
*/
    sprintf(strFileName, "%s/mpe_%02dhr%sZ", logFilePath, num_hours, tmpTime);
    
    if((logFile = fopen(strFileName,"w")) == NULL)
    {
        sprintf ( message , "ERROR: can't open log file:\"%s\"."
                    "\n\tProgram exit.", strFileName) ;
        shutDownMPE( message, logFile );
    }
}

/********************************************************************
 * This function deletes the temporary log files
 * and close the main log file.
 **/
void deleteLogFiles()
{
    if(logFile != NULL)
    {
        fclose(logFile);
        logFile = NULL;
    }

}

/**
 * This function prints out the program usage message.
 */
void printUsage()
{
    printf("\n\t****************************************************");
    printf("\n\tUSAGE:");
    printf("\n\t       (1) mpe_fieldgen NN.");
    printf("\n\t       (2) mpe_fieldgen NN HH MMDDYYYY.");
    printf("\n\twhere\n");
    printf("\n\t             NN = number of hours to be run.");
    printf("\n\t             HH = ending hour of runs (Z time).");
    printf("\n\t       MMDDYYYY = ending date of runs (Z time).");
    printf("\n\t****************************************************\n\n");
}

/**
 * This function shuts down the program and releases resources.
 */
void shutDownMPE(const char* message, FILE *log)
{
    printMessage(message, log) ;
    
    if(log != NULL)
    {
        fclose(log);
        log = NULL;     
    }

    CloseDbms();

    exit(-1);
}

/**
 * This function prints out a message.
 */
void printMessage(const char* message, FILE *log)
{
    if(log != NULL)
	{
        fprintf(log, "%s\n", message);
    }
	else
        printf("%s\n", message);
}

/****************************************************************************
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
****************************************************************************/

void hrapsize(const double flat , double * rmesh)
{
    const double pi = 3.141592654 ;
    const double dtorad = pi / 180.0 ;
    
    *rmesh = 4.7625 * (1.0 + sin(flat * dtorad))
             / (1.0 + sin(60.0 * dtorad)) ;
    return ;
}


/***********************************************************************
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
***********************************************************************/

void hrap_to_latlon(const double hrap_x ,
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
        ang += 360.0 ;
    *flon = 270.0 + stlon - ang ;
    if(*flon < 0.0)
        *flon += 360.0 ;
    if(*flon > 360.0)
        *flon -= 360.0 ;
}


/********************************************************************/

/* This is a utility function that can be used to retrieve 'R', 'G', 'B',
*  values from the /usr/lib/X11/rgb.txt file, given a color name. The color
*  name must be an exact match.
*/

void get_rgb(const char* color_name, int *r, int *g, int *b)
{
   char buf1[150];
   char buf2[150];
   char * p = NULL;
 
FILE * pFile = NULL;
 
      memset(buf1, '\0', 150);
      memset(buf2, '\0', 150);
  
  pFile = fopen ( "/usr/lib/X11/rgb.txt", "r" );
  if (pFile == NULL )
  {
     printf ("Could not open color file.\n");
     return;
  }
  p = fgets (buf1 , 150, pFile );
  p = fgets (buf1 , 150, pFile );
  while ( p != NULL )
  {
     memset(buf2, '\0', 150);
     sscanf ( buf1, "%d %d %d %s", r, g, b, buf2);
  
     if(!strcasecmp(buf2, color_name))
     {
        return;
     }

     p = fgets ( buf1, 150, pFile );
  }

  *r = 0;
  *g = 0;
  *b = 0;
  
  return; 

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
