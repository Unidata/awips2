/*******************************************************************************
 * FILENAME:            get_prev_filename.c
 * NUMBER OF MODULES:   1
 * GENERAL INFORMATION:
 *   MODULE 1:          getPreviousFilename
 * DESCRIPTION:         Retrieves the name of the file which is closest to 
 *                      15 minutes before the current run mosaic data which is
 *                      also within wondow [10, 20] minutes.
 * 
 * ORIGINAL AUTHOR:     Guoxian Zhou
 * CREATION DATE:       January 10, 2008
 * ORGANIZATION:        OHD-11, HSEB
 * MACHINE:             Redhat Linux
 * MODIFICATION HISTORY:
 *   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
 *          1       1/10/2008     Guoxian Zhou      Original Coding
 ********************************************************************************
 */

#include <dirent.h>
#include <limits.h>
#include <unistd.h>

#include "empe_fieldgen.h"

#define SEARCH_WINDOW  5 * 60

/*******************************************************************************
 * MODULE NUMBER:  1
 *   MODULE 1:          getPreviousFilename
 * DESCRIPTION:         Retrieves the name of the file which is closest to 
 *                      15 minutes before the current run mosaic data.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
 *   In     char *      targetDir      The directory for searching
 *                                     the closest filename.
 *   In     time_t      targetTime     The run time of the current mosaic.
 *   Out    int *       timeDiff       The time difference (seconds) between
 *                                     the closest file and the current mosaic.
 *   Out    char *      outputFilename The filename of the closest file.
 * 
 * RETURNS:
 *   None   
 *
 * APIs UTILIZED:
 *   Only system routines are utilized. 
 *
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE  NAME      DESCRIPTION
 *
 * DATA FILES AND/OR DATABASE:
 *   None
 *
 * ERROR HANDLING:
 ********************************************************************************
 */

static char inputFilePath[PATH_LEN] = { '\0' };
static struct dirent ** namelist= NULL;
static int numFileFound = -1;

static void getDateString(const char * str, char * digitStr);
static time_t getTimeT(const char * strDateTime);

static void getWholeFilelist()
{
    numFileFound = scandir(inputFilePath, &namelist, 0, alphasort);
}

void getPreviousFilename(const char * targetDir, const time_t targetTime,
        int * timeDiff, char * outputFilename)
{
    int i;
    char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    time_t curr_time_t, old_time_t;

    int diftime = INT_MAX;
    *timeDiff = diftime;

    if ((namelist == NULL) || (strcmp(inputFilePath, targetDir) != 0))
    {
        strcpy(inputFilePath, targetDir);
        getWholeFilelist();
    }

    if (numFileFound > 0)
    {
        for (i = 0; i < numFileFound; i++)
        {
            if ((strcmp(namelist[i]->d_name, ".")) && (strcmp(
                    namelist[i]->d_name, "..")))
            {
                /*
                 * test if the file is mosaic xmrg file
                 */
                if (strstr(namelist[i]->d_name, "MOSAIC") == NULL)
                {
                    continue;
                }

                getDateString(namelist[i]->d_name, strDateTime);

                curr_time_t = getTimeT(strDateTime);

                if (abs(curr_time_t - targetTime) <= diftime)
                {
                    diftime = abs(curr_time_t - targetTime);
                    old_time_t = curr_time_t;
                } else
                {
                    /*
                     * the previous record has the closest date/time
                     * to the given date/time.
                     * retrieve the file name if the time difference
                     * for that record is less than the search window.
                     */
                    if (diftime < SEARCH_WINDOW)
                    {
                        strcpy(outputFilename, namelist[i - 1]->d_name);
                        *timeDiff = 15 * 60 - (old_time_t - targetTime);
                    }

                    break;
                }
            }
        }
    }
}

/*
 * This function retrieve the date string from the mosaic filename.
 */

void getDateString(const char * str, char * digitStr)
{
    int i, index, strLength;

    strLength = strlen(str);
    if (strLength < 1)
    {
        digitStr = NULL;
        return;
    }

    index = 0;
    for (i = 0; i < strLength; i++)
    {
        if (isdigit(str[i]))
        {
            digitStr[index++] = str[i];
        }
    }

    /*
     * add the seconds as "00"
     */
    digitStr[index++] = '0';
    digitStr[index++] = '0';
    digitStr[index++] = '\0';

}

/*
 * This function retrieve the date string from the mosaic filename.
 */

time_t getTimeT(const char * strDateTime)
{
    time_t timet;
    struct tm * pRunTime= NULL;
    int i;

    time(&timet);
    pRunTime = gmtime(&timet);

    char strYear[5] = { '\0' };
    char strMonth[3] = { '\0' };
    char strDay[3] = { '\0' };
    char strHour[3] = { '\0' };
    char strMinute[3] = { '\0' };

    int indx = 0;
    for (i = 0; i < 4; i++)
    {
        strYear[i] = strDateTime[i];
    }

    indx += 4;

    for (i = 0; i < 2; i++)
    {
        strMonth[i] = strDateTime[i + indx];
    }

    indx += 2;

    for (i = 0; i < 2; i++)
    {
        strDay[i] = strDateTime[i + indx];
    }

    indx += 2;

    for (i = 0; i < 2; i++)
    {
        strHour[i] = strDateTime[i + indx];
    }

    indx += 2;

    for (i = 0; i < 2; i++)
    {
        strMinute[i] = strDateTime[i + indx];
    }

    pRunTime->tm_year = atoi(strYear) - 1900;
    pRunTime->tm_mon = atoi(strMonth) - 1;
    pRunTime->tm_mday = atoi(strDay);
    pRunTime->tm_hour = atoi(strHour);
    pRunTime->tm_min = atoi(strMinute);
    pRunTime->tm_sec = 0;

    return gm_mktime(pRunTime);

}

void freeFilelistInfo()
{
    int jcnt;
    for (jcnt = 0; jcnt < numFileFound; jcnt++)
    {
        if (namelist[jcnt] != NULL)
        {
            free(namelist[jcnt]);
            namelist[jcnt] = NULL;
        }
    }

    if (namelist != NULL)
    {
        free(namelist);
        namelist = NULL;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/get_prev_filename.c,v $";
 static char rcs_id2[] = "$Id: get_prev_filename.c,v 1.2 2008/05/14 19:00:11 gzhou Exp $";}
/*  ===================================================  */

}

