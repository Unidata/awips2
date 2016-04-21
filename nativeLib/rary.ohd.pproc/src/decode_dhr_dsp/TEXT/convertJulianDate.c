/***********************************************************************
* Filename: convertJulianDate.c
*
* Original Author: adopted from DPA decoder
*
* File Creation Date: 
*
* Development Group: OHD 
*
* Description:
* converts date to julian date string
*
* Modules:
* convertJulianDate
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "decode_radar_product.h"

/***********************************************************************
* Module Name: convertJulianDate
*
* Original Author: adopted from DPA decoder
*
* Module Creation Date: 
*
* Description:
*   This subroutine converts date to julian date string.
*
*   calling function: 
*
* Calling Arguments:
* Name       Input/Output   Type        Description
* jdate      Input          short int   date value 
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* 
*
* Return Value:
* Type      Description
* char *    The julian date string.
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type     Description
*
* Modification History:
* Date        Developer       Action
*             Feng Ding       Build the research version
* 7/20/2006   Guoxian Zhou    Build the operational version
*
***********************************************************************/

char *decode_dhr_dsp_convertJulianDate(short int jdate)
{

    static char cdate[9] = {'\0'};
    int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} ;
    int year, month, ndays, date, total, leap_year;
    
    total = 0;
    for (year = 1970; year < 2030; year++)
    {
        leap_year = FALSE;
        if (year == 1972 || year == 1976 || year == 1980 ||
            year == 1984 || year == 1988 || year == 1992 || year == 1996 ||
            year == 2000 || year == 2004 || year == 2008 || year == 2012 ||
            year == 2016 || year == 2020 || year == 2024 || year == 2028)
        {
            leap_year = TRUE;
        }

        for (month = 0; month < 12; month++)
        {
            total = total + days[month];
            if (month == 1 && leap_year == TRUE)
            {
                total ++;
            }

            if (total >= jdate)
            {
                ndays = days[month];
                if (month == 1 && leap_year == TRUE)
                {
                    ndays++;
                }
                date = ndays - (total-jdate);
                month ++;
                sprintf(cdate, "%02d%02d%04d", month, date, year);
                return cdate;
            }
        }
    }

    return NULL;
}
