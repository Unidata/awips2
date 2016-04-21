/***********************************************************************
* Filename: wrtodb_DHRRadar.c
*
* Original Author: Feng Ding
*
* File Creation Date: 
*
* Development Group: OHD
*
* Description:
* Contains routine for inserting a record into the DHRRadar table.
*
* Modules:
* wrtodb_DHRRadar
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "DHRRadar.h"
#include "decode_radar_product.h"
#include "time_convert.h"

/***********************************************************************
* Module Name: wrtodb_DHRRadar
*
* Original Author: Feng Ding
*
* Module Creation Date: 
*
* Description:
*   This subroutine inserts a record into the DHRRadar table.
*
*    calling function: decodeDHR
*
* Calling Arguments:
* Name             Input/Output Type          Description
* obstime          Input        char *      The obstime string
* radid            Input        char *        The radar id
* volcovpat        Input        short int   
* opermode         Input        short int   
* dbz_min          Input        float 
* dbz_inc          Input        float 
* dbz_cnt          Input        float 
* j_date           Input        short int 
* j_time           Input        short int 
* mean_field_bias  Input        short int 
* sample_size      Input        short int 
* grid_filename    Input        char * 
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
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer     Action
*             Feng Ding     Build the research version
* 7/03/2006   Guoxian Zhou  Build the operational version
*
***********************************************************************/

void wrtodb_DHRRadar(char * obstime,          const char * radid, 
                     const short volcovpat,   const short opermode,
                     const float dbz_min,     const float dbz_inc,
                     const float dbz_cnt,     const short j_date, 
                     const short j_time,      const short mean_field_bias, 
                     const short sample_size, const char *grid_filename)
{
    DHRRadar fppp;
    int status;
    dtime_t edttm;

    strcpy(fppp.radid, radid);

    status = yearsec_ansi_to_dt(obstime, &edttm) ;
    if(status != 0)
    {
        printf("error number %d", status);
        printf(" generating edttm in function wrtodb_DHRRradar ");
        printf("-- record not written to database\n");
        exit(0);
    }
    fppp.obstime = edttm;

    fppp.volcovpat = volcovpat;
    fppp.opermode = opermode;
    fppp.dbzmin = dbz_min;
    fppp.dbzinc = dbz_inc;
    fppp.dbzcnt = dbz_cnt;
    fppp.j_date = j_date;
    fppp.j_time = j_time;    
    fppp.mean_field_bias = mean_field_bias;
    fppp.sample_size = sample_size;
    strcpy(fppp.grid_filename, grid_filename);

    InsertOrUpdateDHRRadar(&fppp);

}  
