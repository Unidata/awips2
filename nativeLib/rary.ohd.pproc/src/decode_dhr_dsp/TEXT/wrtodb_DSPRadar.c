/***********************************************************************
* Filename: wrtodb_DSPRadar.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 
*
* Development Group: OHD
*
* Description:
* Contains routine for inserting a record into the DSPRadar table.
*
* Modules:
* wrtodb_DSPRadar
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "DSPRadar.h"
#include "decode_radar_product.h"
#include "time_convert.h"

/***********************************************************************
* Module Name: wrtodb_DSPRadar
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 
*
* Description:
*   This subroutine inserts a record into the DSPRadar table.
*
*    calling function: decodeDSP
*
* Calling Arguments:
* Name             Input/Output Type          Description
* obstime          Input        char *        The obstime string
* radid            Input        char *        The radar id
* volcovpat        Input        short int   
* opermode         Input        short int   
* minval           Input        float 
* maxval           Input        float 
* num_data_lev     Input        float 
* scale_factor     Input        float 
* start_time       Input        char *        The start time string
* end_time         Input        char *        The end time string
* j_beg_date       Input        short int 
* j_beg_time       Input        short int 
* j_end_date       Input        short int 
* j_end_time       Input        short int 
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
* 7/19/2006   Guoxian Zhou  Build the operational version
*
***********************************************************************/

void wrtodb_DSPRadar(char *obstime,            const char *radid, 
                     const short volcovpat,    const short opermode,
                     const float minval,       const float maxval,
                     const float num_data_lev, const float scale_factor, 
                     char *begin_time,         char *end_time,  
                     const short j_beg_date,   const short j_beg_time,  
                     const short j_end_date,   const short j_end_time,
                     const short mean_field_bias, 
                     const short sample_size, const char *grid_filename)
{
    DSPRadar fppp;
    int status;
    dtime_t edttm;

    strcpy(fppp.radid, radid);

	status = yearsec_ansi_to_dt(obstime, &edttm);
    if(status != 0 )
    {
        printf("error number %d", status);
        printf(" generating edttm in function wrtodb_DSPRadar ");
        printf("-- record not written to database\n");
        exit(0);
    }
    fppp.obstime = edttm;

    fppp.volcovpat = volcovpat;
    fppp.opermode = opermode;
    fppp.minval = minval;
    fppp.maxval = maxval;
    fppp.num_data_lev = num_data_lev;
    fppp.scale_factor = scale_factor;

	status = yearsec_ansi_to_dt(begin_time, &edttm);
    if(status != 0 )
    {
        printf("error number %d", status);
        printf(" generating edttm in function wrtodb_DSPRadar ");
        printf("-- record not written to database\n");
        exit(0);
    }
    fppp.begin_time = edttm;

	status = yearsec_ansi_to_dt(end_time, &edttm);
    if(status != 0 )
    {
        printf("error number %d", status);
        printf(" generating edttm in function wrtodb_DSPRadar ");
        printf("-- record not written to database\n");
        exit(0);
    }
    fppp.end_time = edttm;

    fppp.j_beg_date = j_beg_date;
    fppp.j_beg_time = j_beg_time;
    fppp.j_end_date = j_end_date;
    fppp.j_end_time = j_end_time;
    fppp.mean_field_bias = mean_field_bias;
    fppp.sample_size = sample_size;
    strcpy(fppp.grid_filename, grid_filename);

    InsertOrUpdateDSPRadar(&fppp);

}  
