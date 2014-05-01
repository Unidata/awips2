/***********************************************************************
* Filename: write_decoded_dhr.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: June 29, 2006
*
* Development Group: OHD
*
* Description:
* Contains routine for writing decoded DHR data into a file.
* 
* This file is converted from FORTRAN file write_decoded_file.f. 
*
* Modules:
* write_decoded_dhr
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "decode_radar_product.h"

/***********************************************************************
* Module Name: write_decoded_dhr
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: June 29, 2006
*
* Description:
*   This subroutine writes the array containing the decoded rain rate
*     to a file.  
*
*   the units of the rain rate values in the file -- mm/hr .
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* ptrDBZ       Input        short int **  dbz data array
* filename     Input        char *        output file name
* dbz_min      Input        float         dbz output parameter
* dbz_inc      Input        float         dbz output parameter
* dbz_cnt      Input        float         dbz output parameter
* zrmult       Input        float         dbz output parameter
* zrexp        Input        float         dbz output parameter
* mxpra        Input        float         dbz output parameter
* bias         Input        float         dbz output parameter
* bias_flag    Input        short int     bias flag
* end_date     Input        short int     ending date
* end_time     Input        short int     ending time
* opermode     Input        short int     operate mode
* radar_lat    Input        int           radar latitude
* radar_lon    Input        int           radar longitude
* ier          Output       int *         status value
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* build_lookup_table, quarter_hrap_to_radar_azimuth
*
* Return Value:
* Type          Description
* int           The status of the writing data to file.
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
* 6/29/2006   Guoxian Zhou  Convert to C language
*
***********************************************************************/

void write_decoded_dhr(short int ** ptrDBZ, const char* filename,
                       const float dbz_min, const float dbz_inc,
                       const float dbz_cnt, const float zrmult,
                       const float zrexp,  const float mxpra,
                       float bias, const short int bias_flag,
                       const short int end_date, const short int end_time,
                       const short int opermode, const int radar_lat,
                       const int radar_lon, int* ier)
{
    int num_bin_1km[MAX_IHRAP][MAX_JHRAP];
    float dbz, logmult;
    float radar_1km[MAX_IHRAP][MAX_JHRAP];
    float tmp_radar_1km[MAX_IHRAP][MAX_JHRAP];
    float sum_rad_1km[MAX_IHRAP][MAX_JHRAP];
    float rate_pol[MAX_AZIMUTH][MAX_RANGE];
    float dbz_to_rate[NUM_LEVEL_DBZ];
    float min_rate, max_rate ;
    int i, j;
    int az, ra, bdbz_min, bdbz_max ;
    int range, azimuth;
    short head[3];

    *ier = 0 ;

    min_rate = MAX_RATE;
    max_rate = MIN_RATE;

    for(i = 0; i < MAX_AZIMUTH; i++)
    {
        for(j = 0; j < MAX_RANGE; j++)
        {
            rate_pol[i][j] = 0.0 ;
        }        
    }

    for(i = 0; i < NUM_LEVEL_DBZ; i++)
    {
        dbz_to_rate[i] = 0.0 ;
    }        

    /*
     * Generate lookup table for conversion of byte-compressed dBZs to
     * rainrate (mm/hr) based on Z-R parameter and maximum rain rate etc.
     */

    logmult = log10(zrmult);

    dbz_to_rate[0] = 0.0 ;        // below SNR
    dbz_to_rate[1] = 0.0 ;        // missing data

    /*
     * Compute byte-compressed dBZ corresponding to minimum resolvable
     * rainrate of 0.05 mm/hr (which rounds to 0.1 mm/hr) 
     * and maximum rainrate of 'mxpra' mm/hr.
     * 
     * For z=300r^1.4, bdbz_min is 79 and bdbz_max is 171
     * (for mxpra=103.8 mm/hr)
     */

    dbz = 10.0 * log10(zrmult * pow(0.05, zrexp)) ;
    bdbz_min = (dbz - dbz_min) / dbz_inc + 2.0 ;
    dbz = 10.0 * log10(zrmult * pow(mxpra, zrexp)) ;
    bdbz_max = (dbz - dbz_min) / dbz_inc + 2.0 ;

    for(i = 2; i < (int)dbz_cnt; i++)
    {    
        if(i < bdbz_min)
        {
            dbz_to_rate[i] = 0.0;
        }
        else if (i > bdbz_max)
        {
            dbz_to_rate[i] = mxpra ;
        }
        else
        {
            dbz = ((i - 2.0) * dbz_inc) + dbz_min ;
            dbz_to_rate[i] = pow(10.0, (dbz - 10.0 * logmult) / (10.0 * zrexp)) ;
        }
    }

    /*
     * For EMPE do not apply bias even bias flag is set.
     */

    if ( bias_flag != 1 )
    {
        bias = 1.0 ;
    }

    /*
     * Make sure bias is not equal to 0. due to known problems in 
     * writing bias info from PPS into DHR product
     */

    if ( bias == 0.0 )
    {
        bias = 1.0 ;
    }

    for(az = 0; az < MAX_AZIMUTH; az ++)
    {
        for(ra = 0; ra < MAX_RANGE; ra ++)
        {

            /*
             * Transform byte-compressed reflectivity to unbiased rainrate;
             * Utilize data from 2nd range gate in 1st gate
             * due to missing data there
             * 
             * Bias is not applied here because the bias will be applied in empe 
             * bmosaic.
             */

            if ( ra != 0 )
            {
//                rate_pol[az][ra] = dbz_to_rate[ptrDBZ[az][ra]] * bias ;
                rate_pol[az][ra] = dbz_to_rate[ptrDBZ[az][ra]] ;
            }
            else
            {
//                rate_pol[az][ra] = dbz_to_rate[ptrDBZ[az][1]] * bias ;
                rate_pol[az][ra] = dbz_to_rate[ptrDBZ[az][1]] ;
            }

            if(rate_pol[az][ra] < min_rate)
            {
                min_rate = rate_pol[az][ra] ;
            }

            if(rate_pol[az][ra] > max_rate)
            {
                max_rate = rate_pol[az][ra] ;
            }
        }
    }

    for(i = 0; i < MAX_IHRAP; i++)
    {
        for(j = 0; j < MAX_JHRAP; j++)
        {
            radar_1km[i][j]   = DEFAULT_RADAR_1KM ;
            num_bin_1km[i][j] = 0 ;
            sum_rad_1km[i][j] = 0.0 ;
        }
    }

	/*
	 * build lookup table for conversion between quarter HRAP(I,J) and 
	 * radar polar coordinate system(azimuth, range)
	 */

    build_lookup_table(radar_lat, radar_lon) ;
    int index ;

    for ( i = 0; i < MAX_IHRAP; i ++ )
    {
        for ( j = 0; j < MAX_JHRAP; j ++ )
        {
            index = MAX_JHRAP  - (j + 1) ;

            azimuth = quarter_hrap_to_radar_azimuth[i][j] ;

            if(azimuth > MAX_AZIMUTH)
            {
                azimuth = MAX_AZIMUTH;
            }

            range = quarter_hrap_to_radar_range[i][j] ;

            if ( (range != BEYOND_RANGE) && (azimuth != BEYOND_RANGE) )
            {
                if(azimuth  < 1 || azimuth > MAX_AZIMUTH)
                {
                    printf("Azimuth angle (%d, %d) out of range at [%d][%d]\n",
                    azimuth, range, j, i);
                }

                if(range < 1 || range > MAX_RANGE)
                {
                    printf("Range (%d, %d) out of range at [%d][%d]\n",
                    azimuth, range, j, i);
                }

                num_bin_1km[i][index] ++ ;

                sum_rad_1km[i][index] 
                    += rate_pol[azimuth - 1][range - 1];       
            }
        }
    }

    for ( i = 0; i < MAX_IHRAP; i ++ )
    {
        for ( j = 0; j < MAX_JHRAP; j ++ )
        {
            if ( num_bin_1km[i][j] > 0)
            {
                radar_1km[i][j] 
                    = sum_rad_1km[i][j] / num_bin_1km[i][j];

            }
        }
    }

    for(i = 0; i < NUM_ROW; i++)
    {
        for(j = 0; j < NUM_COL; j++)
        {
            tmp_radar_1km[i][j] = radar_1km[j][i];
        }
    }

    FILE * fp = NULL;

    if((fp = fopen(filename, "wb")) == NULL)
    {
        *ier = -1;
        printf ( "ERROR:in write_decoded_dhr.c, "
                "cannot open output file: %s\n", filename) ;
        return ;
    }
    printf("decoded file: %s\n", filename);

    /*
     * Write data to file.
     */

    head[0] = end_date;
    head[1] = end_time;
    head[2] = opermode;

    size_t record_length = 3 * sizeof(short);
    fwrite ( head, record_length, 1, fp );
    
    record_length = MAX_JHRAP * sizeof(float);

    for ( i = 0; i < MAX_IHRAP; i ++ )
    {
        fwrite ( tmp_radar_1km[i], record_length, 1, fp );
    }

    /* Close the file. */
    fclose ( fp );
    fp = NULL;

}
