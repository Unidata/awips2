/***********************************************************************
* Filename: write_decoded_dsp.c
*
* Original Author: Feng Ding
*
* File Creation Date: July 20, 2006
*
* Development Group: OHD
*
* Description:
* Contains routine for writing decoded DSP data into a file.
* 
* Modules:
* write_decoded_dsp
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "decode_radar_product.h"

/***********************************************************************
* Module Name: write_decoded_dsp
*
* Original Author: Feng Ding
*
* Module Creation Date: June 29, 2006
*
* Description:
*   This subroutine writes the array containing the decoded storm-total 
*     rainfall to a file.
*
*   the units of the rainfall values in the file -- mm.
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* ptrDBZ       Input        short int **  dbz data array
* filename     Input        char *        output file name
* scale_factor Input        short int     scale factor
* beg_date     Input        short int     storm beginning date
* beg_time     Input        short int     storm beginning time
* end_date     Input        short int     storm ending date
* end_time     Input        short int     storm ending time
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
* 
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

void write_decoded_dsp(short int ** ptrDBZ, const char* filename,
                        const short int scale_factor, 
                        const short int opermode,
                        const short int beg_date, const short int beg_time,
                        const short int end_date, const short int end_time,
                        const int radar_lat, const int radar_lon, 
                        int* ier)
{
    int num_bin_1km[MAX_IHRAP][MAX_JHRAP];
    float radar_1km[MAX_IHRAP][MAX_JHRAP];
    float tmp_radar_1km[MAX_IHRAP][MAX_JHRAP];
    float sum_rad_1km[MAX_IHRAP][MAX_JHRAP];
    float precip[MAX_AZIMUTH][MAX_RANGE];
    int i, j;
    int range, azimuth;
    short head[6];

    *ier = 0 ;

    for(i = 0; i < MAX_AZIMUTH; i++)
    {
        for(j = 0; j < MAX_RANGE; j++)
        {
            precip[i][j] = 0.0 ;
        }        
    }

    for(i = 0; i < MAX_AZIMUTH; i++)
    {
        for(j = 0; j < MAX_RANGE; j++)
        {
            precip[i][j] = (float)ptrDBZ[i][j] * (float)scale_factor * 0.254;
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
            index = MAX_JHRAP + 1 - (j + 1)  - 1;
    
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
                    += precip[azimuth - 1][range - 1];
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
        printf ( "ERROR:in write_decoded_dsp.c, "
                "cannot open output file: %s\n", filename) ;
        return ;
    }

    printf("decoded file: %s\n", filename);

    /*
     * Write data to file.
     */

    head[0] = beg_date;
    head[1] = beg_time;
    head[2] = opermode;
    head[3] = end_date;
    head[4] = end_time;
    head[5] = opermode;

    size_t record_length = 6 * sizeof(short);
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
