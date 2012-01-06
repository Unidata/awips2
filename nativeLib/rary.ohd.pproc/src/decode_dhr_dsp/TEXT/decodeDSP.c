/***********************************************************************
* Filename: decodeDSP.c
*
* Original Author: Feng Ding
*
* File Creation Date: 
*
* Development Group: OHD
*
* Description:
* decoding a DSP product and outputting the storm-total rainfall
* in quarterly HRAP grid.
* 
* Modules:
* decodeDSP
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "closedb.h"
#include "decode_radar_product.h"
#include "Swap2Bytes.h"

/***********************************************************************
* Module Name: decodeDSP
*
* Original Author: Feng Ding
*
* Module Creation Date: 
* 
* Description:
*   This function for decoding a DSP product and outputting the storm-total
*   rainfall in quarterly HRAP grid.
*
*   calling function: main_decode 
*   functions called: wrtodb_DSPRadar, wrtodb_DSPAdapt,               
*                     write_decoded_dsp, get_adapt            
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* radid        Input        char *        radar id
* raw_filename Input        char *        raw DSP file name
* write_to_db  Input        int           flag indicates if need write to db.
* os           Input        OperSys       operation system.
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
*             Feng Ding     Build research version
* 7/20/2006   Guoxian Zhou  Build operational version 
*
***********************************************************************/

void decodeDSP(const char * radid,
               const char *raw_filename,
               const int write_to_db,
               const OperSys os)
{

    int i, j, temp_j, n, count, minute, len, numparm;
    int ier = 0;
    short hdr[75];
    short i2, hour, jul_day_num;
    long int irc;
 
    short int ** ptrDBZ = NULL; 

    unsigned char data[116] = {'\0'};
    short j_beg_date, j_beg_time, j_end_date, j_end_time;
    short scale_factor;
    short nradials, nbytes; 
    short start_angle, delta_angle;
    short mean_field_bias, sample_size;

    short volcovpat, opermode, minval, maxval, num_data_lev;
    char filename[FNAME_LEN] = {'\0'}; 
    char outname[FILEPATH_LEN] = {'\0'};

    static char *dsp_grid_dir_token = {"dsp_grid_dir"};
    static char *dsp_grid_dir = NULL;

    long radar_lat, radar_lon;
    unsigned long tmplong;
    unsigned short tmpshortr, tmpshortl; 

    float params[45];
    char cdate[10] = {'\0'};
    char obstime[22] = {'\0'};
    char begin_time[22] = {'\0'}, end_time[22] = {'\0'};
    char param46[2] = {'\0'};
    
    FILE * fp = NULL;
    
    div_t time;
    size_t num_elements ;

    /*
     * open the raw DSP file
     */

    if((fp = fopen(raw_filename, "rb")) == NULL)
    {
        printf("error opening raw DSP file -- product not decoded\n");
        exit(3);
    }

    /*
     * decode header portion of product
     *
     * read until finding 138 (DSP product ID number)
     * this signifies beginning of DSP product header
     */

    count = 0;
    for (;;)
    {
        n = fread(&i2, sizeof(short), 1, fp);
        if (n == EOF || n == 0)
        {
            printf("Error: EOF encountered before reading header");
            printf(" for %s -- product not decoded\n", raw_filename);
            fclose(fp);
    
            if ( write_to_db == 1 )
            { 
                closedb(&irc);
                if(irc != 0)
                {
                    printf("PostgreSQL error# %ld ", irc);
                    printf(" occurred attempting to close database \n");
                }
            }
    
            exit(2);
        }

        count++;
        if(os == OS_LINUX)
        {

             /*
              * The data is assumed Big Endian.
              * The bytes must be swapped.
              */

             num_elements = 1;
             Swap2Bytes_ (&i2, &num_elements) ;            
        }

        if (i2 == 138) break;
    }

    /*
     * read DSP product header
     */

    hdr[0] = i2;
    for(i = 1; i < 75; i++)
    {
        fread(&hdr[i], sizeof(short), 1, fp);
    }

    if(os == OS_LINUX)
    {

         /*      
          * The data is assumed Big Endian.
          * The bytes must be swapped.
          */

         num_elements = 75;
         Swap2Bytes_ (hdr, &num_elements) ;            
    }
    hdr[0] = i2;

    /*
     * check for product code 138 at headr location 15
     */

    if( hdr[15] != 138 )
    {
        printf("file %s does not contain a DSP product\n", raw_filename);
        fclose(fp);

        if ( write_to_db == 1 )
        { 
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ", irc);
                printf(" occurred attempting to close database \n");
            }
        }

        exit(2);
    }

    /*
     * Get the begining and ending date and time in Julian 
     */

    j_beg_date = hdr[26];
    j_beg_time = hdr[27]; 
    
    j_end_date = hdr[47];
    j_end_time = hdr[48]; 

    /*
     * Get the DSP begin date time 
     */

    strcpy(cdate, decode_dhr_dsp_convertJulianDate(j_beg_date));
     
    time = div(j_beg_time, 60);
    hour = (short) time.quot;
    minute = time.rem;

    /*
     * create ending date/time value  
     */

    sprintf(begin_time, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",
            cdate[4], cdate[5], cdate[6], cdate[7],
            cdate[0], cdate[1], cdate[2], cdate[3], hour,minute);

    /*
     * 24z product found 
     * change hour to 00z
     * change date to next day
     */

    if(hour == 24)
    {
        printf("24z product - hour changed to 00z, date changed to next day\n");
        hour = 0;

        jul_day_num = hdr[26];
        jul_day_num ++;
        strcpy(cdate, decode_dhr_dsp_convertJulianDate(jul_day_num));

        sprintf(begin_time, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",
                cdate[4], cdate[5], cdate[6], cdate[7],
                cdate[0], cdate[1], cdate[2], cdate[3], hour, minute);
    }

    /*
     * convert ending date/time form julian to z time
     */

    strcpy(cdate, decode_dhr_dsp_convertJulianDate(j_end_date));
    
    time = div(j_end_time, 60);
    hour = (short) time.quot;
    minute = time.rem;

    /*
     * create ending date/time value
     */

    sprintf(obstime, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",
            cdate[4], cdate[5], cdate[6], cdate[7],
            cdate[0], cdate[1], cdate[2], cdate[3], hour, minute);


    if(hour < 0 || hour > 24 || minute < 0 || minute > 60)
    {
        printf("Invalid end precip time found -- product not decoded\n");
        fclose(fp);

        if ( write_to_db == 1 )
        { 
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ", irc);
                printf(" occurred attempting to close database \n");
            }
        }

        exit(2);
    }

    if(hour == 24)
    {
        printf("24z product - hour changed to 00z, date changed to next day\n");
        hour = 0;
    
        jul_day_num = hdr[47];
        jul_day_num ++;
        strcpy(cdate, decode_dhr_dsp_convertJulianDate(jul_day_num));
    
        sprintf(obstime, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",
                cdate[4], cdate[5], cdate[6], cdate[7],
                cdate[0], cdate[1], cdate[2], cdate[3], hour,minute);
    }

    /*
     * print info to log
     * check for valid date and time   
     * 
     * NOTE:
     *  products with 24z time stamp have been determined to be valid products 
     *  update to PPS to fix this problem is in progress until fixed,
     *  code below will change time stamp of 24z products to 00z
     *  with date = next days's date.
     */

    printf("%s %s %02d:%02d  DSP decoding...\n",
            radid, cdate, hour, minute);

    /*
     * allocate memory and initialize ptrDBZ array
     */

    ptrDBZ = (short int **)malloc(MAX_AZIMUTH * sizeof(short int *)); 
    if(ptrDBZ == NULL)
    {
        printf ( "ERROR: memory allocation failure"
            " in decodeDSP function.\n\tProgram exit.\n") ;
        exit(-1);
    }
    for(i = 0; i < MAX_AZIMUTH; i++)
    {
        ptrDBZ[i] = (short int *)malloc(MAX_RANGE * sizeof(short int)); 
        if(ptrDBZ[i] == NULL)
        {
            printf ( "ERROR: memory allocation failure"
                " in decodeDSP function.\n\tProgram exit.\n") ;
            exit(-1);
        }

        for(j = 0; j < MAX_RANGE; j++)
        {
            ptrDBZ[i][j] = 0;
        }
    }    

    /*
     * Get scale factor
     */

    scale_factor = hdr[31];
 
    /*
     * decode data portion of product
     * 
     * read product symbology & header of data array packet
     */

    nradials = hdr[74];

    if( nradials != MAX_AZIMUTH )
    {
        printf(" insufficient number of radials encountered -- ");
        printf("product not decoded\n");
        fclose(fp);

        if ( write_to_db == 1 )
        { 
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ", irc);
                printf(" occurred attempting to close database \n");
            }
        }

        exit(2);
    }

    for (i = 0; i < nradials; i++)
    {
        n = fread(&nbytes, sizeof(short), 1, fp);

        if(os == OS_LINUX)
        {

             /**      
              * The data is assumed Big Endian.
              * The bytes must be swapped.
              **/

             num_elements = 1;
             Swap2Bytes_ (&nbytes, &num_elements) ;            
        }

        if( nbytes != 116 )
        {
            printf("nbytes = %d\n", nbytes);
            printf(" data corruption encountered -- ");
            printf("product not decoded\n");
            fclose(fp);

            if ( write_to_db == 1 )
            { 
                closedb(&irc);
                if(irc != 0)
                {
                    printf("PostgreSQL error# %ld ", irc);
                    printf(" occurred attempting to close database \n");
                }
            }
    
            exit(2);
        }
    
        /*
         * read radial start angle, radial delta angle 
         */
    
        n = fread(&start_angle, sizeof(short), 1, fp);
        n = fread(&delta_angle, sizeof(short), 1, fp);
    
        n = fread(data, sizeof(unsigned char), nbytes, fp);
    
        if(n != nbytes)
        {
            printf(" loss of data encountered -- ");
            printf(" number of bytes expected = %d", nbytes);
            printf(" number of bytes found = %d \n", n);
            fclose(fp);
    
            if ( write_to_db == 1 )
            { 
                closedb(&irc);
                if(irc != 0)
                {
                    printf("PostgreSQL error# %ld ", irc);
                    printf(" occurred attempting to close database \n");
                }
            }
    
            exit(2);
        }
    
        /*
         * Throw out the last element (the 116th element) data[115]
         */
    
        for (j = 0; j < nbytes - 1; j++)
        {
            temp_j = 2 * j;
            ptrDBZ[i][temp_j] = (short)data[j];
            temp_j = 2 * j + 1;
            ptrDBZ[i][temp_j] = (short)data[j];
        }

    }  /* end of for loop on nradials */

    /*
     * search for and read adaptation parameters
     * if found, then write to DSPAdapt table
     */

    int status = get_adapt(&numparm, params, param46, fp) ;

    if(status != 0)
    {
        printf("    EOF encountered before finding adaptable parameters header");
        printf(" -- product not decoded\n");
        fclose(fp);

        if ( write_to_db == 1 )
        { 
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ", irc);
                printf(" occurred attempting to close database \n");
            }
        }

        exit(2);
    }

    volcovpat = hdr[17];
    opermode = hdr[16];
    minval = hdr[30];
    maxval = hdr[46];
    num_data_lev = hdr[32];
    mean_field_bias = hdr[29];
    sample_size = hdr[49];

    strcpy(end_time, obstime);

    sprintf(filename, "DSP%s%s%02d%02dZ", radid, cdate, hour, minute); 

    /*
     * Get radar location: lat and lon (in the unit of 0.001 deree)
     */

    tmpshortr = (unsigned short)hdr[11];
    tmpshortl = (unsigned short)hdr[10];

    tmplong = (unsigned long) tmpshortl;
    tmplong <<= 16;
    radar_lat = (long) (tmplong + (unsigned long)tmpshortr);

    tmpshortr = (unsigned short)hdr[13];
    tmpshortl = (unsigned short)hdr[12]; 

    tmplong = (unsigned long) tmpshortl;
    tmplong <<= 16;
    radar_lon = (long) (tmplong + (unsigned long)tmpshortr);

    /*
     * load and build the dsp grid filename 
     */

    if(dsp_grid_dir == NULL)
    {
        len = strlen(dsp_grid_dir_token);
        dsp_grid_dir = (char *)malloc( PATH_LEN * sizeof(char));
        get_apps_defaults(dsp_grid_dir_token, &len, dsp_grid_dir, &len);
    }

    if(dsp_grid_dir[len-1] == '/')
    {
        sprintf(outname, "%s%s", dsp_grid_dir, filename);
    }
    else
    {
        sprintf(outname, "%s/%s", dsp_grid_dir, filename);
    }

    /*
     * if header and decoding are good, then write
     * decoded precip data to file.
	 * write some header data to DSPRadar and adaptation data
	 * to DSPAdapt table.
     */

    ier = 0;
    write_decoded_dsp(ptrDBZ, outname, scale_factor, opermode,
                      j_beg_date,  j_beg_time,  j_end_date,  j_end_time,
                      radar_lat, radar_lon, &ier);

    if(ier != 0)
    {
        printf("error number %d during writing decoded file\n", ier);
    }
    else
    {
        wrtodb_DSPAdapt(obstime, radid, params, param46);
    
        wrtodb_DSPRadar(obstime, radid, volcovpat, opermode,
                        minval, maxval, num_data_lev, scale_factor, 
                        begin_time, end_time,  
                        j_beg_date,  j_beg_time,  j_end_date,  j_end_time,
                        mean_field_bias, sample_size, filename) ;
    }

    /*
     * close raw DSP file 
     */

    fclose(fp);
    fp = NULL;

    /*
     * releases memory for idbz pointer
     */

    if(ptrDBZ != NULL)
    {
        for(i = 0; i < MAX_AZIMUTH; i++)
        {
            if(ptrDBZ[i] != NULL)
            {
                free(ptrDBZ[i]);
                ptrDBZ[i] = NULL;
            }
        }
        free(ptrDBZ);
        ptrDBZ = NULL;
    }

    
    if(dsp_grid_dir != NULL)
    {
        free(dsp_grid_dir);
        dsp_grid_dir = NULL;
    }

}
