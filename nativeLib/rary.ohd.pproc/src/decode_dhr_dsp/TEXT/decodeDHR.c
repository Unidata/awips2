/***********************************************************************
* Filename: decodeDHR.c
*
* Original Author: Feng Ding
*
* File Creation Date: 
*
* Development Group: OHD
*
* Description:
* decoding a DHR product and outputting the rain rate in quarterly HRAP grid.
* 
* Modules:
* decodeDHR
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <math.h>
#include "closedb.h"
#include "decode_radar_product.h"
#include "Swap2Bytes.h"

/***********************************************************************
* Module Name: decodeDHR
*
* Original Author: Feng Ding
*
* Module Creation Date: 
* 
* Description:
*   This function for decoding a DHR product and outputting the rain rate
*   in quarterly HRAP grid.
*
*   calling function: main_decode 
*   functions called: get_adapt, write_decoded_dhr
*                     wrtodb_DHRAdapt, wrtodb_DHRRadar
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* radid        Input        char *        radar id
* raw_filename Input        char *        raw DHR file name
* write_to_db  Input        int           flag indicate if need write to db.
* os           Input        OperSys       operation system.
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* Swap2Bytes_, get_adapt, write_decoded_dhr, wrtodb_DHRRadar, wrtodb_DHRAdapt
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
* 7/19/2006   Guoxian Zhou  Build operational version 
*
***********************************************************************/

void decodeDHR(const char * radid,
               const char *raw_filename,
               const int write_to_db,
               OperSys os)
{
    int i, j, n, count, minute, len, ier, numparm;
    short i2, hdr[75], hour, jul_day_num;
    long int irc;

    short int ** ptrDBZ = NULL; 
    unsigned char strDBZData[MAX_RANGE] = {'\0'};
    short end_date, end_time;
    short num_radials, nbytes; 
    short radar_start_angle, radar_delta_angle;

    long radar_lat, radar_lon;
    unsigned long tmplong;
    unsigned short tmpshortr, tmpshortl; 

    short volcovpat, opermode; 
    float dbzmin, dbzinc, dbzcnt;
    float zrmult, zrexp, bias, mxpra;
    short bias_flag;  
    char  str_e_time[22] = {'\0'};
    short mean_field_bias, sample_size;

    float params[45];
    char cdate[10] = {'\0'};
    char filename[FNAME_LEN] = {'\0'}; 
    char outname[FILEPATH_LEN] = {'\0'};
    static char *dhr_grid_dir_token = {"dhr_grid_dir"};
    static char *dhr_grid_dir = NULL;
    char strdt[22] = {'\0'};
    char param46[2] = {'\0'};

    static int build_version = 8;

    char dhr_build_version[5] = {'\0'};

    FILE * fp = NULL;
 
    div_t time;
    size_t num_elements ;

    /*
     *  open the raw DHR file.
     */

    if((fp = fopen(raw_filename, "rb")) == NULL)
    {
        if ( write_to_db == 1 )
        {
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ", irc);
                printf(" occurred attempting to close database \n");
            }
        }
        printf("error opening raw DHR file -- product not decoded\n");
        exit(3);
    }

    /*
     * decode header portion of product
     * 
     * read until finding 32 (DHR product ID number)
     * this signifies beginning of DHR product header
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
            fp = NULL;

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

             /**      
              * The data is assumed Big Endian.
              * The bytes must be swapped.
              **/

             num_elements = 1;
             Swap2Bytes_ (&i2, &num_elements) ;            
        }

        if (i2 == 32) break;
    }

    /*
     *  read DHR product header
     */

    hdr[0] = i2;
    for(i = 1; i < 75; i++)
    {
        fread(&hdr[i], sizeof(short), 1, fp);
    }

    if(os == OS_LINUX)
    {

         /**      
          * The data is assumed Big Endian.
          * The bytes must be swapped.
          **/

         num_elements = 75;
         Swap2Bytes_ (hdr, &num_elements) ;            
    }
    hdr[0] = i2;

    /*
     * check for product code 32 at headr location 15
     */

    if( hdr[15] != 32 )
    {
        printf("file %s does not contain a DHR product\n",raw_filename);
        fclose(fp);
        fp = NULL;

        if ( write_to_db == 1 )
        { 
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ",irc);
                printf(" occurred attempting to close database \n");
            }
        }
        exit(2);
    }

    /*
     * Get the scan ending date and time in Julian
     */

    end_date = hdr[47];
    end_time = hdr[48]; 

    /*
     * convert ending date/time form julian to z time
     */

    strcpy(cdate, decode_dhr_dsp_convertJulianDate(end_date));

    time = div(end_time, 60);
    hour = (short) time.quot;
    minute = time.rem;

    /*
     * create ending date/time value
     */

    sprintf(strdt, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",
            cdate[4], cdate[5], cdate[6], cdate[7], cdate[0],
            cdate[1], cdate[2], cdate[3], hour, minute);


    if(hour < 0 || hour > 24 || minute < 0 || minute > 60)
    {
        printf("Invalid end precip time found -- product not decoded\n");
        fclose(fp);
        fp = NULL;

        if ( write_to_db == 1 )
        { 
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ",irc);
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
        jul_day_num++;
        strcpy(cdate, decode_dhr_dsp_convertJulianDate(jul_day_num));

        sprintf(strdt, "%c%c%c%c-%c%c-%c%c %02d:%02d:00",
                cdate[4], cdate[5], cdate[6], cdate[7],cdate[0],
                cdate[1], cdate[2], cdate[3], hour, minute);
    }

    /*
     * print info to log
     * check for valid date and time  
     * 
     * NOTE:
     * products with 24z time stamp have been determined to be valid products 
     * update to PPS to fix this problem is in progress
     * until fixed, code below will change time stamp of 24z products to 00z 
     * with date = next days's date
     */

    printf("%s %s %02d:%02d  DHR decoding...\n", radid, cdate, hour, minute);
 
    /*
     * allocate memory and initialize ptrDBZ array
     */

    ptrDBZ = (short int **)malloc(MAX_AZIMUTH * sizeof(short int *)); 
    if(ptrDBZ == NULL)
    {
        printf ( "ERROR: memory allocation failure"
            " in decodeDHR function.\n\tProgram exit.\n") ;
        exit(-1);
    }
    for(i = 0; i < MAX_AZIMUTH; i++)
    {
        ptrDBZ[i] = (short int *)malloc(MAX_RANGE * sizeof(short int)); 
        if(ptrDBZ[i] == NULL)
        {
            printf ( "ERROR: memory allocation failure"
                " in decodeDHR function.\n\tProgram exit.\n") ;
            exit(-1);
        }

        for(j = 0; j < MAX_RANGE; j++)
        {
            ptrDBZ[i][j] = 0;
        }
    }    

    /*
     * decode data portion of product
     * read product symbology & header of data array packet
     */

    num_radials = hdr[74];

    if( num_radials != MAX_AZIMUTH )
    {
        printf(" insufficient number of radials encountered -- ");
        printf("product not decoded\n");
        fclose(fp);
        fp = NULL;

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
 
    for (i = 0; i < num_radials; i++)
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

        if( nbytes != MAX_RANGE )
        {
            printf("nbytes = %d\n", nbytes);
            printf(" data corruption encountered -- ");
            printf("product not decoded\n");
            fclose(fp);
            fp = NULL;

            if ( write_to_db == 1 )
            { 
                closedb(&irc);
                if(irc != 0)
                {
                    printf("PostgreSQL error# %ld ",irc);
                    printf(" occurred attempting to close database \n");
                }
            }
            exit(2);
        }

        /*
         * read radial start angle, radial delta angle.
         * data need swap if it is in LINUX.
         */

        n = fread(&radar_start_angle, sizeof(short), 1, fp);
        n = fread(&radar_delta_angle, sizeof(short), 1, fp);

        n = fread(strDBZData, sizeof(unsigned char), nbytes, fp);

        if(n != nbytes)
        {
            printf(" loss of data encountered -- ");
            printf(" number of bytes expected = %d", nbytes);
            printf(" number of bytes found =%d \n", n);
            fclose(fp);
            fp = NULL;

            if ( write_to_db == 1 )
            { 
                closedb(&irc);
                if(irc != 0)
                {
                    printf("PostgreSQL error# %ld ",irc);
                    printf(" occurred attempting to close database \n");
                }
            }
            exit(2);
        }


        for (j = 0; j < nbytes; j++)
        {
            ptrDBZ[i][j] = (short)strDBZData[j];
        }
    }  /* end of for loop on nradials */

    /*
     * search for and read adaptation parameters
     * if found, then write to DHRAdapt table  
     */

    int status = get_adapt(&numparm, params, param46, fp) ;
    if( status != 0)
    {
        printf("EOF encountered before finding adaptable parameters header");
        printf(" -- product not decoded\n");
        fclose(fp);
        fp = NULL;

        if ( write_to_db == 1 )
        {
            closedb(&irc);
            if(irc != 0)
            {
                printf("PostgreSQL error# %ld ",irc);
                printf(" occurred attempting to close database \n");
            }
        }
        exit(2);
    }

    volcovpat = hdr[17];
    opermode = hdr[16];
    dbzmin = (float)hdr[30] / 10.0;
    dbzinc = (float)hdr[31] / 10.0;
    dbzcnt = (float)hdr[32];

    mean_field_bias = hdr[29];
    bias = (float)mean_field_bias / 100.0;

    sample_size = hdr[49];

    zrmult = params[9];
    zrexp = params[10];  

	/*
	 * load the DHR ORPG build version.
	 * Different versions have different adaptation data.
	 */

	len = strlen("dhr_build_version");
	
    get_apps_defaults("dhr_build_version", &len, dhr_build_version, &len);
    int build = atoi(dhr_build_version);
    if( (build == 5) || (build == 8) )
    {
    	build_version = build;
    }

    if(build_version == 5)
    {
    	mxpra = params[25];
    }
    else if(build_version >= 8)
    {
    	mxpra = params[19];
    }
    

    bias_flag = 0;
    if(strcmp(param46, "T") == 0) 
    {
        bias_flag = 1;
    }

    strcpy(str_e_time, strdt);

    sprintf(filename,"DHR%s%s%02d%02dZ", radid, cdate, hour, minute); 

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
     * load and build the dhr grid filename 
     */

    if(dhr_grid_dir == NULL)
    {
        len = strlen(dhr_grid_dir_token);
        dhr_grid_dir = (char *)malloc( PATH_LEN * sizeof(char));
        get_apps_defaults(dhr_grid_dir_token, &len, dhr_grid_dir, &len);
    }

    if(dhr_grid_dir[len-1] == '/')
    {
        sprintf(outname, "%s%s", dhr_grid_dir, filename);
    }
    else
    {
        sprintf(outname, "%s/%s", dhr_grid_dir, filename);
    }

    /*
     * if header and decoding are good, 
     * then write decoded idbz data to file.
	 * write some header data to DHRRadar and adaptation data
	 * to DHRAdapt table.
     */

    ier = 0;
    write_decoded_dhr(ptrDBZ, outname, dbzmin, dbzinc,
                      dbzcnt, zrmult, zrexp, mxpra,
                      bias, bias_flag, end_date, end_time,
                      opermode, radar_lat, radar_lon, &ier) ;

    if(ier != 0)
    {
        printf("error number %d during writing decoded file\n", ier);
    }
    else
    {
        wrtodb_DHRAdapt(strdt, radid, params, param46);

        wrtodb_DHRRadar(strdt, radid, volcovpat, opermode, dbzmin,
                        dbzinc, dbzcnt, end_date, end_time, 
                        mean_field_bias, sample_size, filename);  
    }
    
    /*
     * close raw DHR file.
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
    
    if(dhr_grid_dir != NULL)
    {
        free(dhr_grid_dir);
        dhr_grid_dir = NULL;
    }
}
