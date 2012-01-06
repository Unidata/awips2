/**********************************************************************
 * readClimoData ( )
 * 
 * This function reads 12 monthly-averaged precip data
 *********************************************************************/

#include "dqc_preproc_setup.h"

extern void read_xmrg ( const int * mx , const int * my , const int * irow ,
                    const char * fname , const int * length , int * ifile ,
                    short * mosaic ) ;

void qdc_preproc_setup_readxmrg(const char * os, const int rowSize, const int colSize, 
    const char * fname, const int lenf, const double factor,
    double ** xmrg , int * irc)
{
    int i, j ;
    short int * mosaic = NULL ;
    int num_elements ;

    mosaic = (short *)malloc(colSize * sizeof(short)); 
    if(mosaic == NULL)
    {
        fprintf ( stderr , "ERROR: memory allocation failure"
            " in readxmrg function."
            "\n\tProgram exit.\n") ;
            
        return ;
    }

    /*
     * note: mfactor = 100  could be a problem for large
     *          precip  accumulations (> 12 in)
     *          because resulting value could be > 32000
     *          (max for I*2)
     */
    for(i = 0; i < rowSize; i ++)
    {
        read_xmrg(&colSize, &rowSize, &i,
            fname, &lenf, irc, mosaic) ;

        /*
         * the file read is assumed Big Endian
         * if Linux, then swap bytes
         * misbin and prism files are delivered
         * to sites in Big Endian format
         * if running on Linux, bytes in misbin
         * and prism files are swapped
         */
        if((strcmp(os, "lx") == 0) ||
            (strcmp(os, "LX") == 0))
        {
            num_elements = colSize ;
            Swap2Bytes_(mosaic, &num_elements) ;
        }

        if(*irc != 0)
        {
            fprintf ( stderr , "ERROR: reading PRISM array failure. "
                " array of 1.0 used.\n") ;

            free(mosaic) ;
            mosaic = NULL ;
            return ;
        }

        for(j = 0; j < colSize; j ++)
            xmrg[i][j] = mosaic[j] * 1.0 / factor ;
    }

    if( mosaic != NULL )
    {
        free(mosaic) ;
        mosaic = NULL ;
    }
}

/**********************************************************************
 * get_climate ( )
 * 
 * This function reads PRISM data for a given month
 *********************************************************************/

void qdc_preproc_setup_get_climate(const int type, const char * os,
                 const int rowSize, const int colSize,
                 const char * cem, double ** umeang)
{
    char mosaicName[FILE_LEN] = {'\0'} ;
    int i, j, lenfn ;
    double factor = 1.0 ;
    int irc = 0 ;

    struct stat statInfo;
    int status;
    
    switch(type)
    {
    	case 1 :    // for precip climo data
            sprintf(mosaicName, "%s/prism_mean_precip_%s_%s", mpe_prism_dir, dqc_preproc_setup_mpe_site_id, cem );
            break;

    	case 2 :    // for temp max climo data
            sprintf(mosaicName, "%s/prism_max_temp_%s_%s", mpe_prism_dir, dqc_preproc_setup_mpe_site_id, cem );
            break;
    	
    	case 3 :    // for temp min climo data
            sprintf(mosaicName, "%s/prism_min_temp_%s_%s", mpe_prism_dir, dqc_preproc_setup_mpe_site_id, cem );
            break;

		default :
			fprintf ( stderr, "An invalid option: \"%d\" was passed in.\n", type ) ;
			exit(-1);
    }

     /*
      * Check to determine if the PRISM file exists and is readable.
      */
     status = stat ( mosaicName, & statInfo );

     if ( ( status != 0) || !( statInfo.st_mode & S_IFREG ) )
     {
        fprintf ( stderr , "WARNING: file = \"%s\" not found"
                  "    - array of %7.2f used.\n", mosaicName, CLIMO_DEFAULT) ;

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
                umeang[i][j] = CLIMO_DEFAULT ;
        }
     }
     else
     {
        lenfn = strlen(mosaicName);

        qdc_preproc_setup_readxmrg(os, rowSize, colSize, mosaicName,
            lenfn, factor, umeang, &irc) ;

        if(irc != 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                    umeang[i][j] = CLIMO_DEFAULT ;
            }
        }
    }
}

void readClimoData(double *** pClimoData, const int type )
{
    const char * os = "lx";

    const int rowSize = ptrGeoData->num_rows ;
    const int colSize = ptrGeoData->num_cols ;

    int k ;

    for(k = 0; k < 12; k++)
    {
        qdc_preproc_setup_get_climate(type, os, rowSize, colSize, mon[k], pClimoData[k]) ;    
    }
}

