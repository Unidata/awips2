/**********************************************************************
 * processClimoData ( )
 * 
 * This function reads climo data of mean precip and max/min temperature, 
 * and build climo records. 
 *********************************************************************/

#include "dqc_preproc_setup.h"

double *** ptrClimoPrecip ;
double *** ptrClimoMaxTemp ;
double *** ptrClimoMinTemp ;

void initClimoArray ();
void releaseClimoArray ();

void  processClimoData (const char * path, const char * areaName)
{
    int i, j;
    int irow, icol ;
    int status ;
    char strClimoRecord[258] = {'\0'};
    char strClimoValue[10] = {'\0'};
    int type;

    /*
     * Allocate memory for the climo data array.
     */
    initClimoArray() ;

    /*
     * Load the climo precip data arrays.
     */
    type = 1;
    readClimoData(ptrClimoPrecip, type) ;

    /*
     * Load the climo max temperature data arrays.
     */
    type = 2;
    readClimoData(ptrClimoMaxTemp, type) ;

    /*
     * Load the climo min temperature data arrays.
     */
    type = 3;
    readClimoData(ptrClimoMinTemp, type) ;

    /*
     * Build the monthly mean precip records
     * and write out to the file.
     */
    if(dqc_preproc_setup_precip_count > 0)
    {
        openClimoOutputFile (path, areaName );

        fprintf ( ptrClimoFile, "%d\n", dqc_preproc_setup_precip_count );

        for ( i = 0; i < dqc_preproc_setup_precip_count; i++)
        {
            sprintf ( strClimoRecord, "%-5s  PPM%sCM",
                    ptrPrecipInfo[i].lid,
                    prismTypeSource );

            status = buildLocalHrap(ptrGeoData,
                    ptrPrecipInfo[i].lat, ptrPrecipInfo[i].lon,
                    &irow, &icol) ;

            for(j = 0; j < 12; j++)
            {
                if(status != 1) // out of geo range, set all value to missing.
                {
                    sprintf ( strClimoValue, "%7.2f", CLIMO_DEFAULT);
                }
                else
                {
                    /*
                     * Precip output in INCH unit
                     * converted from cm.
                     */
                    sprintf ( strClimoValue, "%7.2f", ptrClimoPrecip[j][irow][icol] / 25.4);
                }
                strcat(strClimoRecord, strClimoValue);
            }

            fprintf ( ptrClimoFile, "%s\n", strClimoRecord );
        }
    }

    /*
     * Build the monthly max/min temperature records
     * and write out to the file.
     */
    if(dqc_preproc_setup_temperature_count > 0)
    {
        openClimoOutputFile (path, areaName );

        fprintf ( ptrClimoFile, "%d\n", dqc_preproc_setup_temperature_count );

        for ( i = 0; i < dqc_preproc_setup_temperature_count; i++)
        {    
            status = buildLocalHrap(ptrGeoData,
                    ptrTempInfo[i].lat, ptrTempInfo[i].lon,
                    &irow, &icol) ;

		    /*
		     * Build the monthly min temperature climo record
		     */
            sprintf ( strClimoRecord, "%-5s  TAI%sNM",
                    ptrTempInfo[i].lid,
                    prismTypeSource);

            for(j = 0; j < 12; j++)
            {
                if(status != 1) // out of geo range, set for missing value.
                {
                    sprintf ( strClimoValue, "%7.2f", CLIMO_DEFAULT);
                }
                else
                {
                    sprintf ( strClimoValue, "%7.2f", ptrClimoMinTemp[j][irow][icol] / 10.0);
                }

                strcat(strClimoRecord, strClimoValue);
            }

            fprintf ( ptrClimoFile, "%s\n", strClimoRecord );


		    /*
		     * Build the monthly max temperature climo record
		     */
            sprintf ( strClimoRecord, "%-5s  TAI%sXM",
                    ptrTempInfo[i].lid,
                    prismTypeSource);

            for(j = 0; j < 12; j++)
            {
                if(status != 1) // out of geo range, set for missing value.
                {
                    sprintf ( strClimoValue, "%7.2f", CLIMO_DEFAULT);
                }
                else
                {
                    sprintf ( strClimoValue, "%7.2f", ptrClimoMaxTemp[j][irow][icol] / 10.0);
                }

                strcat(strClimoRecord, strClimoValue);
            }

            fprintf ( ptrClimoFile, "%s\n", strClimoRecord );

        }

    }

    releaseClimoArray () ;

    return;
}



/**********************************************************************
 * initClimoArray ( )
 * 
 * This function allocates memory and
 * initializes the climo precip data array.
 *********************************************************************/

void initClimoArray ()
{
    int i, j, k;
    const int rowSize = ptrGeoData->num_rows ;
    const int colSize = ptrGeoData->num_cols ;

    /*
     * Allocate memory for the climo precip variable.
     */
    ptrClimoPrecip = (double ***)malloc(12 * sizeof(double **)); 
    if(ptrClimoPrecip == NULL)
    {
        fprintf ( stderr , "ERROR: memory allocation failure"
            " in initialClimoData function."
            "\nProgram exit.\n") ;
        exit(-1) ;
    }

    for(k = 0; k < 12; k++)
    {
        ptrClimoPrecip[k] = (double **)malloc(rowSize * sizeof(double *)); 
        if(ptrClimoPrecip[k] == NULL)
        {
            fprintf ( stderr , "ERROR: memory allocation failure"
                " in initialClimoData function."
                "\nProgram exit.\n") ;
            exit(-1) ;
        }

        for(i = 0; i < rowSize; i++)
        {
            ptrClimoPrecip[k][i] = (double *)malloc(colSize * sizeof(double)); 
            if(ptrClimoPrecip[k][i] == NULL)
            {
                fprintf ( stderr , "ERROR: memory allocation failure"
                    " in initialClimoData function."
                    "\nProgram exit.\n") ;
                exit(-1) ;
            }

            for(j = 0; j < colSize; j++)
            {
                ptrClimoPrecip[k][i][j] = CLIMO_DEFAULT;
            }
        }
    }

    /*
     * Allocate memory for the climo max temperature variable.
     */
    ptrClimoMaxTemp = (double ***)malloc(12 * sizeof(double **)); 
    if(ptrClimoMaxTemp == NULL)
    {
        fprintf ( stderr , "ERROR: memory allocation failure"
            " in initialClimoData function."
            "\nProgram exit.\n") ;
        exit(-1) ;
    }

    for(k = 0; k < 12; k++)
    {
        ptrClimoMaxTemp[k] = (double **)malloc(rowSize * sizeof(double *)); 
        if(ptrClimoMaxTemp[k] == NULL)
        {
            fprintf ( stderr , "ERROR: memory allocation failure"
                " in initialClimoData function."
                "\nProgram exit.\n") ;
            exit(-1) ;
        }

        for(i = 0; i < rowSize; i++)
        {
            ptrClimoMaxTemp[k][i] = (double *)malloc(colSize * sizeof(double)); 
            if(ptrClimoMaxTemp[k][i] == NULL)
            {
                fprintf ( stderr , "ERROR: memory allocation failure"
                    " in initialClimoData function."
                    "\nProgram exit.\n") ;
                exit(-1) ;
            }

            for(j = 0; j < colSize; j++)
            {
                ptrClimoMaxTemp[k][i][j] = CLIMO_DEFAULT;
            }
        }
    }

    /*
     * Allocate memory for the climo min temperature variable.
     */
    ptrClimoMinTemp = (double ***)malloc(12 * sizeof(double **)); 
    if(ptrClimoMinTemp == NULL)
    {
        fprintf ( stderr , "ERROR: memory allocation failure"
            " in initialClimoData function."
            "\nProgram exit.\n") ;
        exit(-1) ;
    }

    for(k = 0; k < 12; k++)
    {
        ptrClimoMinTemp[k] = (double **)malloc(rowSize * sizeof(double *)); 
        if(ptrClimoMinTemp[k] == NULL)
        {
            fprintf ( stderr , "ERROR: memory allocation failure"
                " in initialClimoData function."
                "\nProgram exit.\n") ;
            exit(-1) ;
        }

        for(i = 0; i < rowSize; i++)
        {
            ptrClimoMinTemp[k][i] = (double *)malloc(colSize * sizeof(double)); 
            if(ptrClimoMinTemp[k][i] == NULL)
            {
                fprintf ( stderr , "ERROR: memory allocation failure"
                    " in initialClimoData function."
                    "\nProgram exit.\n") ;
                exit(-1) ;
            }

            for(j = 0; j < colSize; j++)
            {
                ptrClimoMinTemp[k][i][j] = CLIMO_DEFAULT;
            }
        }
    }
}


/**********************************************************************
 * releaseClimoArray ( )
 * 
 * This function releases memory of the climo precip data array.
 *********************************************************************/

void releaseClimoArray ()
{
    int i, k;
    const int rowSize = ptrGeoData->num_rows ;

    /*
     * Release memory for the climo precip variable.
     */
    if(ptrClimoPrecip != NULL)
    {
        for(k = 0; k < 12; k++)
        {
            if(ptrClimoPrecip[k] != NULL)
            {
                for(i = 0; i < rowSize; i++)
                {
                    if(ptrClimoPrecip[k][i] != NULL)
                    {
                        free(ptrClimoPrecip[k][i]);
                        ptrClimoPrecip[k][i] = NULL;
                    }
                }
                free(ptrClimoPrecip[k]);
                ptrClimoPrecip[k] = NULL;
            }
        }
        free(ptrClimoPrecip);
        ptrClimoPrecip = NULL;
    }

    /*
     * Release memory for the climo max temperature variable.
     */
    if(ptrClimoMaxTemp != NULL)
    {
        for(k = 0; k < 12; k++)
        {
            if(ptrClimoMaxTemp[k] != NULL)
            {
                for(i = 0; i < rowSize; i++)
                {
                    if(ptrClimoMaxTemp[k][i] != NULL)
                    {
                        free(ptrClimoMaxTemp[k][i]);
                        ptrClimoMaxTemp[k][i] = NULL;
                    }
                }
                free(ptrClimoMaxTemp[k]);
                ptrClimoMaxTemp[k] = NULL;
            }
        }
        free(ptrClimoMaxTemp);
        ptrClimoMaxTemp = NULL;
    }


    /*
     * Release memory for the climo min temperature variable.
     */
    if(ptrClimoMinTemp != NULL)
    {
        for(k = 0; k < 12; k++)
        {
            if(ptrClimoMinTemp[k] != NULL)
            {
                for(i = 0; i < rowSize; i++)
                {
                    if(ptrClimoMinTemp[k][i] != NULL)
                    {
                        free(ptrClimoMinTemp[k][i]);
                        ptrClimoMinTemp[k][i] = NULL;
                    }
                }
                free(ptrClimoMinTemp[k]);
                ptrClimoMinTemp[k] = NULL;
            }
        }
        free(ptrClimoMinTemp);
        ptrClimoMinTemp = NULL;
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/dqc_preproc_setup/RCS/process_climo_data.c,v $";
 static char rcs_id2[] = "$Id: process_climo_data.c,v 1.5 2007/10/23 18:11:46 lawrence Exp $";}
/*  ===================================================  */

}
