/*******************************************************************************
* FILENAME:            empe_constructor.c
*
* DESCRIPTION:         This function allocates memory for global data variables.
*
* calling function:        main_mpe_fieldgen
* functions called:        NULL
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         March, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Mar 2005     Guoxian Zhou      Finish first version 
*   Jan 2005     Guoxian Zhou      Modified for EMPE 
*
********************************************************************************
*/

#include "empe_fieldgen.h"
#include "read_stage1_decoded.h"

extern double  ** RMosaic;
extern double  ** DHRMosaic;
extern double  ** BMosaic;
extern double  ** LMosaic;
extern int     ** ID;
extern double  ** LSatpre;
extern double  ** umeang;
extern double  ** RadarBeamHeight;
extern double  ** QPEMosaic; 
extern double   * meanFieldBias;

// Added by Ram for the average and max mosaic calculations
// -------------------------

extern double  ** MaxMosaic;
extern double  ** AvgMosaic;
extern int     ** AvgMosaicNumRadars;

// ------------------------
// structure for p3 lmosaic calculation
// ----------------------

extern double  ** P3Mosaic;

/* For the array of misbin radar masks. This only need to be
   read in once per run of mpe_fieldgen. */

extern short ** radarMiscBins;

/*
 * This function allocates memory for global struct data and initialization.
 */

void hpe_fieldgen_constructor()
{

    /*
     * allocate memory and Initialize the run time struct data.
     */

    ptrRunDate = (run_date_struct *)malloc(sizeof(run_date_struct)); 
    if(ptrRunDate == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    /*
     * allocate memory for geo struct data.
     */

    ptrGeoData = (geo_data_struct *)malloc(sizeof(geo_data_struct)); 
    if(ptrGeoData == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    /*
     * allocate memory for global paramters struct data.
     */

    ptrEMPEParams = (empe_params_struct *)malloc(sizeof(empe_params_struct)); 
    if(ptrEMPEParams == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    memset(ptrEMPEParams->rfc_name, '\0', RFC_NAME_LEN + 1);
    memset(ptrEMPEParams->db_name, '\0', DB_DESCR_LEN + 1);
    memset(ptrEMPEParams->xmrgdtform, '\0', XMRGDTFORM_LEN + 1);
    memset(ptrEMPEParams->qpe_fieldtype, '\0', MOSAIC_TYPE_LEN);
    memset(ptrEMPEParams->os, '\0', 3);
    memset(ptrEMPEParams->user, '\0', 6);
    memset(ptrEMPEParams->fxa_local_site, '\0', WFO_LEN + 1);

    ptrEMPEParams->ptrRWParams = (RWParams *)malloc(sizeof(RWParams)); 
    if(ptrEMPEParams->ptrRWParams == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    ptrEMPEParams->ptrRWBiasStat = (RWBiasStat *)malloc(sizeof(RWBiasStat)); 
    if(ptrEMPEParams->ptrRWBiasStat == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    /*      
     * allocate memory for radarloc table struct data
     */

    ptrRadarLocTable = (radarLoc_table_struct *)
            malloc(sizeof(radarLoc_table_struct)); 
    if(ptrRadarLocTable == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in constructor function.\n"
            "\tProgram exit.");
        shutdown( message );
    }
    ptrRadarLocTable->ptrRadarLocRecords = 
        (radarLoc_record_struct *)
        malloc(RADAR_NUMBER * sizeof(radarLoc_record_struct)); 
    if(ptrRadarLocTable->ptrRadarLocRecords == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in constructor function.\n"
            "\tProgram exit");
        shutdown( message );
    }

    int j;
    for(j = 0; j < RADAR_NUMBER; j++)
    {
        memset(ptrRadarLocTable->ptrRadarLocRecords[j].radarID, '\0',
              (RADAR_ID_LEN + 1));
    }

}

/*
 * This function allocates memory for global struct data and initialization
 * based on the run time data.
 */

void hpe_fieldgen_constructorByRunTime()
{
    int i ;

    /*
     * allocate memory and Initialize the gage struct data.
     */

    ptrGageTable = 
        (gage_table_struct **)
        malloc(ptrRunDate->hourNum * sizeof(gage_table_struct *)); 
    if(ptrGageTable == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
        shutdown( message );
    }

    for(i = 0; i < ptrRunDate->hourNum; i++)
    {
        ptrGageTable[i] = (gage_table_struct *)    
            malloc(sizeof(gage_table_struct)); 
        if(ptrGageTable[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutdown( message );
        }

        ptrGageTable[i]->pseudoGageNum = 0;
        ptrGageTable[i]->totalGageNum  = 0;

        ptrGageTable[i]->ptrGageRecords = 
            (gage_record_struct *)
            malloc(GAGE_NUMBER * sizeof(gage_record_struct)); 
        if(ptrGageTable[i]->ptrGageRecords == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutdown( message );
        }

        int j;
        for(j = 0; j < GAGE_NUMBER; j++)
        {
            memset(ptrGageTable[i]->ptrGageRecords[j].gageID,
             '\0', (LOC_ID_LEN + 1));
            memset(ptrGageTable[i]->ptrGageRecords[j].gageTS,
             '\0', (SHEF_TS_LEN + 1));
        }

    }

    /*
     * allocate memory and Initialize the gage struct data for p3.
     */

    ptrGageTableP3 = 
        (gage_table_struct **)
        malloc(ptrRunDate->hourNum * sizeof(gage_table_struct *)); 
    if(ptrGageTableP3 == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
        shutdown( message );
    }

    for(i = 0; i < ptrRunDate->hourNum; i++)
    {
        ptrGageTableP3[i] = (gage_table_struct *)    
            malloc(sizeof(gage_table_struct)); 
        if(ptrGageTableP3[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutdown( message );
        }

        ptrGageTableP3[i]->pseudoGageNum = 0;
        ptrGageTableP3[i]->totalGageNum  = 0;

        ptrGageTableP3[i]->ptrGageRecords = 
            (gage_record_struct *)
            malloc(GAGE_NUMBER_P3 * sizeof(gage_record_struct)); 
        if(ptrGageTableP3[i]->ptrGageRecords == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutdown( message );
        }

        int j;
        for(j = 0; j < GAGE_NUMBER_P3; j++)
        {
            memset(ptrGageTableP3[i]->ptrGageRecords[j].gageID,
             '\0', (LOC_ID_LEN + 1));
            memset(ptrGageTableP3[i]->ptrGageRecords[j].gageTS,
             '\0', (SHEF_TS_LEN + 1));
        }
    }

    /*
     * allocate memory and fill in the qc gage struct data.
     **/
    ptrQCGageTable = 
        (gage_table_struct **)
        malloc(ptrRunDate->hourNum * sizeof(gage_table_struct *)); 
    if(ptrQCGageTable == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
        shutdown( message );
    }

    for(i = 0; i < ptrRunDate->hourNum; i++)
    {
        ptrQCGageTable[i] = (gage_table_struct *) 
                malloc(sizeof(gage_table_struct)); 
        if(ptrQCGageTable[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutdown( message );
        }

        ptrQCGageTable[i]->pseudoGageNum   = 0;
        ptrQCGageTable[i]->totalGageNum    = 0;

        ptrQCGageTable[i]->ptrGageRecords = 
            (gage_record_struct *)
            malloc(GAGE_NUMBER * sizeof(gage_record_struct)); 
        if(ptrQCGageTable[i]->ptrGageRecords == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutdown( message );
        }
        int j;
        for(j = 0; j < GAGE_NUMBER; j++)
        {
            memset(ptrQCGageTable[i]->ptrGageRecords[j].gageID,
             '\0', (LOC_ID_LEN + 1));
            memset(ptrQCGageTable[i]->ptrGageRecords[j].gageTS,
             '\0', (SHEF_TS_LEN + 1));
        }
    }
}


/*
 * This function allocates memory for global struct data 
 * and initialization based on the geo grid data.
 */

void hpe_fieldgen_constructorByGeodata(int blnMosaic[])
{
    mosaicType index;

    const int rowSize = ptrGeoData->num_rows ;
    const int colSize = ptrGeoData->num_cols ;

    /*
     * allocates memory for the write array mosaic.
     */

    writeArrayConstructor (ptrGeoData);

     /*
      * allocates memory for the edit polygon mosaic.
      */

    editPolygonConstructor ( ptrGeoData ); 

    RMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    DHRMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    MaxMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    AvgMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    P3Mosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    LSatpre = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    QPEMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

    umeang = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize );

    AvgMosaicNumRadars = init2DIntArray(ZERO_CONSTANT, rowSize, colSize );

    ID = init2DIntArray(ID_DEFAULT, rowSize, colSize );

    /*
     * allocate memory for bmosaic variable
     * if blnMosaic[bmosaic] = 1.
     */

    index = ebmosaic ; 
    if(blnMosaic[index] == 1)
    {
        BMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );
    }

    /*
     * allocate memory for lmosaic variable
     * if blnMosaic[lmosaic] = 1.
     */

    index = lmosaic ; 
    if(blnMosaic[index] == 1)
    {
        LMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );
    }    

    const int RADAR_ROWS = NUM_DPA_ROWS * ptrEMPEParams->hrap_grid_factor;
    const int RADAR_COLS = NUM_DPA_COLS * ptrEMPEParams->hrap_grid_factor;

    RadarBeamHeight = init2DDoubleArray(HEIGHT_DEFAULT, RADAR_ROWS, RADAR_COLS);
}

/*
 * This function allocates memory for global struct data 
 * and initialization based on the radar number data.
 */

void constructorForMeanBias(int radarLocNum)
{
    meanFieldBias = init1DDoubleArray(ZERO_CONSTANT, radarLocNum);
}
