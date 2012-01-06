/*******************************************************************************
* FILENAME:            mpe_constructor.c
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
*   March 2005   Guoxian Zhou      Finish first version
*
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "read_stage1_decoded.h"

extern double  ** MPEFieldGen_RMosaic;
extern double  ** MPEFieldGen_BMosaic;
extern double  ** MPEFieldGen_LMosaic;
extern double  ** LQMosaic;
extern double  ** QMosaic;
extern int     ** MPEFieldGen_ID;
extern int     ** Q2ID;
extern double  ** MPEFieldGen_LSatpre;
extern double  ** MPEFieldGen_umeang;
extern double  ** MPEFieldGen_QPEMosaic;
extern double * MPEFieldGen_meanFieldBias;
extern double ** RfcBMosaic;

// Added by Ram for the average and max mosaic calculations
// -------------------------
extern double  ** MPEFieldGen_MaxMosaic;
extern double  ** MPEFieldGen_AvgMosaic;
extern int     ** MPEFieldGen_AvgMosaicNumRadars;
// ------------------------
// structure for p3 lmosaic calculation
// ----------------------
extern double  ** MPEFieldGen_P3Mosaic;
// -----------------------

/* For the array of misbin radar masks. This only need to be
   read in once per run of mpe_fieldgen. */
extern short ** MPEFieldGen_radarMiscBins;


/***************************************************************************
 * This function allocates memory for global struct data and initialization.
 **/
void constructor()
{
    /**********************************************************
     * allocate memory and Initialize the run time struct data.
     **/
    ptrRunDate = (run_date_struct *)malloc(sizeof(run_date_struct));
    if(ptrRunDate == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    /*******************************************************
     * allocate memory for geo struct data.
     **/
    ptrGeoData = (geo_data_struct *)malloc(sizeof(geo_data_struct));
    if(ptrGeoData == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    /*******************************************************
     * allocate memory for global paramters struct data.
     **/
    ptrMPEParams = (mpe_params_struct *)malloc(sizeof(mpe_params_struct));
    if(ptrMPEParams == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    memset(ptrMPEParams->rfc_name, '\0', RFC_NAME_LEN + 1);
    memset(ptrMPEParams->db_name, '\0', DB_DESCR_LEN + 1);
    memset(ptrMPEParams->xmrgdtform, '\0', XMRGDTFORM_LEN + 1);
    memset(ptrMPEParams->qpe_fieldtype, '\0', MOSAIC_TYPE_LEN);
    memset(ptrMPEParams->os, '\0', 3);
    memset(ptrMPEParams->user, '\0', 6);

    ptrMPEParams->ptrRWParams = (RWParams *)malloc(sizeof(RWParams));
    if(ptrMPEParams->ptrRWParams == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    ptrMPEParams->ptrRWBiasStat = (RWBiasStat *)malloc(sizeof(RWBiasStat));
    if(ptrMPEParams->ptrRWBiasStat == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructor function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    /**
     * allocate memory for radarloc table struct data
     **/
    ptrRadarLocTable = (radarLoc_table_struct *)
            malloc(sizeof(radarLoc_table_struct));
    if(ptrRadarLocTable == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in constructor function.\n"
            "\tProgram exit.");
        shutDownMPE( message, logFile );
    }
    ptrRadarLocTable->ptrRadarLocRecords =
        (radarLoc_record_struct *) malloc(RADAR_NUMBER * sizeof(radarLoc_record_struct));
    if(ptrRadarLocTable->ptrRadarLocRecords == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in constructor function.\n"
            "\tProgram exit");
        shutDownMPE( message, logFile );
    }

    int j;
    for(j = 0; j < RADAR_NUMBER; j++)
    {
        memset(ptrRadarLocTable->ptrRadarLocRecords[j].radarID, '\0',
              (RADAR_ID_LEN + 1));
    }

}

/***************************************************************************
 * This function allocates memory for global struct data and initialization
 * based on the run time data.
 **/
void constructorByRunTime()
{
    int i ;

    /**********************************************************
     * allocate memory and Initialize the gage struct data.
     **/
    ptrGageTable =
        (gage_table_struct **)malloc(ptrRunDate->hourNum * sizeof(gage_table_struct *));
    if(ptrGageTable == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
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
            shutDownMPE( message, logFile );
        }

        ptrGageTable[i]->pseudoGageNum = 0;
        ptrGageTable[i]->totalGageNum  = 0;

        ptrGageTable[i]->ptrGageRecords =
            (gage_record_struct *)malloc(GAGE_NUMBER * sizeof(gage_record_struct));
        if(ptrGageTable[i]->ptrGageRecords == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        int j;
        for(j = 0; j < GAGE_NUMBER; j++)
        {
            memset(ptrGageTable[i]->ptrGageRecords[j].gageID, '\0', (LOC_ID_LEN + 1));
            memset(ptrGageTable[i]->ptrGageRecords[j].gageTS, '\0', (SHEF_TS_LEN + 1));
        }

    }



    /**********************************************************
     * allocate memory and Initialize the gage struct data for p3.
     **/

    ptrGageTableP3 =
        (gage_table_struct **)malloc(ptrRunDate->hourNum * sizeof(gage_table_struct *));
    if(ptrGageTableP3 == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
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
            shutDownMPE( message, logFile );
        }

        ptrGageTableP3[i]->pseudoGageNum = 0;
        ptrGageTableP3[i]->totalGageNum  = 0;

        ptrGageTableP3[i]->ptrGageRecords =
            (gage_record_struct *)malloc(GAGE_NUMBER_P3 * sizeof(gage_record_struct));
        if(ptrGageTableP3[i]->ptrGageRecords == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        int j;
        for(j = 0; j < GAGE_NUMBER_P3; j++)
        {
            memset(ptrGageTableP3[i]->ptrGageRecords[j].gageID, '\0', (LOC_ID_LEN + 1));
            memset(ptrGageTableP3[i]->ptrGageRecords[j].gageTS, '\0', (SHEF_TS_LEN + 1));
        }

    }



    /**********************************************************
     * allocate memory and fill in the qc gage struct data.
     **/
    ptrQCGageTable =
        (gage_table_struct **)malloc(ptrRunDate->hourNum * sizeof(gage_table_struct *));
    if(ptrQCGageTable == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
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
            shutDownMPE( message, logFile );
        }

        ptrQCGageTable[i]->pseudoGageNum   = 0;
        ptrQCGageTable[i]->totalGageNum    = 0;

        ptrQCGageTable[i]->ptrGageRecords =
            (gage_record_struct *)malloc(GAGE_NUMBER * sizeof(gage_record_struct));
        if(ptrQCGageTable[i]->ptrGageRecords == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByRunTime function."
                    "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        int j;
        for(j = 0; j < GAGE_NUMBER; j++)
        {
            memset(ptrQCGageTable[i]->ptrGageRecords[j].gageID, '\0', (LOC_ID_LEN + 1));
            memset(ptrQCGageTable[i]->ptrGageRecords[j].gageTS, '\0', (SHEF_TS_LEN + 1));
        }

    }

}


/**********************************************************************
 * This function allocates memory for global struct data
 * and initialization based on the geo grid data.
 **/
void constructorByGeodata(int blnMosaic[])
{
    mosaicType index;
    int i,j;
    const int rowSize = ptrGeoData->num_rows ;
    const int colSize = ptrGeoData->num_cols ;

    /********************************************************************
     * allocates memory for the write array mosaic.
     **/
     MPEFieldGen_writeArrayConstructor (ptrGeoData);

     /*******************************************************************
      * allocates memory for the edit polygon mosaic.
      **/
     MPEFieldGen_editPolygonConstructor ( ptrGeoData );

    // Added by Ram for the average and max mosaic allocation

    // Allocating memory for the max mosaic, average mosaic and AvgMosaicNumRadars
    // structures.
    // -------------------------------
    MPEFieldGen_MaxMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_MaxMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_MaxMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_MaxMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        for(j=0;j<colSize;j++)
        {
            MPEFieldGen_MaxMosaic[i][j] = MOSAIC_DEFAULT;
        }
    }

    MPEFieldGen_AvgMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_AvgMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_AvgMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_AvgMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        for(j=0;j<colSize;j++)
        {
            MPEFieldGen_AvgMosaic[i][j] = MOSAIC_DEFAULT;
        }
    }

    MPEFieldGen_AvgMosaicNumRadars = (int **)malloc(rowSize * sizeof(int *));
    if(MPEFieldGen_AvgMosaicNumRadars == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_AvgMosaicNumRadars[i] = (int *)malloc(colSize * sizeof(int));
        if(MPEFieldGen_AvgMosaicNumRadars[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        for(j=0;j<colSize;j++)
        {
            MPEFieldGen_AvgMosaicNumRadars[i][j] = 0;
        }
    }

    MPEFieldGen_P3Mosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_P3Mosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_P3Mosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_P3Mosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        for(j=0;j<colSize;j++)
        {
            MPEFieldGen_P3Mosaic[i][j] = MOSAIC_DEFAULT;
        }
    }
    // ----------------------------------





    /****************************************************************
     * allocate memory for rmosaic variable.
     **/
    MPEFieldGen_RMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_RMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_RMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_RMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        for(j = 0; j < colSize; j++)
        {
            MPEFieldGen_RMosaic[i][j] = MOSAIC_DEFAULT;
        }

    }

    /* allocate memory for gage ID variable */
    MPEFieldGen_ID = (int **)malloc(rowSize * sizeof(int *));
    if(MPEFieldGen_ID == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_ID[i] = (int *)malloc(colSize * sizeof(int));
        if(MPEFieldGen_ID[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
    /* allocate memory for gage Q2ID variable */
    Q2ID = (int **)malloc(rowSize * sizeof(int *));
    if(Q2ID == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        Q2ID[i] = (int *)malloc(colSize * sizeof(int));
        if(Q2ID[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }


    /****************************************************************
     * allocate memory for local bias satellite precipitation variable.
     **/
    MPEFieldGen_LSatpre = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_LSatpre == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_LSatpre[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_LSatpre[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }

        for(j = 0; j < colSize; j++)
        {
            MPEFieldGen_LSatpre[i][j] = MOSAIC_DEFAULT;
        }

    }


    /**********************************************************
     * allocate memory for "best estimate" mosaic variable.
     **/
    MPEFieldGen_QPEMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_QPEMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_QPEMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_QPEMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        for(j = 0; j < colSize; j++)
        {
            MPEFieldGen_QPEMosaic[i][j] = MOSAIC_DEFAULT;
        }

    }

    /**********************************************************
     * allocate memory for prism variable.
     **/
    MPEFieldGen_umeang = (double **)malloc(rowSize * sizeof(double *));
    if(MPEFieldGen_umeang == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByGeodata function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MPEFieldGen_umeang[i] = (double *)malloc(colSize * sizeof(double));
        if(MPEFieldGen_umeang[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        for(j = 0; j < colSize; j++)
        {
            MPEFieldGen_umeang[i][j] = 0;
        }

    }

    /*******************************************************
     * allocate memory for bmosaic variable
     * if blnMosaic[bmosaic] = 1.
     **/
    index = bmosaic ;
    if(blnMosaic[index] == 1)
     {
        MPEFieldGen_BMosaic = (double **)malloc(rowSize * sizeof(double *));
        if(MPEFieldGen_BMosaic == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        for(i = 0; i < rowSize; i++)
        {
            MPEFieldGen_BMosaic[i] = (double *)malloc(colSize * sizeof(double));
            if(MPEFieldGen_BMosaic[i] == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByGeodata function."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            for(j = 0; j < colSize; j++)
            {
                MPEFieldGen_BMosaic[i][j] = MOSAIC_DEFAULT;
            }

        }
    }

    /**************************************************
     * allocate memory for lmosaic variable
     * if blnMosaic[lmosaic] = 1.
     **/
    index = lmosaic ;
    if(blnMosaic[index] == 1)
     {
        MPEFieldGen_LMosaic = (double **)malloc(rowSize * sizeof(double *));
        if(MPEFieldGen_LMosaic == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        for(i = 0; i < rowSize; i++)
        {
            MPEFieldGen_LMosaic[i] = (double *)malloc(colSize * sizeof(double));
            if(MPEFieldGen_LMosaic[i] == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByGeodata function."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            for(j = 0; j < colSize; j++)
            {
                MPEFieldGen_LMosaic[i][j] = MOSAIC_DEFAULT;
            }

        }
    }

    /**************************************************
     * allocate memory for lqmosaic variable
     * if blnMosaic[lqmosaic]=1
     **/
    index = qmosaic ;
    if(blnMosaic[index] == 1)
     {
        QMosaic = (double **)malloc(rowSize * sizeof(double *));
        if(QMosaic == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        for(i = 0; i < rowSize; i++)
        {
            QMosaic[i] = (double *)malloc(colSize * sizeof(double));
            if(QMosaic[i] == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByGeodata function."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }
    }


    /**************************************************
      * allocate memory for lqmosaic variable
      * if blnMosaic[lqmosaic]=1
      **/
     index = lqmosaic ;
     if(blnMosaic[index] == 1)
      {
         LQMosaic = (double **)malloc(rowSize * sizeof(double *));
         if(LQMosaic == NULL)
         {
             sprintf ( message , "ERROR: memory allocation failure"
                 " in constructorByGeodata function."
                 "\n\tProgram exit.") ;
             shutDownMPE( message, logFile );
         }
         for(i = 0; i < rowSize; i++)
         {
             LQMosaic[i] = (double *)malloc(colSize * sizeof(double));
             if(LQMosaic[i] == NULL)
             {
                 sprintf ( message , "ERROR: memory allocation failure"
                     " in constructorByGeodata function."
                     "\n\tProgram exit.") ;
                 shutDownMPE( message, logFile );
             }
         }
     }

     /**************************************************
     * allocate memory for lmosaic variable
     * if blnMosaic[lmosaic] = 1.
     **/
    index = rfcbmosaic ;
    if(blnMosaic[index] == 1)
     {
        RfcBMosaic = (double **)malloc(rowSize * sizeof(double *));
        if(RfcBMosaic == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in constructorByGeodata function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        for(i = 0; i < rowSize; i++)
        {
            RfcBMosaic[i] = (double *)malloc(colSize * sizeof(double));
            if(RfcBMosaic[i] == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in constructorByGeodata function."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            for(j = 0; j < colSize; j++)
            {
                RfcBMosaic[i][j] = MOSAIC_DEFAULT;
            }
        }
    }
}

/****************************************************************
 * This function allocates memory for global struct data
 * and initialization based on the radar number data.
 **/
void constructorByRadarLoc(int radarLocNum)
{
    int i ;

    /**********************************************************
     * allocate memory the mean field bias array.
     **/
    MPEFieldGen_meanFieldBias = (double *)malloc(radarLocNum * sizeof(double));

    if(MPEFieldGen_meanFieldBias == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in constructorByRadarLoc function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    for(i = 0; i < radarLocNum; i++)
    {
        MPEFieldGen_meanFieldBias[i] = 0;
    }

    /**********************************************************
     * allocate memory for the array of radar misbin masks.
     **/
    MPEFieldGen_radarMiscBins = ( short ** ) malloc ( radarLocNum * sizeof ( short ** ) );

    if ( MPEFieldGen_radarMiscBins == NULL )
    {
       sprintf ( message, "ERROR: memory allocation failure"
                          " in constructorByRadarLoc function."
                          "\n\tProgram exit." );
       shutDownMPE ( message, logFile );
    }

    for ( i = 0; i < radarLocNum ; ++i )
    {
       MPEFieldGen_radarMiscBins [ i ] = ( short * ) malloc ( NUM_DPA_ELEMENTS
                                                  * sizeof ( short  ) );

       if ( MPEFieldGen_radarMiscBins [ i ] == NULL )
       {
          sprintf ( message, "ERROR: memory allocation failure"
                             " in constructorByRadarLoc function."
                             "\n\tProgram exit." );
          shutDownMPE ( message, logFile );
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/mpe_constructor.c,v $";
 static char rcs_id2[] = "$Id: mpe_constructor.c,v 1.1 2007/10/15 12:19:10 dsa Exp lawrence $";}
/*  ===================================================  */

}
