/*******************************************************************************
* FILENAME:            mpe_destructor.c
*
* DESCRIPTION:         This function releases memory.
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

#include "empe_fieldgen.h"

// ADDED BY RAM FOR THE AVERAGE AND MAX MOSAIC CALCULATION
// ------------------
extern double  ** MaxMosaic;
extern double  ** AvgMosaic;
extern int     ** AvgMosaicNumRadars;

extern double  ** P3Mosaic;
// -------------------

extern double  ** RMosaic;
extern double  ** DHRMosaic;
extern double  ** BMosaic;
extern double  ** LMosaic;
extern int     ** ID;
extern double ** QPEMosaic;
extern double ** umeang;
extern double ** RadarBeamHeight;
extern double * meanFieldBias;
extern short ** radarMiscBins;

void hpe_fieldgen_destructor()
{
    int i;
    int rowSize = ptrGeoData->num_rows ;

    /*
     * releases memory for write array.
     */

    writeArrayDestructor ( ptrGeoData ) ;

    /*
     * releases memory for edit polygon array
     */

    editPolygonDestructor ( ptrGeoData );

    /*
     * releases memory for arrays
     */

    free2DIntArray(ID, rowSize );

    free2DIntArray(AvgMosaicNumRadars, rowSize );

    free2DDoubleArray(RMosaic, rowSize );

    free2DDoubleArray(DHRMosaic, rowSize );

    free2DDoubleArray(MaxMosaic, rowSize );

    free2DDoubleArray(AvgMosaic, rowSize );

    free2DDoubleArray(P3Mosaic, rowSize );

    free2DDoubleArray(BMosaic, rowSize );

    free2DDoubleArray(LMosaic, rowSize );

    free2DDoubleArray(QPEMosaic, rowSize );

    free2DDoubleArray(umeang, rowSize );

    free2DDoubleArray(RadarBeamHeight,
                      NUM_DPA_ROWS * ptrEMPEParams->hrap_grid_factor );

    if(ptrGageTable != NULL)
    {
        for(i = 0; i < ptrRunDate->hourNum; i++)
        {
            if(ptrGageTable[i]->ptrGageRecords != NULL)
            {
                free(ptrGageTable[i]->ptrGageRecords);
                ptrGageTable[i]->ptrGageRecords = NULL;
            }
            if(ptrGageTable[i] != NULL)
            {
                free(ptrGageTable[i]);
                ptrGageTable[i] = NULL;
            }

        }
	}
  
    if(ptrGageTableP3 != NULL)
    {
        for(i = 0; i < ptrRunDate->hourNum; i++)
        {
            if(ptrGageTableP3[i]->ptrGageRecords != NULL)
            {
                free(ptrGageTableP3[i]->ptrGageRecords);
                ptrGageTableP3[i]->ptrGageRecords = NULL;
            }
            if(ptrGageTableP3[i] != NULL)
            {
                free(ptrGageTableP3[i]);
                ptrGageTableP3[i] = NULL;
            }

        }
	}
    if(ptrGageTableP3 != NULL)
    {
        free(ptrGageTableP3);
        ptrGageTableP3 = NULL;
    }

    if(ptrQCGageTable != NULL)
    {
        for(i = 0; i < ptrRunDate->hourNum; i++)
        {
            if(ptrQCGageTable[i]->ptrGageRecords != NULL)
            {
                free(ptrQCGageTable[i]->ptrGageRecords);
                ptrQCGageTable[i]->ptrGageRecords = NULL;
            }
            if(ptrQCGageTable[i] != NULL)
            {
                free(ptrQCGageTable[i]);
                ptrQCGageTable[i] = NULL;
            }

        }
	}
    if(ptrQCGageTable != NULL)
    {
        free(ptrQCGageTable);
        ptrQCGageTable = NULL;
    }

    /* Free memory used by the satellite precipitation field. */

    free_spe_memory ( ptrGeoData, ptrEMPEParams->hrap_grid_factor );

    /* Free memory used by the allpyLocalBias function. */
    free_locbias_memory(ptrGeoData, ptrEMPEParams->hrap_grid_factor);

    /* Free memory used by the retrieveOfficeIDByRadarID function. */
    freeRadarLocMemory();

    if(ptrRunDate != NULL)
    {
        free(ptrRunDate);
        ptrRunDate = NULL;
    }

    if(ptrEMPEParams != NULL)
    {
        if(ptrEMPEParams->ptrRWParams != NULL)
        {
            free(ptrEMPEParams->ptrRWParams);
            ptrEMPEParams->ptrRWParams = NULL;
        }

        if(ptrEMPEParams->ptrRWBiasStat != NULL)
        {
            free(ptrEMPEParams->ptrRWBiasStat);
            ptrEMPEParams->ptrRWBiasStat = NULL;
        }
    }
    if(ptrEMPEParams != NULL)
    {
        free(ptrEMPEParams);
        ptrEMPEParams = NULL;
    }

    /*      
     * release memory for radarloc table struct data
     */

    if(ptrRadarLocTable != NULL)
    {
        if(ptrRadarLocTable->ptrRadarLocRecords != NULL)
        {
            free(ptrRadarLocTable->ptrRadarLocRecords);
            ptrRadarLocTable->ptrRadarLocRecords = NULL;
        }

        free(ptrRadarLocTable);
        ptrRadarLocTable = NULL;
    }

    /* Free memory used by the neighbor list array. */

    freeNeighborList ( ptrGeoData );

    if(ptrGeoData != NULL)
    {
        free(ptrGeoData);
        ptrGeoData = NULL;
    }

    /* Free memory associated with the station location buffer. */

    free_mpe_latlon_info ( );
}

void hpe_fieldgen_destructorByRadarLoc ( const int rowSize )
{
	free2DShortArray(radarMiscBins, rowSize );
}


