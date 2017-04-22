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

#include "mpe_fieldgen.h"

// ------------------
extern double  ** MPEFieldGen_MaxMosaic;
extern double  ** MPEFieldGen_AvgMosaic;
extern int     ** MPEFieldGen_AvgMosaicNumRadars;

extern double  ** MPEFieldGen_MaxRDMosaic;
extern double  ** MPEFieldGen_AvgRDMosaic;
extern int     ** MPEFieldGen_AvgRDMosaicNumRadars;

extern int     ** MPEFieldGen_P3Mosaic;
// -------------------

extern double  ** MPEFieldGen_RMosaic;
extern double  ** MPEFieldGen_RDMosaic;
extern double  ** MPEFieldGen_BMosaic;
extern double  ** MPEFieldGen_BDMosaic;
extern double  ** MPEFieldGen_LMosaic;
extern double  ** MPEFieldGen_QMosaic;
extern double  ** MPEFieldGen_LQMosaic;
extern int     ** MPEFieldGen_ID;
extern int     ** MPEFieldGen_IDDP; //djsiii not sure about the prefix here
extern int     ** Q2ID;
extern double ** MPEFieldGen_QPEMosaic;
extern double ** MPEFieldGen_umeang;
//extern double * MPEFieldGen_meanFieldBias;
extern short ** MPEFieldGen_radarMiscBins;
extern double ** RfcBMosaic;

void destructor()
{
    int i;
    const int rowSize = ptrGeoData->num_rows ;

    /*******************************************
     * releases memory for write array.
     **/
    MPEFieldGen_writeArrayDestructor ( ptrGeoData ) ;

    /*******************************************
     * releases memory for edit polygon array
     **/
    MPEFieldGen_editPolygonDestructor ( ptrGeoData );

     if(MPEFieldGen_ID != NULL)
    {
    	for(i = 0; i < rowSize; i++)
    	{
        	if(MPEFieldGen_ID[i] != NULL)
        	{
            	free(MPEFieldGen_ID[i]);
            	MPEFieldGen_ID[i] = NULL;
        	}
    	}
	}
    if(MPEFieldGen_ID != NULL)
    {
        free(MPEFieldGen_ID);
        MPEFieldGen_ID = NULL;
    }

    if(MPEFieldGen_IDDP != NULL)
    {
    	for(i = 0; i < rowSize; i++)
    	{
        	if(MPEFieldGen_IDDP[i] != NULL)
        	{
            	free(MPEFieldGen_IDDP[i]);
            	MPEFieldGen_IDDP[i] = NULL;
        	}
    	}
    }
    if(MPEFieldGen_IDDP != NULL)
    {
        free(MPEFieldGen_IDDP);
        MPEFieldGen_IDDP = NULL;
    }

    if(Q2ID != NULL)
   {
       for(i = 0; i < rowSize; i++)
       {
               if(Q2ID[i] != NULL)
               {
               free(Q2ID[i]);
               Q2ID[i] = NULL;
               }
       }
   }
   if(Q2ID != NULL)
   {
       free(Q2ID);
       Q2ID = NULL;
   }

    if(MPEFieldGen_RMosaic != NULL)
    {
    	for(i = 0; i < rowSize; i++)
    	{
        	if(MPEFieldGen_RMosaic[i] != NULL)
        	{
            	free(MPEFieldGen_RMosaic[i]);
            	MPEFieldGen_RMosaic[i] = NULL;
        	}
    	}
    }
    if(MPEFieldGen_RMosaic != NULL)
    {
        free(MPEFieldGen_RMosaic);
        MPEFieldGen_RMosaic = NULL;
    }

    if(MPEFieldGen_RDMosaic != NULL)
    {
    	for(i = 0; i < rowSize; i++)
    	{
        	if(MPEFieldGen_RDMosaic[i] != NULL)
        	{
            	free(MPEFieldGen_RDMosaic[i]);
            	MPEFieldGen_RDMosaic[i] = NULL;
        	}
    	}
    }
    if(MPEFieldGen_RDMosaic != NULL)
    {
        free(MPEFieldGen_RDMosaic);
        MPEFieldGen_RDMosaic = NULL;
    }

    if(MPEFieldGen_QMosaic != NULL)
        {
            for(i = 0; i < rowSize; i++)
            {
                    if(MPEFieldGen_QMosaic[i] != NULL)
                    {
                    free(MPEFieldGen_QMosaic[i]);
                    MPEFieldGen_QMosaic[i] = NULL;
                    }
            }
        }
        if(MPEFieldGen_QMosaic != NULL)
        {
            free(MPEFieldGen_QMosaic);
            MPEFieldGen_QMosaic = NULL;
        }


    // Added by Ram for the average and max mosaic destruction/deallocation
     // ------------------------
    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_MaxMosaic[i] != NULL)
        {
            free(MPEFieldGen_MaxMosaic[i]);
            MPEFieldGen_MaxMosaic[i] = NULL;
        }
    }
    if(MPEFieldGen_MaxMosaic != NULL)
    {
        free(MPEFieldGen_MaxMosaic);
        MPEFieldGen_MaxMosaic = NULL;
    }


    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_AvgMosaic[i] != NULL)
        {
            free(MPEFieldGen_AvgMosaic[i]);
            MPEFieldGen_AvgMosaic[i] = NULL;
        }
    }
    if(MPEFieldGen_AvgMosaic != NULL)
    {
        free(MPEFieldGen_AvgMosaic);
        MPEFieldGen_AvgMosaic = NULL;
    }



    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_AvgMosaicNumRadars[i] != NULL)
        {
            free(MPEFieldGen_AvgMosaicNumRadars[i]);
            MPEFieldGen_AvgMosaicNumRadars[i] = NULL;
        }
    }
    if(MPEFieldGen_AvgMosaicNumRadars != NULL)
    {
        free(MPEFieldGen_AvgMosaicNumRadars);
        MPEFieldGen_AvgMosaicNumRadars = NULL;
    }

/* new block */

     // ------------------------
    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_MaxRDMosaic[i] != NULL)
        {
            free(MPEFieldGen_MaxRDMosaic[i]);
            MPEFieldGen_MaxRDMosaic[i] = NULL;
        }
    }
    if(MPEFieldGen_MaxRDMosaic != NULL)
    {
        free(MPEFieldGen_MaxRDMosaic);
        MPEFieldGen_MaxRDMosaic = NULL;
    }

    
    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_AvgRDMosaic[i] != NULL)
        {
            free(MPEFieldGen_AvgRDMosaic[i]);
            MPEFieldGen_AvgRDMosaic[i] = NULL;
        }
    }
    if(MPEFieldGen_AvgRDMosaic != NULL)
    {
        free(MPEFieldGen_AvgRDMosaic);
        MPEFieldGen_AvgRDMosaic = NULL;
    }


    
    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_AvgRDMosaicNumRadars[i] != NULL)
        {
            free(MPEFieldGen_AvgRDMosaicNumRadars[i]);
            MPEFieldGen_AvgRDMosaicNumRadars[i] = NULL;
        }
    }

    if(MPEFieldGen_AvgRDMosaicNumRadars != NULL)
    {
        free(MPEFieldGen_AvgRDMosaicNumRadars);
        MPEFieldGen_AvgRDMosaicNumRadars = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(MPEFieldGen_P3Mosaic[i] != NULL)
        {
            free(MPEFieldGen_P3Mosaic[i]);
            MPEFieldGen_P3Mosaic[i] = NULL;
        }
    }
    if(MPEFieldGen_P3Mosaic != NULL)
    {
        free(MPEFieldGen_P3Mosaic);
        MPEFieldGen_P3Mosaic = NULL;
    }
// ---------------------------


/* end of new block */

    if(MPEFieldGen_BMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MPEFieldGen_BMosaic[i] != NULL)
            {
                free(MPEFieldGen_BMosaic[i]);
                MPEFieldGen_BMosaic[i] = NULL;
            }
        }
	}
    if(MPEFieldGen_BMosaic != NULL)
    {
        free(MPEFieldGen_BMosaic);
        MPEFieldGen_BMosaic = NULL;
    }

/* new block */

    if(MPEFieldGen_BDMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MPEFieldGen_BDMosaic[i] != NULL)
            {
                free(MPEFieldGen_BDMosaic[i]);
                MPEFieldGen_BDMosaic[i] = NULL;
            }
        }
    }
    if(MPEFieldGen_BDMosaic != NULL)
    {
        free(MPEFieldGen_BDMosaic);
        MPEFieldGen_BDMosaic = NULL;
    }

/* end of new block */

    if(MPEFieldGen_LMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MPEFieldGen_LMosaic[i] != NULL)
            {
                free(MPEFieldGen_LMosaic[i]);
                MPEFieldGen_LMosaic[i] = NULL;
            }
        }
	}
    if(MPEFieldGen_LMosaic != NULL)
    {
        free(MPEFieldGen_LMosaic);
        MPEFieldGen_LMosaic = NULL;
    }

    if(MPEFieldGen_LQMosaic != NULL)
        {
            for(i = 0; i < rowSize; i++)
            {
                if(MPEFieldGen_LQMosaic[i] != NULL)
                {
                    free(MPEFieldGen_LQMosaic[i]);
                    MPEFieldGen_LQMosaic[i] = NULL;
                }
            }
            }
        if(MPEFieldGen_LQMosaic != NULL)
        {
            free(MPEFieldGen_LQMosaic);
            MPEFieldGen_LQMosaic = NULL;
        }


    if(MPEFieldGen_QPEMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MPEFieldGen_QPEMosaic[i] != NULL)
            {
                free(MPEFieldGen_QPEMosaic[i]);
                MPEFieldGen_QPEMosaic[i] = NULL;
            }
        }
	}
    if(MPEFieldGen_QPEMosaic != NULL)
    {
        free(MPEFieldGen_QPEMosaic);
        MPEFieldGen_QPEMosaic = NULL;
    }

    if(MPEFieldGen_umeang != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MPEFieldGen_umeang[i] != NULL)
            {
                free(MPEFieldGen_umeang[i]);
                MPEFieldGen_umeang[i] = NULL;
            }
        }
	}

    if(RfcBMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(RfcBMosaic[i] != NULL)
            {
                free(RfcBMosaic[i]);
                RfcBMosaic[i] = NULL;
            }
        }
	}
    if(RfcBMosaic != NULL)
    {
        free(RfcBMosaic);
        RfcBMosaic = NULL;
    }

    if(MPEFieldGen_umeang != NULL)
    {
        free(MPEFieldGen_umeang);
        MPEFieldGen_umeang = NULL;
    }

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
        free(ptrGageTableP3);
        ptrGageTableP3 = NULL;
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

    if(ptrRunDate != NULL)
    {
        free(ptrRunDate);
        ptrRunDate = NULL;
    }


    if(ptrMPEParams != NULL)
    {
        if(ptrMPEParams->ptrRWParams != NULL)
        {
            free(ptrMPEParams->ptrRWParams);
            ptrMPEParams->ptrRWParams = NULL;
        }

        if(ptrMPEParams->ptrRWBiasStat != NULL)
        {
            free(ptrMPEParams->ptrRWBiasStat);
            ptrMPEParams->ptrRWBiasStat = NULL;
        }
    }
    if(ptrMPEParams != NULL)
    {
        free(ptrMPEParams);
        ptrMPEParams = NULL;
    }

    /**
     * release memory for radarloc table struct data
     **/
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

    /* Free memory used by the satellite precipitation field. */
    MPEFieldGen_free_spe_memory ( ptrGeoData );

    /* Free memory used by the neighbor list array. */
    MPEFieldGen_freeNeighborList ( ptrGeoData );

    if(ptrGeoData != NULL)
    {
        free(ptrGeoData);
        ptrGeoData = NULL;
    }

    /* Free memory associated with the station location buffer. */
    free_mpe_latlon_info ( );

} /* end destructor() */

void destructorByRadarLoc ( int radarLocNum )
{
   int i ;

   for ( i = 0; i < radarLocNum ; ++i )
   {
      if ( MPEFieldGen_radarMiscBins [ i ] != NULL )
      {
         free ( MPEFieldGen_radarMiscBins [ i ] );
         MPEFieldGen_radarMiscBins [ i ] = NULL;
      }
   }

   if ( MPEFieldGen_radarMiscBins != NULL )
   {
      free ( MPEFieldGen_radarMiscBins );
      MPEFieldGen_radarMiscBins = NULL;
   }

/*  ===================================================  */

} /* end destructorByRadarLoc */


