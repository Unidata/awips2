/*******************************************************************************
* FILENAME:            build_neighbor_list.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
* MODULE 1:          buildNeighborList
* DESCRIPTION:      build the gage neighbor list for each bin cell within the geo area.
*
* ORIGINAL AUTHOR:     Guoxian Zhou
* CREATION DATE:       September 8, 2005
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        Sept 8, 2005 Guoxian Zhou      Original Coding
********************************************************************************
*/
#include "mpe_fieldgen.h"

static neighbor_list_struct ** ptrNeighborList  = NULL;

void allocNeighborList(const geo_data_struct * pGeoData) ;

void  MPEFieldGen_buildNeighborList (const geo_data_struct * pGeoData ,
                         mpe_params_struct * pMPEParams,
                         const int gageSize, short * iug,  short * ivg, float * zg )
{   
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    static int first = 1;

    short arrIndex[gageSize] ;
    float arrDist[gageSize];

    short ivv_save[gageSize], in_save[gageSize] ;

    int i_prev = -1 ;
    int m_save = -1 ;
    int listNum ;
    int i, j;
    int iradi = 0;

    double radi = RADIUS ;
    double flat, flon, rmesh;
    double mid_hrap ;
    int indx;

    if ((pMPEParams->polarizationType == SinglePol) && (pMPEParams->build_neighbor_list_SP == 1)) return;

    if((pMPEParams->polarizationType == DualPol) && (pMPEParams->build_neighbor_list_DP == 1)) return;

    if (pMPEParams->polarizationType == SinglePol) 
    { 
            pMPEParams->build_neighbor_list_SP = 1;
    }

    if (pMPEParams->polarizationType == DualPol) 
    { 
            pMPEParams->build_neighbor_list_DP = 1;
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin building neighbor list." , 
                    currTime) ;
    printMessage( message, logFile );        

    if ( first == 1 )
    {
       /* This only needs to be done once. */
       first = 0;
       allocNeighborList( pGeoData) ;
    }

    if(gageSize > 1)
        MPEFieldGen_heapSortForGeoIndex(zg, iug, ivg, gageSize) ;

    /**      
     * compute the size of the HRAP grid in km
     * at the center of the estimation domain.
     **/
    mid_hrap = pGeoData->hrap_y * 1.0 + 
               rowSize * 1.0 / 2.0 ;

    hrap_to_latlon((double)pGeoData->hrap_x, mid_hrap, &flon, &flat);
    hrapsize(flat, &rmesh) ;

    /**      
     * convert distances in km to those in HRAP bins.
     **/
    if(rmesh != 0.0)
    {
        radi /= rmesh ;
        iradi = (int)(radi + 0.5) ;
    }

    for(i = 0; i < colSize; i ++)
    {
        for(j = 0; j < rowSize; j++)
        {
            /**
             * use double heap-sorting of array indices
             **/
            MPEFieldGen_find_nbrsX(gageSize, iug, ivg, i, j, iradi, &listNum,
                arrIndex, arrDist, &i_prev, &m_save, ivv_save,
                in_save) ;

            if(ptrNeighborList[i][j].pIndex != NULL)
            {
                free(ptrNeighborList[i][j].pIndex);
                ptrNeighborList[i][j].pIndex = NULL;
            }

            if(ptrNeighborList[i][j].pDistance != NULL)
            {
                free(ptrNeighborList[i][j].pDistance);
                ptrNeighborList[i][j].pDistance = NULL;
            }

            if(listNum <= 0)
            {
               ptrNeighborList[i][j].listSize = 0 ;
			   continue ;
            }

            ptrNeighborList[i][j].listSize = listNum ;

            ptrNeighborList[i][j].pIndex = 
                (short *)malloc(listNum * sizeof(short)); 
            if(ptrNeighborList[i][j].pIndex == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in buildNeighborList function."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }

            ptrNeighborList[i][j].pDistance = 
                (float *)malloc(listNum * sizeof(float)); 
            if(ptrNeighborList[i][j].pDistance == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in buildNeighborList function."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }

            for(indx = 0; indx < listNum; indx++)
            {
                ptrNeighborList[i][j].pIndex[indx]      = arrIndex[indx] ;
                ptrNeighborList[i][j].pDistance[indx] = arrDist[indx] ;
            }
        }
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end building neighbor list." , 
                    currTime) ;
    printMessage( message, logFile );        

    return ;
}


void allocNeighborList(const geo_data_struct * pGeoData)
{
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;
	
    int i, j;

    ptrNeighborList = 
        (neighbor_list_struct **)malloc(colSize * sizeof(neighbor_list_struct *)); 
    if(ptrNeighborList == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocNeighborList function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < colSize; i++)
    {
        ptrNeighborList[i] =
            (neighbor_list_struct *)malloc(rowSize * sizeof(neighbor_list_struct)); 
        if(ptrNeighborList[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocNeighborList function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        
        for(j=0; j < rowSize; j++)
        {
            ptrNeighborList[i][j].pIndex    = NULL ;
            ptrNeighborList[i][j].pDistance = NULL ;
        }
    }
}

void MPEFieldGen_freeNeighborList(const geo_data_struct * pGeoData)
{
    int i, j;
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    if(ptrNeighborList != NULL)
    {
        for(i = 0; i < colSize; i++)
        {
            if(ptrNeighborList[i] != NULL)
            {
                for(j = 0; j < rowSize; j++)
                {
                    if(ptrNeighborList[i][j].pIndex != NULL)
                    {
                        free(ptrNeighborList[i][j].pIndex);
                        ptrNeighborList[i][j].pIndex = NULL;
                    }
        
                    if(ptrNeighborList[i][j].pDistance != NULL)
                    {
                        free(ptrNeighborList[i][j].pDistance);
                        ptrNeighborList[i][j].pDistance = NULL;
                    }
                }

                free(ptrNeighborList[i]);
                ptrNeighborList[i] = NULL;
            }
        }

        free(ptrNeighborList);
        ptrNeighborList = NULL;
    }
}


/*******************************************************************************
* MODULES NAME:        findNeighborList
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          findNeighborList
* DESCRIPTION:         pick up the gage neighbor list within radius
*
* ORIGINAL AUTHOR:     Guoxian Zhou
* CREATION DATE:       September 8, 2005
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        Sept 8, 2005 Guoxian Zhou      Original Coding
********************************************************************************
*/
void  MPEFieldGen_findNeighborList (const int radius,
                        		 const int index_x,
                        		 const int index_y,
                        		 short * arrIndex,
                        		 float * arrDist,
                        		 int * listNum )
{
    int i;
    int count = 0;
    int neighbor_distance;

    if(ptrNeighborList[index_x][index_y].listSize > 0)
    {
        for(i = 0; i < ptrNeighborList[index_x][index_y].listSize; i ++)
        {
            /* Try rounding the neighbor distance. */
            neighbor_distance = (int )( 
                        ptrNeighborList[index_x][index_y].pDistance[i] + 0.5 ); 

            if( neighbor_distance > radius)
            {
                break ;
            }

            arrIndex[count] = ptrNeighborList[index_x][index_y].pIndex[i] ;
             arrDist[count] = ptrNeighborList[index_x][index_y].pDistance[i] ;

            count ++ ;
        }
    }

    *listNum = count ;

    return ;
}

/*******************************************************************************
* MODULES NAME:        findLocalBiasNeighborList
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          findLocalBiasNeighborList
* DESCRIPTION:         pick up the neighbor list for gage/radar pair within radius
*
* ORIGINAL AUTHOR:     Guoxian Zhou
* CREATION DATE:       September 8, 2005
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        Sept 8, 2005 Guoxian Zhou      Original Coding
********************************************************************************
*/
void  MPEFieldGen_findLocalBiasNeighborList (
						const gage_radar_pair_table_struct * pGageRadarPair,
                        const short * iug,
                        const short * ivg,
                        const int radius,
                        const int index_x,
                        const int index_y,
                        short * arrIndex,
                        float * arrDist,
                        int * listNum )
{
    int i, j;
    int count = 0;
    int hrap_x, hrap_y, index;
    int blnSearch ;

    for(i = 0; i < ptrNeighborList[index_x][index_y].listSize; i ++)
    {
        if((int)(ptrNeighborList[index_x][index_y].pDistance[i]) >= radius)
            break ;

        blnSearch = 0; 

        index = ptrNeighborList[index_x][index_y].pIndex[i] ;
        hrap_x = iug[index] ;
        hrap_y = ivg[index] ;

        for ( j = 0; j < pGageRadarPair->pairNum; ++j )
        {
            if(  (hrap_x == pGageRadarPair->ptrGageRadarPair[j].hrap_x) &&
                (hrap_y == pGageRadarPair->ptrGageRadarPair[j].hrap_y) )
            {
                blnSearch = 1;
                break ;
            }
        }

        if(blnSearch == 1)
        {
            arrIndex[count] = j ;
              arrDist[count] = ptrNeighborList[index_x][index_y].pDistance[i] ;

            count ++ ;
        }
    }
    
    *listNum = count ;

    return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/build_neighbor_list.c,v $";
 static char rcs_id2[] = "$Id: build_neighbor_list.c,v 1.1 2007/10/15 12:19:05 dsa Exp lawrence $";}
/*  ===================================================  */

}
