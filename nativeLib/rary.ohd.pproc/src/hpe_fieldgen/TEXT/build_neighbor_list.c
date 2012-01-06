/*******************************************************************************
* FILENAME:            build_neighbor_list.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
* MODULE 1:          buildNeighborList
* DESCRIPTION:       build the gage neighbor list for each bin cell
*                    within the geo area.
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

#include "empe_fieldgen.h"

static neighbor_list_struct ** ptrNeighborList  = NULL;

static void allocNeighborList(const geo_data_struct * pGeoData) ;

void  buildNeighborList (const geo_data_struct * pGeoData ,
                         empe_params_struct * pMPEParams,
                         const int gageSize,
                         short * iug,  short * ivg, float * zg )
{
    if(pMPEParams->build_neighbor_list == 1)
    {
        return ;
    }

    pMPEParams->build_neighbor_list = 1 ;
    
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

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin building neighbor list." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );        

    if ( first == 1 )
    {
       /* This only needs to be done once. */
       first = 0;
       allocNeighborList( pGeoData) ;
    }

    if(gageSize > 1)
        heapSortForGeoIndex(zg, iug, ivg, gageSize) ;

    /*      
     * compute the size of the HRAP grid in km
     * at the center of the estimation domain.
     * 
     * made a change to consider HRAP and Quarter HRAP
     * -- gzhou 12/2006
     */

    mid_hrap = pGeoData->hrap_y * 1.0 + 
               rowSize * 1.0 / 2.0 ;

    double hrap_x = (double)pGeoData->hrap_x / pMPEParams->hrap_grid_factor;
    double hrap_y = mid_hrap / pMPEParams->hrap_grid_factor;

/*
    hrap_to_latlon((double)pGeoData->hrap_x, mid_hrap, &flon, &flat);
*/

    hpe_fieldgen_hrap_to_latlon(hrap_x, hrap_y, &flon, &flat);

    hpe_fieldgen_hrapsize(flat, &rmesh) ;

    /**      
     * convert distances in km to those in HRAP bins.
     **/

    if(rmesh != 0.0)
    {
        /*
         * adjust the grid range based by HRAP grid factor.
         * added by gzhou 06-2007
         */
        rmesh /= pMPEParams->hrap_grid_factor ;
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

            find_nbrsX(gageSize, iug, ivg, i, j, iradi, &listNum,
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
                shutdown( message );
            }

            ptrNeighborList[i][j].pDistance = 
                (float *)malloc(listNum * sizeof(float)); 
            if(ptrNeighborList[i][j].pDistance == NULL)
            {
                sprintf ( message , "ERROR: memory allocation failure"
                    " in buildNeighborList function."
                    "\n\tProgram exit.") ;
                shutdown( message );
            }

            for(indx = 0; indx < listNum; indx++)
            {
                ptrNeighborList[i][j].pIndex[indx]    = arrIndex[indx] ;
                ptrNeighborList[i][j].pDistance[indx] = arrDist[indx] ;
            }
        }
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end building neighbor list." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );        

    return ;
}


static void allocNeighborList(const geo_data_struct * pGeoData)
{
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;
    
    int i, j;

    ptrNeighborList = 
        (neighbor_list_struct **)
        malloc(colSize * sizeof(neighbor_list_struct *)); 

    if(ptrNeighborList == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocNeighborList function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    for(i = 0; i < colSize; i++)
    {
        ptrNeighborList[i] =
            (neighbor_list_struct *)
            malloc(rowSize * sizeof(neighbor_list_struct)); 

        if(ptrNeighborList[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocNeighborList function."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        
        for(j=0; j < rowSize; j++)
        {
            ptrNeighborList[i][j].pIndex    = NULL ;
            ptrNeighborList[i][j].pDistance = NULL ;
        }
    }
}

void freeNeighborList(const geo_data_struct * pGeoData)
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
void  findNeighborList (const int radius,
                        const int index_x,
                        const int index_y,
                        short * arrIndex,
                        float * arrDist,
                        int * listNum )
{
    int i;
    int count = 0;
    if(ptrNeighborList[index_x][index_y].listSize > 0)
    {
        for(i = 0; i < ptrNeighborList[index_x][index_y].listSize; i ++)
        {
            if((int)(ptrNeighborList[index_x][index_y].pDistance[i]) >= radius)
                break ;

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
* DESCRIPTION:         pick up the neighbor list for gage/radar pair
*                      within radius
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
void  findLocalBiasNeighborList (
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
            if( (hrap_x == pGageRadarPair->ptrGageRadarPair[j].hrap_x) &&
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
}
