/*******************************************************************************
* FILENAME:    check_spatial_consistency.c
*
* Purpose:
* This function is converted from FORTRAN code: sccqc.f.
* it does the spatial consisteny check.
*
* calling function: readGageData
* functions called: subCheckSpatialConsistency
*
*    We calculate QC region boundaries.
*    minLatitude  = southern most latitude
*    maxLatitude  = norhtern most latitude 
*    maxLongitude = western most longitude
*    minLongitude = eastern most longitude
*
* input variables
*
* pMPEParams - static parameters
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* pGageTable - array of gage data
*
* output variables
*
* gageqc - result array for spatial consisteny check for gage array. 
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*                C. R. Kondragunta Original FORTRAN code  
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/

#include "mpe_fieldgen.h"

void MPEFieldGen_sort (double * data, const int size, int * index) ;

void MPEFieldGen_subCheckSpatialConsistency(const gage_table_struct * pGageTable,
                            const double minLatitude,
                            const double maxLatitude,
                            const double minLongitude,
                            const double maxLongitude,
                            const double threshold,
                            int * gageqc) ;

void MPEFieldGen_checkSpatialConsistency(const mpe_params_struct * pMPEParams, 
                            const geo_data_struct *pGeoData,
                            const gage_table_struct * pGageTable,
                            int * gageqc)
{
    double minLatitude, maxLatitude ;
    double minLongitude, maxLongitude ;
    double row, col;
    double lat, lon;
    double threshold ;
    char   tokenvalue[TOKEN_VALUE_LEN] = {'\0'};
    int    qctype, i;

    static int first = 1 ;
    static int flag = 1 ;

    qctype = 0 ;

    /**
      * Retrieve the longitude and latitude from the HRAP coordinates
      * Compute the boundary of this site based on geo_data file.
      **/
      
    minLatitude = 90.0 ;
    maxLatitude = 0.0 ;
    minLongitude = 180.0 ;
    maxLongitude = 0.0 ;

    row = pGeoData->hrap_y ;
    col = pGeoData->hrap_x ;
    HrapToLatLongByReference(row, col, &lat, &lon);
    if (lat < minLatitude)
        minLatitude = lat ;
    if (lat > maxLatitude)
        maxLatitude = lat ;
    if (lon < minLongitude)
        minLongitude = lon ;
    if (lon > maxLongitude)
        maxLongitude = lon ;

    row = pGeoData->hrap_y + pGeoData->num_rows ;
    col = pGeoData->hrap_x ;
    HrapToLatLongByReference(row, col, &lat, &lon);
    if (lat < minLatitude)
        minLatitude = lat ;
    if (lat > maxLatitude)
        maxLatitude = lat ;
    if (lon < minLongitude)
        minLongitude = lon ;
    if (lon > maxLongitude)
        maxLongitude = lon ;

    row = pGeoData->hrap_y ;
    col = pGeoData->hrap_x + pGeoData->num_cols ;
    HrapToLatLongByReference(row, col, &lat, &lon);
    if (lat < minLatitude)
        minLatitude = lat ;
    if (lat > maxLatitude)
        maxLatitude = lat ;
    if (lon < minLongitude)
        minLongitude = lon ;
    if (lon > maxLongitude)
        maxLongitude = lon ;

    row = pGeoData->hrap_y + pGeoData->num_rows ;
    col = pGeoData->hrap_x + pGeoData->num_cols ;
    HrapToLatLongByReference(row, col, &lat, &lon);
    if (lat < minLatitude)
        minLatitude = lat ;
    if (lat > maxLatitude)
        maxLatitude = lat ;
    if (lon < minLongitude)
        minLongitude = lon ;
    if (lon > maxLongitude)
        maxLongitude = lon ;

    minLatitude -= 1.0 ;
    maxLatitude += 1.0 ;
    minLongitude -= 1.0 ;
    maxLongitude += 1.0 ;
    
    if(flag == 1)
    {
        sprintf ( message , "\tminLatitude  maxLatitude"
            "  minLongitude  maxLongitude\n"
            "\t%10.2f   %10.2f    %10.2f    %10.2f",
            minLatitude, maxLatitude, minLongitude, maxLongitude ) ;
        printMessage( message, logFile );
    }

    threshold = 2.0 ;
      
    if(first == 1)
    {
        if(getAppsDefaults("mpe_sccqc_threshold", tokenvalue) == -1)
        {
            sprintf ( message , "WARNING: Invalid token value for"
                " \"mpe_sccqc_threshold\"."
                "\nDefault value to %f.",  threshold) ;
            printMessage(message, logFile );
        }
        else
        {
            threshold = atof(tokenvalue);
            if((threshold < 0.5) || (threshold > 6.0))
                threshold = 2.0 ;
        }
    }

    if(flag == 1)
    {
        sprintf ( message , "\tThreshold of Spatial Consistency Check is:"
                " %5.2f (dimensionless)", threshold ) ;
        printMessage( message, logFile );
    }
    
    sprintf ( message , "\tNumber of gauge data reports is: %d",
            (pGageTable->totalGageNum - pGageTable->pseudoGageNum) ) ;
    printMessage( message, logFile );

    for(i = 0; i < pGageTable->totalGageNum; i++)
        gageqc[i] = 0 ;

    /**    
      * Spatial Consistency Check for given region
      * within the boundaries minLatitude,maxLatitude,
      * maxLongitude and minLongitude.
    **/
    MPEFieldGen_subCheckSpatialConsistency(pGageTable, minLatitude, maxLatitude, 
        minLongitude, maxLongitude, threshold, gageqc) ;
    
    for(i = pGageTable->pseudoGageNum; i < pGageTable->totalGageNum; i++)
    {
        if(gageqc[i] > 0)
        {
            sprintf ( message , "\t  GageID Longitude Latitude"
                     " Value(mm) num_boxes_failed" );
            printMessage( message, logFile );
            break ;
        }
    }

    for(i = pGageTable->pseudoGageNum; i < pGageTable->totalGageNum; i++)
    {
        if(gageqc[i] > 0)
        {
            sprintf ( message , "\t%8s %9.3f %8.3f %9.4f %10d", 
                    pGageTable->ptrGageRecords[i].gageID,
                    pGageTable->ptrGageRecords[i].longitude,
                    pGageTable->ptrGageRecords[i].latitude,
                    pGageTable->ptrGageRecords[i].gageValue,
                    gageqc[i]);
            printMessage( message, logFile );
        }
    }
    
    flag = 0 ;
    first = 0 ;

}


/*******************************************************************************
* Purpose:
* This function performs Spatial Consistency Check based on
* standard deviation to quality control spatial data.
*
* calling function: subCheckSpatialConsistency
* functions called: none
*
* input variables
* data      : Input data
* arraySize : Number of data points
* stndid    : Id of each station
* threshold : Precip. threshold to compare index against.
*
* output variables
* qc_flag    : QC flag indicating whether a station is 
*              good or questionable according to SCC.  
*              0 indiacates data is OK
*              1 indicates data is an outlier     
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*                C. R. Kondragunta Original FORTRAN code  
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/
void MPEFieldGen_stat1(const double * data,
           const int arraySize,
           const int * stnid, 
           const double threshold,
           int * qc_flag)
{
    double sum, std, mean, temp, dev ;
    int outlier[arraySize] ;
    int id, i ;

    std  = 0.0 ;
    sum  = 0.0 ;
    mean = 0.0 ;
    dev  = 0.0 ;

    for(i = 0; i < arraySize ; i++)
    {
        outlier[i] = 0 ;
        qc_flag[i] = 0 ;
    }

    if(arraySize <= 2)
    {
        for(i = 0; i < arraySize ; i++)
        {
            if(data[i] > 0.0 )
                outlier[i] = 1 ;
        }
    }
    else
    {
        for(i = 0; i < arraySize ; i++)
            mean += data[i] / (double)arraySize ;
        
        sum = 0.0 ;
        for(i = 0; i < arraySize ; i++)
        {
            temp = data[i] - mean ;
            sum += temp * temp ;
        }

        std = sqrt( sum / (double)( arraySize - 1 ) ) ;

        for(i = 0; i < arraySize ; i++)
        {
            outlier[i] = 0 ;
            dev = data[i] - mean ;
            if(dev > (1.0 * std))
            {
                if(data[i] > 0.0)
                    outlier[i] = 1 ;
            }
        }
    }

    for(i = 0; i < arraySize ; i++)
    {
        id = stnid[i] ;
        qc_flag[id] = outlier[i] ;
    }
}


/*******************************************************************************
* Purpose:
* This function performs Spatial Consistency Check to
* quality control spatial data.
*
* calling function: subCheckSpatialConsistency
* functions called: none
*
* input variables
* data        : Input data
* arraySize    : Number of data points
* stndid    : Id of each station
* threshold : Precip. threshold to compare index against.
*
* output variables
* qc_flag    : QC flag indicating whether a station is 
*              good or questionable according to SCC.  
*            0 indiacates data is OK
*            1 indicates data is an outlier     
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*                C. R. Kondragunta Original FORTRAN code  
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/
void MPEFieldGen_stat2(double * data,
           const int arraySize,
           int * stnid, 
           const double threshold,
           int * qc_flag)
{
    double    sum, std, mean, rmed, mad, diff, temp ;
    double    an1, an2, p75, p25 ;
    double    qc_index[arraySize] ;
    int outlier[arraySize] ;
    int med, n1, n2, id ;
    int i ;

    med = 0 ;
    mad = 0.0 ;
    std = 0.0 ;
    diff = 0.0 ;
    an1 = 0.0 ;
    an2 = 0.0 ;
    p75 = 0.0 ;
    p25 = 0.0 ;
    sum = 0.0 ;
    n1 = 0 ;
    n2 = 0 ;
    
    /* sort data array in ascendant order */
    MPEFieldGen_sort (data, arraySize, stnid) ;
    
    for(i = 0; i < arraySize; i++)
    {
        outlier[i] = 0 ;
        qc_flag[i] = 0 ;
        qc_index[i] = 0.0 ;
    }

    mean = 0.0 ;
    for(i = 0; i < arraySize; i++)
    {
        mean += data[i] / (double)arraySize ;
    }

    sum = 0.0 ;
    for(i = 0; i < arraySize; i++)
    {
        temp = data[i] - mean ;
        sum += temp * temp ;
    }

    std = sqrt(sum / (double)(arraySize-1)) ;

    n2 = arraySize / 2 ;
    n1 = arraySize - n2 * 2 ;

    if(n1 == 1)
    {
        med = n2;
        rmed = data[med] ;
    }
    else
        rmed = (data[n2] + data[n2-1]) / 2.0 ;

    sum = 0.0 ;
    for(i = 0; i < arraySize; i++)
    {
        sum += fabs(data[i] - rmed) ;
    }

    mad = sum / (double)arraySize ;

    an1 = arraySize * 0.75 ;
    an2 = arraySize * 0.25 ;
    n1 = (int)an1 ;
    n2 = (int)an2 ;
    diff = an1 - n1 ;

    if(diff != 0.0)
    {
        p75 = data[n1] ;
        p25 = data[n2] ;
    }
    else
    {
        p75 = (data[n1] + data[n1 - 1]) / 2.0 ;
        p25 = (data[n2] + data[n2 - 1]) / 2.0 ;
    }

    for(i = 0; i < arraySize; i++)
    {
        outlier[i] = 0 ;
        if (mad == 0)
            qc_index[i] = 0.0 ;
        else if (p75 != p25)
            qc_index[i] = (data[i] - rmed) / (double)(p75 - p25) ;
        else
            qc_index[i] = (data[i] - rmed) / (double)mad ;

        if(qc_index[i] >= threshold)
            outlier[i] = 1 ;
    }

    for(i = 0; i < arraySize; i++)
    {
         id = stnid[i] ;
         qc_flag[id] = outlier[i] ;
    }
}


/*******************************************************************************
* Purpose:
* This function sorts the data in ascending order
* and update the corresponding index.
*
* calling function: stat2
* functions called: none
*
* input variables
* data    : data to be sorted in ascending order
* size    : Number of stations 
* index    : record position in gage array.
*
* output variables
* data    : data to be sorted in ascending order
* index    : record position in gage array after sort.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*                C. R. Kondragunta Original FORTRAN code  
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/
void MPEFieldGen_sort (double * data, const int size, int * stnid)
{
    int i, j, itemp;
    double temp ;

    for(i = 0; i < size - 1; i++)
    {
        if(data[i] > data[i+1])
        {
            temp = data[i+1] ;
            itemp = stnid[i+1] ;
            j = i ;

            while((j >= 0) && (data[j] > temp))
            {
                data[j+1] = data[j] ;
                stnid[j+1] = stnid[j] ;
                j = j - 1 ;
            }

            data[j+1] = temp ;
            stnid[j+1] = itemp ;
        }
    }
}


/*******************************************************************************
* Purpose:
* This function performs Spatial Consistency Check for given region
* within the boundaries minLatitude,maxLatitude,maxLongitude and minLongitude.
*
* calling function: checkSpatialConsistency
* functions called: stat1, stat2
*
* input variables
* pGageTable    : array of gage_table_struct for gage data.
* minLatitude    : southern most lat. of the region
* maxLatitude    : northern most lat. of the region
* maxLongitude    : western most lon. of the region
* minLongitude    : eastern most lon. of the region
* threshold        : threshold used in the SCC, usually set to 2.0
*
* output variables
* gageqc        : QCed gages, 3 or less is OK and 4 is an suspect.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*                C. R. Kondragunta Original FORTRAN code  
*   March 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/
void MPEFieldGen_subCheckSpatialConsistency(const gage_table_struct * pGageTable,
                            const double minLatitude,
                            const double maxLatitude,
                            const double minLongitude,
                            const double maxLongitude,
                            const double threshold,
                            int * gageqc)
{
    const int intlat = ((int)maxLatitude - (int)minLatitude) * 2 ;
    const int intlon = ((int)maxLongitude - (int)minLongitude) * 2 ;
    const int pseudoSize = pGageTable->pseudoGageNum ;
    const int gageSize = pGageTable->totalGageNum ;

    int stndtflg[gageSize], stnid[gageSize] ;
    int i, j, k, it1, count ;
    int intlat1, intlon1 ;
    double    dlat, dlon ;
    double    stndt[gageSize] ;
    double    dlat1[intlat+1], dlon1[intlon+1] ;

    for(i = 0; i < gageSize; i++)
    {
        gageqc[i] = 0.0 ;
        stndtflg[i] = 0 ;
        stnid[i] = 0 ;
        stndt[i] = 0.0 ;
    }

    dlat = (maxLatitude - minLatitude) / (double)intlat ;
    dlon = (maxLongitude - minLongitude) / (double)intlon ;
    intlat1 = intlat + 1 ;
    intlon1 = intlon + 1 ;

    for(i = 0; i < intlat1; i++)
    {
        dlat1[i] = maxLatitude - i * dlat ;
    }

    for(i = 0; i < intlon1; i++)
    {
        dlon1[i] = maxLongitude - i * dlon ;
    }

    for(j = 1; j < intlat; j++)
    {
        for(i = 1; i < intlon; i++)
        {
            count = 0 ;
            for ( k = pseudoSize; k < gageSize; k++ )
            {
                if( (pGageTable->ptrGageRecords[k].latitude <= dlat1[j-1] ) &&
                    (pGageTable->ptrGageRecords[k].latitude >= dlat1[j+1] ) &&
                    (pGageTable->ptrGageRecords[k].longitude <= dlon1[i-1] ) &&
                    (pGageTable->ptrGageRecords[k].longitude >= dlon1[i+1] ) )
                {
                    if(pGageTable->ptrGageRecords[k].gageValue >= 0.0)
                    {
                        stndt[count] = pGageTable->ptrGageRecords[k].gageValue ;
                        stnid[count] = k ;
                        count ++ ;
                    }
                }
            }

            if(count >= 1)
            {
                if(count < 5)
                    MPEFieldGen_stat1(stndt, count, stnid, threshold, stndtflg) ;
                else
                    MPEFieldGen_stat2(stndt, count, stnid, threshold, stndtflg) ;

                for ( k = 0; k < count; k++ )
                {
                    it1 = stnid[k] ;
                    gageqc[it1] += stndtflg[it1] ;
                }
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/check_spatial_consistency.c,v $";
 static char rcs_id2[] = "$Id: check_spatial_consistency.c,v 1.1 2007/10/15 12:19:07 dsa Exp $";}
/*  ===================================================  */

}
