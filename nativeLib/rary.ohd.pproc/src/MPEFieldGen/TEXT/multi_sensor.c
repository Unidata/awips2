#include "mpe_fieldgen.h"
#include "multi_sensor.h"

int ** ndgag = NULL ;
int ** npgag = NULL ;
int ** ndata = NULL ;
int ** nposi = NULL ;
double ** gag = NULL ;

/***********************************************************************
* FILENAME:            multi_sensor.c
*
* Purpose:
* This function is converted from FORTRAN code: multi_sensor.f.
* This function performs multi-sensor rainfall estimation
* using radar and rain gage data
* via the Single Optimal Estimation  (Seo 1998).
*
* calling function:
* functions called: HrapToLatLong, hrapsize, coef_var_r,
*        cor_scale_r, find_indices, local_stat, findNeighborList
*        srch_nbrs, ms_soe
*
* input variables
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain
*
* pRWParams - static parameters from the RWParams table.
*
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
* rad      - [rowSize][colSize] array containing bias-adjusted
*            radar rainfall in mm
*
* mis      - [rowSize][colSize] misbin array
*
* umeang   - [rowSize][colSize] PRISM array in mm
*
* output variables
*
* est      - [rowSize][colSize] array of estimated rainfall in mm
*
* errFlag  - error flag; 0 if OK, 1 if setting multi-sensor field to radar based field
*                        2 if error in multi-sensor field generation
*
* MODIFICATION HISTORY:
*   DATE           PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998   D.-J. Seo         Original FORTRAN code
*   Jun 29, 2005   Guoxian Zhou      finish conversion to C Language
*   Jul 17, 2005   Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_multi_sensor(const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** rad ,
                int ** mis ,
                double ** umeang ,
                double ** est ,
                int * errFlag)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    RWParams * pRWParams = pMPEParams->ptrRWParams ;
    /**
     * save original static value for the following parameters
     * (load from rwparams table)
     *
     * isrch_g  - nearest-neighbor search method
     *          - 1 for double heap sorting
     *          - 2 for spiral search
     * rain_min - minimum detectable rainfall in mm
     * dist_min - separation distance in km
     * crscori  - indicator cross-correlation coefficient
     * crscorc  - conditional cross-correlation coefficient
     * nbrs_gag - number of nearest gage data to be located
     * nbrs_rad - number of nearest radar data to be located
     * cvr_def  - default conditional coefficient of variation
     *            of radar rainfall
     * rngi_def - default indicator correlation scale in km
     * rngc_def - default conditinal correlation scale in km
     * rngi_min - minimum indicator correlation scale in km
     * rngc_min - minimum conditional correlation scale in km
     * rngi_max - maximum indicator correlation scale in km
     * rngc_max - maximum conditional correlation scale in km
     **/

    double rngi_def = pRWParams->def_ind_corr_scl ;
    double rngc_def = pRWParams->def_cond_corr_scl ;
    double rngi_min = pRWParams->min_ind_corr_scl ;
    double rngc_min = pRWParams->min_cond_corr_scl ;
    double rngi_max = pRWParams->max_ind_corr_scl ;
    double rngc_max = pRWParams->max_cond_corr_scl ;
    double cvr_def  = pRWParams->def_cond_var_rad ;
    double rain_min = pRWParams->rw_min_rain ;
    double dist_min = pRWParams->rw_sep_dist ;
    double crscori  = pRWParams->rw_lag0_ind_corr ;
    double crscorc  = pRWParams->rw_lag0_cond_corr ;
    int isrch_g  = pRWParams->nn_srch_method ;
    int nbrs_gag = pRWParams->num_near_gages ;
    int nbrs_rad = pRWParams->num_near_rad_bins ;

    int i, j, errFl = 0;
    short iur[nbrs_rad], ivr[nbrs_rad] ;
    short ilist_g[gageSize] ;

    float zr[nbrs_rad] ;
    float rlist_g[gageSize];

    /**
     * if there is less than one rain gage availible,
     * the radar only field will be directly
     * substituted for the multisensor field.
     **/
    if(gageSize < 1)
    {
        sprintf( message , "no gages available - "
            "setting multisensor field"
            " to radar based field.") ;
        printMessage( message, logFile );
        *errFlag = 1 ;

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
                est[i][j] = rad[i][j] ;
        }
        return ;
    }



    allocMultiMemory(pGeoData);

    /**
     * check if the number of gage data available
     * is less than the number of neighbors to be located:
     * if so, reset the number of neighbors to be
     * located to the number of gage data available.
     **/
    if(gageSize < nbrs_gag)
        nbrs_gag = gageSize ;

    for(i = 0; i < gageSize; i ++)
    {
        ilist_g[i] = 0 ;
        rlist_g[i] = 0.0 ;
    }

    /**
     * locate the nearest neighbors
     **/
    if(isrch_g == 1)
    {
        /**
         * use double heap-sorting
         **/
/*
        if(gageSize > 1)
            heapSortForGeoIndex(zg, iug, ivg, gageSize) ;
*/
    }
    else if(isrch_g == 2)
    {
        /**
         * use spriral search
         **/
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
                gag[i][j] = -0.1 ;
        }

        for(i = 0; i < gageSize; i ++)
        {
            gag[ivg[i]][iug[i]] = zg[i] ;
        }
    }
    else
    {
        sprintf( message , "undefined option for"
            " nearest neighbor search\n"
            "setting multisensor field"
            " to radar based field") ;
        printMessage( message, logFile );

        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
                est[i][j] = rad[i][j] ;
        }

        *errFlag = 1 ;
        return ;
    }

    /**
     * compute the size of the HRAP grid in km
     * at the center of the estimation domain
     **/
    double mid_hrap = pGeoData->hrap_y * 1.0 + rowSize * 1.0 / 2.0 ;
    double flon, flat, rmesh ;

    hrap_to_latlon((double)pGeoData->hrap_x, mid_hrap, &flon, &flat);

    hrapsize(flat, &rmesh) ;

    /**
     * convert distances in km to HRAP bins
     **/
    if(rmesh != 0.0)
    {
        dist_min /= rmesh ;
        rngi_def /= rmesh ;
        rngc_def /= rmesh ;
        rngi_min /= rmesh ;
        rngc_min /= rmesh ;
        rngi_max /= rmesh ;
        rngc_max /= rmesh ;
    }

    /**
     * estimate the coefficient of variation of positive radar rainfall
     **/
    double cvr = 0.0 ;
    MPEFieldGen_coef_var_r(rowSize, colSize, rad, mis, rain_min, &cvr, errFlag) ;

    /**
     * if calculation is not successful, assign default value
     * cvr = 0.0
     **/
    if(*errFlag != 0)
        cvr = cvr_def ;
    /**
     * estimate the indicator and conditional correlation
     * coefficients (in HRAP bins)
     **/
    double rngi = 0.0, rngc = 0.0 ;
    MPEFieldGen_cor_scale_r(rowSize, colSize, rad, mis, &rngi, &rngc, errFlag) ;

    if(*errFlag == 0)
    {
        /**
         * calculation is successful: check if they fall
         * within the ranges prescribed
         **/
        if(rngi < rngi_min)
            rngi = rngi_min ;
        if(rngc < rngc_min)
            rngc = rngc_min ;
        if(rngi > rngi_max)
            rngi = rngi_max ;
        if(rngc > rngc_max)
            rngc = rngc_max ;
    }
    else
    {
        /**
         * calculation is not successful:
         * assign default values
         **/
        rngi = rngi_def ;
        rngc = rngc_def ;
    }

    /**
     * specify indicator correlation parameters
     **/
    double cor0pig = 1.0 ;
    double cor0pir = 1.0 ;
    double cor0ig = 1.0 ;
    double cor0ir = 1.0 ;
    double rngig = rngi ;
    double rngir = rngi ;
    double rngic = rngi ;

    /**
     * specify conditional correlation parameters
     **/
    double cor0pcg = 1.0 ;
    double cor0pcr = 1.0 ;
    double cor0cg = 1.0 ;
    double cor0cr = 1.0 ;
    double rngcg = rngc ;
    double rngcr = rngc ;
    double rngcc = rngc ;

    /**
     * specify lag-zero indicator and conditional
     * cross-correlation coefficients
     **/
    double cor0pic = crscori ;
    double cor0ic = crscori ;

    double cor0pcc = crscorc ;
    double cor0cc = crscorc ;

    /**
     * specify the radius of influence in HRAP bins
     **/
    double radi ;
    if(rngi > rngc)
        radi = rngi ;
    else
        radi = rngc ;

    const int iradi = (int)(radi + 0.5) ;
    const int index = (2 * iradi + 1) * (2 * iradi + 1) ;
    int n ;
    int idis[index];
    int jdis[index];

    /**
     * find array indices for nearest-neighbor search
     **/
    MPEFieldGen_find_indices(iradi,&n,idis,jdis) ;

    /**
     * collect via successive subtraction and
     * addition local statistics of
     * bias-adjusted radar rainfall or,
     * if isrch_g eq 2, of gage rainfall
     * within the radius of influence
     **/
    int nfill = 0 ;
    MPEFieldGen_local_stat(rowSize, colSize, 2, iradi, rad,
        mis, &nfill, ndata, nposi) ;

    int nfgag = 0.0 ;
    if(isrch_g == 2)
        MPEFieldGen_local_stat(rowSize, colSize, 1, iradi, gag,
            mis, &nfgag, ndgag, npgag) ;

    /**
     * bin-by-bin estimation
     **/
    int nbrs_g ;
    int nbrs_r ;
    int ng ;
    int npos ;
    int i1, k ;
    double f0_g, f0_r, f0 ;
    double tempEst ;

    /**
     * build the neighbor list within range
     * determined by radius.
     **/
    MPEFieldGen_buildNeighborList (pGeoData , pMPEParams,
                 gageSize, iug,  ivg, zg ) ;

    for(i = 0; i < colSize; i ++)
    {
        for(j = 0; j < rowSize; j++)
        {
            /**
             * check to see if radar value is missing at this grid box.
             * If it is missing, use nbrs_rad as read from
             * data base table. If the radar is available for this point,
             * set nbrs_r = 1, which will cause the algorithm to estimate
             * a weight for this radar pixel only.
             *
             * reset the numbers of nearest gage and radar rainfall data
             * to be located.
             **/
            nbrs_g = nbrs_gag ;
            if(rad[j][i] >= 0.0)
                nbrs_r = 1 ;
            else
                nbrs_r = nbrs_rad ;

            /**
             * locate data within the radius of influence
             **/

            if(isrch_g == 1)
            {
                /**
                 * build the neighbor list within range
                 * determined by radius.
                 **/
                MPEFieldGen_findNeighborList (iradi, i, j,
                            ilist_g, rlist_g, &ng );

                /**
                 * collect fractional coverage statistics
                 **/
                npos = 0 ;
                for(i1 = 0; i1 < ng; i1 ++)
                {
                    k = ilist_g[i1] ;
                    if(zg[k] >= rain_min)
                        npos ++ ;
                }

                /**
                 * if the number of gage data within the radius of
                 * influence is less than the number of neighboring gage
                 * data to be used, reduce the latter to the former
                 **/
                if(ng < nbrs_g)
                    nbrs_g = ng ;
            }
            else
            {
                ng = ndgag[j][i] ;
                npos = npgag[j][i] ;

                if(ng > 0)
                {
                    /**
                     * use spiral search to locate the nearest
                     * nbrs_g gage data.
                     **/
                    MPEFieldGen_srch_nbrs(rowSize, colSize, 1, n, idis, jdis, i, j,
                        gag, ndgag, &nbrs_g, iug, ivg, zg) ;
                }
                else
                {
                    /**
                     * no need to search:
                     * no gage data exist within the radius of influence.
                     **/
                    nbrs_g = 0 ;
                }
            }

            /**
             * specify the number of radar data available
             * and the number of nearest neighbors to be located.
             **/
            if(ndata[j][i] < 0)
            {
                /**
                 * this is an impossible event: substitute radar based field
                 * for multisensor field and return
                 **/
                sprintf( message , "ndata[%d][%d]: %d less than 0..."
                    "impossible\n no gages available "
                    " - setting multisensor field to radar based field.",
                     j, i, ndata[j][i]) ;
                printMessage( message, logFile );

                *errFlag = 1 ;

                int ii, ij ;
                for(ii = 0; ii < rowSize; ii ++)
                {
                    for(ij = 0; ij < colSize; ij ++)
                        est[ii][ij] = rad[ii][ij] ;
                }

                return ;
            }
            else if(ndata[j][i] == 0)
            {
                /**
                 * no radar data exist within the radius of influence:
                 * gage_only analysis will be performed
                 **/
                nbrs_r = 0 ;
            }
            else
            {
                /**
                 * radar data exist within the radius of influence
                 **/
                MPEFieldGen_srch_nbrs(rowSize, colSize, 2, n, idis, jdis, i, j,
                    rad, mis, &nbrs_r, iur, ivr, zr) ;
            }

            /**
             * perform estimation
             **/
            if(nbrs_r == 0)
            {
                /**
                 * radar data do not exist
                 **/
                if(nbrs_g == 0)
                {
                    /**
                     * gage data do not exist either: cannot estimate
                     **/
                    est[j][i] = -0.1 ;
                }
                else
                {
                    /**
                     * gage data exist
                     **/
                    if(npos == 0)
                    {
                        /**
                         * gages indicate no rainfall
                         * within the radius of influence:
                         * assume no rainfall with certainty
                         **/
                        est[j][i] = 0.0 ;
                    }
                    else
                    {
                        /**
                         * gages report rainfall somewhere
                         * in the radius of influence: calculate
                         * the fractional coverage over the radius of influence
                         * based on the rain gage data.
                         **/
                        f0_g = npos * 1.0 / ng ;

                        MPEFieldGen_ms_soe(rowSize, colSize, isrch_g, cvr, f0_g,
                            nbrs_g, nbrs_r, umeang, i, j, rain_min, dist_min,
                            cor0pig, cor0ig, rngig, cor0pic, cor0ic, rngic,
                            cor0pir, cor0ir, rngir, cor0pcg, cor0cg, rngcg,
                            cor0pcc, cor0cc, rngcc, cor0pcr, cor0cr, rngcr,
                            iug, ivg, zg, iur, ivr, zr, ilist_g, &tempEst, &errFl);

                        if(errFl == 0)
                        {
                        est[j][i] = tempEst ;
                        }
                        else
                        {
                           *errFlag = 2;
                           return;
                        }
                    }
                }
            }
            else
            {
                /**
                 * radar data exist
                 **/
                if(nbrs_g == 0)
                {
                    /**
                     * gage data do not exist: perform radar-only estimation
                     **/
                    if(rad[j][i] >= 0.0 && mis[j][i] > 0)
                    {
                        /**
                         * radar datum exists at the point of estimation:
                         * assume the observed
                         * radar rainfall with certainty.
                         **/
                        est[j][i] = rad[j][i] ;
                    }
                    else
                    {
                        /**
                         * radar data exists somewhere within the radius of
                         * influence other than the point of estimation
                         **/
                        if(nposi[j][i] == 0)
                        {
                            /**
                             * radar indicates no rainfall:
                             * assume no rainfall with certainty
                             **/
                            est[j][i] = 0.0 ;
                        }
                        else
                        {
                            /**
                             * radar indicates rainfall:
                             * estimate the fractional coverage of
                             * rainfall over the radius of influence
                             **/
                            f0_r = nposi[j][i] * 1.0 / ndata[j][i] ;

                            MPEFieldGen_ms_soe(rowSize, colSize, isrch_g, cvr, f0_r,
                                nbrs_g, nbrs_r, umeang, i, j, rain_min,
                                dist_min, cor0pig, cor0ig, rngig, cor0pic,
                                cor0ic, rngic, cor0pir, cor0ir, rngir,
                                cor0pcg, cor0cg, rngcg, cor0pcc, cor0cc,
                                rngcc, cor0pcr, cor0cr, rngcr, iug, ivg, zg,
                                iur, ivr, zr, ilist_g, &tempEst, &errFl);

                            if(errFl == 0)
                            {
                            est[j][i] = tempEst ;
                            }
                            else
                            {
                               *errFlag = 2;
                               return;
                            }

                        }
                    }
                }
                else
                {
                    /**
                     * gage data exist: perform radar-gage estimation
                     **/
                    if((npos + nposi[j][i]) == 0)
                    {
                        /**
                         * both gages and radar indicate no rainfall:
                         * assume no rainfall with certainty
                         **/
                        est[j][i] = 0.0 ;
                    }
                    else
                    {
                        /**
                         * at least one sensor reports rainfall:
                         * estimate the fractional
                         * coverage via arithmetic averaging
                         **/
                        f0_g = npos * 1.0 / ng ;
                        f0_r = nposi[j][i] * 1.0 / ndata[j][i] ;
                        f0 = (f0_g + f0_r) / 2.0 ;

                        MPEFieldGen_ms_soe(rowSize, colSize, isrch_g, cvr, f0, nbrs_g,
                            nbrs_r, umeang, i, j, rain_min, dist_min, cor0pig,
                            cor0ig, rngig,    cor0pic, cor0ic, rngic, cor0pir,
                            cor0ir, rngir, cor0pcg, cor0cg, rngcg, cor0pcc,
                            cor0cc,rngcc, cor0pcr, cor0cr, rngcr, iug, ivg,
                            zg, iur, ivr, zr, ilist_g, &tempEst, &errFl);

                        if(errFl == 0)
                        {
                        est[j][i] = tempEst ;
                        }
                        else
                        {
                           *errFlag = 2;
                           return;
                        }
                    }
                }
            }
        }
    }

    releaseMultiMemory(pGeoData) ;

    *errFlag = 0 ;
    return ;
}


/***********************************************************************
* Purpose:
* This function performs radar-gage merging via Single Optimal Estimation
* (Seo 1998)
*
* calling function: multi_sensor
* functions called: covmatms, covvecms, lsolve
*
* input variables
*
* isrch_g  - method for nearest-neighbor search; 1 for double heap-
*            sorting, 2 for spiral search
* cvr      - conditional coefficient of variation of radar rainfall
* f0       - fractional coverage of rainfall within the radius of
*            influence as estimated by gage and radar data
* nbrs_g   - number of nearest gage data to be used
* nbrs_r   - number of nearest radar data to be used
* umeang   - [rowSize][colSize] array of PRISM data
* i        - HRAP x-coordinate of the point of estimation
* j        - HRAP y-coordinate of the point of estimation
* rain_min - minimum detectable rainfall in mm
* dist_min - separation distance in km
* cor0pig  - lag-0+ conditional correlation coeffient of gage rainfall
* cor0ig   - lag-0 conditional correlation coeffient of gage rainfall
* rngig    - conditional correlation scale in km of gage rainfall
* cor0pic  - lag-0+ conditional cross-correlation coefficient
* cor0ic   - lag-0 conditional cross-correlation coefficient
* rngic    - conditional cross-correlation scale in km
* cor0pir  - lag-0+ conditional correlation scale in km of radar
*            rainfall
* cor0ir   - lag-0 conditional correlation scale in km of radar rainfall
* rngir    - conditional correlation scale in km of radar rainfall
* cor0pcg  - lag-0+ indicator correlation coeffient of gage rainfall
* cor0cg   - lag-0 indicator correlation coeffient of gage rainfall
* rngcg    - indicator correlation scale in km of gage rainfall
* cor0pcc  - lag-0+ indicator cross-correlation coefficient
* cor0cc   - lag-0 indicator cross-correlation coefficient
* rngcc    - indicator cross-correlation scale in km
* cor0pcr  - lag-0+ indicator correlation scale in km of radar
*            rainfall
* cor0cr   - lag-0 indicator correlation scale in km of radar rainfall
* rngcr    - indicator correlation scale in km of radar rainfall
* iug      - array of HRAP x-coordinates of gage data
* ivg      - array of HRAP y-coordinates of gage data
* zg       - array of gage rainfall in mm
* iur      - array of HRAP x-coordinates of radar data
* ivr      - array of HRAP y-coordinates of radar data
* zr       - array of radar rainfall in mm
* ilist_g  - array of index of nearest gage data
*
* output variables
*
* est      - estimated rainfall in mm
*
* MODIFICATION HISTORY:
*   DATE           PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998   D.-J. Seo         Original FORTRAN code
*   April 2005     Guoxian Zhou      finish conversion to C Language
*   Jul 15, 2005   Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_ms_soe(const int rowSize , const int colSize ,
            const int isrch_g, const double cvr,
            const double f0, const int nbrs_g,
            const int nbrs_r, double ** umeang,
            const int i, const int j,
            const double rain_min, const double dist_min,
            const double cor0pig, const double cor0ig,
            const double rngig,    const double cor0pic,
            const double cor0ic, const double rngic,
            const double cor0pir, const double cor0ir,
            const double rngir,    const double cor0pcg,
            const double cor0cg, const double rngcg,
            const double cor0pcc, const double cor0cc,
            const double rngcc, const double cor0pcr,
            const double cor0cr, const double rngcr,
            short * iug, short * ivg, float * zg,
            short * iur, short * ivr, float * zr,
            short * ilist_g, double * est,
            int   * errFlag)
{
    const int ndim = nbrs_g + nbrs_r + 1;
    double *b = NULL;
    double *bi = NULL;
    double *w = NULL ;
    double **cov = NULL;
    double **covi = NULL;

    int ictr, jctr ;
    int ii, errFl;
    double var = 1.0 ;
    /**
     * allocate memory
     **/
    b = (double *)malloc(ndim * sizeof(double));
    if(b == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in ms_soe function."
            "\n\tProgram exit.") ;
        *errFlag = 1;         
        return;         
    }

    bi = (double *)malloc(ndim * sizeof(double));
    if(bi == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in ms_soe function."
            "\n\tProgram exit.") ;
        *errFlag = 1;         
        return;         
    }

    w = (double *)malloc(ndim * sizeof(double));
    if(w == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in ms_soe function."
            "\n\tProgram exit.") ;
        *errFlag = 1;         
        return;         
    }

    cov = (double **)malloc(ndim * sizeof(double *));
    if(cov == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in ms_soe function."
            "\n\tProgram exit.") ;
        *errFlag = 1;         
        return;         
    }
    for(ii = 0; ii < ndim; ii++)
    {
        cov[ii] = (double *)malloc(ndim * sizeof(double));
        if(cov[ii] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in ms_soe function."
                "\n\tProgram exit.") ;
            *errFlag = 1;         
            return;         
        }
    }

    covi = (double **)malloc(ndim * sizeof(double *));
    if(covi == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in ms_soe function."
            "\n\tProgram exit.") ;
        *errFlag = 1;         
        return;         
    }
    for(ii = 0; ii < ndim; ii++)
    {
        covi[ii] = (double *)malloc(ndim * sizeof(double));
        if(covi[ii] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in ms_soe function."
                "\n\tProgram exit.") ;
            *errFlag = 1;         
            return;         
        }
    }

    /**
    * construct indicator correlation matrix
    **/
    for(ictr = 0; ictr < ndim; ictr ++)
    {
        bi[ictr] = 0.0 ;
        b[ictr] = 0.0 ;
        for(jctr = 0; jctr < ndim; jctr ++)
        {
            covi[jctr][ictr] = 0.0 ;
             cov[jctr][ictr] = 0.0 ;
        }
    }

    if (f0 < 1.0)
    {
        /**
        * construct conditional correlation matrix
        **/
        MPEFieldGen_covmatms(isrch_g, 1.0, 1.0, nbrs_g, nbrs_r, dist_min,
            cor0pig, cor0ig, rngig, cor0pic, cor0ic, rngic,
            cor0pir, cor0ir, rngir, iug, ivg, iur, ivr,
            ilist_g, covi) ;

        /**
        * construct indicator correlation vector
        **/
        MPEFieldGen_covvecms(isrch_g, i, j, 1.0, 1.0, nbrs_g, nbrs_r,
            dist_min, cor0pig, cor0ig, rngig, cor0pic, cor0ic,
            rngic, iug, ivg, iur, ivr, ilist_g, bi) ;
    }

    /**
    * construct conditional correlation matrix
    **/
    MPEFieldGen_covmatms(isrch_g, 1.0, 1.0, nbrs_g, nbrs_r, dist_min,
        cor0pcg, cor0cg, rngcg, cor0pcc, cor0cc, rngcc,
        cor0pcr, cor0cr, rngcr, iug, ivg, iur, ivr,
        ilist_g, cov) ;

    /**
    * construct indicator correlation vector
    **/
    MPEFieldGen_covvecms(isrch_g, i, j, 1.0, 1.0, nbrs_g, nbrs_r,
        dist_min, cor0pcg, cor0cg, rngcg, cor0pcc,
        cor0cc, rngcc, iug, ivg, iur, ivr, ilist_g, b) ;
    /**
    * compute unconditional gage covariance matrix
    **/
    int j1, j2 ;
    double denum , denom ;

    for(j1 = 1; j1 <= (nbrs_g + nbrs_r); j1 ++ )
    {
        for(j2 = 1; j2 <= j1; j2 ++)
        {
            denum = (cvr * cvr) * (1.0 - f0)
                * cov[j2-1][j1-1] * covi[j2-1][j1-1]
                + (1.0 - f0) * covi[j2-1][j1-1]
                + (cvr * cvr) * f0 * cov[j2-1][j1-1] ;
            denom = cvr * cvr + (1.0 - f0) ;
            if(denom != 0.0)
                cov[j2-1][j1-1] = denum / denom ;
        }
    }

    /**
    * fill using symmetry
    **/
    for(j1 = 1; j1 <= (nbrs_g + nbrs_r + 1); j1 ++)
    {
        for(j2 = j1; j2 <= (nbrs_g + nbrs_r + 1); j2 ++)
            cov[j2-1][j1-1] = cov[j1-1][j2-1] ;
    }

    /**
    * compute unconditional covariance vector
    **/
    for(j1 = 1; j1 <= (nbrs_g + nbrs_r); j1 ++)
    {
        denum = (cvr * cvr) * (1.0 - f0) * b[j1-1] * bi[j1-1]
             + (1.0 - f0) * bi[j1-1]
             + (cvr * cvr) * f0 * b[j1-1] ;
        denom = cvr * cvr + (1.0 - f0) ;
        if(denom != 0.0)
            b[j1-1] = denum / denom ;
    }

    /**
    * copy before calling lsolve
    **/
    for(j1 = 0; j1 < (nbrs_g + nbrs_r + 1); j1 ++)
        w[j1] = b[j1] ;

    /**
    * solve linear system
    **/
    MPEFieldGen_lsolve(nbrs_g + nbrs_r + 1, cov, b, &errFl);

    /**
    * check error flag here
    **/
    if(errFl != 0)
    {
        sprintf ( message , "in ms_soe...error in lsolve");
        *errFlag = 1;
        return;         
    }

    /**
    * compute estimate and normalized estimation variance
    **/
    var = 1.0 ;
    *est = 0.0 ;
    int jj, k ;
    if(nbrs_g >= 1)
    {
        for( k = 0; k < nbrs_g; k ++)
        {
            if(isrch_g == 1)
                jj = ilist_g[k] ;
            else
                jj = k ;

            if((umeang[ivg[jj]][iug[jj]] <= 0.0)
                || (umeang[j][i] <= 0.0))
            {
                /**
                * Do not make climatological adjustment
                **/
                *est += b[k] * zg[jj] ;
                var -= b[k] * w[k] ;
            }
            else
            {
                /**
                * Make Climatological Adjustment
                **/
                *est += b[k] * zg[jj] * umeang[j][i]
                     / umeang[ivg[jj]][iug[jj]] ;
                var -= b[k] * w[k] ;
            }
        }
    }

    if(nbrs_r >= 1)
    {
        for(k = 0; k < nbrs_r; k ++)
        {
            jj = k ;
            if((umeang[ivr[jj]][iur[jj]] <= 0.0) ||
                (umeang[j][i] <= 0))
            {
                /**
                 * Do not make climatological adjustment
                 **/
                *est += b[nbrs_g + k] * zr[jj] ;
                var -= b[nbrs_g + k] * w[nbrs_g + k] ;
            }
            else
            {
                /**
                * Make Climatological Adjustment
                **/
                *est += b[nbrs_g + k] * zr[jj] * umeang[j][i]
                     / umeang[ivr[jj]][iur[jj]] ;
                var -= b[nbrs_g + k] * w[nbrs_g + k] ;
            }
        }
    }

    var += b[nbrs_g + nbrs_r] ;

    /**
    * check nonnegativity of estimation variance and estimate
    **/
    if(var < -0.0001)
    {
        sprintf ( message , "in ms_soe...negative estimation"
            " variance:%f\n", var);
        *errFlag = 1;
        return;         
    }

    if(*est < 0.0) *est = 0.0 ;

    /**
     * release memory
     **/
    if(cov != NULL)
    {
        for(ii = 0; ii < ndim; ii++)
        {
            if(cov[ii] != NULL)
            {
                free(cov[ii]);
                cov[ii] = NULL;
            }
        }
        free(cov);
        cov = NULL;
    }

    if(covi != NULL)
    {
        for(ii = 0; ii < ndim; ii++)
        {
            if(covi[ii] != NULL)
            {
                free(covi[ii]);
                covi[ii] = NULL;
            }
        }
        free(covi);
        covi = NULL;
    }
    if(b != NULL)
    {
        free(b);
        b = NULL;
    }
    if(bi != NULL)
    {
        free(bi);
        bi = NULL;
    }
    if(w != NULL)
    {
        free(w);
        w = NULL;
    }

    return ;
}

/***********************************************************************
* Purpose:
* This function constructs the covariance matrix for SOE.
*
* calling function: ms_soe
* functions called: gammf
*
* input variables
*
* isrch_g - method for nearest-neighbor search; 1 for double heap-
*           sorting, 2 for spiral search
* varg    - (indicator) variance of gage rainfall
* varr    - (indicator) variance of radar rainfall
* nbrs_g  - number of nearest rain gage measurements used
* nbrs_r  - number of nearest radar data to be located
* dr      - nugget distance
* cng     - lag-0+ correlation coefficient of gage rainfall
* cg      - lag-0 correlation coefficient of gage rainfall
* rg      - correlation scale of gage rainfall
* cnc     - lag-0+ cross-correlation coefficient
* cc      - lag-0+ cross-correlation coefficient
* rc      - cross-correlation scale
* cnr     - lag-0+ correlation coefficient
* cr      - lag-0 correlation coefficient
* rr      - correlation scale in km of radar rainfall in mm
* iug     - array of HRAP x-coordinates of gage data
* ivg     - array of HRAP y-coordinates of gage data
* iur     - array of HRAP x-coordinates of radar data
* ivr     - array of HRAP y-coordinates of radar data
* ilist_g - array of index of nearest gage data
*
* output variables
*
* cov     - (nbrs_g+nbrs_r+1)x(nbrs_g+nbrs_r+1) covariance matrix
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language
*   Jun 30, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_covmatms(const int isrch_g, const double varg,
            const double varr, const int nbrs_g,
            const int nbrs_r, const double dr,
            const double cng, const double cg,
            const double rg, const double cnc,
            const double cc,    const double rc,
            const double cnr, const double cr,
            const double rr,
            short * iug, short * ivg,
            short * iur, short * ivr,
            short * ilist_g,
            double ** cov)
{
    /**
    * specify the size of the matrix
    **/
    int iend = nbrs_g + nbrs_r ;

    /**
    * initialize covariance matrix
    **/
    int irow, icol, j, j1, j2, i1, i2 ;
    double h, tempCov ;
    for(irow = 0; irow < iend; irow ++)
    {
        for(icol = 0; icol < iend; icol ++)
            cov[irow][icol] = 0.0 ;
    }

    if(nbrs_g >= 1)
    {
        /**
        * construct the indicator covariance submatrix
        * of gage rainfall (lower triangular)
        **/

        for(j1 = 1; j1 <= nbrs_g; j1 ++)
        {
            if(isrch_g == 1)
                i1 = ilist_g[j1-1] ;
            else
                i1 = j1 - 1 ;

            for(j2 = 1; j2 <= j1; j2 ++)
            {
                if(isrch_g == 1)
                    i2 = ilist_g[j2-1] ;
                else
                    i2 = j2 - 1 ;

                h = (iug[i1] - iug[i2]) * (iug[i1] - iug[i2]) +
                    (ivg[i1] - ivg[i2]) * (ivg[i1] - ivg[i2]) ;

                h = sqrt(h) ;
                MPEFieldGen_gammf(dr, cng, cg, rg, h, &tempCov) ;
                cov[j1-1][j2-1] = tempCov * varg ;
            }
        }

        if(nbrs_r >= 1)
        {
            /**
            * construct the indicator cross-covariance
            * submatrix (full)
            **/
            for(j1 = 1; j1 <= nbrs_r; j1 ++)
            {
                i1 = j1 - 1 ;
                for(j2 = 1; j2 <= nbrs_g; j2 ++)
                {
                    if(isrch_g == 1)
                        i2 = ilist_g[j2-1] ;
                    else
                        i2 = j2 - 1 ;

                    h = (iur[i1] - iug[i2]) * (iur[i1] - iug[i2]) +
                        (ivr[i1] - ivg[i2]) * (ivr[i1] - ivg[i2]) ;
                    h = sqrt(h) ;

                    MPEFieldGen_gammf(dr, cnc, cc, rc, h, &tempCov) ;
                    cov[nbrs_g+j1-1][j2-1] = tempCov * sqrt(varg * varr) ;
                }
            }
        }
    }

    if(nbrs_r >= 1)
    {
        /**
        * construct the indicator covariance submatrix of
        * radar rainfall(lower triangular)
        **/
        for(j1 = 1; j1 <= nbrs_r; j1 ++)
        {
            i1 = j1 - 1 ;
            for(j2 = 1; j2 <= j1; j2 ++)
            {
                i2 = j2 - 1 ;
                h = (iur[i1] - iur[i2]) * (iur[i1] - iur[i2]) +
                    (ivr[i1] - ivr[i2]) * (ivr[i1] - ivr[i2]) ;
                h = sqrt((double)h) ;
                MPEFieldGen_gammf(dr, cnr, cr, rr, h, &tempCov) ;
                cov[nbrs_g+j1-1][nbrs_g+j2-1] = tempCov * varr ;
            }
        }
    }

    /**
    * append unbiasedness constraint
    **/
    for(j = 1; j <= iend; j++)
        cov[iend][j-1] = 1.0 ;

    cov[iend][iend] = 0.0 ;

    /**
    * fill using symmetry
    **/
    for(j1 = 1; j1 <= (iend + 1); j1 ++)
    {
        for(j2 = j1; j2 <= (iend + 1); j2 ++)
            cov[j1-1][j2-1] = cov[j2-1][j1-1] ;
    }
}


/***********************************************************************
* Purpose:
* This function constructs the covariance vector for SOE.
*
* calling function:
* functions called: gammf
*
* input variables
*
* isrch_g - method for nearest-neighbor search; 1 for double heap-
*           sorting, 2 for spiral search
* iu0     - HRAP x-coordinate of the point of estimation
* iv0     - HRAP y-coordinate of the point of estimation
* varg    - (indicator) variance of gage rainfall
* varr    - (indicator) variance of radar rainfall
* nbrs_g  - number of nearest rain gage measurements used
* nbrs_r  - number of nearest radar data to be located
* dr      - nugget distance
* cng     - lag-0+ correlation coefficient of gage rainfall
* cg      - lag-0 correlation coefficient of gage rainfall
* rg      - correlation scale of gage rainfall
* cnc     - lag-0+ cross-correlation coefficient
* cc      - lag-0+ cross-correlation coefficient
* rc      - cross-correlation scale
* iug     - array of HRAP x-coordinates of gage data
* ivg     - array of HRAP y-coordinates of gage data
* iur     - array of HRAP x-coordinates of radar data
* ivr     - array of HRAP y-coordinates of radar data
* ilist_g - array of index of nearest gage data
*
* output variables
*
* b       - [nbrs_g+nbrs_r+1] array of covariance vector
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code
*   Jun 20, 2005 Guoxian Zhou      finish conversion to C Language
*   Jun 30, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_covvecms(const int isrch_g, const int iu0,
            const int iv0, const double varg,
            const double varr, const int nbrs_g,
            const int nbrs_r, const double dr,
            const double cng, const double cg,
            const double rg, const double cnc,
            const double cc,    const double rc,
            short * iug, short * ivg,
            short * iur, short * ivr,
            short * ilist_g,
            double * b)
{
    int iend = nbrs_g + nbrs_r ;
    int i, j ;
    double h, temp ;

    if(nbrs_g >= 1)
    {
        /**
        * construct the gage rainfall covariance subvector
        **/
        for(j = 0; j < nbrs_g; j ++)
        {
            if(isrch_g == 1)
                i = ilist_g[j] ;
            else
                i = j ;

            h = (iu0 - iug[i]) * (iu0 - iug[i]) +
                (iv0 - ivg[i]) * (iv0 - ivg[i]) ;
            h = sqrt((double)h) ;
            MPEFieldGen_gammf(dr, cng, cg, rg, h, &temp) ;
            b[j] = temp * varg ;
        }
    }

    if(nbrs_r >= 1)
    {
        /**
        * construct the cross-covariance subvector
        **/
        for(j = 0; j < nbrs_r; j ++)
        {
            i = j ;
            h = (iu0 - iur[i]) * (iu0 - iur[i]) +
                (iv0 - ivr[i]) * (iv0 - ivr[i]) ;
            h = sqrt(h) ;
            MPEFieldGen_gammf(dr, cnc, cc, rc, h, &temp) ;
            b[nbrs_g + j] = temp * sqrt(varg * varr) ;
        }
    }

    /**
    * append the unbiasedness constraint
    **/
    b[iend] = 1.0 ;
}

/****************************************************************************
* Purpose:
* This function estimates indicator and conditional correlation
* coefficients in space assuming exponential correlation functions
* with no nugget effect.
*
* calling function: multi_sensor
* functions called: none
*
* input variables
*
* rad  - [rowSize][colSize] array of radar rainfall in mm
* mis  - [rowSize][colSize] of misbin array
*
* output variables
*
* rngi - indicator correlation scale in km
* rngc - conditional correlation scale in km
* errFlag - error flag; 0 for OK, 1 if not
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language
*   Jun 28, 2005 Guoxian Zhou      finish component testing
*
****************************************************************************/

void MPEFieldGen_cor_scale_r(const double rowSize , const double colSize ,
                double ** rad, int ** mis,
                double * rngi, double * rngc, int *errFlag)
{
    int numbi = 0 ;
    int numbc = 0 ;
    double sumi1 = 0.0 ;
    double sumih = 0.0 ;
    double sumiv = 0.0 ;
    double sumc1 = 0.0 ;
    double sumc2 = 0.0 ;
    double sumch = 0.0 ;
    double sumcv = 0.0 ;

    int i, j ;
    double rain0, rainh, rainv ;
    int irain0, irainh, irainv ;

    for(i = 0; i < colSize - 1; i ++)
    {
        for(j = 0 ; j < rowSize - 1; j ++)
        {
            if(mis[j][i] == 0)   continue ;
            if(mis[j][i+1] == 0) continue ;
            if(mis[j+1][i] == 0) continue ;

            rain0 = rad[j][i] ;
            rainh = rad[j][i+1] ;
            rainv = rad[j+1][i] ;

            if(rain0 < 0.0) continue ;
            if(rainh < 0.0) continue ;
            if(rainv < 0.0) continue ;

            if(rain0 > 0.0)
                irain0 = 1 ;
            else
                irain0 = 0 ;

            if(rainh > 0.0)
                irainh = 1 ;
            else
                irainh = 0 ;

            if(rainv > 0.0)
                irainv = 1 ;
            else
                irainv = 0 ;

            numbi ++ ;
            sumi1 += irain0 ;
            sumih += irain0 * irainh ;
            sumiv += irain0 * irainv ;

            if(rain0 == 0.0) continue ;
            if(rainh == 0.0) continue ;
            if(rainv == 0.0) continue ;

            numbc ++ ;
            sumc1 += rain0 ;
            sumc2 += rain0 * rain0 ;
            sumch += rain0 * rainh ;
            sumcv += rain0 * rainv ;
        }
    }

    if((numbi == 0) || (numbc <= 1))
    {
        *errFlag = 1 ;
        return ;
    }

    /**
    * compute lag-1 indicator correlation coefficient
    **/
    double avei = sumi1 / numbi ;
    double vari = avei * (1.0 - avei) ;

    if(vari <= 0.0)
    {
        *errFlag = 1 ;
        return ;
    }

    double rhoi = 0.5 * (sumih + sumiv) / numbi - avei * avei ;
    rhoi = rhoi / vari ;

    if((rhoi < 0.0) || (rhoi >  1.0))
    {
        *errFlag = 1 ;
        return ;
    }

    /**
    * compute lag-1 conditional correlation coefficient
    **/
    double avec = sumc1 / numbc ;

    double varc = sumc2 / (numbc - 1) -
            sumc1 * sumc1 / (numbc * (numbc - 1)) ;

    if(varc <= 0.0)
    {
        *errFlag = 1 ;
        return ;
    }

    double rhoc = 0.5 * (sumch + sumcv) / numbc - avec * avec ;
    rhoc = rhoc / varc ;

    if((rhoc < 0.0) || (rhoc >  1.0))
    {
        *errFlag = 1 ;
        return ;
    }

    /**
    * etimate correlation scales in HRAP
    * assuming exponential model with no nugget effect
    **/
    *rngi = -1.0 / log(rhoi) ;
    *rngc = -1.0 / log(rhoc) ;
    *errFlag = 0 ;
    return ;
} /* end MPEFieldGen_cor_scale_r */



void allocMultiMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for ndgag variable
     **/
    ndgag = (int **)malloc(rowSize * sizeof(int *));
    if(ndgag == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMultiMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        ndgag[i] = (int *)malloc(colSize * sizeof(int));
        if(ndgag[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMultiMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * allocate memory for npgag variable
     **/
    npgag = (int **)malloc(rowSize * sizeof(int *));
    if(npgag == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMultiMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        npgag[i] = (int *)malloc(colSize * sizeof(int));
        if(npgag[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMultiMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    gag = (double **)malloc(rowSize * sizeof(double *));
    if(gag == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMultiMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize ; i++)
    {
        gag[i] = (double *)malloc(colSize * sizeof(double));
        if(gag[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMultiMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * allocate memory for ndata variable
     **/
    ndata = (int **)malloc(rowSize * sizeof(int *));
    if(ndata == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMultiMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        ndata[i] = (int *)malloc(colSize * sizeof(int));
        if(ndata[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMultiMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * allocate memory for nposi variable
     **/
    nposi = (int **)malloc(rowSize * sizeof(int *));
    if(nposi == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMultiMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        nposi[i] = (int *)malloc(colSize * sizeof(int));
        if(nposi[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMultiMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

} /* end allocMultiMemory */

void releaseMultiMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;

    if(ndgag != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(ndgag[i] != NULL)
            {
                free(ndgag[i]);
                ndgag[i] = NULL;
            }
        }

        free(ndgag);
        ndgag = NULL;
    }

    if(npgag != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(npgag[i] != NULL)
            {
                free(npgag[i]);
                npgag[i] = NULL;
            }
        }

        free(npgag);
        npgag = NULL;
    }

    if(gag != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(gag[i] != NULL)
            {
                free(gag[i]);
                gag[i] = NULL;
            }
        }

        free(gag);
        gag = NULL;
    }

    if(ndata != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(ndata[i] != NULL)
            {
                free(ndata[i]);
                ndata[i] = NULL;
            }
        }

        free(ndata);
        ndata = NULL;
    }

    if(nposi != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(nposi[i] != NULL)
            {
                free(nposi[i]);
                nposi[i] = NULL;
            }
        }

        free(nposi);
        nposi = NULL;
    }

} /* end releaseMultiMemory */
