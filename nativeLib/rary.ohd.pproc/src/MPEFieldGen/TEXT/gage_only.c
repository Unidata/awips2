#include "mpe_fieldgen.h"
#include "multi_sensor.h"

/**********************************************************************
*
* Purpose:
* This function performs gage-only estimation.
*
* calling function: runGageonly
* functions called: hrap_to_latlon, hrapsize, coef_var_g, local_stat
*
* input variables
*
* pRunDate - date/time 
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain 
*
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
* umeang   - [rowSize][colSize] PRISM array in mm
*
* output variables
*
* est      - array of estimated rainfall in mm.
*            [rowSize][colSize]
* errFlag  - error flag; 0 if OK, 1 if not
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language 
*
***********************************************************************/

void MPEFieldGen_gage_only(const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** umeang ,
                double ** est ,
                int * errFlag)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    short ilist_g[gageSize] ;
    float rlist_g[gageSize];

    /**      
     * specify the minimum detectable rainfall in mm
     * and the separation distance in km.
     **/
    const double rain_min = 0.00001 ;
    double dist_min = 0.1 ;

    /**      
     * specify the method to be used for
     * nearest-neighbor search in gage data:
     * 1 for double heap-sorting,
     * 2 for spiral search.
     **/
    const int isrch_g = 1 ;

    /* specify go-parameters. */

    /**      
     * estimation technique designation:
     * 1 for RDS, 2 for SOE.
     **/
    int itype = 2 ;

    /**      
     * number of nearest gage data to be located.
     **/
    int nbrs_gag = 5 ;

    /**      
     * radius of influence in km.
     **/
    double radi = 60.0 ;

    /**      
     * indicator correlation scale in km.
     **/
    double rngi = 60.0 ;

    /**      
     * conditional correlation scale in km.
     **/
    double rngc = 60.0 ;

    int iradi = (int)(radi + 0.5) ;
    int i, j ;

    double flat, flon, rmesh;
    double mid_hrap ;

    /**      
     * check if the number of gage data available
     * is less than the number of
     * neighbors to be located:
     * if so, reset the latter to the former.
     **/
    if(gageSize < nbrs_gag)
        nbrs_gag = gageSize ;    

    for(i = 0; i < gageSize; i ++)
    {
        ilist_g[i] = 0 ;
        rlist_g[i] = 0.0 ;
    }

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
        dist_min /= rmesh ;
        rngi /= rmesh ;
        rngc /= rmesh ;
        radi /= rmesh ;
        iradi = (int)(radi + 0.5) ;
    }

    /**      
     * compute coefficient of variation
     * of positive gage rainfall.
     **/
    double cvg ;
    MPEFieldGen_coef_var_g(gageSize, zg, rain_min, &cvg, errFlag) ;

    if(*errFlag != 0)
    {
        sprintf( message , "STATUS: in gage_only, "
                "cannot calculate cv... "
                "can only perform RDS.") ;
        printMessage( message, logFile );        
        itype = 1 ;
    }

    /**      
     * specify indicator correlation parameters.
     **/
    double cor0pi = 1.0 ;
    double cor0pc = 1.0 ;
    double cor0i = 1.0 ;
    double cor0c = 1.0 ;
    double temp_est;

    /**
     * perform bin-by-bin estimation.
     **/
    int ng ;
    int npos = 0, i1, k ;

    /**      
     * build the neighbor list within range
     * determined by radius.
     **/
    MPEFieldGen_buildNeighborList (pGeoData , pMPEParams,
                 gageSize, iug,  ivg, zg ) ;

    for(i = 0 ; i < colSize; i ++)
    {
        for(j = 0; j < rowSize; j ++)
        {
            /**      
             * reset the numbers of nearest gage
             * and radar rainfall data to be located.
             **/
            int nbrs_g = nbrs_gag ;

            /**      
             * pick up the neighbor list within range
             * determined by radius.
             **/
            MPEFieldGen_findNeighborList (iradi, i, j,
                        ilist_g, rlist_g, &ng );

            npos = 0 ;
            if(ng > 0)
            {
                /**      
                 * collect fractional coverage statistics.
                 **/
                for(i1 = 0; i1 < ng; i1 ++)
                {
                    k = ilist_g[i1] ;
                    if(zg[k] >= rain_min )
                        npos ++ ;
                }
            }

            /**      
             * if the number of gage data
             * within the radius of influence is less
             * than the number of neighboring gage data
             * to be used, reduce the
             * latter to the former.
             **/
            if(ng < nbrs_g)
                nbrs_g = ng ;

            /**      
             * perform estimation.
             **/
            if(nbrs_g == 0)
            {
                /* no gage data exist. */
                est[j][i] = -0.1 ;
            }
            else if(nbrs_g == 1)
            {
                /* only one gage datum exists. */
                est[j][i] = zg[ilist_g[0]] ;
            }
            else
            {
                /**      
                 * there exist at least two gage data.
                 **/
                if(npos == 0)
                {
                    /**      
                     * gage data do not indicate rainfall.
                     **/
                    est[j][i] = 0.0 ;
                }
                else
                {
                    /**      
                     * gage data indicate rainfall.
                     **/
                    if(itype == 1)
                    {
                        /**      
                         * perform RDS.
                         **/
                        MPEFieldGen_go_rds(rowSize, colSize, isrch_g, nbrs_g,
                            i, j, iug, ivg, zg, umeang, 
                            ilist_g, rlist_g, &temp_est) ;

                        est[j][i] = temp_est ;
                    }
                    else
                    {
                        /**      
                         * perform SOE.
                         **/
                        double f0_g = npos * 1.0 / ng ;

                        MPEFieldGen_go_soe(rowSize, colSize, isrch_g, cvg,
                            f0_g, nbrs_g, umeang, i, j, 
                            rain_min, dist_min, cor0pi, cor0i,
                            rngi, cor0pc, cor0c, rngc, iug, ivg, zg,
                            ilist_g, &temp_est) ;

                        est[j][i] = temp_est ;
                    }
                }
            }
        }
    }
    
    *errFlag = 0 ;
}


/***********************************************************************
* Purpose:
* This function performs Gage-Only estimation
* via the Reciprocal Distance Squared method.
*
* calling function: gage_only
* functions called: none
*
* input variables
*
* isrch_g - method for nearest-neighbor search; 1 for double heap-
*           sorting, 2 for spiral search
* nbrs_g  - number of nearest gage data to be used
* i       - HRAP x-coordinate of the point of estimation
* j       - HRAP y-coordinate of the point of estimation
* iug     - array of HRAP x-coordinate of nearest gage data
* ivg     - array of HRAP y-coordinate of nearest gage data
* zg      - array of nearest gage data
* umeang  - [rowSize][colSize] array of PRISM data
* ilist_g - array of index of nearest gage data
* rlist_g - array of distance to nearest gage data in km
*
* output variables
*
* est     - estimated rainfall in mm
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 27, 2005 Guoxian Zhou      finish testing
*
***********************************************************************/

void MPEFieldGen_go_rds(const int rowSize , const int colSize ,
            const int isrch_g , const int nbrs_g ,
            const int i , const int j ,
            short * iug, short * ivg , float * zg ,
            double ** umeang , 
            short * ilist_g , float * rlist_g ,
            double * est)
{

    int k, jj, iu, iv ;
    double disx, disy, distan, g_rain, sum, wgt ;
    double dist[nbrs_g] ;

    if(nbrs_g == 0)
    {
        sprintf ( message , "nbrs_g == 0 in go_rds...stop.");
        shutDownMPE( message, logFile );        
    }

    if(isrch_g == 2)
    {
        /**
         * spiral search used
         **/
        for( k = 0; k < nbrs_g; k ++)
        {
            disx = i - iug[k] ;
            disy = j - ivg[k] ;
            dist[k] = (double)(disx * disx + disy * disy) ;
            dist[k] = sqrt(dist[k]) ;
        }
    }

    if(isrch_g == 1)
    {
        distan = rlist_g[0] ;
        g_rain = zg[ilist_g[0]] ;
    }
    else
    {
        distan = dist[0] ;
        g_rain = zg[0] ;
    }

    if(distan == 0.0)
    {
        *est = g_rain ;
        return ;
    }

    sum = 0.0 ;
    for( k = 0; k < nbrs_g; k ++)
    {
        if(isrch_g == 1)
            sum += 1.0 / (rlist_g[k] * rlist_g[k]) ;
        else
            sum += 1.0 / (dist[k] * dist[k]) ;
    }

    *est = 0.0 ;
    for( k = 0; k < nbrs_g; k ++)
    {
        if(isrch_g == 1)
        {
            wgt = 1.0 / (rlist_g[k] * rlist_g[k]) ;
            jj = ilist_g[k] ;
        }
        else
        {
            wgt = 1.0 / (dist[k] * dist[k]) ;
            jj = k ;
        }

        iu = iug[jj] ;
        iv = ivg[jj] ;
        if((umeang[iv][iu] <= 0.0) ||
            (umeang[j][i] <= 0.0))
        {
            /**
             * Do not perform climatological adjustment
             **/
            *est += wgt * zg[jj] ;
        }
        else
        {
            /**
             * Perform climatological adjustment
             **/
            *est += wgt * zg[jj] * umeang[j][i] / umeang[iv][iu] ;
        }
    }

    if(sum != 0.0)
        *est /= sum ;
    else
        *est = 0.0 ;
}

/***********************************************************************
* Purpose:
* This function performs Gage-Only estimation
* via Single Optimal Estimation (Seo 1998).
*
* calling function: 
* functions called: 
*
* input variables
*
* isrch_g  - method for nearest-neighbor search; 1 for double heap-
*            sorting, 2 for  spiral search
* cvg      - conditional coefficient of variation of gage rainfall
* f0       - fractional coverage of rainfall within the radius of
*            influence
* nbrs_g   - number of nearest gages to be used
* umeang   - [rowSize][colSize] PRISM field
* i        - HRAP x-coordinate of the point of estimation
* j        - HRAP y-coordinate of the point of estimation
* rain_min - minimum detectable rainfall in mm
* dist_min - separation distance in km
* cor0pi   - lag-0+ conditional correlation coefficient
* cor0i    - lag-0 conditional correlation coefficient
* rngi     - conditional correlation scale
* cor0pc   - lag-0+ indicator correlation coefficient
* cor0c    - lag-0 indiator correlation coefficient
* rngc     - indicator correlation scale
* iug      - array of HRAP x-coordinates of gage data
* ivg      - array of HRAP y-coordinates of gage data
* zg       - array of gage data
* ilist_g  - array of index of nearest gages
*
* output variables
*
* est     - estimated rainfall in mm
*
* var     - estimated error variance in mm**2
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 21, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jul 12, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_go_soe(const int rowSize , const int colSize ,
            const int isrch_g , const double cvg, 
            const double f0, const int nbrs_g ,
            double ** umeang ,
            const int i , const int j ,
            const double rain_min , const double dist_min ,
            const double cor0pi , const double cor0i ,
            const double rngi , const double cor0pc ,
            const double cor0c , const double rngc ,
            short * iug, short * ivg , float * zg ,
            short * ilist_g , 
            double * est)
{
    int ictr, jctr ;
    double var ;
    int errFlag ;

    double *b = NULL;
    double *bi = NULL;
    double *w = NULL ; 
    double **cov = NULL;
    double **covi = NULL;

    /**      
     * allocate memory
     **/
    b = (double *)malloc((nbrs_g + 1) * sizeof(double)); 
    if(b == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in go_soe function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    bi = (double *)malloc((nbrs_g + 1) * sizeof(double)); 
    if(bi == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in go_soe function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    w = (double *)malloc((nbrs_g + 1) * sizeof(double)); 
    if(w == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in go_soe function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    cov = (double **)malloc((nbrs_g + 1) * sizeof(double *)); 
    if(cov == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in go_soe function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(ictr = 0; ictr < (nbrs_g + 1); ictr++)
    {
        cov[ictr] = (double *)malloc((nbrs_g + 1) * sizeof(double)); 
        if(cov[ictr] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in go_soe function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    

    covi = (double **)malloc((nbrs_g + 1) * sizeof(double *)); 
    if(covi == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in go_soe function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(ictr = 0; ictr < (nbrs_g + 1); ictr++)
    {
        covi[ictr] = (double *)malloc((nbrs_g + 1) * sizeof(double)); 
        if(covi[ictr] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in go_soe function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    

    /**
    * construct indicator correlation matrix
    **/
    for(ictr = 0; ictr < nbrs_g + 1; ictr ++)
    {
        bi[ictr] = 0.0 ;
         b[ictr] = 0.0 ;

        for(jctr = 0; jctr < nbrs_g + 1; jctr ++)
        {
            covi[jctr][ictr] = 0.0 ;
             cov[jctr][ictr] = 0.0 ;
        }
    }

    if(f0 < 1.0)
    {
        /**
        * construct conditional correlation matrix
        **/
        MPEFieldGen_covmatgo(isrch_g, 1.0, nbrs_g, dist_min, cor0pi, cor0i,
            rngi, iug, ivg, ilist_g, covi) ;

        /**
         * construct indicator correlation vector
         **/
        MPEFieldGen_covvecgo(isrch_g, i, j, 1.0, nbrs_g, dist_min, cor0pi,
            cor0i, rngi, iug, ivg, ilist_g, bi) ;
    }

    /**
     * construct conditional correlation matrix
     **/
    MPEFieldGen_covmatgo(isrch_g, 1.0, nbrs_g, dist_min, cor0pc, cor0c,
        rngc, iug, ivg, ilist_g, cov) ;

    /**
     * construct indicator correlation vector
     **/
    MPEFieldGen_covvecgo(isrch_g, i, j, 1.0, nbrs_g, dist_min, cor0pc,
        cor0c, rngc, iug, ivg, ilist_g, b) ;

    /**
     * compute unconditional gage covariance matrix
     **/
    double denum, denom ;
    int j1, j2 ;

    for(j1 = 1; j1 <= nbrs_g; j1 ++)
    {
        for(j2 = 1; j2 <= j1; j2 ++)
        {
            denum = (cvg * cvg) * (1.0 - f0)
                     * cov[j2-1][j1-1] * covi[j2-1][j1-1]
                     + (1.0 - f0) * covi[j2-1][j1-1]
                     + (cvg * cvg) * f0 * cov[j2-1][j1-1] ;
            denom = cvg * cvg + (1.0 - f0) ;

            if(denom != 0.0)
                cov[j2-1][j1-1] = denum / denom ;
        }
    }

    /**
     * fill using symmetry
     **/
    for(j1 = 1; j1 <= (nbrs_g+1); j1 ++)
    {
        for(j2 = j1; j2 <= (nbrs_g+1); j2 ++)
            cov[j2-1][j1-1] = cov[j1-1][j2-1] ;
    }

    /**
     * compute unconditional covariance vector
     **/
    for(j1 = 1; j1 <= nbrs_g; j1 ++)
    {
        denum = (cvg * cvg) * (1.0 - f0)
                * b[j1-1] * bi[j1-1]
                + (1.0 - f0) * bi[j1-1]
                + (cvg * cvg) * f0 * b[j1-1] ;
        denom = cvg * cvg + (1.0 - f0) ;

        if(denom != 0.0)
            b[j1-1] = denum / denom ;
    }

    /**
     * copy before calling lsolve1
     **/
     for(j1 = 0; j1 < (nbrs_g + 1); j1++)
         w[j1] = b[j1] ;

    /**
     * solve linear system 
     **/
    MPEFieldGen_lsolve(nbrs_g + 1, cov, b, &errFlag) ;

    /**
     * check error flag here
     **/

    if(errFlag != 0)
    {
        sprintf ( message , "in go_soe..."
            "error in lsolve...stop.");
        shutDownMPE( message, logFile );
    }

    /**
     * compute estimate and normalized estimation variance
     **/
    var = 1.0 ;
    *est = 0.0 ;
    int k, jj ;
    if(nbrs_g >= 1)
    {
        for(k = 0; k < nbrs_g; k ++)
        {
            if(isrch_g == 1)
                jj = ilist_g[k] ;
            else
                jj = k ;
            if((umeang[ivg[jj]][iug[jj]] <= 0.0)
                || (umeang[j][i] <= 0.0))
            {
                /**
                 * Do not perform climatological adustment
                 **/
                *est += b[k] * zg[jj] ;
            }
            else
            {
                /**
                 * Perform climatological adjustment
                 **/
                if(umeang[ivg[jj]][iug[jj]] != 0)
                    *est += b[k] * zg[jj] * umeang[j][i]
                         / umeang[ivg[jj]][iug[jj]] ;

                var -= b[k] * w[k] ; 
            }
        }
    }

    var += b[nbrs_g] ;
    
    if(var > 1.0)
    {
        sprintf ( message , "in go_soe...var: %f gt 1.0", var);
        printMessage( message, logFile );
    }

    /**
     * check nonnegativity of estimation variance and estimate
     **/
    if(var < -0.0001)
    {
        sprintf ( message , "in go_soe..."
            "negative estimation variance: %f ...stop.", var);
        shutDownMPE( message, logFile );
    }

    if(*est < 0.0 ) *est = 0.0 ;

    /**      
     * release memory
     **/
    if(cov != NULL)
    {
        for(ictr = 0; ictr < (nbrs_g + 1); ictr++)
        {
            if(cov[ictr] != NULL)
            {
                free(cov[ictr]);
                cov[ictr] = NULL;
            }
        }

        free(cov);
        cov = NULL;
    }

    if(covi != NULL)
    {
        for(ictr = 0; ictr < (nbrs_g + 1); ictr++)
        {
            if(covi[ictr] != NULL)
            {
                free(covi[ictr]);
                covi[ictr] = NULL;
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

}


/***********************************************************************
* Purpose:
* This function constructs the covariance matrix for SOE.
*
* calling function: 
* functions called:
*
* input variables
*
* isrch_g - method for nearest-neighbor search; 1 for double heap-
*           sorting, 2 for spiral search
* varg    - (indicator) variance of gage rainfall
* nbrs_g  - number of nearest rain gage measurements used
* dr      - nugget distance
* cng     - limiting lag-zero (indicator) correlation coefficient
*           of gage rainfall
* cg      - lag-zero (indicator) correlation coefficient of gage
*            rainfall
* rg      - (indicator) correlation scale of gage rainfall
* iug     - array of HRAP x-coordinate of gage data
* ivg     - array of HRAP y-coordinate of gage data
* ilist_g - array of index of nearest gage data
*
* output variables
*
* cov     - (nbrs_g+1)x(nbrs_g+1) covariance matrix
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 24, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jul 01, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_covmatgo(const int isrch_g , const double varg, 
            const int nbrs_g , const double dr ,
            const double cng, const double cg , 
            const double rg , 
            short * iug, short * ivg ,             
            short * ilist_g , 
            double ** cov)
{
    int i, j, j1, j2, i1, i2 ;
    double h, tempCov;
    
    /**
     * specify the size of the matrix
     **/
    int iend = nbrs_g ;

    /**
     * initialize covariance matrix
     **/
    for(i = 0; i < iend; i ++)
    {
        for(j = 0; j <= i; j ++)
            cov[i][j] = 0.0 ;
    }

    if(nbrs_g >= 1)
    {
        /**
         * construct the indicator covariance submatrix of
         * gage rainfall (lower triangular)
         **/
        for(j1 = 0; j1 < nbrs_g; j1++)
        {
            if(isrch_g == 1)
                i1 = ilist_g[j1] ;
            else
                i1 = j1 ;

            for(j2 = 0; j2 <= j1; j2 ++)
            {
                if(isrch_g == 1)
                    i2 = ilist_g[j2] ;
                else
                    i2 = j2 ;

                h = (iug[i1] - iug[i2]) * (iug[i1] - iug[i2]) +
                    (ivg[i1] - ivg[i2]) * (ivg[i1] - ivg[i2]) ;
                h = sqrt(h) ;

                MPEFieldGen_gammf(dr, cng, cg, rg, h, &tempCov) ;
                cov[j1][j2] = varg * tempCov ;
            }
        }
    }

    /**
     * append unbiasedness constraint
     **/
    for(j = 0; j < iend; j ++)
        cov[iend][j] = 1.0 ;

    cov[iend][iend] = 0.0 ;

    /**
     * fill using symmetry
     **/
    for(j1 = 0; j1 < (iend + 1); j1 ++)
    {
        for(j2 = j1; j2 < (iend + 1); j2 ++)
            cov[j1][j2] = cov[j2][j1] ;
    }
}


/***********************************************************************
* Purpose:
* This function constructs the covariance vector for SOE.
*
* calling function: 
* functions called: none
*
* input variables
*
* input variables
*
* isrch_g - method of nearest-neighbor search; 1 for double heap-
*           sorting, 2 for spiral search
* iu0     - HRAP x-coordinate of the point of estimation
* iv0     - HRAP y-coordinate of the point of estimation
* varg    - normailzed variance of gage rainfall
* nbrs_g  - number of nearest gage data to be located
* dr      - separation distance in km
* cng     - lag-0+ correlation coefficient of gage rainfall
* cg      - lag-0 correlation coefficient of gage rainfall
* rg      - correlation scale in km of gage rainfall 
* iug     - array of HRAP x-coordinate of gage rainfall
* ivg     - array of HRAP y-coordinate of gage rainfall
* ilist_g - array of index of nearest gage data
*
* output variables
*
* b       - (nbrs_g+1)x[1] array of covariance vector
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 24, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jul 01, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/

void MPEFieldGen_covvecgo(const int isrch_g , const int iu0, const int iv0, 
            const double varg , const int nbrs_g ,
            const double dr , const double cng ,
            const double cg , const double rg , 
            short * iug, short * ivg ,             
            short * ilist_g , 
            double * b)
{
    int i=0, j=0 ;
    double h, tempB ;

    if(nbrs_g >= 1)
    {
        /**
         * construct the gage rainfall covariance subvector
         **/
        for( j = 0; j < nbrs_g; j ++)
        {
            if(isrch_g == 1)
                i = ilist_g[j] ;
            else
                i = j ;

            h = (iu0 - iug[i]) * (iu0 - iug[i]) +
                (iv0 - ivg[i]) * (iv0 - ivg[i]) ;
            h = sqrt(h) ;

            MPEFieldGen_gammf(dr, cng, cg, rg, h, &tempB) ;
            b[j] = varg * tempB ;
        }
    }

    /**
     * append the unbiasedness constraint
     **/
    b[nbrs_g] = 1.0 ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
