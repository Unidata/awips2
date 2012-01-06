/***********************************************************************
* Purpose:
* This function computes the sample coefficient of variation of 
* bias-adjusted radar rainfall.
*
* calling function: multi_sensor
* functions called: none
*
* input variables
*
* rad      - [rowSize][colSize] array of bias-adjusted radar rainfall
* mis      - [rowSize][colSize] array of binary 'misbin' indexz
* rain_min - minimum detectable rainfall in mm
*
* output variables
*
* cvr      - sample coefficient of variation of positive bias-adjusted
*            radar rainfall
* errFlag  - error flag; 0 for successful calculation, 1 for
*            unsuccessful calculation
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 24, 2005 Guoxian Zhou      finish component testing 
*
***********************************************************************/
#include "empe_fieldgen.h"
#include "multi_sensor.h"
#include "BinarySearch.h"

void coef_var_r(const int rowSize , const int colSize , 
                double ** rad, int ** mis, const double rain_min ,
                double * cvr, int * errFlag)
{
    int num = 0 ;
    double sum1 = 0.0 ;
    double sum2 = 0.0 ;

    int i, j ;

    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            /**
            * throw out nonpositive values
            **/

            if(rad[i][j] < rain_min)
            {
            	continue ;
            }

            /**
             * throw out values subject to beam blockage
             **/

            if(mis[i][j] == 0)
            {
            	continue ;
            }

            num ++ ;
            sum1 += rad[i][j] ;
            sum2 += rad[i][j] * rad[i][j] ;
        }
    }

    /**
     * check for the minimum sample size
     **/

    if(num < 2)
    {
        *errFlag = 1 ;
        return ;
    }

    /**
     * calculate sample mean and variance
     **/

    double ave = sum1 / num ;
    double var = sum2 / (num - 1) -
                 sum1 * sum1 / (num * (num - 1)) ;

    /**
     * check if the sample variance is positive
     **/

    if(var <= 0.0)
    {
        *errFlag = 1 ;
        return ;
    }

    /**
     * calculate the sample coefficient of variation
     **/

    *cvr = sqrt(var) / ave ;

    *errFlag = 0 ;
    return ;
}


/***********************************************************************
* Purpose:
* This function computes coefficient
* of variation of positive gage rainfall.
*
* calling function: gage_only
* functions called: none
*
* input variables
*
* gageSize - array of gage rainfall in mm
*
* z        - array of gage rainfall in mm
*
* rain_min - minimnum detectable rainfall in mm
*
* output variables
*
* cvg      - conditional coefficient of variation
*
* errFlag  - error flag; 0 if OK, 1 if not
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 21, 2005 Guoxian Zhou      finish conversion to C Language 
*
***********************************************************************/
void coef_var_g(const int gageSize,
                float * z ,
                const double rain_min ,
                double * cvg ,
                int * errFlag)
{
    int npos = 0 ;
    double ave, var ;
    double sum1 = 0.0 ;
    double sum2 = 0.0 ;
    int i ;

    *cvg = 0.0 ;
    
    for(i = 0; i < gageSize; i++)
    {
        if(z[i] < rain_min)
        {
        	continue ;
        }

        npos ++ ;
        sum1 += z[i] ;
        sum2 += z[i] * z[i] ;
    }

    if(npos <= 1)
    {
        *errFlag = 1 ;
        return ;
    }

    ave = sum1 / npos ;
    var = sum2 / (npos - 1) - sum1 * sum1 / (npos * (npos - 1)) ;

    if(var <= 0.0)
    {
        *errFlag = 1 ;
        return ;
    }

    *cvg = sqrt(var) / ave ;

    *errFlag = 0 ;
    return ;
}

/****************************************************************************
* Purpose:
* This function evaluates exponential correlation coefficient.
*
* calling function: 
* functions called: none
*
* input variables
*
* dr - separation distance in km
* cn - limiting lag-0 correlation coefficient
* c  - lag-0 correlation coeffcient
* r  - correlation distance in km
* h  - distance in km
*
* output variables
*
* covar - covariance in mm**2
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 21, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 29, 2005 Guoxian Zhou      finish component testing
*
****************************************************************************/
void gammf(const double dr, const double cn,
        const double c, const double r, const double h,
        double * covar)
{
    if(r == 0.0)
    {
        sprintf( message , "r eq 0.0 in gammf...stop" );
        shutdown( message );
    }

    if(h <= dr)
    {
        *covar = c ;
    }
    else
    {
        *covar = cn * exp(-h/r) ;
    }

    return ;
}


/*******************************************************************
* Purpose:
* This function locates all data points within the radius of
* influence via double heap-sorting.
*
* calling function: 
* functions called: 
*
* input variables
*
* pGageTable - array of gage data
*
* iu0   - HRAP x-coordinate at the point of estimation
* iv0   - HRAP y-coordinate at the point of estimation
* iradi - radius of influence in HRAP bins
*
* output variables
*
* k     - number of neighbors located
* ilist - array of the indices of the neighbors
* rlist - array of the distances to the neighbors
*
* I/O variables which hold previous values
*
* iu0_prev - HRAP x-coordinate of previous point of estimation
* m        - preserve number of values sorted
* ivv      - preserve copy of previous ivv array
* in       - preserve copy of previous in  array
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Dec 02, 2003 Russell Erb       Modification
*   Jul 12, 2005 Guoxian Zhou      modified from find_nbrs1X 
*   Jul 15, 2005 Guoxian Zhou      finish component testing
*
*******************************************************************/
void find_nbrsX(const int gageSize ,
                short * iug ,
                short * ivg , 
                const int iu0 ,
                const int iv0 ,
                const int iradi ,
                int * k ,
                short * ilist ,
                float * rlist ,
                int * iu0_prev ,
                int * m_save, 
                short * ivv_save, 
                short * in_save)
{

   /* Variable declarations. */
   double dist;
   double xdis;
   double ydis;
   int i;
   int j = 0;
   int i0;
   int it;
   int m = 0;
   short jn [ gageSize ];

   /* if iu0 is the same as the previous one then skip to 41 */

   if (iu0 != * iu0_prev)
   {

      /* iu, iv, and z arrays must be sorted in the ascending order of iu 
         before calling this subroutine.  This can be done by calling the
         qksort4 routine. */

      for ( i = 0; i < gageSize; ++i )
      {  
         if ( iu0 <= iug[i] )
         {
         	break;
         }
      }

      i0 = i - 1;

      if ( i0 == -1 )
      { 
         /* iu0 is the smallest */

         for ( i = 0; i < gageSize; ++i )
         { 
            if ( ( iug[i] - iu0 ) > iradi)
            {
               break;
            }

            ivv_save [ m ] = ivg[i] ;
            in_save [ m ] = i;
            ++m;
         }
      }
      else if ( i0 == gageSize - 1 )
      {
         /* u0 is the largest */

         for ( i = 0; i < gageSize; ++i )
         {
            it = gageSize - i - 1;

            if ( ( iu0 - iug[it]) > iradi)
            {
               break;
            }
    
            ivv_save [ m ] = ivg[it];
            in_save [ m ] = it;
            ++m;
         }
      }
      else
      {
         /* u0 is somewhere in between */
      
         for ( i = i0 + 1; i < gageSize; ++i ) 
         {
            if ( ( iug[i] - iu0 ) > iradi)
            {
               break;
            }

            ivv_save [ m ] = ivg[i];
            in_save [ m ] = i;
            ++m;
         }

         for ( i = 0; i <= i0; ++i )
         {
            it = i0 - i;

            if ( ( iu0 - iug[it] ) > iradi)
            {
               break;
            }

            /* ++m; */ 
            ivv_save [ m ] = ivg[it];
            in_save [ m ] = it;
            ++m; 
         }
      }

      
      if ( m > 1 )
      {
         qksorti22 ( m, ivv_save, in_save ); 
      }

      * iu0_prev = iu0;
      * m_save = m;
   } /* Closing brace for the iu0 and iu0_prev comparison. */

   for ( i = 0; i < * m_save; ++i ) 
   { 
      if ( iv0 <= ivv_save [ i ] )
      {
      	break;
      }
   }
   
   i0 = i - 1;

   if ( i0 == -1 )
   {
      /* iv0 is the smallest */
      for ( i = 0; i < *m_save; ++i )
      {
         if ( ( ivv_save[i] - iv0 ) > iradi )
         {
         	break;
         }

         jn [ j ] = in_save [ i ];
         ++j;
      }
   }
   else if ( i0 == *m_save - 1 )
   {
      /* iv0 is the largest */
      for ( i = 0; i < * m_save; ++i )
      {
         it = *m_save - i - 1;

         if ( ( iv0 - ivv_save [ it ] ) > iradi )
         {
         	break;
         }

         jn [ j ] = in_save [ it ];

         ++j;
      }
   }
   else
   {
      /* iv0 is somewhere in between */
      j = 0;
     
      for ( i = i0 + 1; i < * m_save; ++i )
      { 
         if ( ( ivv_save[i] - iv0 ) > iradi )
         {
         	break;
         }

         jn[j]=in_save[i];
         ++j;
      }

      for ( i = 0; i <= i0; ++i )
      {
         it = i0 - i;

         if ( ( iv0 - ivv_save[it] ) > iradi )
         {
         	break;
         }

         jn[j]=in_save[it];
         ++j;
      }
   }

   /* calculate the distance */
   * k = 0;  /* Initialize the number of neighbors found to 0. */

   for ( i = 0; i < j; ++i )
   {
      xdis = ( double ) ( iug[ jn[ i ] ] - iu0 );
      ydis = ( double ) ( ivg[ jn[ i ] ] - iv0 );

      dist = xdis * xdis * 1.0 + ydis * ydis * 1.0;
      dist = sqrt ( dist ); 
      if ( dist > (double) iradi )
      {
      	continue;
      }
      ilist [ *k ] = jn [ i ];
      rlist [ *k ] = ( float ) dist;
      ++ ( *k ); 
   }

   /* sort in the ascending order of the distance */
   if ( *k > 1 )
   {
	  qksort22 ( *k, rlist, ilist );
   }

   return;
}


/***********************************************************************
* Purpose:
* This function computes local statistics of radar rainfall 
* over the circle of radius of max(rngc,rngi) 
* via successive addition and subtraction.
*
* calling function: gage_only
* functions called: none
*
* input variables
*
* isnsr - 1 for gage, 2 for radar
* iradi - radius of influence in HRAP bins
* rad   - [rowSize][colSize] array of radar rainfall in mm
* mis   - [rowSize][colSize] misbin array
*
* output variables
*
* nfill - number of HRAP bins that the radius of influence encompasses
* ndata - [rowSize][colSize] array of number of data points 
*         within the radius of influence
* nposi - [rowSize][colSize] array of number of positive data points 
*         within the radius of influence
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 21, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 24, 2005 Guoxian Zhou      finish component testing 
*
***********************************************************************/

void local_stat(const int rowSize , const int colSize , const int isnsr ,
                const int iradi , double ** rad, int ** mis, 
                int * nfill, int ** ndata, int ** nposi)
{

    int i, j, k  ;
    int iside ;
    double xdis, ydis, dist ;
    int il[rowSize], jl[rowSize], ir[rowSize], jr[rowSize] ;

    /**
     * initialize arrays.
     **/

    for(i = 0; i < rowSize; i++)
    {
        for(j = 0; j < colSize; j++)
        {
            ndata[i][j] = 0 ;
            nposi[i][j] = 0 ;
        }
    }

    /**
     * determine the radius of influence at the current bin.
     **/

    iside = 2 * iradi + 1 ;
    for(j = 1; j <= iside; j++)
    {
        ydis = j * 1.0 - (iradi + 1) ;
        for(i = 1; i <= (iside+1); i++)
        {
            xdis = i * 1.0 - (iradi + 1) ;
            dist = xdis * xdis + ydis * ydis ;
            dist = sqrt(dist) ;

            if(dist <= iradi)
            {
                ndata[j-1][i-1] = 1 ;
            }
            else
            {
                ndata[j-1][i-1] = 0 ;
            }
        }
    }

    /**
     * determine the radius of influence at the bin
     * immediately to the right of the current bin.
     **/

    for(j = 1; j <= iside; j++)
    {
        ydis = j * 1.0 - (iradi + 1) ;
        for(i = 2; i <= (iside+2); i++)
        {
            xdis = i * 1.0 - (iradi + 2) ;
            dist = xdis * xdis + ydis * ydis ;
            dist = sqrt(dist) ;

            if(dist <= iradi)
            {
                nposi[j-1][i-1] = 2 ;
            }
            else
            {
                nposi[j-1][i-1] = 0 ;
            }
        }
    }

    /**
     * current bin is at [iradi][iradi+1]....
     * determine additive and subtractive crescents
     * with respect to the current bin.
     **/

    int imid = iradi + 1 ;
    int jmid = iradi + 1 ;
    int nl = 0 ;
    int nr = 0 ;
    int nful = 0 ;
    int ndat = 0 ;
    int npos = 0 ;
    for(j = 1; j <= iside; j++)
    {
        if(j > rowSize)
        {
            sprintf ( message , "rad_local_stat...j > rowSize...stop.");
            shutdown( message );        
        }
        for(i = 1; i <= (iside + 2); i++)
        {
            if(i > colSize)
            {
                sprintf ( message , "rad_local_stat...i > colSize...stop.");
                shutdown( message );        
            }
            if((nposi[j-1][i-1] - ndata[j-1][i-1]) == -1)
            {
                il[nl] = i - imid ;
                jl[nl] = j - jmid ;
                nl ++ ;
                if(nl > colSize)
                {
                    sprintf ( message , "rad_local_stat...nl > colSize"
                        "...stop and reset colSize.");
                    shutdown( message );        
                }
                continue ;
            }
            if(nposi[j-1][i-1] - ndata[j-1][i-1] == 2)
            {
                ir[nr] = i - imid ;
                jr[nr] = j - jmid ;
                nr ++ ;
                if(nr > colSize)
                {
                    sprintf ( message , "rad_local_stat...nr > colSize"
                        "...stop and reset colSize.");
                    shutdown( message );        
                }
                continue ;
            }
        }
    }

    /**
     * initialize.
     **/

    for(i = 0; i < rowSize; i++)
    {
        for(j = 0; j < colSize; j++)
        {
            ndata[i][j] = -99 ;
        }
    }

    /**
     * start computing local statisics at every bin.
     **/

    int j0, i0, ibeg, iend, jbeg, jend;
    *nfill = 0 ;
    for(j0 = 1; j0 <= rowSize; j0 ++)
    {
        for(i0 = 1; i0 <= colSize; i0 ++)
        {
            if(i0 == 1)
            {
                /**
                 * determine the square
                 * that circumscribes the radius of influence.
                 **/

                if((i0 - iradi) < 1)
                {
                    ibeg = 1 ;
                }
                else
                {
                    ibeg = i0 - iradi ;
                }

                if((i0 + iradi) > colSize)
                {
                    iend = colSize ;
                }
                else
                {
                    iend = i0 + iradi ;
                }

                if((j0 - iradi) < 1)
                {
                    jbeg = 1 ;
                }
                else
                {
                    jbeg = j0 - iradi ;
                }

                if((j0 + iradi) > rowSize)
                {
                    jend = rowSize ;
                }
                else
                {
                    jend = j0 + iradi ;
                }

                nful = 0 ;
                ndat = 0 ;
                npos = 0 ;
                
                for(j=jbeg; j <= jend; j ++)
                {
                    int iydis = j - j0 ;
                    for(i = ibeg; i <= iend; i ++)
                    {
                        int ixdis = i - i0 ;
                        dist = sqrt((double)(ixdis * ixdis + iydis * iydis)) ;
                        if(dist > iradi)
                        {
                            continue ;
                        }
                        nful ++ ;
                        
                        /**
                         * check if the bin is out of the umbrella.
                         **/
 
                        if(rad[j-1][i-1] < 0.0)
                        {
                            continue ;
                        }

                        if(isnsr == 2)
                        {
                            /**
                             * check if the bin is subject to
                             * beam blockage.
                             **/

                            if(mis[j-1][i-1] == 0)
                            {
                                continue ;
                            }
                        }
                        ndat ++ ;
                        if(rad[j-1][i-1] > 0.0)
                        {
                            npos ++ ;
                        }
                    }
                }
            }
            else
            {
                /**
                 * current bin is at [j0-1][i0-1]...subtract and add.
                 **/

                int nfull = 0 ;
                int ndatl = 0 ;
                int nposl = 0 ;
                for(k = 1; k <= nl; k ++)
                {
                    i = (i0-1) + il[k-1] ;
                    if(i < 1)
                    {
                    	continue ;
                    }
                    if(i > colSize)
                    {
                    	continue ;
                    }
                    j = j0 + jl[k-1] ;

                    if(j < 1)
                    {
                    	continue ;
                    }
                    if(j > rowSize)
                    {
                    	continue ;
                    }

                    nfull ++ ;
                    nful -- ;

                    if(rad[j-1][i-1] < 0.0)
                    {
                    	continue ;
                    }
                    if(isnsr == 2)
                    {
                        if(mis[j-1][i-1] == 0)
                        {
                        	continue ;
                        }
                    }
                    ndatl ++ ;
                    ndat -- ;

                    if(rad[j-1][i-1] > 0.0)
                    {
                        nposl ++ ;
                        npos -- ;
                    }
                }

                int nfulr = 0 ;
                int ndatr = 0 ;
                int nposr = 0 ;
                for(k = 1; k <= nr; k ++)
                {
                    i = (i0-1) + ir[k-1] ;
                    if(i < 1)
                    {
                    	continue ;
                    }
                    if(i > colSize) 
                    {
                    	continue ;
                    }

                    j = j0 + jr[k-1] ;
                    if(j < 1) 
                    {
                    	continue ;
                    }
                    if(j > rowSize) 
                    {
                    	continue ;
                    }

                    nful ++ ;
                    nfulr ++ ;

                    if(rad[j-1][i-1] < 0.0) 
                    {
                    	continue ;
                    }
                    if(isnsr == 2)
                    {
                        if(mis[j-1][i-1] == 0) 
                        {
                        	continue ;
                        }
                    }
                    ndat ++ ;
                    ndatr ++ ;
                    if(rad[j-1][i-1] > 0.0)
                    {
                        npos ++ ;
                        nposr ++ ;
                    }
                }
            }

            if(nful > *nfill)
            {
                *nfill = nful ;
            }

            if(ndat < 0)
            {
                sprintf( message , "in rad_local_stat...ndat: %d"
                    " less than 0,", ndat) ;
                hpe_fieldgen_printMessage( message );            
            }
            if(npos < 0)
            {
                sprintf( message , "in rad_local_stat...npos: %d"
                    " less than 0,", npos) ;
                hpe_fieldgen_printMessage( message );
            }

            ndata[j0-1][i0-1] = ndat ;
            nposi[j0-1][i0-1] = npos ;
        }
    }
}


/***************************************************************************
* Purpose:
* This function computes the size of the HRAP box in km 
* given latitude in fractinal degrees.
*
* calling function: gage_only
* functions called: 
*
* input variables
*
* iradi - radius of influence in HRAP bins
*
* output variables
*
* num   - number of data points in arrays idisi and jdisi
* idis  - array containing array indices along the x-axis
* jdis  - array containing array indices along the y-axis
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 20, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 24, 2005 Guoxian Zhou      finish component testing 
*
*****************************************************************************/
void find_indices(const int iradi ,
            int * num , int * idis , int * jdis)
{
    int i, j ;
    int ix = iradi + 1 ;
    int iy = iradi + 1 ;
    const int index = 2 * iradi + 1 ;
    double * dist = NULL ;
    int count = 0 ;

    dist = (double *)malloc((index * index) * sizeof(double)); 
    if(dist == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in find_indices function."
            "\n\tProgram exit.") ;
        shutdown( message);
    }

    for(i = 1; i <= index; i++)
    {
        for(j = 1; j <= index; j ++)
        {
            idis[count] = i - ix ;
            jdis[count] = j - iy ;
            dist[count] = (double)(idis[count] * idis[count] + 
                          jdis[count] * jdis[count]) ;
            dist[count] = sqrt(dist[count]) ;
            count ++ ;
        }
    }

     heapSortForDoubleAndGeoIndex(dist, idis, jdis, count) ;

    for(i = 0; i < count; i++)
    {
        if(dist[i] > (double)iradi)
        {
            *num = i ;
            if(dist != NULL)
            {
                free(dist);
                dist = NULL ;
            }
            return ;
        }
    }

    *num = count ;

    if(dist != NULL)
    {
        free(dist);
        dist = NULL ;
    }
}

/***********************************************************************
* Purpose:
* This function finds n nearest neighbors via spiral search.
*
* calling function: 
* functions called: none
*
* input variables
*
* isnsr - sensor designation; 1 for gage, 2 for radar
* n     - number of valid data points within the ellipsoid of
*         influence
* idis  - x-coordinates of the above data points
* jdis  - y-coordinates of the above data points
* ix    - x-coordinate of the point of estimation
* iy    - y-cooridnate of the point of estimation
* nbrs  - number of nearest neighbors to be located
* rad   - field of interest
* mis   - [rowSize][colSize] misbin array
*
* output variables
*
* nbrs  - number of valid neighbors
* iu    - x-coordinates of the neighbors
* iv    - y-coordinates of the neighbors
* z     - values of the neighbors
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 21, 2005 Guoxian Zhou      finish conversion to C Language 
*
***********************************************************************/
void srch_nbrs(const int rowSize, const int colSize, 
                const int isnsr, const int n ,
                int * idis, int * jdis, 
                const int ix, const int iy, 
                double ** rad, int ** mis, int * nbrs, 
                short * iu, short * iv, float * z)
{
    int i, j, k ;
    int ndat = 0 ;

    for(k = 0; k < n; k ++)
    {
        i = idis[k] + ix ;

        if(i < 0) 
        {
        	continue ;
        }
        if(i >= colSize) 
        {
        	continue ;
        }

        j = jdis[k] + iy ;

        if(j < 0) 
        {
        	continue ;
        }
        if(j >= rowSize ) 
        {
        	continue ;
        }
        if(rad[j][i] < 0.0) 
        {
        	continue ;
        }

        if(isnsr == 2)
        {
            /**
             * consider beam blockage only for radar data.
             **/

            if(mis[j][i] == 0) 
            {
            	continue ;
            }
        }

        iu[ndat] = i ;
        iv[ndat] = j ;
         z[ndat] = rad[j][i] ;

        ndat ++ ;

        if(ndat == *nbrs)
        {
        	return ;
        }
    }

    /**
     * the total number of valid data points is less than nbrs
     **/

    *nbrs = ndat ;
}

/***********************************************************************
* Purpose:
* Solution of a System of Linear Equations.
*
* calling function: 
* functions called: none
*
* input variables
*
* nright,nsb number of columns in right hand side matrix.
*            for OKB2D: nright=1, nsb=1
* neq  number of equations
* a()  upper triangular left hand side matrix (stored 
*      columnwise)
* r()  right hand side matrix (stored columnwise)
*      for okb2d, one column per variable
*
* output variables
*
* s()   solution array, same dimension as  r  above.
* ising singularity indicator
*       0,  no singularity problem
*       -1,  neq  <=  1
*       k,  a null pivot appeared at the kth iteration
* PROGRAM NOTES:
*
*   1. Requires the upper triangular left hand side matrix.
*   2. Pivots are on the diagonal.
*   3. Does not search for max. element for pivot.
*   4. Several right hand side matrices possible.
*   5. USE for ok and sk only, NOT for UK.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 24, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/
void ksol(const int nright, const int neq, const int nsb,
        double * a, double * r, double * s, int * ising)
{
    int i1, i2, i3, i4, i5;

    int i, j, k, m1;
    double ak, ap;
    int ii, kk, in, ll, nm, nn, lp, iv, km1, ll1, nm1, llb, ijm;
    double tol, piv;
    int ij = 0 ;

    --s;
    --r;
    --a;

    /**
     * If there is only one equation then set ising and return:
     **/

    if (neq <= 1)
    {
        *ising = -1;
        sprintf ( message , "error from ksol: neq <= 1...return.");
        hpe_fieldgen_printMessage( message );
        return ;
    }

    /* Initialize: */

    tol = 1e-5;
    *ising = 0;
    nn = neq * (neq + 1) / 2;
    nm = nsb * neq;
    m1 = neq - 1;
    kk = 0;

    /* Start triangulation: */

    i1 = m1;
    for (k = 1; k <= i1; ++k)
    {
        kk += k;
        ak = a[kk];
        if (fabs(ak) < tol)
        {
            sprintf ( message , "error from ksol: fabs(ak) < tol...return.");
            hpe_fieldgen_printMessage( message );
            *ising = k;
            return ;
        }

        km1 = k - 1;
        i2 = nright;
        for (iv = 1; iv <= i2; ++iv)
        {
            nm1 = nm * (iv - 1);
            ii = kk + nn * (iv - 1);
            piv = 1.0 / a[ii];
            lp = 0;
            i3 = m1;
            for (i = k; i <= i3; ++i)
            {
                ll = ii;
                ii += i;
                ap = a[ii] * piv;
                ++lp;
                ij = ii - km1;
                i4 = m1;
                for (j = i; j <= i4; ++j)
                {
                    ij += j;
                    ll += j;
                    a[ij] -= ap * a[ll];
                }
                i4 = nm;
                i5 = neq;
                for (llb = k;
                     i5 < 0 ? llb >= i4 : llb <= i4;
                     llb += i5)
                {
                    in = llb + lp + nm1;
                    ll1 = llb + nm1;
                    r[in] -= ap * r[ll1];
                }
            }
        }
    }

    ijm = ij - nn * (nright - 1);
    if (fabs(a[ijm]) < tol)
    {
        *ising = neq;
        sprintf ( message , "error from ksol: fabs(a[ijm]) < tol...return.");
        hpe_fieldgen_printMessage( message );
        return ;
    }

    /**
     * Finished triangulation, start solving back
     **/

    i1 = nright;
    for (iv = 1; iv <= i1; ++iv)
    {
        nm1 = nm * (iv - 1);
        ij = ijm + nn * (iv - 1);
        piv = 1.0 / a[ij];
        i2 = nm;
        i3 = neq;
        for (llb = neq;
             i3 < 0 ? llb >= i2 : llb <= i2;
             llb += i3)
        {
            ll1 = llb + nm1;
            s[ll1] = r[ll1] * piv;
        }
        i = neq;
        kk = ij;
        i3 = m1;
        for (ii = 1; ii <= i3; ++ii)
        {
            kk -= i;
            piv = 1.0 / a[kk];
            --i;
            i2 = nm;
            i5 = neq;
            for (llb = i;
                i5 < 0 ? llb >= i2 : llb <= i2;
                llb += i5)
            {
                ll1 = llb + nm1;
                in = ll1;
                ap = r[in];
                ij = kk;
                i4 = m1;
                for (j = i; j <= i4; ++j)
                {
                    ij += j;
                    ++in;
                    ap -= a[ij] * s[in];
                }
                s[ll1] = ap * piv;
            }
        }
    }

    /**
     * Finished solving back, return
     **/

    return ;
}

/***********************************************************************
* Purpose:
* This function solves linear system via Gauss elimination
* with no pivoting (Deutsch and Journel 1992).
*
* calling function: 
* functions called: 
*
* input variables:
*
* neq   - dimension of the linear system                              
* cov   - (nbrs_g+1)x(nbrs_g+nbrs_r+1) left-hand side matrix
* r     - (nbrs_g+1)x[1] right-hand side vector
*
* output variables:
*
* r     - (nbrs_g+1)x[1] solution vector
* ising - singularity index
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Aug 12, 1998 D.-J. Seo         Original FORTRAN code  
*   Jun 22, 2005 Guoxian Zhou      finish conversion to C Language 
*   Jun 30, 2005 Guoxian Zhou      finish component testing
*
***********************************************************************/
void lsolve(const int neq ,
            double ** cov ,
            double * r ,
            int * ising)
{
    const int size = neq * neq ;
    double a[size], s[neq];
    int i, j ;

    if(neq == 1)
    {
        if(cov[0][0] != 0.0)
            r[0] /= cov[0][0] ;
        else
            r[0] = 0.0;

        *ising = 0 ;
        return ;
    }

    /**
     * initialization
     **/

    for( i = 0; i < size; i ++)
    {
    	a[i] = 0.0 ;
    }

    for( i = 0; i < neq; i ++)
    {
        s[i] = 0.0 ;
    }

    /**
     * copy columnwise the upper triangular matrix
     **/

    int knt = 0 ;
    for(j = 0; j < neq; j ++)
    {
        for(i = 0; i <= j; i ++)
        {
            a[knt] = cov[i][j] ;
            knt ++ ;
        }
    }

    ksol(1, neq, 1, a, r,s,ising) ;

    if(*ising != 0)
    {
        sprintf ( message , "error number from ksol: %d...skip.", *ising);
        hpe_fieldgen_printMessage( message );

        return ;
    }

    for(i = 0; i < neq; i ++)
    {
        r[i] = s[i] ;
    }
}
