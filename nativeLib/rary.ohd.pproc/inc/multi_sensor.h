/*******************************************************************************
* FILENAME:              multi_sensor.h
*
* DESCRIPTION:         This file contains function prototypes
*                      for multi-sensor and gage_only functions.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         April, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef MULTI_SENSOR_H
#define MULTI_SENSOR_H

#include "mpe_fieldgen.h"

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

void MPEFieldGen_coef_var_r(const int rowSize , const int colSize , 
                double ** rad, int ** mis, const double rain_min ,
                double * cvr, int * errFlag) ;

void MPEFieldGen_coef_var_g(const int gageSize,
                float * z ,
                const double rain_min ,
                double * cvg ,
                int * errFlag) ;

void MPEFieldGen_cor_scale_r(const double rowSize , const double colSize ,
                double ** rad, int ** mis, 
                double * rngi, double * rngc, int *errFlag);

void MPEFieldGen_find_indices(const int iradi ,
            int * num , int * idis , int * jdis) ;

void MPEFieldGen_local_stat(const int rowSize , const int colSize , const int isnsr ,
                const int iradi , double ** rad, int ** mis, 
                int * nfill, int ** ndata, int ** nposi) ;

void MPEFieldGen_srch_nbrs(const int rowSize, const int colSize, 
                const int isnsr, const int n ,
                int * idis, int * jdis, 
                const int ix, const int iy, 
                double ** rad, int ** mis,
                int * nbrs,    short * iu, short * iv, float * z) ;

void MPEFieldGen_ksol(const int nright, const int neq, const int nsb,
        double * a, double * r, double * s, int * ising);

void MPEFieldGen_lsolve(const int neq ,
            double ** cov ,
            double * r ,
            int * ising) ;

void MPEFieldGen_gammf(const double dr, const double cn,
        const double c, const double r, const double h,
        double * covar) ;

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
                int * errFlag) ;

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
            int   * errFlag);

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
            double ** cov) ;

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
            double * b) ;

void MPEFieldGen_gage_only(const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** umeang ,
                double ** est ,
                int * errFlag) ;

void MPEFieldGen_go_soe(const int rowSize , const int colSize ,
            const int isrch_g , const double cvg, 
            const double f0, const int nbrs_g ,
            double ** umeang ,
            const int i , const int j ,
            const double rain_min ,const double dist_min ,
            const double cor0pi ,const double cor0i ,
            const double rngi ,const double cor0pc ,
            const double cor0c ,const double rngc ,
            short * iug, short * ivg , float * zg ,
            short * ilist_g , double * est);

void MPEFieldGen_go_rds(const int rowSize , const int colSize ,
            const int isrch_g , const int nbrs_g ,
            const int i , const int j ,
            short * iug, short * ivg , float * zg ,
            double ** umeang , 
            short * ilist_g , float * rlist_g ,
            double * est) ;

void MPEFieldGen_covvecgo(const int isrch_g , const int iu0, const int iv0, 
            const double varg , const int nbrs_g ,
            const double dr , const double cng ,
            const double cg , const double rg , 
            short * iug, short * ivg ,             
            short * ilist_g , 
            double * b);

void MPEFieldGen_covmatgo(const int isrch_g , const double varg, 
            const int nbrs_g , const double dr ,
            const double cng, const double cg , 
            const double rg , 
            short * iug, short * ivg ,             
            short * ilist_g , 
            double ** cov) ;

void allocMultiMemory(const geo_data_struct * pGeoData) ;
void releaseMultiMemory(const geo_data_struct * pGeoData) ;

#endif /* #ifndef MULTI_SENSOR_H */
