#ifndef _GB2DEF_H
#define _GB2DEF_H

#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"
#include "grib2.h"
/************************************************************************
 * GB2DEF								*
 *									*
 * This header file contains general include files, ????????????,	*
 *									*
 **									*
 * Log:									*
 * S. Gilbert/NCEP   10/04                                              *
 * S. Gilbert/NCEP   10/05	increased dimension of geminfoparm for  *
 *                               null                                   *
 * S. Gilbert/NCEP   10/05	replaced four prototypes for functions  *
 *                               used to read grib2 tables.             *
 * S. Gilbert/NCEP   03/06	Added gb2_read                          *
 ***********************************************************************/

/*-------------------   G2DIAG defines  ------------------*/
#define G2_IS   1
#define G2_IDS  2
#define G2_GDS  4
#define G2_PDS  8
#define G2_DRS  16
#define G2_BMS  32


/*----------------   Data structures  ----------------------------*/


/*
 * gribmsg struct contains GRIB message and current decoded field.
 */
struct gribmsg {
        unsigned char  *cgrib2;
        int            mlength;
        gribfield      *gfld;
        g2int          field_tot;
        int            g2scan_mode;
        int            kx;
        int            ky;
        int            tmrange;
        char           origcntr[8];
};
typedef struct gribmsg Gribmsg;

/*
 * geminfo struct contains info identifying a GEMPAK grid
 */
struct geminfo {
        /*char           gdattm[DTTMSZ][2];*/
        char           gdattm1[DTTMSZ];
        char           gdattm2[DTTMSZ];
        int            level[2];
        int            vcord;
        char           parm[13];
        int            iuscal;
        float          rmsval;
        float          gdsarr[10];
        char           cproj[24];
        float          corners[4];
        float          navblk[LLNNAV];
};
typedef struct geminfo Geminfo;

/* End Data structures */

/* -------------------   Prototypes  ------------------*/

void gb2_2gem( Gribmsg *, Geminfo *, char **, int, int *);

void gb2_clos ( Gribmsg *, int *);

void gb2_ctim ( int , char *);

void gb2_diag ( gribfield *, int );

void gb2_ens( gribfield *, char *);

int gb2_fcsm(int , int );

void gb2_ftim ( gribfield *, char *, int *, int *);

void gb2_gaus ( gribfield *, float *, int *, int *);

void gb2_gdsnav( gribfield *, char *,  int *,  int *,
                float *, float *, float *,
                int *, int *);

void gb2_gdtlamb( float *navblk, int *igdtmpl, int *iret );
void gb2_gdtltln( float *navblk, int *igdtmpl, int *iret );
void gb2_gdtmerc( float *navblk, int *igdtmpl, int *iret );
void gb2_gdtpstr( float *navblk, int *igdtmpl, int *iret );

void gb2_gmis ( gribfield *, float *, int *);

void gb2_grid ( Gribmsg *, int ,
                float , int *,
                float *, int *);

void gb2_gtcntr( int , char *, char *, int *);

void gb2_gtlcllvltbl( char *, char *, int, G2lvls **, int *);

void gb2_gtwmolvltbl( char *, int, G2lvls **, int *);

void gb2_gtwmovartbl( char *, int, G2vars_t **, int *);

void gb2_gtlclvartbl( char *, char *, int, G2vars_t **, int *);

void gb2_lamb ( gribfield *, float *, int *, int *);

void gb2_level2g2( int vcord, char *wmolvltbl, char *lcllvltbl,
                   int wmover, int lclver, char *wmocntr,
                   G2level *tblentry, int *iret );
				      
void gb2_ltln ( gribfield *, float *, int *, int *);

void gb2_merc ( gribfield *, float *, int *, int *);

void gb2_navgdt( float *navblk, int *igdtnum, int *igdtmpl, int *iret);

void gb2_next( Gribmsg *, int *, int *);

void gb2_open ( char *, int *, Gribmsg *, int *);

void gb2_ornt ( int , int , int , float *,
                float *, int *);

void gb2_param ( char *, char *, int, Gribmsg *,
                char *, int *, float *, int *);

void gb2_param2g2( char *param, char *wmovartbl, char *lclvartbl,
                   int wmover, int lclver, char *wmocntr,
                   G2Vinfo *tblentry, int *iret );
				      
void gb2_polr ( gribfield *, float *, int *, int *);

void gb2_prob ( gribfield *gfld, char *param );

void gb2_read ( int *mxgrib, unsigned char *cpack, int *iret );

void gb2_setftime( char *dattim, int tint, int pdtnum, int *pdtmpl, int *iret );

void gb2_sklvl( int, int, G2lvls *, G2level *, int *);

void  gb2_skparam( char *param , G2vars_t *vartbl,
                 G2Vinfo *g2var, int *iret);
		 
void gb2_skvar( int, int, int, int, G2vars_t *,
                 G2Vinfo *, int *);

void gb2_skvcord( int vcord, G2lvls *lvltbl,
                 G2level *g2lev, int *iret);
		 
void gb2_vcrd ( char *, char *, Gribmsg *,
                int *, int *, int *);

void gb2_vlev ( gribfield *, float *, int *);

/* End Prototypes */

#endif   /* _GB2DEF_H */
