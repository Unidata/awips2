#ifndef FORTRAN_WRAPPERS_H_
#define FORTRAN_WRAPPERS_H_

#include "no_xm_geminc.h"
#include "gemprm.h"


void cgd_ngrd ( int iacss, int *numgrd, char *firstm, char *lasttm,
                int *iret );
void cti_stan ( const char *time, const char *lastim, char *dattim,
                int *iret );
void ctg_full ( const char *gdattm, const char *firstm, const char *lasttm,
		char *fulltm, int *iret );
void cgd_gtim ( int iacss, int maxtim, char **timarr, int *ntimes, int *iret );
void ctg_rnoi ( const char *tbegin, const char *tend, int itftyp, int ntimef,
		char **filtim, int *ntime, char **times, int *iret );
void ctg_rinc ( const char *tbegin, const char *tend, const char *tinc, 
                int itftyp, int *ntime, char **times, int *iret );
void cst_lstc ( char **carr, int num, const char *sep, int maxchr,
                char *string, int *iret );
void cst_narg ( const char *strng, int ipos, char argsep, int *nargs,
                int *iret );
void ctg_vald ( const char *gdattm, char *vdattm, int *iret );
void ctg_vi2f ( const char *vdtm, const char *idtm, char *fdtm, int *iret );
void cgd_rdat ( const int *iacss, const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
		const char *parm, float *grid, int *igx, int *igy,
		int *ighdr, int *iret );
void clv_ccrd ( int ivcord, char *vcoord, int *iret );
void clv_cord ( const char *vcoord, char *vparm, int *ivert, int *iret );
void ctg_cftm ( int ifcast, char *ftype, char *ftime, int *iret );
void ctg_itoc ( const int *intdtf, char *gdattm, int *iret );
void cgd_glev ( const int *iacss, const char *gdattm1, const char *gdattm2,
                const int *ivcord, const int *maxlev, int *levarr1,
	        int *levarr2, int *nlev, int *iret );
void cgd_wpgd ( const int *iacss, const float *grid, const int *igx,
                const int *igy, const int *ighdr, const char *gdattm1,
		const char *gdattm2, const int *level1, const int *level2,
		const int *ivcord, const char *parm, const int *rewrit,
		const int *ipktyp, const int *nbits, int *iret );
void cgd_wppg ( const int *iacss, const int *igrid, const int *lengrd,
                const int *igx, const int *igy, const int *ighdr,
                const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
                const char *parm, const int *rewrit, const int *ipktyp,
                const int *nbits, const int *misflg, const float *ref,
                const float *scale, const float *difmin, int *iret );
void cgd_gnum ( const int *iacss, const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
		const char *parm, int *ignum, int *iret );
void cti_ccnt ( const char *dattim, char *cent, int *iret );






#ifdef UNDERSCORE
    #define FORTRAN_NAME(name) name##_
#else
    #define FORTRAN_NAME(name) name
#endif

void FORTRAN_NAME(gd_ngrd) ( int*, int*, char*, char*, int*, size_t, size_t );
void FORTRAN_NAME(tg_full) ( const char*, const char*, const char*, char*,
			     int*, size_t, size_t, size_t, size_t );
void FORTRAN_NAME(gd_gtimw) ( int*, int*, char*, char*, int*, size_t, size_t );
void FORTRAN_NAME(tg_rnoiw) ( const char*, const char*, int*, const char*,
			      char*, int*, size_t, size_t, size_t, size_t );
void FORTRAN_NAME(tg_rincw) ( const char*, const char*, const char*, int*, 
			      const char*, int*, char*, int*,
			      size_t, size_t, size_t, size_t, size_t );
void FORTRAN_NAME(in_gskp) ( const char*, int*, int*, int*, int*, int*, int*,
                             int*, int*, size_t );
void FORTRAN_NAME(tg_vi2f) ( const char*, const char*, char*, int*, int*,
                             size_t, size_t, size_t );
void FORTRAN_NAME(gd_rdatw) ( const int*, const char*, const char*, const int*,
                              const int*, const int*, const char*, float*, int*,
			      int*, int*, int*, size_t, size_t, size_t );
void FORTRAN_NAME(tg_cftm) ( const int*, char*, char*, int*, size_t, size_t );
void FORTRAN_NAME(gptvis)  ( const char*, const int*, const float*,
			     const float*, int*, int*, size_t );
void FORTRAN_NAME(gd_gnav) ( const int*, float*, int*, int* );
void FORTRAN_NAME(gd_ganl) ( const int*, float*, int*, int* );
void FORTRAN_NAME(gd_glevw) ( const int*, const char*, const char*,
                	      const int*, const int*, int*,
	        	      int*, int*, int*, size_t, size_t );
void FORTRAN_NAME(gd_wpgdw) ( const int*, const float*, const int*,
                	      const int*, const int*, const char*,
			      const char*, const int*, const int*,
			      const int*, const char*, const int*,
			      const int*, const int*, int*, size_t, size_t,
                              size_t );
void FORTRAN_NAME(gd_wppgw) ( const int*, const int*, const int*, const int*,
                              const int*, const int*, const char*, const char*,
                              const int*, const int*, const int*, const char*,
                              const int*, const int*, const int*, const int*,
                              const float*, const float*, const float*, int*,
                              size_t, size_t, size_t );
void FORTRAN_NAME(gd_gnumw) ( const int*, const char*, const char*,
                	      const int*, const int*, const int*,
			      const char*, int*, int*, size_t, size_t );
void FORTRAN_NAME(lc_floc) ( const char*, float*, float*, int*, size_t );
void FORTRAN_NAME(gsmode) ( const int*, int* );




#define gd_ngrd(iacss,numgrd,ftime,ltime,iret) \
	( FORTRAN_NAME(gd_ngrd)(iacss,numgrd,ftime,ltime,iret,MXFLSZ,MXFLSZ) )

#ifdef IRIX 
#define tg_rnoiw(tbegin,tend,itftyp,sep,timstr,iret) \
	( FORTRAN_NAME(tg_rnoiw)(tbegin,tend,itftyp,sep,timstr,iret,21,21,strlen(sep),strlen(timstr)) )
#else
#define tg_rnoiw(tbegin,tend,itftyp,sep,timstr,iret) \
	( FORTRAN_NAME(tg_rnoiw)(tbegin,tend,itftyp,sep,timstr,iret,strlen(tbegin),strlen(tend),strlen(sep),strlen(timstr)) )
#endif

#define tg_rincw(tbegin,tend,tinc,itftyp,sep,maxchr,timstr,iret) \
	( FORTRAN_NAME(tg_rincw)(tbegin,tend,tinc,itftyp,sep,maxchr,timstr,iret,strlen(tbegin),strlen(tend),strlen(tinc),strlen(sep),*(maxchr)) )
#define gd_gtimw(iacss,maxchr,sep,timstr,iret) \
	( FORTRAN_NAME(gd_gtimw)(iacss,maxchr,sep,timstr,iret,strlen(sep),*(maxchr)) )
#define tg_full(gdattm,firstm,lasttm,temptm,iret) \
	( FORTRAN_NAME(tg_full)(gdattm,firstm,lasttm,temptm,iret,strlen(gdattm),strlen(firstm),strlen(lasttm),MXFLSZ) )
#define in_gskp(ijskip,ix1,ix2,nsx,iy1,iy2,nsy,autos,ier) \
	( FORTRAN_NAME(in_gskp)(ijskip,ix1,ix2,nsx,iy1,iy2,nsy,autos,ier,strlen(ijskip)) )
#define tg_vi2f(vdtm,idtm,fdtm,lnth,iret) \
	( FORTRAN_NAME(tg_vi2f)(vdtm,idtm,fdtm,lnth,iret,strlen(vdtm),strlen(idtm),sizeof(fdtm)) )
#define gd_rdatw(iacss,gdattm1,gdattm2,level1,level2,ivcord,parm,grid,igx,igy,ighdr,iret) \
	( FORTRAN_NAME(gd_rdatw)(iacss,gdattm1,gdattm2,level1,level2,ivcord,parm,grid,igx,igy,ighdr,iret,strlen(gdattm1),strlen(gdattm2),strlen(parm)) )
#define tg_cftm(ifcast,ftype,ftime,iret) \
	( FORTRAN_NAME(tg_cftm)(ifcast,ftype,ftime,iret,sizeof(ftype),sizeof(ftime)) )
#define gptvis(sys,np,x,y,vis,iret) \
	( FORTRAN_NAME(gptvis)(sys,np,x,y,vis,iret,strlen(sys)) )
#define gd_gnav(iacss,rnvblk,navsz,iret) \
	( FORTRAN_NAME(gd_gnav)(iacss,rnvblk,navsz,iret) )
#define gd_ganl(iacss,anlblk,ianlsz,iret) \
	( FORTRAN_NAME(gd_ganl)(iacss,anlblk,ianlsz,iret) )
#define gd_glevw(iacss,gdattm1,gdattm2,ivcord,maxlev,levarr1,levarr2,nlev,iret) \
	( FORTRAN_NAME(gd_glevw)(iacss,gdattm1,gdattm2,ivcord,maxlev,levarr1,levarr2,nlev,iret,strlen(gdattm1),strlen(gdattm2)) )
#define gd_wpgdw(iacss,grid,igx,igy,ighdr,gdattm1,gdattm2,level1,level2,ivcord,parm,rewrit,ipktyp,nbits,iret) \
	( FORTRAN_NAME(gd_wpgdw)(iacss,grid,igx,igy,ighdr,gdattm1,gdattm2,level1,level2,ivcord,parm,rewrit,ipktyp,nbits,iret,strlen(gdattm1),strlen(gdattm2),strlen(parm)) )
#define gd_wppgw(iacss,igrid,lengrd,igx,igy,ighdr,gdattm1,gdattm2,level1,level2,ivcord,parm,rewrit,ipktyp,nbits,misflg,ref,scale,difmin,iret) \
        ( FORTRAN_NAME(gd_wppgw)(iacss,igrid,lengrd,igx,igy,ighdr,gdattm1,gdattm2,level1,level2,ivcord,parm,rewrit,ipktyp,nbits,misflg,ref,scale,difmin,iret,strlen(gdattm1),strlen(gdattm2),strlen(parm)) )
#define gd_gnumw(iacss,gdattm1,gdattm2,level1,level2,ivcord,parm,ignum,iret) \
	( FORTRAN_NAME(gd_gnumw)(iacss,gdattm1,gdattm2,level1,level2,ivcord,parm,ignum,iret,strlen(gdattm1),strlen(gdattm2)) )
#define lc_floc(point,rlat,rlon,iret) \
	( FORTRAN_NAME(lc_floc)(point,rlat,rlon,iret,strlen(point)) )
#define gsmode(mode,iret) \
	( FORTRAN_NAME(gsmode)(mode,iret) )
#define gsgmap FORTRAN_NAME(gsgmap)
#define gsggrf FORTRAN_NAME(gsggrf)


#endif
