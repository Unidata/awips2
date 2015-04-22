
/************************************************************************
 * proto_cas.h                                                      	*
 *                                                                      *
 * This include file contains function prototypes for the CAS files   	*
 * in the cgemlib CAS library.						* 
 *									*
 **                                                                     *
 * A. Hardy/GSC		11/01   Created					*
 * A. Hardy/GSC		 2/02   Added cas_rd*				*
 * A. Hardy/GSC		 2/02   Added variable siglvl to cas_wrhdr	*
 * M. Li/SAIC		 5/04	Added cas_wrmhdr			*
 * M. li/SAIC		 8/04	Added cas_wrmcld			*
 * M. Li/SAIC		 9/04	Added chlvl to cas_wrfrt, cas_wrjets,	*
 *				             cas_wrtrop, and cas_wrturb	*
 * M. Li/SAIC		 9/04	Added cas_rdmcld. added idcent, chbase,	*
 *			        and chtop to cas_rdhdr			*
 ***********************************************************************/


#ifndef PROTO_CAS
#define PROTO_CAS

void  cas_clos (  FILE      *fptr,
                  int       *iret );

FILE *cas_open (  char      *ofname,
                  Boolean   readflg,
                  int       *iret );

void cas_rdcld (  FILE 	    *ifpout, 
                  int 	    *numele, 
		  cloud_t   **ptrc,
		  int       *memalc,
                  int       *iret);

void  cas_rdhdr ( FILE 	    *ifpout, 
		  int       itime[], 
		  int       jtime[], 
		  int	    *idcent,
		  float	    *chbase,
		  float	    *chtop,
		  int       *iret);

void cas_rdfrt (  FILE 	    *ifpout, 
                  int 	    *numele, 
		  front_t   **ptrf,
		  int       *memalc,
                  int       *iret);

void cas_rdjets ( FILE 	    *ifpout, 
                  int 	    *numele, 
		  jets_t    **ptrj,
		  int       *memalc,
                  int       *iret);

void cas_rdmcld ( FILE      *ifpout,
                  int       *numele,
                  mcloud_t  **ptrm,
                  int       *memalc,
                  int       *iret);

void cas_rdstm (  FILE 	    *ifpout, 
                  int 	    *numele, 
		  storm_t   **ptrs,
		  int       *memalc,
                  int       *iret);

void cas_rdtrop ( FILE 	    *ifpout, 
                  int 	    *numele, 
		  trop_t    **ptrr,
		  int       *membx,
		  int       *rnum,
		  troplo_t  **ptrl,
		  int       *memlo,
		  int       *lnum,
		  trophi_t  **ptrh,
		  int       *memhi,
		  int       *hnum,
                  int       *iret);

void cas_rdturb ( FILE 	    *ifpout, 
                  int 	    *numele, 
		  turb_t    **ptrb,
		  int       *memalc,
                  int       *iret);

void cas_rdvlrd ( FILE 	    *ifpout, 
                  int 	    *numele, 
		  volrad_t  **ptrv,
		  int       *memalc,
                  int       *iret);

void cas_wrcld ( FILE	    *ifpout, 
		 cloud_t    *ptr,
		 int        numclds,
		 int        *iret );

void cas_wrfrt ( FILE	    *ifpout, 
		 front_t    *ptr,
		 int        numfrt,
		 char	    *chlvl,
		 int        *iret );

void cas_wrhdr ( FILE       *ifpout, 
                 int        idtarr[],
                 int        jdtarr[],
                 char       *siglvl,
                 int        *iret );

void cas_wrjets ( FILE	    *ifpout, 
		  jets_t    *ptr,
		  int       numjet,
		  char	    *chlvl,
		  int       *iret );

void cas_wrmcld ( FILE       *ifpout,
		  mcloud_t   *ptr,
		  int        nummcld,
		  int        *iret );

void cas_wrmhdr ( FILE      *ifpout,
                  char      *dattim,
                  char      *fhour,
                  char      *fxytbl,
                  char      *center,
                  int       *iret);

void cas_wrstm ( FILE	    *ifpout, 
                 storm_t    *ptr,
                 int        numstm,
                 char       *symtyp,
                 int        *iret );

void cas_wrtrop ( FILE       *ifpout, 
                  trop_t     *ptr,
                  int        numbx,
                  trophi_t   *ptrh,
                  int        numhi,
                  troplo_t   *ptrl,
                  int        numlo,
		  char	     *chlvl,
                  int        *iret );

void cas_wrturb ( FILE	    *ifpout,
                  turb_t    *ptr,
                  int       numturb,
		  char	    *chlvl,
                  int       *iret );

void cas_wrvlrd ( FILE	    *ifpout, 
                 volrad_t   *ptr,
                 int        numstm,
                 char       *symtyp,
                 int        *iret );

#endif /* PROTO_CAS */
