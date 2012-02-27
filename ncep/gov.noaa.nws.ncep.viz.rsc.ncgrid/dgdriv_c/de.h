/************************************************************************
 * de.h                                                                 *
 *                                                                      *
 * This include file contains the prototypes of the DE library.		*
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC          2/06   Created                                 *
 * M. Li/SAIC		10/06	Added de_cprb and de_cval		*
 * m.gamazaychikov/SAIC	01/08	Added de_swsprd and de_vwsprd		*
 ***********************************************************************/

#ifndef DE_H_
#define DE_H_

/*
 * prototypes.
 */

void de_driv ( char *efunc, int *iret );
void de_init ( int *iret );
void de_mbr1 ( const int *k, const char *infl, char *outfl, int *iret );
void de_pfpn ( const char *argu, int ifpn[], int *nfil, int *iret );
void de_scan ( const int *nina, int *iret );
void de_save ( const int *indx, int *iret );
void de_mset ( const int *imem, int *iret );
void de_rset ( int *iret );
void de_savg ( const char *uarg, char *stprm, int *iret );
void de_vavg ( const char *uarg, char *stprm, int *iret );
void de_ssprd ( const char *uarg, char *stprm, int *iret );
void de_vsprd ( const char *uarg, char *stprm, int *iret );
void de_smax ( const char *uarg, char *stprm, int *iret );
void de_smin ( const char *uarg, char *stprm, int *iret );
void de_srng ( const char *uarg, char *stprm, int *iret );
void de_prcntl ( const char *uarg, char *stprm, int *iret );
void de_mode ( const char *uarg, char *stprm, int *iret );
void de_cprb ( const char *uarg, char *stprm, int *iret );
void de_cval ( const char *uarg, char *stprm, int *iret );
void de_swsprd ( const char *uarg, char *stprm, int *iret );
void de_vwsprd ( const char *uarg, char *stprm, int *iret );

#endif
