/************************************************************************
 * proto_bridge.h							*
 *									*
 * This file contains header files and global variables for use in the	*
 * decoder bridge routines.						*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC          1/01   Added dc, dcb prototypes		*
 * m.gamazaychikov/SAIC	07/05	Changed dc_gopt prototype		*
 * H. Zeng/SAIC		08/05	Added second station table		*
 * m.gamazaychikov/SAIC	03/08	Added ta_dcod				*
 * m.gamazaychikov/SAIC 07/08   Modified ta_dcod                        *
 * L. Hinson/AWC        06/08   Added crcflag to dc_gopt prototype      *
 ***********************************************************************/

#ifndef PROTO_BRIDGE
#define	PROTO_BRIDGE

/*
 *  dc prototypes
 */
#ifdef Linux
void 	dc_exit ( 	int	*iret ) __attribute__((noreturn));
#else
void	dc_exit (       int     *iret );
#endif

void 	dc_gopt ( 	char	*defprm,
			char	*defstn,
			char	*dfstn2,
			int	idfadd,
			int	idfmax,
			int	ndfhr1,
			int	ndfhr2,
                        int     idfwdh,
			char	cprmfl[],
			char	cstntb[],
			char	cstnt2[],
			int	*nadstn,
			int	*ntimes,
			char	ccrtim[],
			int	*nhrbak,
			int	*txflag,
                        int     *crcflag,
                        int     *jwndht,
			int	*iret );

void 	dc_init ( 	char	*prgnam,
			int	argc,
			char 	**argv,
			int 	numexp,
			char	parms[][DCMXLN],
			int	*num,
			int	*iret );

void	ig_ascii (	char	*filnam,
			int     irptdt[5],
			float	*lat,
			float	*lon,
			float	*sgwh,
			int	*iret );

void	ig_dcod (	char	*cldt,
			char	*bufrta,
			char	*bufrtn,
			int	*nhours,
			int	*iret,
			size_t,
			size_t,
			size_t );

void	ta_dcod (	char	*cldt,
			char	*bufrta,
			char	*gemfil,
			int	*nhours,
                        int     *iret,
                        size_t,
                        size_t,
                        size_t );

#endif	/* PROTO_BRIDGE */
