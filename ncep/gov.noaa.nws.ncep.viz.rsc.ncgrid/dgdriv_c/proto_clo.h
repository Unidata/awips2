
/************************************************************************
 * proto_clo.h                                                      	*
 *                                                                      *
 * This include file contains private function prototypes for the       *
 * c files in the CLO library.						*
 *									*
 * Log:									*
 **                                                                     *
 * A. Hardy/GSC 	11/00	Created					*
 * D.W.Plummer/NCEP	01/02	Added clo_bqtag				*
 * A. Hardy/SAIC 	 4/02	Modified prolog description		*
 * B. Yin/SAIC 	 	 7/04	Changed clo_rdstn calling sequences	*
 ***********************************************************************/



#ifndef PROTO_CLO
#define PROTO_CLO


/*
 *  clo prototypes
 */

int  	clo_bqtag ( 	char	*info );

void    clo_rdbnd ( 	Bnd_t	*bnd,
			char	*fnm,
			int	*iret );

void    clo_rdstn ( 	Stn_t	*stn,
			char	*fnm,
			char	*alias,
			int	*iret );

int  	bnd_sort (  	struct binfo_t  *bnd1, 
			struct binfo_t  *bnd2 );

int  	stn_sort ( 	struct sinfo_t  *stn1, 
			struct sinfo_t  *stn2 );

#endif /* PROTO_CLO */
