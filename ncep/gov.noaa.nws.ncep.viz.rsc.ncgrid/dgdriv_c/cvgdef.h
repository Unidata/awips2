/************************************************************************
 * cvgdef                                                               *
 *                                                                      *
 * This header file contains the declaration and structures for 	*
 * the group number transformation index used by cvg_ggmtrx.            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/SAIC	04/02	Initial coding                          *
 *  E. Safford/SAIC	04/02	correct cvg_gmtrx & cvg_gfrmtrx names   *
 ***********************************************************************/
#ifndef	CVGDEF_H
#define	CVGDEF_H


typedef struct cvg_mtrx
{
	char	grptyp;
	int	*grpin;
	int	*grpout;
	int	numgrps;
} CVG_mtrx_t;


/* 
 *  cvg prototypes using these defined structs
 */

void	cvg_gmtrx ( 	char		*fname, 
			int 		*numtyps, 
			CVG_mtrx_t 	**matrix, 
			int 		*iret );

void	cvg_gfrmtrx (	CVG_mtrx_t	*matrix,
			int		*size );


#endif
