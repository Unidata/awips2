/************************************************************************
 * nmsdef.h								*
 *									*
 * This file contains structure definitions used when reading the MISC	*
 * data type attribute settings.					*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	11/99	Created					*
 * S. Jacobs/NCEP	 1/00	Updated structure definitions		*
 * S. Jacobs/NCEP	 3/00	Removed line.ityp, arrw.ityp;		*
 *				Added ionoff				*
 * M. Li/SAIC		 4/03	Add icolr2 to NMS_types			*
 * F. J. Yen/NCEP	 6/04	Added arrw.ityp				*
 ***********************************************************************/

#ifndef _NMSDEF

#define _NMSDEF

typedef struct nmsline
{
    float	size;
    int		iwid;
} NMS_line;

typedef struct nmssymb
{
    float	code;
    float	size;
    int		iwid;
} NMS_symb;

typedef struct nmsarrw
{
    float	size;
    float	hdsz;
    int		iwid;
    int		ityp;
} NMS_arrw;

typedef struct nmstype
{
    char	name[LLMXLN];
    int		ionoff;
    int		icolr;
    int		icolr2;
    float	value;
    NMS_line	line;
    NMS_symb	symb[2];
    NMS_arrw	arrw;
} NMS_types;

typedef struct nmsflag
{
    char	name[LLMXLN];
    int		iflg;
} NMS_flags;

/*
 *  nms prototypes
 */

void 	nms_ghrn ( 	int	mxstrm,
			char    storms[][LLPATH],
			int     *num,
			int     *iret );

void 	nms_init ( 	int	*iret );

void 	nms_qatt ( 	int	indx,
			char	alias[],
			int	*isbcat,
			char	filnam[],
			int	*ntype,
			NMS_types   types[],
			int	*nflag,
			NMS_flags   flags[],
			int	*iret );

void 	nms_rtbl ( 	char	alias[],
			int	*ntype,
			NMS_types   types[],
			int	*nflag,
			NMS_flags   flags[],
			int	*iret );


void 	nms_satt ( 	int	iindex,
			char	alias[],
			int	isbcat,
			char	filnam[],
			int	ntype,
			NMS_types   types[],
			int	nflag,
			NMS_flags   flags[],
			int	*jindex,
			int	*iret );


#endif
