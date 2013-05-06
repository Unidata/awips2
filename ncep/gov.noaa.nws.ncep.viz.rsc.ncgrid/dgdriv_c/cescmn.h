/************************************************************************
 * cescmn.h  								*
 * Contains the structure for settings of elements that are drawn	*
 * in product generation.						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * F.J. Yen/NCEP	 4/98	Make sett'g tbl nonarray; MAX_SET larger*
 * W. Li/EAI		 7/98	Add txtstr in txt and sptxtstr in spt	*
 * C. Lin/EAI		 9/98	Add smoothing level			*
 * A. Hardy/GSC		12/98	Added CircInfo				* 
 * W. Li/EAI		03/99	Added SymData in union info of setting_t* 
 * S. Law/GSC		03/99	Added filled/closed			*
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * E. Safford/GSC	08/99	increase txtstr from 20 to MAX_TEXT	*
 * F. J. Yen/NCEP	10/99	Changed vg_type from char to signed char*
 * F. J. Yen/NCEP	11/00	Removed reference to CentroidInfo	*
 * H. Zeng/EAI		02/01	Added group type stuff			*
 * H. Zeng/EAI		03/01	Added mapping array for group type table*
 * E. Safford/SAIC	02/02	added new_subtyp			*
 * E. Safford/SAIC	02/02	mv group type structs to cesgtcmn.h	*
 * J. Wu/SAIC		11/02	add ListInfo				*
 * H. Zeng/XTRIA	03/03	added layer_flag			*
 * D.W.Plummer/NCEP	06/03	chgs for ash and volc elements		*
 * J. Wu/SAIC		08/03	add JetType				*
 * J. Wu/SAIC		01/04	add GfaType				*
 * B. Yin/SAIC		02/04	Added TcaType				*
 * J. Wu/SAIC		05/04	add post-processing setting "ppid"	*
 * T. Piper/SAIC	12/05	Major restructuring of Setting_t	*
 * T. Piper/SAIC	12/05	Made set a pointer; defined as Setting_t*
 * T. Piper/SAIC	03/06	Increase length of grp_typ in Setting_t	*
 * B. Yin/SAIC		07/06	Added GFA struct; increased MAX_SET	*
 * L. Hinson/AWC        01/07   Extend GfaAttr structure for text layout*
 * L. Hinson/AWC        06/07   Extend GfaAttr structure for arrow size *
 * E. Safford/SAIC      07/07   increase MAX_SET for new GFA subtypes   *
 ***********************************************************************/
#ifndef CESCMN_H
#define CESCMN_H

#include "vgstruct.h"
#include "pgprm.h"

#define MAX_SET		250
#define DEFLTSET	-99
#define DEFLTPPID	"NONE"

typedef struct
{
    int		linelm;			/* Line or Special Line (1|20) */
    int		lintyp;			/* Line type */
    int		linwid;			/* Line width */
    float       szarrow;                /* Arrow size */
    SpTextInfo  info;                    /* Special Text Info */
    char        textLayout[256];        /* Text Layout String */
} GfaAttr;

typedef struct
{
    int		lincol;			/* Line width */
    int		lintyp;			/* Line type */
    int		filcol;			/* Line width */
    int		filtyp;			/* Line type */
} TceAttr;

typedef struct
{
    int		lincol;			/* Line width */
    int		lintyp;			/* Line type */
} TctAttr;

typedef struct
{
    int		lincol;			/* Line col */
    int		linwid;			/* Line wid */
} TcbAttr;

typedef struct 
{
    int		splcol;			/* Line color */
    SpLineInfo	line;			/* Line attribute info */
    int         nbarb;                  /* Number of barbs */
    BarbAttr    barb[MAX_JETPTS];       /* Wind barb attribute info */
    int         nhash;                  /* Number of hashs */
    HashAttr    hash[MAX_JETPTS];       /* Hash mark attribute info */
} JetInfo;

typedef struct
{
    int		lintyp;
    int		linwid;
} SigAttr;

typedef struct
{
    char	ppid[32];
    SptxType	text;
} SpTxtAttr;

typedef struct
{
    int		width;
    float	size;
} VolAttr;

typedef struct
{
    int		w_type;
    int		w_number;
    int		w_mrktyp;
    int		w_mrkwid;
    float	w_mrksiz;
} WboxAttr;

typedef struct
{
    int		vg_class;
    int		vg_type;
    int         subtyp;
    int		maj_col;
    int		min_col;
    int		closed;
    int		filled;
    int		smooth;
    union {
	int	new_subtyp;
	Boolean layer_flag;
    } cds_or_ces;
    char	grp_typ[24];
    union
    {
	SigAttr		*ash;
	LineInfo	*cir;
	FrontInfo	*frt;
	GfaAttr		*gfa;
	JetInfo		*jet;
	LineInfo	*lin;
	ListInfo	*lst;
	SigAttr		*sig;
        SpLineInfo	*spl;
	SpTxtAttr	*spt;
	SymType		*sym;
	TcaInfo		*tca;
	TceAttr		*tce;
	TctAttr		*tct;
	TcbAttr		*tcb;
	TrackInfo	*trk;
	TextType	*txt;
	VolAttr		*vol;
	WboxAttr        *wbx;
	WindInfo	*wnd;
    } info;
} Setting_t;


#ifdef CES_GLOBAL
	Setting_t		*set;
	int			num_set;
#else
	extern	Setting_t	*set;
	extern	int		num_set;
#endif

#endif /* CESCMN_H */
