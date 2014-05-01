/************************************************************************
 * pgprm.h  								*
 *									*
 * This header file contains the product generation macro definitions	*
 * as well as the nmaplib prototypes.					*
 *                                                                      *
 **                                                                     *
 * Log:									*
 * F.J.Yen/NCEP         01/98	Created					*
 * C.Lin/EAI            04/98	added GRPTYP_OTHERS			*
 * F.J.Yen/NCEP		04/98	added SETTING_TBL			*
 * S. Jacobs/NCEP	05/98	Renamed newdraw.tbl to setting.tbl	*
 * C.Lin/EAI            09/98	added GRPTYP_COMSYM			*
 * E. Safford/GSC	09/98   added ghost (GST_) types & MAXGHOST 	*
 * E. Safford/GSC	09/98	added TYPE_OBJ & TYPE_GRP		*
 * E. Safford/GSC	12/98	moved display levels from cvg routines	*
 * S. Law/GSC		03/99	moved PREV/NEXT from nmap_pgmdfy.c	*
 * S. Law/GSC		03/99	moved MAXCNTY from a couple files	*
 * E. Safford/GSC	12/99	added GRPTYP_WATCH & watch status 	*
 * S. Law/GSC		03/00	added GRPTYP_CCFP			*
 * J. Wu/GSC		11/00	added MAX_GROUP_TYPE for cvgcp.c	*
 * H. Zeng/EAI		11/00	added UNDO constants			*
 * E. Safford/GSC	02/01	added VERTEX_TIEIN & LINE_TIEIN      	*
 * H. Zeng/EAI          03/01   changed MAX_GROUP_TYPE value            *
 * T. Piper/GSC		04/01	increased *_TIEIN from 10 to 20		*
 * J. Wu/SAIC		12/01	added MAX_LAYERS			*
 * E. Safford/SAIC	06/02	added MAX_WFO_LEN			*
 * J. Wu/SAIC		06/02	increased MAX_CNTY from 200 to 400	*
 * E. Safford/SAIC	11/03	moved MAX_LAYERS to gemprm.h 		*
 * T. Lee/SAIC		11/03	changed WORK_FILE to FILE_NAME; defined	*
 *				 global variable work_file		*
 * T. Lee/SAIC		11/03	moved work_file	to cvgcmn.h		*
 * J. Wu/SAIC		10/04   add vgtag.h               		*
 * E. Safford/SAIC	01/05	add MAX_SIG_REF				*
 * J. Wu/SAIC		03/05   add constants for group select		*
 * H. Zeng/SAIC		11/05	increased MAX_WFO_LEN size		*
 * H. Zeng/SAIC		01/06	added struct cwa_incInfo		*
 *				increased MXSTATES to 21		*
 * T. Piper/SAIC	01/08	Moved proto_nmaplib.h from proto.h	*
 ***********************************************************************/
#ifndef PGPRM_H
#define PGPRM_H

#include "vgtag.h"
#include "proto_nmaplib.h"

#define MAX_EDITABLE_ELEMS		5000
#define SETTING_TBL			"setting.tbl"

#define MAX_GROUP_TYPE			256	      /* Max group types in 
                                                         a VGF file         */
#define GRPTYP_OTHERS		        100	
#define GRPTYP_COMSYM		         99	
#define GRPTYP_WATCH 		         98	
#define GRPTYP_CCF 		         97	

#define GST_NORMAL			0
#define GST_NEW_LINE			1
#define GST_INIT_TRNC			2
#define GST_FINAL_TRNC			3
#define GST_BOTH_TRNC			4

#define MAXGHOST			500

#define TYPE_OBJ			0
#define TYPE_GRP			1

#define PREV				0	/* mod direction is backward */
#define NEXT				1	/* mod direction is forward */

#define	MAXCNTY				400	/* Max counties in a watch */
#define MAX_WFO_LEN			(180)	/* Max strlen for a WFO list */

#define MXCHR				16	/* Max chars per element (including NULL) */
#define MXSTATES			21
#define	MXCWAS				30

#define MAX_SIG_REF		       	( 256 )	/* Max len for GFA's sigmet */
						/*     reference text	    */


/*
 *  VG element display levels
 */
#define LEVEL_0                         0
#define LEVEL_1                         1
#define LEVEL_2                         2

#define LEVELS                          3


/*
 *  Watch Issue Status 
 */
#define WATCH_UNISSUED			0
#define WATCH_ISSUED			1
#define WATCH_WITHLINE			2

/*
 *  Tie in distances for element point selection
 */
#define	VERTEX_TIEIN			( 20.0F )
#define	LINE_TIEIN			( 20.0F )

/*
 * UNDO constants
 */
#define UNDO_DEL                        0
#define UNDO_ADD                        1

/*
 * Group select (pghdlb_selectAll) constants
 */
#define NON_GRP_ELMS                    (0)
#define GRP_AND_NON_GRP_ELMS            (1)
#define GRP_ELMS            		(2)

typedef struct incInfo
{
    Boolean     include;
    char        name[MXCHR];
    Widget      wid;
} IncInfo;

typedef struct cwa_incInfo
{
    int         state;
    char        name[MXCHR];
    Widget      form_wid;
    Widget	lbl_wid;
    Widget	in_btn;
    Widget	out_btn;
} CwaIncInfo;

IncInfo*    pgwlst_getStateBtns (  void);
CwaIncInfo* pgwlst_getCwaBtns   (  void);

#endif
