/************************************************************************
 * hints.h								*
 *									*
 * This file contains hint string definitions for the NMAP hints	*
 * window.								*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	11/98						*
 * W. Li/EAI		12/98		added  "MMHINT_APPLY"		*
 * E. Safford/GSC	12/98		added  "MMHINT_EXIT"		*
 * W. Li/EAI		02/99		added  "MMHINT_LABEL"		*
 * W. Li/EAI		02/99		added  "OBJ SELECT"		*
 * E. Safford/GSC	06/99		changed OBJ SEL & OBJ CLASS	*
 * J. Wu/SAIC		10/02		add COPY/MOVE for extrapolation	*
 * E. Safford/SAIC	11/03		add CONFIRM_SEL and SMEAR       *
 ***********************************************************************/

#ifndef _nmap_hints_include
#define _nmap_hints_include

/*
 *  Action hint strings:
 */

#define ACHINT_NOACTION	""
#define ACHINT_ADD	"ADD"

/*
 *  Class hint strings:
 */

#define CLHINT_NOCLASS	""


/*
 *  Left mouse hint strings:
 */

#define LMHINT_NOACTION		""
#define LMHINT_MOVEPOINT	"MOVE PT"
#define LMHINT_APPLY		"APPLY"
#define LMHINT_TOGGLE		"TOGGLE"
#define LMHINT_CONFIRM		"CONFIRM"
#define LMHINT_DRAG		"DRAG"
#define LMHINT_FLIP		"FLIP"
#define LMHINT_START		"START"
#define LMHINT_DELETE		"DELETE"
#define LMHINT_ROTATE		"ROTATE"
#define LMHINT_SELECT		"SELECT"
#define LMHINT_PUT		"PUT"
#define LMHINT_NEXT		"NEXT"
#define LMHINT_ERROR		"ERROR"
#define LMHINT_5MORE		"5 MORE"
#define LMHINT_END		"END"
#define LMHINT_PTSELECT		"PT SELECT"
#define LMHINT_CLASSSELECT	"SEL CLS"
#define LMHINT_OBJSELECT	"SEL OBJ"
#define LMHINT_COPY		"COPY"
#define LMHINT_MOVE		"MOVE"
#define LMHINT_CONFIRM_SEL	"CONFIRM SELECTION"


/*
 *  Middle mouse hint strings:
 */

#define MMHINT_NOACTION		""
#define MMHINT_DONE		"DONE"
#define MMHINT_TOSELECTOPER	"RESET"
#define MMHINT_OK		"OK"
#define MMHINT_APPLY		"APPLY"
#define MMHINT_CANCEL		"CANCEL"
#define MMHINT_EXIT  		"EXIT"
#define MMHINT_LABEL  		"LABEL"
#define MMHINT_DESELECT		"DESELECT"
#define MMHINT_SMEAR		"SMEAR"

#endif
