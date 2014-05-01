#ifndef DATA_H
#define DATA_H

 /***********************************************************************
 * nmap_data.h                                                          *
 *                                                                      *
 * This file contains structure definitions used by NMAP to display     *
 * data overlays.                                                       *
 *                                                                      *
 **                                                                     *
 *  E. Safford/GSC	12/98	add one 1 more pixmap to framesInfo_t	*
 *  S. Law/GSC		12/98	added MAX_STRLEN and use of dttm_t	*
 *  E. Safford/GSC	05/99	mod to load mdl data without using      *
 *				  static structures in nmap_mdl.c       *
 *  E. Safford/GSC	09/99	add nmap2 data structures		*
 *  S. Law/GSC		10/99	_WWN -> _MSC				*
 *  E. Safford/GSC	10/99	add pmidx to frmtm_t     		*
 *  H. Zeng/EAI         11/99   add save_roam_val for loopstate_t       *
 *  E. Safford/GSC	01/00	add data changed flag to loopstate_t	*
 *  E. Safford/GSC	03/00	add ref time values to dominfo_t    	*
 *  E. Safford/GSC	05/00	remove MAX_LOOP & MAX_SOURCE		*
 *  S. Law/GSC		07/00	dsrc.path MAX_STRLEN -> FILE_FULLSZ	*
 *  T. Lee/GSC		02/01	added single time flag to loopstate_t	*
 *  T. Lee/GSC		03/01	increased max frame source from 10 to 20*
 *  E. Safford/GSC	04/01	add roam settings defines    	 	*
 *  S. Jacobs/NCEP	 4/01	Increased MAXDSRC_OF_DCAT from 20 to 40	*
 *  M. Li/GSC		06/01	add fade_ratio to loopstate_t	 	*
 *  M. Li/GSC		06/01	added src_on to dsrc_t 			*
 *  J. Wu/GSC		06/01	add RESTORE_SPF/SAVE_SPF	 	*
 *  H. Zeng/EAI         11/01   changed MAX_FRAME value                 *
 ***********************************************************************/

#include "xwprm.h"	/* MAX_PIXMAP */
#include "nmap_dttm.h"	/* dttm_t */

#define DATA_NIL	0
#define DATA_IMG	1
#define DATA_MOS	2
#define DATA_OBS	3
#define DATA_MOD	4
#define DATA_VGF	5
#define DATA_MSC	6

#define NEW_SOURCE	0
#define	SRC_STATE	1
#define MOD_SOURCE	2

#define MAX_PIX		(MAX_PIXMAP - 1)
#define MAX_FRAME	(MAX_PIXMAP - MAX_LOOP)
#define MAX_PANEL       4
#define MAX_FRMSRC	20

/*
 *  Panel locations  (see panel.hl2)
 */
#define PANEL_ALL	"ALL"		/* full window */
#define PANEL_UL	"UL"		/* upper left  */
#define PANEL_UR	"UR"		/* upper right */
#define PANEL_LL	"LL"		/* lower left  */
#define PANEL_LR	"LR"		/* lower right */
#define PANEL_L 	"L"		/* left half   */
#define PANEL_R 	"R"		/* right half  */
#define PANEL_T 	"T"		/* top half    */
#define PANEL_B 	"B"		/* bottom half */

/*
 *  Fixed roam settings
 */
#define SIZE_OF_SCREEN	( 0 )		/* no roam --> fit to screen */
#define SIZE_OF_IMAGE	( 1 )		/* roam to size of image     */


#define MAX_STRLEN	256		/* maximum string length */

/*
 *  Restore/Save data settings 
 */
#define RESTORE_SPF	0		/* Restore settings from an SPF */
#define SAVE_SPF	1		/* Save settings to an SPF */


typedef struct {
    dattm_t	ftime;			/* time of data		    */
    Boolean	selected;		/* flag for time selection  */
    int		pmidx;			/* pixmap index		    */
} frmtm_t;

typedef struct {
    int		catg;			/* category of data         */
    char	path[FILE_FULLSZ];	/* path to data             */
    int		attridx;		/* driver attribute index   */
    int		num_sel;		/* number of selected times */
    int		skip;			/* skip factor              */
    frmtm_t	frm[MAX_FRAME];		/* array of frame times     */
    Boolean	src_on;			/* source state		    */
} dsrc_t;

typedef struct {
    dsrc_t	*src;			/* pointer to the dom source */
    Boolean	use_refTm;		/* control flag for ref time */ 
    dttmi_t	ref_tm;			/* reference time for dom source */
} dominfo_t;



typedef struct {			/* loop state information      */
    int		npxm; 			/* number of pixmaps           */
    int		fst_pxm;		/* first pixmap index          */
    int		lst_pxm;		/* last pixmap index           */
    int		roam_val;		/* roam value for loop         */
    int         save_roam_val;          /* copy of roam value for loop */
    float	fade_ratio;		/* fade ratio		       */
    Boolean	tm_mode;		/* single time flag for loop   */
    Boolean	save_tm_mode;		/* copy of single time flag    */
    Boolean	auto_updt;		/* state of auto-update        */
    Boolean	data_chngd;		/* data changed flag           */
} loopstate_t;



#define MAXDSRC_OF_DCAT  40	/* maximum data source in one category */
#define MAX_OVL_LAYER	  6	/* maximum overlay layers */


/*
 *  frame type holds the data needed to build each of the data frames
 *  in nmap.  It contains information specific to the frame (pixmap 
 *  index and the date/time of the frame.
 */
typedef struct {
    int		ipxm;  			/* pixmap index */
    dattm_t	frmdttm;		/* GEMPAK dttm of each frame */
}frame_t;		

typedef struct {
    loopstate_t lp;
    frame_t	frames[MAX_PIXMAP + 1];
}loopInfo_t;		

#include "proto_nmap2.h"

#endif	/* DATA_H */
