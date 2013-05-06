/************************************************************************
 * xwcmn.h								*
 *									*
 * This include file saves global variables for the XW device driver.	*
 *									*
 **									*
 * Log:									*
 * J. Whistler/NMC 	 7/91						*
 * M. desJardins/NMC	12/91	GEMPAK 5.1 version			*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window & multi-pixmap & cleanup	*
 * M. desJardins/NMC	10/94	Change font size vars from int to float	*
 * C. Lin/EAI	         1/95	Add RADpixels for radar image		*
 *				add definitions of GRAPH_COLORS,        *
 *				SAT_COLORS, RADAR_COLORS		*
 * C. Lin/EAI            2/95   add ATOM_NAME				*
 *				add color bank structure clrbank_t	*
 *				add Color Bank IDs			*
 *				take out app_context in window structure*
 * C. Lin/EAI		 4/95	Increased MAX_PIXMAP from 30 to 50	*
 * G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
 * C. Lin/EAI		12/95	clrsalloc -> allocflag[]		*
 * C. Lin/EAI		 6/97	add pixmap size to window structure	*
 *			        add roam flag, copy area info		*
 * E. Wehner/EAi	 9/97 	Add pixmaps and refresh count to struct	*
 * S. Jacobs/NCEP	11/97	Added kjust				*
 * S. Jacobs/NCEP	 3/98	Added tsfill, kfillt			*
 * C. Lin/EAI		 4/98	move some constants to xwprm.h		*
 * S. Jacobs/NCEP	 7/98	Added txszx and txszy			*
 * E. Safford/GSC	12/98	Add xw_refresh to Window_str            *
 * E. Safford/GSC	02/99	Add incr_pxmCnt flag			*
 * E. Safford/GSC	10/99	Add nmap2 flag      			*
 * S. Law/GSC		10/99	Changed roamflg to an array		*
 * E. Safford/GSC	12/99	Add pxms, mstr, & xw_rfrsh arrays	*
 * S. Law/GSC		01/00	Changed curpxm to an array		*
 * E. Safford/GSC	05/00	Add bad_frm booleans 			*
 * E. Safford/GSC	05/00	move MAX_LOOP to xwprm.h		*
 * S. Law/GSC		06/00	mstr array only for current loop	*
 * T. Piper/SAIC	10/01	Removed unneeded X11 include files	*
 * J. Wu/SAIC		12/01	add _pgLayer				*
 * R. Tian/SAIC		05/02	Added variables for fax image		*
 * S. Jacobs/NCEP	 9/02	Increased MAXCOLORDATA from 512 to 2048	*
 * T. Piper/SAIC	07/03	moved *Cid to xwprm.h			*
 * T. Piper/SAIC	01/04	Removed NNCOLR				*
 * T. Piper/SAIC	01/04	Moved MAXCOLORS & MAXCOLORDATA to 	*
 *								color.h	*
 * T. Piper/SAIC	07/04	Moved COLR_SCAL to color.h		*
 * E. Safford/SAIC	12/07	add proto_xw.h to includes		*
 ***********************************************************************/

#ifndef XWCMN_H
#define XWCMN_H

#include "geminc.h"
#include "gemprm.h"
#include "xwprm.h"
#include <X11/cursorfont.h>
#include "proto_xw.h"

/*
 * atom names for shared color scheme
 */
#define ShareColorFlag "NXM_SHARED_COLOR_FLAG"
#define ShareColorData "NXM_SHARE_COLOR_DATA"

/*
 * shared color constants
 */
#define SHARECOLOR	  1
#define NSHARECOLOR	  2

#define ATOM_NAME	256	/* maximum atom name length */

#define FNAME_LEN	256	/* maximum file name length */	

/*
 * color bank structure
 */
typedef struct {
    int			nbank;		/* # of color banks */
    int			*banks;		/* # of colors in each bank */
    Pixel		**colrs;	/* color indices for each bank */
} colrbank_t;

/*
 * font structure
 */
typedef struct {
    char    name[90];  /* font name */
    Font    id;        /* font id */
} Font_str;

typedef struct {
    int		pxm_wdth;	/* pixmap width ('D' coord xsize) */
    int		pxm_hght;	/* pixmap height ('D' coord ysize) */
    int		pxm_x;		/* pixmap source upper left x */
    int		pxm_y;		/* pixmap source upper left y */ 
    int		xoffset;	/* xoffset and yoffset for window */
    int		yoffset;
    char	roamflg;	/* 0 = roam off, 1 = roam on */
} winloop_t;

/*
 * window data structure
 *   In nmap curr_loop is initialized to 0 in nmap.c and never touched again.
 */
typedef struct  {
    Window	window;			/* window ID */
    char	name[WNAME_LEN];	/* window name */
    GC		gc;			/* graphic context */
    int		width;			/* window width ('S' coord xsize) */   
    int		height;			/* window height ('S' coord ysize) */
    int		depth;
    int		npxms;			/* total # of pixmaps in this window */
    int		curpxm[MAX_LOOP];	/* current pixmap */
    int		win_x;			/* destination window upper left x */
    int		win_y;			/* destination window upper left y */
    int		area_w;			/* copy area width */
    int		area_h;			/* copy area height */
    int		curr_loop;		/* current loop */
    Boolean	incr_pxmCnt;		/* flag for incr pixmap cnt */
    Boolean	nmap2;			/* flag for nmap2 flag*/
    winloop_t	loop[MAX_LOOP];		/* information that varies by loop */
    Pixmap	pxms    [MAX_LOOP][MAX_PIXMAP];	/* pixmaps */
    Pixmap	mstr    [MAX_PIXMAP];		/* master pixmaps */
    char	xw_rfrsh[MAX_LOOP][MAX_PIXMAP];	/* refresh flags */
    Boolean	bad_frm [MAX_LOOP][MAX_PIXMAP];	/* bad frame tags */

} Window_str;

/*---------------------------------------------------------------------*/

#define MAX_LOAD_FONTS	(    10 )

/*---------------------------------------------------------------------*/

#ifdef XWCMN_GLOBAL

	unsigned	line_width;
	int		line_style, cap_style, join_style;
				/* Line attributes */

	int		ibkcol;
	int		ifrcol;
				/* Background and foreground colors */

	int		txfont_req, txfont_set;
				/* Text font requested, set */
	float		txsize_req, txsize_set;
				/* Text size requested, set */

        char    	 allocflag[4];     
				/* allocation flag for each color bank */

	Display		*gemdisplay; 
				/* server ID */	
	Window		root;        
				/* root window ID */

	Colormap	gemmap;	 
				/* shared color map ID */
	Visual		*gemvis; 
				/* visual element of the display */

	Window_str      gemwindow[MAX_WINDOW];
	int             current_window;

	char            LOOP; 
				/* 1 -- looping, 0 -- otherwise */

	int		dwell_rate; 
	float           dwell[5];

	int 		GColorIsInitialized;
	colrbank_t 	ColorBanks;

	char		_pgpalwIsUp = FALSE;
	int		_pgLayer = 0;   /* current PGEN layer */

#else

	extern unsigned		line_width;
	extern int		line_style, cap_style, join_style;

	extern int		ibkcol;
	extern int		ifrcol;

        extern char    	 	allocflag[];     

	extern int		txfont_req, txfont_set;
	extern float		txsize_req, txsize_set;

	extern Display		*gemdisplay;	
	extern Window		root;

	extern Colormap		gemmap;	
	extern Visual		*gemvis;

	extern Window_str       gemwindow[];
	extern int              current_window;

	extern char             LOOP;

	extern int		dwell_rate; 
	extern float            dwell[];

	extern int 		GColorIsInitialized;
	extern colrbank_t 	ColorBanks;

	extern char		_pgpalwIsUp;
	extern int		_pgLayer;

#endif

#include "xwpcmn.h"

#endif  /* XWCMN_H */
