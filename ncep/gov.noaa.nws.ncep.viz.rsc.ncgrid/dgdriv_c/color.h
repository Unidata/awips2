/************************************************************************
 * color.h								*
 *									*
 * This header file defines the common structures shared by different   *
 * drivers.								*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI 	 1/96							*
 * T. Piper/SAIC	01/04	Added color bank IDs, from xwprm.h	*
 * T. Piper/SAIC	01/04	Added MAXCOLORS & MAXCOLORDATA		*
 * T. Piper/SAIC	07/04	Added CLR_DIR, CLR_TBL, COLR_SCAL	*
 ***********************************************************************/

#ifndef COLOR_H
#define COLOR_H

#define MAX_GEMCTAB	500	 

/*
 *  Color bank IDs
 */
#define GraphCid        0
#define SatCid          1
#define RadCid          2
#define FaxCid          3

/*
 *  Number of default colors per color bank
 */
#define GRAPH_COLORS 33
#define SAT_COLORS   95
#define RAD_COLORS   20
#define FAX_COLORS    2

/*
 *  Maximum colors in a single color bank
 */
#define MAXCOLORS   256

/*
 *  Maximum color data that can be stored in server
 */
#define MAXCOLORDATA    2048

/*
 *  Color scale between X's 16-bit, 0-65535) RGB scale
 *	and GEMPAK/NAWIPS' 8-bit, 0-255) RGB scale
 */
#define	COLR_SCAL	256

/*
 *  Set defaults for location and name of GEMPAK graphics colors
 */
#define CLR_DIR		"colors"
#define CLR_TBL		"coltbl.xwp"

/*
 * GEMPAK color map element
 */
typedef struct {
        char 		 gcname[16];     /* GEMPAK color name */
        char 		 abvname[4];     /* abbreviated color name */
        int    		 red;            /* red color component */
        int    		 green;          /* green color component */
        int    		 blue;           /* blue color component */
        char 		 xname[40];      /* corresponding X name */
} GmcColor;


/*
 * GEMPAK color map structure
 */
typedef struct {
	GmcColor	color[MAX_GEMCTAB];
	int		nc;
} GemCmap;



/*
 * structure for colors currently used in driver 
 */
typedef struct {
        int    		 index;          /* index to the GEMPAK color table */
        char 		 name[40];       /* a name used for this color*/
        int    		 red;            /* red color component */
        int    		 green;          /* green color component */
        int    		 blue;           /* blue color component */
} GemColor;

extern GemCmap	gemCmap; 		/* GEMPAK color table */ 
extern GemColor	gemColrs[];	/* currently selected graphic colors */
extern int 	ngemC;	/* number of selected graphic colors */

#endif  /* COLOR_H */
