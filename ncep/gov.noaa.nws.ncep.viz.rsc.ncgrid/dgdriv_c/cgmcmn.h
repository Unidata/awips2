/************************************************************************
 * CGMCMN.H								*
 *									*
 * The file defines global parameters and structures associated	with 	*
 * the metafile.							*
 *									*
 * This file also defines the codes used in the CGM (Computer Graphics	*
 * Metafile) which identify the drawable items and their attributes, 	*
 * and the file and frame boundaries.					*
 *									*
 * The format of the metafile is described below:			*
 *									*
 * FILE HEADER: 	80 bytes					*
 *	Title		32 bytes					*
 *	Max. frame	 2 bytes					*
 *	Version		 2 bytes					*
 *	Machine type	 2 bytes					*
 *	Reserved	42 bytes					*
 *									*
 * FRAME HEADER #1:	72 bytes \					*
 *	Label title	64 bytes  \					*
 *	Start position	 4 bytes   \					*
 *	End position	 4 bytes    \					*
 *			              Total = 72 x N 			*
 * 	...			    /					*
 *	...			   /					*
 *				  /					*
 * FRAME HEADER #N:	72 bytes /					*
 *	...								*
 *									*
 * BEG_MF      -- Beginning of metafile					*
 * BEG_PIC     \							*
 * BEG_PICBODY	\							*
 * ...		  Frame #1						*
 * ...		/							*
 * END_PIC     /							*
 *									*
 * ...									*
 *									*
 * BEG_PIC     \							*
 * BEG_PICBODY	\							*
 * ...		  Frame #N						*
 * ...		/							*
 * END_PIC     /							*
 * END_MF      -- End of metafile					*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAI		 3/94                                          	*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * R. Tian/SAIC		 4/02		Added fxsize and fysize		*
 ***********************************************************************/

/* Global parameters and structures */

#define NC_TITLE	"NTRANS METAFILE VERSION1.0"
				/* Metafile title */
#define MAX_TWOBT	0x7fff
				/* Maximum 2-byte positive number */
#define	MAX_NFRM	500
				/* Maximum number of frames */
#define MAX_BUFSZ	8192
				/* Maximum buffer size for output*/
#define NUM_BTSZ	2
				/* Byte size of the output values */
#define FIL_HDRSZ	80
				/* File header size in bytes */
#define FIL_TTLSZ	32
				/* File title size in bytes */
#define FIL_RESSZ	( FIL_HDRSZ - FIL_TTLSZ - ( 5 * NUM_BTSZ ) )
				/* Reserved file header space size
				   in bytes */
#define	FRM_HDRSZ	72
				/* Frame header size in bytes */
#define	FRM_LBLSZ 	64	
				/* Frame label size in bytes */

#define MAX_FNLEN	81
				/* Maximum file name length */

#define XY_SCALE	0x7fff
				/* Maximum x,y values */
#define COLR_SCALE	0x00ff
				/* Maximum RGB color values */

#ifndef HEADER_DEFINED
#define HEADER_DEFINED

struct nc_filhdr {
        char            title[FIL_TTLSZ];
				/* File title */
        unsigned short  maxframe;
				/* Maximum number of frames */
        unsigned short  version;
				/* Version number */
        unsigned short  machtype;
				/* Machine type */
	unsigned short	fxsize;
				/* Frame size in X direction */
	unsigned short 	fysize;
				/* Frame size in Y direction */
        char            reserved[FIL_RESSZ];
				/* Reserved space */
}; 
typedef struct nc_filhdr nc_file_header;

struct nc_frmhdr {
        char            label[FRM_LBLSZ];
				/* Frame label */
        unsigned        isbyte;
				/* Frame start byte */
        unsigned        iebyte;       
				/* Frame end byte */
}; 
typedef struct nc_frmhdr nc_frame_header;

#endif				/* HEADER_DEFINED */

/*---------------------------------------------------------------------*/

/* CGM codes */

#define NOOP	 	(short) 0x0000	 /* length always 0 */
#define BEG_MF  	(short) 0x0020	 /* length always 0 */
#define END_MF  	(short) 0x0040	 /* length always 0 */
#define BEG_PIC 	(short) 0x0060	 /* length always 0 */
#define BEG_PICBODY 	(short) 0x0080	 /* length always 0 */
#define END_PIC 	(short) 0x00a0	 /* length always 0 */

#define VDC_TYPE 	(short) 0x1060
#define INT_PREC 	(short) 0x1080
#define REAL_PREC 	(short) 0x10a0
#define INX_PREC 	(short) 0x10c0
#define COL_PREC 	(short) 0x10e0
#define COL_INX_PREC 	(short) 0x1100
#define MAX_COL_INDX 	(short) 0x1120
#define COL_VAL_EXT 	(short) 0x1140
#define ELEMT_LIST 	(short) 0x1160
#define BEG_DEFAULTS 	(short) 0x1180
#define FONT_LIST 	(short) 0x11a0
#define CHARSET_LIST 	(short) 0x11c0
#define CHAR_CODE 	(short) 0x11e0

#define SCALE_MODE 	(short) 0x2020
#define COL_MODE 	(short) 0x2040
#define LN_WIDTH_MODE 	(short) 0x2060
#define MK_SIZE_MODE 	(short) 0x2080
#define EDG_WIDTH_MODE 	(short) 0x20a0
#define VDC_EXT 	(short) 0x20c0
#define BG_COLR 	(short) 0x20e0

#define VDC_INT_PREC 	(short) 0x3020
#define VDC_REAL_PREC 	(short) 0x3040
#define AUX_COLOR 	(short) 0x3060
#define TRANSPARENCY 	(short) 0x3080
#define CLIP_RECT 	(short) 0x30a0   /* length always 8 */
#define CGM_CLIP 	(short) 0x30c0   /* length always 2 */

#define LINE		(short) 0x4020   /* long form possible */
#define DISJT_LINE	(short) 0x4040   
#define MARKER		(short) 0x4060   /* long form possible */  
#define TEXT		(short) 0x4080   /* long form possible */  
#define RESTR_TEXT	(short) 0x40a0     
#define APPEND_TEXT	(short) 0x40c0     
#define POLYGON		(short) 0x40e0   /* long form possible */  
#define POLYGON_SET	(short) 0x4100     
#define CELL_ARRAY	(short) 0x4120     
#define GDP		(short) 0x4140     
#define RECT		(short) 0x4160     
#define CIRCLE		(short) 0x4180     
#define ARC_3PT		(short) 0x41a0     
#define ARC_3PT_CLS	(short) 0x41c0     
#define ARC_CENTR	(short) 0x41e0     
#define ARC_CTR_CLS	(short) 0x4200     
#define ELLIPSE		(short) 0x4220     
#define ELLIP_ARC	(short) 0x4240     
#define ELLIP_ARC_CLS	(short) 0x4260     

#define LINE_INDX	(short) 0x5020     
#define LINE_TYPE	(short) 0x5040    /* length always 2 */    
#define LINE_WIDTH	(short) 0x5060        
#define LINE_COLOR	(short) 0x5080    /* length always 2 */    
#define MARK_INDX	(short) 0x50a0       
#define MARK_TYPE	(short) 0x50c0    /* length always 2 */
#define MARK_SIZE	(short) 0x50e0       
#define MARK_COLOR	(short) 0x5100    /* length always 2 */
#define TEXT_INDX	(short) 0x5120    
#define TEXT_FONT_INX	(short) 0x5140    
#define TEXT_PREC	(short) 0x5160    
#define CHAR_EXP	(short) 0x5180    
#define CHAR_SPACE	(short) 0x51a0    
#define TEXT_COLOR	(short) 0x51c0    /* length always 2 */
#define CHAR_HEIGHT	(short) 0x51e0    /* length always 2 */ 
#define CHAR_ORIENT	(short) 0x5200     
#define TEXT_PATH	(short) 0x5220     
#define TEXT_ALIGN	(short) 0x5240    /* length always 12 */ 
#define CHARSET_INDX	(short) 0x5260     
#define ALT_CHRSET_INX  (short) 0x5280     
#define FILL_INDX  	(short) 0x52a0     
#define FILL_STYLE  	(short) 0x52c0    /* length always 2 */
#define FILL_COLR  	(short) 0x52e0    /* length always 2 */
#define HATCH_INX  	(short) 0x5300    
#define PATTN_INX  	(short) 0x5320    
#define EDGE_INX  	(short) 0x5340    
#define EDGE_TYPE  	(short) 0x5360    
#define EDGE_WIDTH  	(short) 0x5380    
#define EDGE_COLOR  	(short) 0x53a0    
#define FILL_REF_PT  	(short) 0x53e0    
#define PATTN_TABLE  	(short) 0x5400    
#define PATTN_SIZE  	(short) 0x5420    
#define COLOR_TAB  	(short) 0x5440    /* length always 5 */    
#define ASF	  	(short) 0x5460        

#define ESCAPE	  	(short) 0x6020        

#define MESSAGE	  	(short) 0x7020        

#define LONG_FORM	(short) 0x001f

#define CLASS_MASK      (short) 0xf000
#define CLASS_SHIFT	12
