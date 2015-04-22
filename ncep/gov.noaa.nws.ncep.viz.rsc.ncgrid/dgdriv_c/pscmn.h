/************************************************************************
 * PSCMN.H								*
 *									*
 * This header file declares the variables used in the PostScript (PS)	*
 * device driver.							*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 * S. Jacobs/NCEP	 5/96	Updated and reordered variables		*
 * S. Jacobs/NCEP	 1/97	Added color structure; Changed		*
 *				declaration of fontus			*
 * S. Jacobs/NCEP	11/97	Added kjust				*
 * S. Jacobs/NCEP	 3/98	Added NPATFL, tsfill and kfillt		*
 * R. Tian/SAIC		05/02	Changed clrbank[] size to 4		*
 * m.gamazaychikov/SAIC 01/03 	removed NPATFL				*
 * T. Piper/SAIC	03/04	Added numerous prototypes		*
 * E. Safford/SAIC	01/08	include proto_xw.h			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "proto_xw.h"


/*
 * Color bank structures
 */
typedef struct {
    int		krgun;
    int		kggun;
    int		kbgun;
} rgb_t;

typedef struct {
    int		ncolr;
    rgb_t	color[256];
} colrbank_t;


#ifdef PGLOBAL

	char		filnam[80];
				/* Output file name */

	FILE		*flun;
				/* Output file identifier */

	int		opnfil;
				/* Open file flag */

	int		psplot;
				/* Plotting flag */

	unsigned	nnpage;
				/* Page count */

	int		iawdth;
	int		irwdth;
				/* Dot width set and requested */

	float		txsizs, txsizr;
				/* Text size set and requested */

	int		isfont, irfont;
	char		*fontus[12];
	unsigned	nfontu;
				/* Font number set and requested,
				   List of fonts used,
				   Number of fonts used */

	int		resetc;
				/* Color RGB reset flag */

	int		mcolr;
				/* Current color */

	int		nncolr;
				/* Number of colors */

	char 		tblnam[40];
				/* Color table name */

        float		xsize;
				/* X size of paper */

        float		ysize;
				/* Y size of paper */

	int		landscape;
				/* Paper orientation flag */

	char		pprnam[20];
				/* Paper size name */

	colrbank_t	clrbank[4];

#else

	extern char		filnam[80];
	extern FILE		*flun;
	extern int		opnfil;
	extern int		psplot;
	extern unsigned		nnpage;
	extern int		iawdth;
	extern int		irwdth;
	extern float		txsizs, txsizr;
	extern int		isfont, irfont;
	extern char		*fontus[12];
	extern unsigned		nfontu;
	extern int		resetc;
	extern int		mcolr;
	extern int		nncolr;
	extern char 		tblnam[40];
	extern float		xsize;
	extern float		ysize;
	extern int		landscape;
	extern char		pprnam[20];
	extern colrbank_t	clrbank[4];

#endif

#include "xwpcmn.h"

/*
 *  PS prototypes
 */
void pcirc ( float *xcen, float *ycen, float *xrad, float *yrad, int *iret );
void pclear ( int *iret );
void pclosp ( int *iret );
void pcvtclr ( void );
void pdots ( int *ix, int *iy, int *ilwid, int *iret );
void pendd ( int *ieop, int *iret );
void pfill ( int *np, int ix[], int iy[], int *iret );
void pfontn ( int itxfn, char *fntnam );
void pinita ( int *iunit, char *fname, int *lenf, int *itype,
                float *xsz, float *ysz, int *ileft, int *ibot,
                int *iright, int *itop, int *numclr, int *iret );
void pinitclr ( int *itype );
void pline ( int *np, int ix[], int iy[], int *iret );
void pqclrs ( int *cbank, int *ncolors, int *colors, int *iret );
void pqcomp ( int *indx, char *clrnam, int *red, int *green,
                int *blue, char *xname, int *clen, int *xlen, int *iret );
void pqdatt ( int *iunit, char *fname, int *lenf, int *itype,
                        float *xsz, float *ysz, int *ncurwn, int *iret );
void psatim ( char filnam[], int *xispace0, int *yispace0,
                                int *xispace1, int *yispace1, int *iret );
void pscint ( int *iret );
void pscolr ( int *icbnk, int *icolr, int *iret );
void pscnam ( int *cindex, char *cname, int *len, int *iret );
void pscrgb ( int *type, int *icolr, int *ired, int *igreen,
                                                int *iblue, int *iret );
void psdatt ( int *iunit, char *fname, int *lenf, int *itype,
                float *xsz, float *ysz, int *ileft, int *ibot,
                int *iright, int *itop, int *numclr, int *iret );
void psfill ( float *szfil, int *iftyp, int *iret );
void psicmn ( int *hv, int *iret );
void psltyp ( int *iltyp, int lpat[10][8], int *lpscal, int *iret );
void pslwid ( int *ilwid, int *iret );
void psopen ( int *iret );
void pstext ( int *itxfn, float *sztext, int *ijust, float *txsize, int *iret );
void ptext ( float *xr, float *yr, char *cchar, int *lens,
                int *ixoff, int *iyoff, float *rotat, int *ispanx,
                int *ispany, int *icleft, int *icrght, int *icbot,
                int *ictop, int *iret );
void ptextc ( float *xr, float *yr, char *cchar, int *lens,
                int *ixoff, int *iyoff, float *rotat, int *ispanx,
                int *ispany, int *icleft, int *icrght, int *icbot,
                int *ictop, int *iret );
void pupdclr ( int *itype );

