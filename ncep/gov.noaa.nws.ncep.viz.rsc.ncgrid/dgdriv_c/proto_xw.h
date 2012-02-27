/************************************************************************
 * proto_xw.h                                                      	*
 *                                                                      *
 * This include file contains function prototypes for the XW files   	*
 * in the XW driver library.						* 
 *									*
 **                                                                     *
 * A. Hardy/GSC		12/00   Created					*
 * A. Hardy/GSC		12/00   Added gn prototypes			*
 * A. Hardy/GSC		02/01   Added gf prototype 			*
 * E. Safford/GSC	08/01	rmv pxmarry[] param from xmloop_loopSet	*
 * T. Piper/SAIC	10/01	rmv private functions			*
 * J. Wu/SAIC		12/01	add xpgsetlayer()			*
 * R. Tian/SAIC		05/02	Added crnfax()				*
 * T. Piper/SAIC	05/03	added xsncolr()				*
 * T. Piper/SAIC	07/03	added xqpxms()				*
 * T. Piper/SAIC	07/03	renamed and added parameter to xmloop	*
 * T. Piper/SAIC	07/03	added xqgemgc()				*
 * T. Piper/SAIC	08/03	added xginit()				*
 * T. Piper/SAIC	01/04	changed type for *colors in xqclrs	*
 * T. Piper/SAIC	03/04	added xstext()				*
 * H. Zeng/SAIC		04/04	added xmfrmtg_saveFrmTag()		*
 * C. Bailey/HPC        02/05   added xgsave()                          *
 * T. Piper/SAIC	02/08	Added gf prototypes			*
 ***********************************************************************/

#ifndef PROTO_XW
#define PROTO_XW

/*
 *  gf prototypes
 */

void	gfclosp (       int             *ixsize,
                        int             *iysize,
                        int             *ncurwn,
                        int             *iret );
void	gfdatt (        int             *iunit,
                        char            *filnam,
                        int             *lenf,
                        int             *itype,
                        float           *xsize,
                        float           *ysize,
                        int             *ixsize,
                        int             *iysize,
                        int             *isxsiz,
                        int             *isysiz,
                        int             *ixoff,
                        int             *iyoff,
                        int             *ncurwn,
                        int             *iret );
void	gfendd (        int             *iret );
void	gfflsh (        int             *raiseit,
                        int             *iret );
void gfinita ( char *dev, int *lend, int *iunit, char *filnam, int *lenf,
                int *itype, float *xsize, float *ysize, int *ixsize,
                int *iysize, int *isxsiz, int *isysiz, int *ncurwn,
                int *iret );
void	gfopenw (       char            win_name[],
                        int             win_index,
                        float           xsize,
                        float           ysize,
                        int             *ixsize,
                        int             *iysize,
                        int             *iret );
void	gfselwin (      char            win_name[],
                        int             *len,
                        float           xsize,
                        float           ysize,
                        int             *ixsize,
                        int             *iysize,
                        int             *iret );
void	xwrgif ( 	int     	*iret );

/*
 *  gn prototypes
 */

void 	cctabl ( 	char		*coltbl,
			int		*iret );

void 	cqcomp ( 	int		*cindex,
			char		*clrname,
			int		*red,
			int		*green,
			int		*blue,
			char		*xname,
			int		*iret );

void 	crarea ( 	char		*imgnam,
			int		*iret );

void 	crastr ( 	unsigned char	*radial,
			int		num_radial,
			int		size,
			float		start,
			float		delta,
			int		*iret );

void 	crgini ( 	char		*imgnam,
			int		*iret );

void 	crncdf ( 	char		*imgnam,
			int		*iret );

void 	crnexz ( 	char		*imgnam,
			int		*iret );

void 	crnfax ( 	char 		*dev,
			char		*imgnam,
			int		*iret );

void 	crnids ( 	char		*imgnam,
			int		*iret );

void 	crnowr ( 	char		*imgnam,
			int		*iret );

void 	cscnam ( 	int		*indx,
			char		*clrname,
			int		*iret );

void 	cscrgb ( 	int		*indx,
			int		*red,
			int		*green,
			int		*blue,
			int		*iret );

void 	csctbl ( 	char		*tblnam,
			int		*iret );

void 	csinit ( 	int		*iret );

/*
 *  xw prototypes
 */

void    delay (         struct timeval  start_time,
                        long            interval );

long    lapse (         struct timeval  time0,
                        struct timeval  time1 );

void    looper (        Window          gwin,
                        int             command );

void 	xaiclr ( 	int		*cbank,
			int		*numc,
			int		*iret );

int 	xarea ( 	void );

void 	xcaloc ( 	int		cbank,
			int		*iret );

void 	xcamgr ( 	Display 	*dpy,
			Colormap        cmap,
			int     	nbank,
			int    	 	*banks, 
			int    	 	*return_banks, 
			int		*iret );

void 	xcirc ( 	float   	*xcen,
			float   	*ycen,
			float   	*xrad,
			float   	*yrad,
			int		*iret );

void 	xclear ( 	int		*idwdth,
			int		*idhght,
			int		*iswdth,
			int		*ishght,
			int		*iret );

void 	xclosp ( 	int		*ixsize,
			int		*iysize,
			int		*ncurwn,
			int		*iret );

void 	xclpnl ( 	int		*ix1,
			int		*iy1,
			int		*ix2,
			int		*iy2,
			int		*iret );

void 	xclrpxm ( 	int     	*ipxm,
			int		*iret );

void 	xcpypxm ( 	int		from,
			int		to,
			int		*iret );

void 	xcpypxm2 ( 	int		lp,
			int		from,
			int		to,
			int		*iret );

int 	xcsdat ( 	register Display *dpy,
			char             *block_name,
			char             *data,
			unsigned int     nbyte );


void 	xdot ( 		int		*ilwid,
			int		*ix,
			int		*iy,
			int		*iret );

void    xdsclr (        Display         *dpy );

void 	xdwtbl ( 	int		*iret );

void 	xenanm ( 	int		*iret );

void 	xendd ( 	int		*iret );

void 	xfill (	 	int		*npt,
			int		*ix,
			int		*iy,
			int		*iret );

void 	xg2pxm ( 	int     	*ipxm,
			int		*iret );

void 	xgbank ( 	register Display *dpy,
			int 		 *iret );

void	xginit (	Widget		wid,
			int		*iret );

int     xgrwc (         Display         *dpy,
                        Colormap        cmap,
                        unsigned long   pxls[],
                        int             ncolors );

void    xgsave (        char            filnam[],
                        int             *len,
                        int             *iframe,
                        int             *nframe,
                        int             *iret );

int 	xgsdat ( 	register Display *dpy,
			char             *block_name,
			unsigned char    *data,
			unsigned int     *nbyte );

void 	xgtoff ( 	int		*xoff,
			int		*yoff,
			int		*iret );

void 	xgtpnt ( 	int		*ityp,
			int		*ix,
			int		*iy,
			int		*iret );

void 	xinita ( 	char		*dev,
			int		*lend,
			int		*iunit,
			char		*filnam,
			int		*lenf,
			int		*itype,
			float		*xsize,
			float		*ysize,
			int		*ixsize,
			int		*iysize,
			int		*isxsiz,
			int		*isysiz,
			int		*ncurwn,
			int		*iret );

void 	xinitclr ( void );

void 	xline  ( 	int		*npt,
			int		*ix,
			int		*iy,
			int		*iret );

void 	xloopc ( 	int		*number,
			int		*iret );

void 	xmexpo (	 XEvent 	*event );

void 	xmfrmtg_resetLp ( int		lp,
			  int	 	*iret );

void 	xmfrmtg_setFrmTag ( int		lp,
			    int		frm,
			    Boolean	value );

Boolean xmfrmtg_getFrmTag ( int		lp,
			    int		frm );

void    xmfrmtg_saveFrmTag ( int        lp, 
			     int      *iret );

void    xmfrmtg_restoreFrmTag ( int        lp, 
			        int      *iret );

void 	xmloopSetDwellPtr ( int 	*dwellptr );

void 	xmloopSet ( 	Widget  	wid,
			int     	curpxm,
			int     	pxmarry[],
			int     	npxm,
			void    	(*func)(int, int),
			int     	*iret );

Boolean xmloop_atBlankPxm ( void );

void 	xmloop_loopSet ( Widget  	wid,
			int		loop,
			Boolean		loopcur,
			int     	curpxm,
			int     	npxm,
			void    	(*func)(int, int),
			int     	*iret );

void 	xmloop_switchLoop ( int		loop,
			    Boolean	update );

void 	xmloop_loop ( 	int     	command, 
			Boolean		loopstop );

void 	xmloop_getPxmInfo ( int		loop,
			    int		*npxm,
			    int		*fpxm,
			    int		*lpxm,
			    int		*bpxm,
			    int		*iret );

int  xmloop_getGoodPxm ( int		lp,
			 int		start,
			 int		dir );

XtTimerCallbackProc xmloop_timeoutProc ( XtIntervalId  *id );

void 	xmotifw ( 	Window  	win_id,
			char    	win_name[],
			GC      	win_gc,
			int     	width,
			int     	height,
			int		depth,
			int		*iret );

void 	xmroam_setPos ( int		xx,
			int		yy );

void 	xmroam_getPos ( int		*xx,
			int		*yy );

void 	xopenw ( 	char		win_name[],
			int		win_index,
			float		xsize,
			float		ysize,
			int		*ixsize,
			int		*iysize,
			int		*iret );

void 	xpgpaste ( 	float		llx,
			float		lly,
			float		urx,
			float		ury,
			int 		*iret );

void 	xpgrestlp (	void );

void 	xpgrfrsh (	void );

void 	xpgsetlayer ( 	int		layer );

void 	xpgsetpg ( 	Boolean		pg_status );

void 	xpgsvfrm ( 	int 		fn );

void 	xpgsvfrm2 ( 	int 		fn );			

void 	xpgsvlp ( 	int		*iret );

void 	xpoint  ( 	int		*ix,
			int		*iy,
			int		*iret );

void 	xputpxms ( 	Pixmap 		*pixmaps );

void 	xpxm2win ( 	int     	src,
			int		dest,
			int		*iret );

void 	xqclrs ( 	int		*cbank,
    			int 		*ncolors,
    			Pixel		*colors,
  			int 		*iret );

void 	xqcmps ( 	int		*icbank,
			int     	*ncolr,
			int		*ired,
			int		*igreen,
			int		*iblue,
			int		*iret );

void 	xqcolr ( 	int		*cbank,
			int		*jcolr,
			unsigned long	*colpxl,
			int		*iret );

void 	xqcomp ( 	int		*indx,
			char		*clrnam,
			int		*red,
			int		*green,
			int		*blue,
			char		*xname,
			int		*clen,
			int		*xlen,
			int		*iret );

void 	xqcpxm ( 	int     	*npxms,
			int     	*curpxm );

void 	xqdatt ( 	int		*iunit,
			char		*filnam,
			int		*lenf,
			int		*itype,
			float		*xsize,
			float		*ysize,
			int		*ncurwn,
			int		*iret );

GC	xqgemgc (	void	);

void 	xqnclr ( 	int		*cbank,
			int 		*ncolors,
			int 		*iret );

Pixmap	xqpxms (	int		loop,
			int		npix );

void 	xrbpxm ( 	int		idx,
			int		lp );

void    xrmfnt (        int             font_no );

void 	xroam ( 	int		*ityp,
			float		*xx,
			float		*yy,
			int		*ix,
			int		*iy,
			int		*iret );


void 	xsatim ( 	char 		*imgnam,
			int		*xispace0,
			int		*yispace0,
			int		*xispace1,
			int		*yispace1,
			int		*iret );

void 	xsatpx ( 	float   	*dx,
			float   	*dy,
			int		*offx,
			int		*offy,
			char    	*imgnam,
			int     	*xispace0,
			int     	*yispace0,
			int     	*xispace1,
			int     	*yispace1,
			int		*iarea,
			int		*mode,
			int     	*ipix,
			float		*dxo,
			float		*dyo,
			int     	*iret );

void 	xscint ( 	int		*iret );

void 	xscnam ( 	int		*cbank,
			int		*cindex,
			char		*cname,
			int		*len,
			int		*iret );

void 	xscolr ( 	int		*cbank,
			int		*jcolr,
			int		*iret );

void 	xscpxm ( 	Pixel     	ipxm,
			int		*iret );

void 	xscrgb ( 	int		*cbank,
			int		*jcolr,
			int		*ired,
			int		*igreen,
			int		*iblue,
			int		*iret );

void 	xsdatt ( 	int		*iunit,
			char		*filnam,
			int		*lenf,
			int		*itype,
			float		*xsize,
			float		*ysize,
			int		*ixsize,
			int		*iysize,
			int     	*isxsiz,
			int     	*isysiz,
			int     	*ixoff,
			int     	*iyoff,
			int		*ncurwn,
			int		*iret );

void 	xselwin ( 	char    	win_name[],
			int     	*len,
			float		xsize,
			float		ysize,
			int		*ixsize,
			int		*iysize,
			int		*iret );

void 	xsetver ( 	Boolean		nmap2_value );

void 	xsfill ( 	float		*szfil,
			int		*iftyp,
			int		*iret );

void 	xsicmn ( 	int		*hv, 
			int	 	*iret );

void 	xsincr ( 	Boolean		incr_value );

void 	xslwid ( 	int		*ilwid,
			int		*iret );

void 	xslwin ( 	char    	winnam[],
			int     	*len,
			int		*ixsize,
			int		*iysize,
			int		*isxsiz,
			int		*isysiz,
			int		*ixo,
			int		*iyo,
			int		*ncurwn,
			int		*iret );

void	xsncolr (	char		*cname,
			Pixel		*pcolor,
			int		*iret );

void 	xsplot ( 	int		*iret );

void 	xsroam ( 	int		*roamflg,
			int		*ipwdth,
			int		*iphght,
			int		*idwdth,
			int		*idhght,
			int		*iret );

void 	xstanm ( 	int		*iret );

void	xstext ( 	int 		*itxfn, 
			float 		*txtsz, 
			int 		*ijust, 
			float 		*txsize,
			int 		*iret );

void 	xtext ( 	float		*xr,
			float		*yr,
			char		strout[],
			int 		*lenstr,
			int		*ixoff,
			int		*iyoff,
			float		*rotat,
			int		*ispanx,
			int		*ispany,
			int		*icleft,
			int		*icrght,
			int		*icbot,
			int		*ictop,
			int		*iret );

void 	xtextc ( 	float		*xr,
			float		*yr,
			char		strout[],
			int 		*lenstr,
			int		*ixoff,
			int		*iyoff,
			float		*rotat,
			int		*ispanx,
			int		*ispany,
			int		*icleft,
			int		*icrght,
			int		*icbot,
			int		*ictop,
			int		*iret );


void 	xupdclr ( void );

void 	xw_gxor ( 	int     	*ixor,
			int     	*iret );

void 	xxevnt ( 	int		*iret );

void 	xxflsh ( 	int		*raiseit,
			int		*iret );

#endif /* PROTO_XW */
