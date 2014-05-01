/************************************************************************
 * proto_nwx.h								*
 *									*
 * This file contains header files and global variables for use in the	*
 * NWX routines.							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC          1/01   					*
 ***********************************************************************/

#ifndef PROTO_NWX
#define	PROTO_NWX


/*
 *  NWX prototypes
 */

int 	dchk_alpha ( char  *str );

int 	dchk_digit ( char  *str );

void 	dir_getflist ( 	struct datatype_list	*dtyp_info,
			int			idtyp,
			struct date_time_info   startdttm,
			struct date_time_info   enddttm,
			struct directory_info	*dir_info,
			int			*iret );

void 	dir_getnextf ( 	struct directory_info	*dir_info,
			int			strtflg,
			struct data_file_info	*file_info,
			int			*iret );

void 	draw_map ( 	int			num,
			struct maptype_list	*map_info,
			int			zoomflg,
			mapbnd_t		*mapb,
			int			*iret );

void 	draw_stnmark ( 	int		numpts,
			float		*slat,
			float		*slon,
			int		jcolr,
			int		imrk,
			int		*iret );

void 	draw_value ( 	struct mrkv 	*markdata );

void 	draw_cntr ( 	struct contour 	*contours );

void 	draw_wbox ( 	struct watchbox *wtchbox );

Widget 	dslw_create ( 	Widget	w );

void 	dslw_toggle ( 	void );

void 	dttm_cnvt ( 	char	*dattm,
			int	*iyear,
			int	*imonth,
			int	*iday,
			int	*ihour,
			int	*iret );

int 	dttm_cmp ( 	struct date_time_info 	dttm1,
			struct date_time_info   dttm2 );

void 	dttm_cpy ( 	struct date_time_info 	*dttm1,
			struct date_time_info 	dttm2 );

int 	fosd_txtrd ( void );

void 	fosd_decode ( void );

void 	fosd_plot ( void );

void 	fosd_getdata (  struct datatype_list	*dt_info,
			srchinfo_t		*srch_info,
			char			*report,
			int			*iret );

void 	fosd_wbox ( void );

int 	gmpk_init ( Widget   w );


void	map_draw ( char *map, char *garea, char *proj, char *latlon, 
		   char *panel, char *text, char *title,
	 	   int *ititl, int *linttl, char *shrttl, int *clear, 
		   int *iret, int, int, int, int, int, int, int, int );

/* void	map_init ( int *iret ); */
void	map_init ( int *iret, char *wname, int );

void	map_mark ( int *nltln, float *rlat, float *rlon, int *ivalue,
		   int *ncolor, int *breaks, int *icolrs, int *mrktyp,
		   float *sizmrk, int *mrkwid, int *pltval, int *iposn,
		   int *iret);

Widget 	mapw_create ( Widget	parent );

int 	mapw_rgstr ( Widget  mapwin );

void 	mapw_rmselstn ( void );


void	nsfopn	(	char *filnam, int *iflno, int *iret, int );

int 	nwxtbl_init ( void );

int 	nwxtbl_sdtyp ( char  *datatype );

void	nwxtbl_getstns ( int		inxdt,
			 stnlist_t	*stns,
			 int		*iret );

void 	prf_decode ( 	char 	    *text,
			stnlist_t   *stnlist,
			plotdata_t  *plotdata );

void 	qpf_decode ( 	char 	   *text,
			plotdata_t *plotdata );

void 	srchb_fosdGetrep ( struct datatype_list	*dtinfo,
			   srchinfo_t           *srchinfo,
			   char			*report,
			   int			*iret );

void 	srchb_fosdGetnxt ( srchinfo_t	*srchinfo,
			   char		*report,
			   int		*iret );

void 	srchb_repInit ( void );

void 	srcho_fosdGetrep ( srchinfo_t   *srchinfo,
			   char         *report,
			   int          *iret );

void 	srchw_fosdScan ( srchinfo_t     *srchinfo,
			 int		*iret );

void 	srchw_fosdGetrep ( srchinfo_t     *srchinfo,
			   char           *report,
			   int            *iret );

void 	sstruct_stxtmk ( int                     idtype,
			 struct datatype_list    *dtypinfo,
			 int                     *iret );

Widget 	txtw_create ( Widget  parent );

void 	txtw_prdgrpSet ( void );

void	txtw_dttmSet ( void );

void 	uvi_decode ( 	char       *text,
			stnlist_t  *stnlist,
			plotdata_t *plotdata );

void 	wbox_decode (   char 	    *text,
			int	    nwatch,
			plotdata_t  *plotdata );

void 	ww_crnr (	float	*side,
			int	*iflag,
			char	locid1[],
			char	locid2[],
			float	dist[],
			float	bear[],
			float	rlat[],
			float	rlon[],
			int	*npt,
			int	*iret );

void	ww_dcod (	char *bultin, int *lenbul, int *itype, char *wnum, char *strtim,
			char *stptim, char *tissue, int *icorr, int *icancl, 
			float *rlat, float *rlon, int *npt, int *irepl, char *rnums, 
			int *iret, int, int, int, int, int, int );


#endif	/* PROTO_NWX */

