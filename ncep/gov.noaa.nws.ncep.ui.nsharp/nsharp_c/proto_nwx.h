/************************************************************************
 * proto_nwx.h								*
 *									*
 * This file contains header files and global variables for use in the	*
 * NWX routines.							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC          1/01   					*
 * T. Piper/SAIC	 5/02	Added parameter to txtw_dttmSet		*
 * T. Piper/SAIC	 7/03	removed gmpk_init and map_init		*
 * R. Tian/SAIC		 7/03	added mapw_rmappstn			*
 * R. Tian/SAIC		11/03	added auto_start/stopAutoUpdt,dslw_load	*
 * T. Piper/SAIC	01/04	added nwxerr				*
 * T. Piper/SAIC	04/05	CSC for nsfopn				*
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

void 	draw_wbox ( 	struct watchbox *wtchbox,
			int		called_from );

Widget 	dslw_create ( 	Widget	w );

void 	dslw_toggle ( 	void );

void 	dslw_load (	int		called_from,
			int 		*iret );

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

int 	fosd_txtrd ( 	int	called_from );

void 	fosd_decode ( void );

void 	fosd_plot ( 	int	called_from );

void 	fosd_getdata (  struct datatype_list	*dt_info,
			srchinfo_t		*srch_info,
			int			called_from,
			char			*report,
			int			*iret );

void 	fosd_wbox ( int called_from );

void	map_draw ( char *map, char *garea, char *proj, char *latlon, 
		   char *panel, char *text, char *title,
	 	   int *ititl, int *linttl, char *shrttl, int *clear, 
		   int *iret, Cardinal, Cardinal, Cardinal, Cardinal, Cardinal,
                   Cardinal, Cardinal, Cardinal );

void	map_mark ( int *nltln, float *rlat, float *rlon, int *ivalue,
		   int *ncolor, int *breaks, int *icolrs, int *mrktyp,
		   float *sizmrk, int *mrkwid, int *pltval, int *iposn,
		   int *iret);

Widget 	mapw_create ( Widget	parent );

int 	mapw_rgstr ( Widget	mapwin );

void 	mapw_rmappstn ( void );

void 	mapw_rmselstn ( void );


void	nsfopn	(	char *datatype, 
			char		*filnam,
			int		*iflno,
			int		*iret,
			Cardinal,
			Cardinal );

void	nwxerr	(	int	iret	);

int	nwxtbl_init ( void );

int	nwxtbl_sdtyp ( char  *datatype );

void	nwxtbl_getstns ( int		inxdt,
			 stnlist_t	*stns,
			 int		*iret );

void	prf_decode (	char	    *text,
			stnlist_t   *stnlist,
			plotdata_t  *plotdata );

void	qpf_decode (	char	   *text,
			plotdata_t *plotdata );

void	srchb_fosdGetrep ( struct datatype_list	*dtinfo,
			   srchinfo_t		*srchinfo,
			   char			*report,
			   int			*iret );

void	srchb_fosdGetnxt ( srchinfo_t	*srchinfo,
			   char		*report,
			   int		*iret );

void	srchb_repInit ( void );

void	srcho_fosdGetrep ( srchinfo_t   *srchinfo,
			   int		called_from,
			   char		*report,
			   int		*iret );

void	srcho_timChange ( srchinfo_t	*srchinfo,
			  int		minutes,
			  int		*iret );

void	srchw_fosdScan ( srchinfo_t	*srchinfo,
			 int 		called_from,
			 int		*iret );

void	srchw_fosdGetrep ( srchinfo_t	*srchinfo,
			   char		*report,
			   int		*iret );

void	sstruct_stxtmk ( int			idtype,
			 struct datatype_list	*dtypinfo,
			 int			*iret );

Widget	txtw_create ( Widget	parent );

void	txtw_prdgrpSet ( void );

void	txtw_dttmSet ( char	*filnme );

void	uvi_decode ( 	char       *text,
			stnlist_t  *stnlist,
			plotdata_t *plotdata );

void	wbox_decode (   char 	    *text,
			int	    nwatch,
			plotdata_t  *plotdata );

void	ww_crnr (	float	*side,
			int	*iflag,
			char	locid1[],
			char	locid2[],
			float	dist[],
			float	bear[],
			float	rlat[],
			float	rlon[],
			int	*npt,
			int	*iret );

void	ww_dcod (	char *bultin,
			int *lenbul, int *itype, char *wnum, char *strtim,
			char *stptim, char *tissue, int *icorr, int *icancl, 
			float *rlat, float *rlon, int *npt, int *irepl, char *rnums, 
			int *iret, Cardinal, Cardinal, Cardinal, Cardinal, Cardinal, 
			Cardinal );

void	auto_startAutoUpdt ( void );
void	auto_stopAutoUpdt ( void );

#endif	/* PROTO_NWX */
