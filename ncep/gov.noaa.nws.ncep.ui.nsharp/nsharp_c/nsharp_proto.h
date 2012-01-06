#ifdef UNDERSCORE
#define get_acars_times		get_acars_times_
#define get_acars_points	get_acars_points_
#define get_acars_snd		get_acars_snd_
#define get_file_alias		get_file_alias_
#define get_gem_snd		get_gem_snd_
#define get_gem_stns		get_gem_stns_
#define get_gem_times		get_gem_times_
#define get_mdl_snd		get_mdl_snd_
#define get_nearest		get_nearest_

/* gemlib routines */
#define fl_mfls			fl_mfls_
#define gd_fltm			gd_fltm_
#define gd_gtmf			gd_gtmf_
#define gr_ftim			gr_ftim_
#define tb_rstn			tb_rstn_

#endif

/********************************************************************************
 * Public function prototypes							*
 *******************************************************************************/

/*
 *	GEMPAK FORTRAN functions called
 */
void gqdev ( char *device, int *iunit, int *iatyp, int *iret, int );
void fl_clos ( int *lun, int *iret);
void fl_mfls ( char *filtyp, char *dattim, char *cycle, int *maxfls,
			char *filnms, int *nfiles, char *templt, int *iret,
			Cardinal, Cardinal, Cardinal, Cardinal, Cardinal );
void fl_tbop ( char *table, char *type, int *lun, int *iret, Cardinal, Cardinal );
void gd_gtmf ( char *gdfile, char *gdatim, char *cycle, int *maxt, int *ngdftm, 
		char gdtlst[][20], int *iret, Cardinal, Cardinal, Cardinal, Cardinal );
void gd_fltm ( char *filnms, int *nfiles, int *maxt, int *ngdftm, char gdtlst[][20], 
		int *iret, Cardinal, Cardinal );
void tb_rstn ( int *lun, char *stid, char *stnnam, int *istnm, char *stat,
			char *coun, float *slat, float *slon, float *selv,
			int *ispri, char *tbchrs, int *iret, Cardinal,
			Cardinal, Cardinal, Cardinal, Cardinal );

/*
 *	acars_selection.c
 */
void acars_selection ( void );
void get_acars_snd ( char *sffile, int *ntime, char tlist[][20], int *mode,
                        float *srad, float *lat, float *lon, char *cstn,
                        float **rdata /*float rdata[][7]*/, int *nlev,
			int, int, int );

/*
 *      acars_status.c
 */
void show_status ( char *stn, char *auxinfo );

/*
 *	file_browse_popup.c
 */
void file_browse_popup (char *path, char *tmpl, Widget toplevel, 
			void cbfunc(Widget, XtPointer, XtPointer));

/*
 *	file_selection_menu.c
 */
void file_selection_menu ( char *conffile, char *confdir, _NXMmenuItem **menu,
			   XtCallbackProc cbfunc, int *iret);
			   
/*
 *	get_acars_times.f
 */
void get_acars_times ( char *sffile, char time_list[][20], int *ntimf,
                        int *iret, int, int );
void get_acars_points ( char *sffile, int *ntime, char tlist[][20],
			int *iret, int, int );
void get_nearest ( char *sffile, int *ntime, char tlist[][20], float *lat,
                        float *lon, char *cstn, char *auxinfoi, int, int,
			int, int );

/*
 *	 get_gem_snd.f
 */
void get_gem_snd ( char *snfile, char *dattim, char *area,
                        float **rdata, int *numlev, 
			Cardinal, Cardinal, Cardinal );

/*
 *	 get_gem_stns.f
 */
void get_gem_stns ( char *snfile, char *counin, char *time_dat,
                        char stn_list[][18], int *nstns, float *sta_lat,
                        float *sta_lon, Cardinal, Cardinal, Cardinal, 
			Cardinal );

/*
 *	 get_mdl_snd.f
 */
void get_mdl_snd ( char *gdfile, char *gdatim, char *gpoint,
                        float **rdata /*float rdata[][7]*/, int *numlev, 
			Cardinal, Cardinal, Cardinal );

/*
 *	 get_gem_times.f
 */
void get_gem_times (char *snfile, int *iflag, char time_list[][20],
                    int *ntimf, int *iret, Cardinal, Cardinal);

/*
 *	get_file_alias.f
 */
void get_file_alias (char *alias, char *path, char *tmpl, int *iret, 
			Cardinal, Cardinal, Cardinal);

/*
 *	mapw.c functions
 */
void mapw_exposeCb ( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void mapw_pickstnCb     ( Widget, XtPointer, XEvent*, Boolean* );
void mapw_pickstnCb_pfc ( Widget, XtPointer, XEvent*, Boolean* );
void mapw_resizeCb ( Widget, XtPointer, XtPointer );
int  nsharp_mapw_rgstr ( Widget mapwin, char *xwinname );
void nsharp_draw_map ( char *map_winname, mapstruct *mod_map, int *ier);

/*
 *	mod_snd.c
 */
void model_sounding_cb (Widget wdgt);

/*
 *	NxmHelp.c functions
 */
int  NxmHelp_loadFile ( Widget text_widget, char *filename );

/*
 *	obs_snd.c
 */
void observed_sounding_cb (Widget wdgt);
void sta_select_cb ( int which_sta );

/*
 *	pfc_snd.c
 */
void pfc_sounding_cb (Widget wdgt);
void sta_select_cb_pfc ( int which_sta );

/*
 *	print_selection.c functions
 */
int _prtSetXWPdev ( char *wname );
int _prtSetPSDev ( char *wname );
void printdialog_ok_cb ( void );
void print_selection ( Widget wdgt );

/*
 *	ps_driv.c functions	
 */
void print_sounding_ps ( int COLRMODE );

/*
 *	readdata.c
 */
void restore_origsndg ( void );
void save_origsndg (void );

/*
 *	show_text.c functions
 */
void show_textCb ( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void update_text_values ( void );

/*
 *	xwvid1.c
 */
void draw_hodo ( void );
void draw_skewt( void );
void make_screen ( void );
void pix_to_hodo ( short x, short y, float *dir, float *mag );
float pix_to_pres ( short pix );
float pix_to_temp ( short x, short y );
short pres_to_pix ( float pres );
short temp_to_pix( float temp, float pres );
void trace_parcel ( float pres, float temp, float dwpt );

/*
 *	xwvid2.c
 */
void disp_param ( char *value, short rcol, short rlin );
void hodo_cursor_data ( short x, short y );
void redraw_graph ( short mode );
void skewt_cursor_data ( short x, short y );
short switch_modes ( short mode );

/*
 *	xwvid3.c
 */
void clear_paramarea ( void );
void show_page ( short page );
void show_parcel ( void );

/*
 *	xwvid4.c
 */
void reset_options ( short mode, short pagenum );

/*
 *	xwvid5.c
 */
void draw_hoinset ( void );
void draw_skinset ( void );
void inset_options ( short mode );

/*
 *	xwvid6.c functions
 */
void _mapzoom_cb ( Widget mapCanvW, XtEventHandler pickstn_func, 
			mapstruct *mapdata, void (*cb_func)(void) );
void Load_gem_sounding (Widget parent, Widget wdgt, char *station, char *stid);
void Load_mdl_sounding (Widget, XtPointer, XtPointer );
void Load_stationlist ( void);
void Load_stationlist_pfc ( void );
void StartLoop ( void );
void X_Init ( void );
void clean_uvvs ( struct sndg_struct *sp );
void ellipse ( int type, short x, short y, short width, short height );
int  getgtextextent ( char *st );
void get_listitem (char *srchstr, int findex, char sep, char *outstr, int *iret);
char *itoa ( int value, char *st, int radx );
void lineto ( short x, short y );
void mdl_cursor_fmt	(Widget, long, XtPointer);
void mdl_pointer	(Widget, XtPointer, XEvent* );
void modmap_selCb	(Widget, XtPointer, XEvent* );
void moveto ( short x, short y );
void outgtext ( char *st, int x, int y );
void outcursor ( char *st, int x, int y );
void outtext ( char *st, int x, int y );
void popdown_cb		(Widget, XtPointer, XtPointer);
void rectangle ( int type, short x, short y, short width, short height );
void rectangle_cursor ( int type, short x, short y, short width, short height );
void set_font ( short siz );
void set_font_cursor (short font, Widget _canvas, GC _gc, XFontStruct **_fs );
void setcliprgn ( short tlx, short tly, short brx, short bry, Widget _canvas, GC _gc );
void setcolor ( int color, Widget canvas, GC _gc );
void setlinestyle ( short style, short width );
