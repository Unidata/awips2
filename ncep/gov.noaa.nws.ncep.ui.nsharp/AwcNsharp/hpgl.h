	/* HP Laserjet Function Prototypes */

	FILE *open_printer( void );
	void hpgl_setup( FILE *fp );
	void hpselect_pen( FILE *fp , short onoff, float width, short type );
	void hpmoveto( FILE *fp , short x, short y );
	void hplineto( FILE *fp , short x, short y );
	void hprectangle( FILE *fp , short x1, short y1, short x3, short y3,
			  short fill, short pct);
	void hpclose_printer( FILE *fp );
	void hpdraw_skewt( FILE *fp );
	void hpdraw_hodo( FILE *fp );
	void hpplot_elevs( FILE *fp );
	void hpisobar( FILE *fp, float pres, short flag);
	void hpisotherm( FILE *fp, float temp);
	void hpdry_adiabat( FILE *fp, float thta);
	void hpmoist_adiabat(FILE *fp, float thtae);
	void hpmixingratio(FILE *fp, float w);
	void hpcliprgn( FILE *fp , short x1, short y1, short x2, short y2 );
	void hptrace_temp( FILE *fp );
	void hptrace_dwpt( FILE *fp );
	void hptrace_vtmp( FILE *fp );
	void hptrace_wetbulb( FILE *fp );
	void hpouttext( FILE *fp, char *st, short x, short y, short pts, short bold);
	void hpset_font( FILE *fp, short typ, short points);
	void print_sounding_hpgl( void );
	short hptemp_to_pix( float temp, float pres );
	short hppres_to_pix( float pres );
	void hpplot_barbs( FILE *fp );
	void hptrace_parcel(FILE *fp, float pres, float temp, float dwpt);
	void hptrace_parcelx(FILE *fp, float pres, float temp, float dwpt);
	void hphodo_to_pix( float dir, float mag, short *x, short *y );
	void hptrace_hodo( FILE *fp );
	void hplabel_hodo( FILE *fp );
	void hphodo_circs( FILE *fp );
	void hpcircle( FILE *fp, short x, short y, short radius);
	void hpwrite_parcel( FILE *fp );
	void hpwrite_thermo( FILE *fp );
	void hpdisp_param( FILE *fp, char *st, short x, short y, short font, short tf);
	void hpcentertext( FILE *fp, char *st, short x, short y, short font, short tf);
	void hplvldata( FILE *fp, float pres, char *nam, short x, short y);
	void hpwind_barb( FILE *fp, float wdir, float wspd, short x, short y,
			  short siz);
	void hptriangle( FILE *fp, short x1, short y1, short x2, short y2,
			 short x3, short y3, short fill);
	void hpfill_cape( FILE *fp, short pct);
	void hpfill_capex( FILE *fp, short pct);
	void sharp_label( FILE *fp, short x1, short y1);
	void hpwrite_winds( FILE *fp );
	void hpwrite_storm( FILE *fp );
	void hpplot_uvvs( FILE *fp );	

