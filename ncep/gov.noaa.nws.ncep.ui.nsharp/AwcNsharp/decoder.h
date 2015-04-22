	/* SOUNDING DECODER ROUTINES  */

	short decode_sounding( char *filename );
	short read_ttaa(char *filename);
	short read_ttbb(char *filename);
	short read_ppbb(char *filename);
	short decode_ttaa( void );
	short decode_ttbb( void );
	short decode_ppbb( void );
	void lvl_man( short ptr, short *incx, float *lvl, float *hght,
		      float *temp, float *dwpt, float *wdir, float *wspd);
	void lvl_sig( short i, float *lvl, float *hght, float *temp,
		       float *dwpt, float *wdir, float *wspd);
	void lvl_wnd( short i, float *lvl, float *hght, float *temp,
		       float *dwpt, float *wdir, float *wspd);
	float i_h( float pres );
	float i_p( float hght );
	void uasort( void );
	float find_temp( float lvl );
	float find_dwpt( float lvl );
	float find_wdir( float lvl );
	float find_wspd( float lvl );
	void remove_lvl( float lvl );
	void swap_arrays( float nsndg[100][6] );
	void write_file( char* filename);
	float get_sfc_elev( long id );
	void get_title( char *finam );
	char *get_stn_name( char *stn );
        void kainfritsch( float *kain[100][6] );
