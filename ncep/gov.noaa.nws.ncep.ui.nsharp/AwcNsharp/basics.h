	/* BASICS Function Prototypes */

	float i_temp( float pres );
	float i_dwpt( float pres );
	float i_hght( float pres );
	float i_vtmp( float pres );
	float i_wdir( float pres );
	float i_wspd( float pres );
	float i_wndu( float pres );
	float i_wndv( float pres );
	float i_pres( float hght );
        float i_omeg( float pres );
	short sfc( void );
	float top_pres( void );
	short qc( float value );
	char *qc2( float value, char *label, short prec );
	float ftom( float value );
	float mtof( float value );
	float ftoc( float value );
	float ctof( float value );
	float agl( float height );
	float msl( float height );
	float kt_to_mps( float spd );
	float virtemp( float pres, float temp, float dwpt );

