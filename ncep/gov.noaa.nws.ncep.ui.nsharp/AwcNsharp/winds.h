	/* WINDS Function Prototypes */

	float ucomp( float wdir, float wspd );
	float vcomp( float wdir, float wspd );
	float angle( float u, float v );
	void mean_wind( float pbot, float ptop, float *mnu, float *mnv,
			float *wdir, float *wspd);
	void sr_wind( float pbot, float ptop, float stdir, float stspd,
		      float *mnu, float *mnv, float *wdir, float *wspd);
	void wind_shear( float pbot, float ptop, float *shu, float *shv,
			float *sdir, float *smag);
	float helicity( float lower, float upper, float sdir, float sspd,
			float *phel, float *nhel);

