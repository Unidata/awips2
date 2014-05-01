	/*  Thermodynamic Parameters Routines */

	float bulk_rich( struct _parcel pcl, float *brnshear);
	float cnvtv_temp( float *param, float mincinh );
	void  define_parcel( short flag, float pres );
	float delta_t( float *param );
	float ehi( float cape, float hel );
	short grab_level( float pres );
	float k_index( float *param );
	float lapse_rate( float *param, float lower, float upper );
	void  low_inv ( float *inv_mb, float *inv_dC );
	float max_temp( float *param, float mixlyr);
	float mean_dwpt( float *param, float lower, float upper );
	float mean_mixratio( float *param, float lower, float upper );
	float mean_relhum( float *param, float lower, float upper );
	float mean_theta( float *param, float lower, float upper );
	float Mean_WBtemp( float *param, float lower, float upper);
	void mix_height ( float *mh_mb, float *mh_drct, float *mh_sped, 
			  float *mh_dC, float *mh_lr, float *mh_drct_max, 
			  float *mh_sped_max, short flag );
	float old_cnvtv_temp( float *param );
	float parcel( float lower, float upper, float pres, float temp,
		      float dwpt, struct _parcel *pcl);
	float parcelx( float lower, float upper, float pres, float temp,
		       float dwpt, struct _parcel *pcl);
	float precip_water( float *param, float lower, float upper );
	float Rogash_QPF( float *param);
	float sweat_index( float *param );
	float temp_lvl( float temp, float *param );
	float ThetaE_diff( float *param );
	float top_moistlyr( float *param );
	float t_totals( float *param, float *ct, float *vt );
	float unstbl_lvl( float *param, float lower, float upper );
	float vert_tot( float *param );
	float wb_lvl( float temp, float *param );
