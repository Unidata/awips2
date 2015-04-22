	/*  Thermodynamic Parameters Routines */

	float k_index( float *param );
	float t_totals( float *param, float *ct, float *vt );
	float ehi( float cape, float hel );
	float precip_water( float *param, float lower, float upper );
	float parcel( float lower, float upper, float pres, float temp,
		      float dwpt, struct _parcel *pcl);
	float parcelx( float lower, float upper, float pres, float temp,
		       float dwpt, struct _parcel *pcl);
	float visual1( float lower, float upper, float pres, float temp, float dwpt);
	float temp_lvl( float temp, float *param );
	int mult_temp_lvl(float temp,float params[2],int *number); 
	float wb_lvl( float temp, float *param );
	float mean_relhum( float *param, float lower, float upper );
	float mean_mixratio( float *param, float lower, float upper );
	float mean_dwpt( float *param, float lower, float upper );
	float max_temp( float *param, float mixlyr);
	float mean_theta( float *param, float lower, float upper );
    float min_cinh_lvl(float *param, float lower, float upper );
	float delta_t( float *param );
	float vert_tot( float *param );
	float top_moistlyr( float *param );
	float lapse_rate( float *param, float lower, float upper );
	float bulk_rich( struct _parcel pcl, float *brnshear);
	float cnvtv_temp( float *param, float mincinh );
	float old_cnvtv_temp( float *param );
	float unstbl_lvl( float *param, float lower, float upper );
	float sweat_index( float *param );
	void define_parcel( short flag, float pres );
	short grab_level( float pres );
	float ThetaE_diff( float *param );
        float Mean_WBtemp( float *param, float lower, float upper);
        float Rogash_QPF( float *param);
