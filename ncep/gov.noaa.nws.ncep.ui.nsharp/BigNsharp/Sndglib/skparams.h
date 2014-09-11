#ifndef _SKPARAMS_H 
#define _SKPARAMS_H

/* Entrainment rate see dcape() routine */
#define ENTRAIN_DEFAULT 0.0

/*  Thermodynamic Parameters Routines */

float k_index(float *param);
float t_totals(float *param, float *ct, float *vt);
float ehi(float cape, float hel);
float precip_water(float *param, float lower, float upper);
float parcel(float lower, float upper, float pres, float temp,
	     float dwpt, Parcel *pcl);
float parcelx(float lower, float upper, float pres, float temp,
	      float dwpt, Parcel *pcl);
float temp_lvl(float temp, float *param);
float wb_lvl(float temp, float *param);
float mean_relhum(float *param, float lower, float upper);
float mean_mixratio(float *param, float lower, float upper);
float mean_dwpt(float *param, float lower, float upper);
float max_temp(float *param, float mixlyr);
float mean_theta(float *param, float lower, float upper);
float delta_t(float *param);
float vert_tot(float *param);
float top_moistlyr(float *param);
float lapse_rate(float *param, float lower, float upper);
float bulk_rich(Parcel pcl, float *brnshear);
float cnvtv_temp(float *param, float mincinh);
float old_cnvtv_temp(float *param);
float unstbl_lvl(float *param, float lower, float upper);
float sweat_index(float *param);
void  define_parcel(short flag, float pres);
short grab_level(float pres);
float ThetaE_diff(float *param);
float ThetaE_diff2(float *param);
float Mean_WBtemp(float *param, float lower, float upper);
float Rogash_QPF(float *param);
float Mean_thetae(float *param, float lower, float upper);
float dcape(float *level, float *drtemp);
void  setdcape_entrain(float value);
float getdcape_entrain(void);
float mean_temp(float *param, float lower, float upper);
float uncapped_cape(float *param, float *level, float maxcin);
float scp(float stdir, float stspd);
float sigtorn(float stdir, float stspd);
float sigtorn_cin(float stdir, float stspd);
float CB_sigtor(void);
float mean_omeg(float *param, float lower, float upper);
float advection_layer(float *param, float lower, float upper);
float coniglio1(void);
void blep_technique( float *blep1, float *blep2);
float fosberg(float *param);
float pbl_top(float *pres);

#endif  /* _SKPARAMS_H */
