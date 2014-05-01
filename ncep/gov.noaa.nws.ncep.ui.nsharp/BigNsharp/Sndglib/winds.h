#ifndef _WINDS_H
#define _WINDS_H

	/* WINDS Function Prototypes */

float ucomp(float wdir, float wspd);
float vcomp(float wdir, float wspd);
float angle(float u, float v);
float speed(float u, float v);
void  mean_wind(float pbot, float ptop, float *mnu, float *mnv,
	        float *wdir, float *wspd);
void  mean_wind_npw(float pbot, float ptop, float *mnu, float *mnv,
	        float *wdir, float *wspd);
void  sr_wind(float pbot, float ptop, float stdir, float stspd,
	      float *mnu, float *mnv, float *wdir, float *wspd);
void  wind_shear(float pbot, float ptop, float *shu, float *shv,
		 float *sdir, float *smag);
float helicity(float lower, float upper, float sdir, float sspd,
	       float *phel, float *nhel);
/*
void  bunkers_storm_motion(float *u, float *v, float *dir, float *spd);
void bunkers_left_motion(float *u, float *v, float *dir, float *spd);
*/
void corfidi_MCS_motion(float *fpu, float *fpv, float *fp_dir, float *fp_spd, float *bpu, float *bpv, float *bp_dir, float *bp_spd);
float max_wind(float *lvl, float *dir, float *spd, float lower, float upper);

#endif  /* _WINDS_H */
