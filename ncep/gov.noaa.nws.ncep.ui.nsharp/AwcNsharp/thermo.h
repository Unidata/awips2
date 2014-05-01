	/* THERMO Function Prototypes */

	float lifted(float pres, float temp, float dwpt, float lvl2);
	void drylift(float p1, float t1, float td1, float *p2, float *t2);
	float wetlift(float pres, float temp, float pres2);
	float lcltemp(float temp, float dwpt);
	float thalvl(float thta, float temp);
	float theta(float pres, float temp, float pres2 );
	float mixratio(float pres, float temp);
	float temp_at_mixrat( float mr, float pres);
	float satlft(float pres, float thm);
	float vappres(float temp);
	float wobf(float temp);
	float wetbulb( float pres, float temp, float dwpt );
	float thetaw( float pres, float temp, float dwpt );
	float thetae( float pres, float temp, float dwpt );
        float esfc( float val );
