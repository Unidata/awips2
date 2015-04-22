#ifndef _WINTER_H
#define _WINTER_H

float precip_type(void);
void posneg_wetbulb(float start, float *pos, float *neg, 
	            float *top, float *bot);
void posneg_temperature(float start, float *pos, float *neg,
	                float *top, float *bot);
char *init_phase(float *plevel, short *phase);
char *best_guess(struct _ptype p);

#endif /* _WINTER_H */
