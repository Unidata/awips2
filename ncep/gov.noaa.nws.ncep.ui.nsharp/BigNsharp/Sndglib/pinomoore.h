#ifndef _PINOMOORE_H
#define _PINOMOORE_H

float pinomoore(void);
float hailgrowth(float *max_diameter);
float hailreduction(float max_diameter, float *sfc_diameter);
float hailspeed(float diam, float pres, float dd_thw);

#endif  /* _PINOMOORE_H */
