
#ifndef _va_advanced_h
#define _va_advanced_h

#ifdef __cplusplus
extern "C" {
#endif

void va_weighting(float wgt);

void va_just_goodness(int );

void va_aspect(float a);

void va_literal(int mn, int mx);

void va_dist_pass(int );

void va_recomp(int );
void va_advanced(float lats[], float lons[], int goodness[],
                 float dist[], int ns);

#ifdef __cplusplus
};
#endif

#endif
