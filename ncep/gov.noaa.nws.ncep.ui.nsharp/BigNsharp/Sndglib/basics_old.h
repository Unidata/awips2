#ifndef _BASICS_H
#define _BASICS_H

/* Constants defining interpolation type */
#define I_PRES 0
#define I_HGHT 1

#define ABOVE 1
#define BELOW 0

/* BASICS Function Prototypes */

/* General interpolation functions */
float i_var(char *var, float level, short itype);
float interp_gen(float valueabove, float valuebelow, float level,
                 float levelabove, float levelbelow, short itype);
short getlevelindex(short parmIndex, float level, short direction, short itype);

float i_temp(float pres, short itype);
float i_dwpt(float pres, short itype);
float i_hght(float pres, short itype);
float i_vtmp(float pres, short itype);
float i_wdir(float pres, short itype);
float i_wspd(float pres, short itype);
float i_wndu(float pres, short itype);
float i_wndv(float pres, short itype);
float i_pres(float hght);
float i_omeg(float pres, short itype);
short sfc(void);
float top_pres(void);
short qc(float value);
char *qc2(float value, char *label, short prec);
float ftom(float value);
float mtof(float value);
float ftoc(float value);
float ctof(float value);
float agl(float height);
float msl(float height);
float kt_to_mps(float spd);
char *itoa(int value, char *st, int radx);
void xtnd_sndg(void);
void interp_sndg(void);
void check_data();

#endif  /* _BASICS_H */
