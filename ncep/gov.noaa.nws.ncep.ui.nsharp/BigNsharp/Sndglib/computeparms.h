#ifndef _SNDGCOMPUTE_H
#define _SNDGCOMPUTE_H

struct _SndgParms {
	float LI;
}; 
typedef struct _SndgParms SndgParms;

/* Functions */
void computethermoparms(void);
void computewindparms(void);
void computeallparms(void);

#endif /* _SNDGCOMPUTE_H */
