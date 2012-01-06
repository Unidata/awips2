#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <float.h>

typedef int logical;
#define TRUE 1
#define FALSE 0

/* define variables used in AODT library */
#define mxcdsz 640
#define maxd 500
#define bufsiz maxd*maxd
#define slots 150

/* define mathematic fundtions used in AODT library */
#define  A_ABS(x)     ((x) >= 0 ? (x) : -(x))
#define  A_MAX(x, y)  (((x) > (y)) ? (x) : (y))    /* use greater of 2 values */
#define  A_MIN(x, y)  (((x) < (y)) ? (x) : (y))    /* use lesser of 2 values  */
#define  A_SIGN(x, y) ((x)/(y))*A_ABS(y)
#define  A_SIN(x)     (float)( sin( (double)(x) ) )
#define  A_COS(x)     (float)( cos( (double)(x) ) )
#define  A_TAN(x)     (float)( tan( (double)(x) ) )
#define  A_ASIN(x)    (float)( asin( (double)(x) ) )
#define  A_ACOS(x)    (float)( acos( (double)(x) ) )
#define  A_ATAN(x)    (float)( atan( (double)(x) ) )
#define  A_SQRT(x)    (float)( sqrt( (double)(x) ) )
#define  A_EXP(x)     (float)( exp( (double)(x) ) )
#define  A_POW(x, y)  (float)( pow( (double)(x),(double)(y) ) )

/* define structures used in AODT library */

struct ringdata {
  float dist;
  float angle;
  float temp;
  struct ringdata *nextrec;
};

struct datagrid {
  float temp[500][500];
  float lat[500][500];
  float lon[500][500];
  int   numx;
  int   numy;
};

#include "odtapi.h"
