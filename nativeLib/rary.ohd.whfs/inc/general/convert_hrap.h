#ifndef CONVERT_HRAP_H
#define CONVERT_HRAP_H


/* physical and mathematical constants */

#define  DEGRAD              0.017453293
#define  RADIANS_PER_DEGREE  0.017453293
#define  DEGREES_PER_RADIAN 57.29577951
#define  EARTH_RADIUS     6371.2
#define  MESH_LEN            4.7625
#define  STDLAT  60
#define  STDLON 105

#define LOCAL_HRAP_ROWS 131
#define LOCAL_HRAP_COLS 131


/* prototypes */

void LatLongToHrapByReference(double lat,
		   double lon,
		   double *row,
		   double *col);

void HrapToLatLongByReference(double row,
		   double col,
		   double *lat,
		   double *lon);


#endif
