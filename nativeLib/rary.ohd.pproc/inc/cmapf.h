/*
 * cmapf.h
 *
 *  Created on: Aug 26, 2011
 *      Author: snaples
 */

#ifndef CMAPF_H_
#define CMAPF_H_
#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#include "limmath.h"
#include "vector_3.h"
#define REARTH 6367.47  /* NCEP uses this value and so does MDL degribber */
/*#define REARTH 6367.47*/
/*  REARTH=6356.766  from U.S. Standard Atmosphere, 1976
 *  REARTH=6367.47   for spherical earths in GRIB grids
 *  REARTH=6371.2    original assumption for CMAPF routines.
 *                   source lost, probably old NWPC grids.
 */

typedef struct {
double lat,lng;
} lat_lon;

typedef struct {
double x,y;
} x_y;

typedef struct {
  double gamma;
  double rotate[3][3];
  double x0,y0,crotate,srotate,gridszeq;
  double EarthRad;
} maparam;
#ifdef __cplusplus
extern "C" {
#endif
/*Generic definition of maparam structures; called by user-involed
 * initialization routines such as sobstr,stlmbr,etc.*/
void mapf_start(maparam * stcprm, double cone_ang,
		double p_lat, double p_longit,
					 double r_lat, double r_longit) ;    /*mpstrt*/

/* User-invoked initialization routines for first-stage initialization.
 * Sets parameters in maparam stcprm to create projection of type
 * conforming to user specifications.
 */
void sobstr(maparam * stcprm,double p_lat,double p_longit) ;  /*sobstr*/
void stlmbr(maparam * stcprm,double reflat,double reflon) ;   /*stlmbr*/
void stcmap(maparam * stcprm,double tnglat,double reflon) ;   /*stlmbr*/
void sobmrc(maparam * stcprm, double r_lat, double r_longit,
					double anch_lat, double anch_longit) ;
void stvmrc(maparam * stcprm,double r_lat,double r_longit) ;  /*stvmrc*/
void soblmbr(maparam * stcprm, double ref_lat,double ref_long,
					 double an_lat1, double an_long1,
					 double an_lat2, double an_long2) ;

/* User altered radius of Earth.  Default is REARTH; a call to cstrad
 * with a user-defined radius must be made -after- stage 1 initialization
 * (sobstr,stlmbr,stcmap,sobmrc,stvmrc, or soblmbr) and -before- stage 2
 * initialization (stcm1p or stcm2p)
 */
void cstrad (maparam * stcprm, double radius) ;
double cgtrad(maparam * stcprm) ;

/* User-invoked initialization routines for second-stage initialization.
 * Adjusts parameters in maparam stcprm to user specifications on scaling
 * and map orientation.
 */
void stcm2p(maparam * stcprm,
		 double x1, double y1, double xlat1, double xlong1,
		 double x2, double y2, double xlat2, double xlong2) ;
void stcm1p(maparam * stcprm,
		 double x1, double y1, double xlat1, double xlong1,
		 double xlatrf, double xlonrf,
		 double gridsz, double orient) ;


/* User-invoked functions to convert from latitude-longitude coordinate
 * pairs to x-y coordinate pairs on the map
 */
void cll2xy(maparam * stcprm,double lat,double longit,
				  double * x,double *y ) ;
void cxy2ll(maparam * stcprm,double x,double y,
				double * lat,double * longit ) ;

/* Internal routines to move back and forth among the various coordinate
 * systems: user-accessible lat-long, 3d geographic vectors, 3d map-oriented
 * coordinates, canonical two-dimensional map coordinates (xi,eta), and
 * user-accessible two-dimensional scaled map coordinates x,y.
 */
void geog_ll(vector_3d * geog,double * lat,double * longit) ;/*geo_ll*/
vector_3d ll_geog(double lat, double longit) ;               /*ll_geo*/
vector_3d xy_map(maparam * stcprm, double x, double y) ;     /*xymap*/
void map_xe(maparam * stcprm, vector_3d map,x_y xieta[],int mode);   /*map_xe*/
void map_xy(maparam * stcprm,vector_3d map,double * x,double * y) ;
																				 /*map_xy*/
void xe_xy(maparam * stcprm, double xi, double eta,
			double * x,double * y) ;                            /*xe_xy*/
void xy_xe(maparam * stcprm, double x, double y,
			double * xi,double * eta) ;                         /*xy_xe*/
/* internally used routines to rotate position vectors from "geographic" Cartesian
 * coordinates (z-axis to North Pole, x-axis to Greenwich Meridian at equator)
 * to "map-based" coordinates, where the "cut" lies along the x-z circle on the
 * negative x- side
 */
vector_3d basegtom(maparam * stcprm,vector_3d xin) ;         /*basg2m*/
vector_3d basemtog(maparam * stcprm,vector_3d xin) ;         /*basm2g*/

/* User-invoked routines to transform between wind vector components in the
 * geographic coordinate system (c or w) and the map- based grid system (g)
 */
void cg2cxy(maparam * stcprm,double x,double y,
		 double ug,double vg, double * ue,double * vn) ;
void cc2gxy(maparam * stcprm,double x,double y,
		 double ue,double vn, double * ug,double * vg) ;
void cg2cll(maparam * stcprm,double latit,double longit,
		 double ug,double vg, double * ue,double * vn) ;
void cc2gll(maparam * stcprm,double latit,double longit,
		 double ue,double vn, double * ug,double * vg) ;
void cg2wxy(maparam * stcprm,double x,double y,
		 double ug,double vg, double * ue,double * vn) ;
void cw2gxy(maparam * stcprm,double x,double y,
		 double ue,double vn, double * ug,double * vg) ;
void cg2wll(maparam * stcprm,double latit,double longit,
		 double ug,double vg, double * ue,double * vn) ;
void cw2gll(maparam * stcprm,double latit,double longit,
		 double ue,double vn, double * ug,double * vg) ;

/* User-invoked functions to obtain the size - in kilometers on the Earth -
 * of a grid cell of unit size on the map
 */
double cgszxy(maparam * stcprm,double x,double y) ;
double cgszll(maparam * stcprm,double lat,double longit) ;

void cgrnxy(maparam * stcprm,double x, double y,
		double * enx,double * eny, double * enz) ;
void cpolxy(maparam * stcprm,double x, double y,
		double * enx,double * eny, double * enz) ;
void cpolll(maparam * stcprm,double lat, double longit,
		double * enx,double * eny, double * enz) ;
void cgrnll(maparam * stcprm,double lat, double longit,
		double * enx,double * eny, double * enz) ;
void ccrvxy(maparam * stcprm, double x, double y, double * gx, double * gy);
void ccrvll(maparam * stcprm, double lat, double longit,
			      double * gx, double * gy);

/* Internally used routine to obtain the components of the vector "vect" in
 * the coordinate system at map position "point"
 */
void proj_3d(maparam * stcprm,vector_3d * point,vector_3d * vect,
		  double * enx,double * eny,double * enz) ;
/* User invoked routines to assist in the handling of strings of latitude-longitude
 * coordinates to x-y coordinates on a map, possibly crossing "cut" boundaries.
 */
int kcllxy ( maparam * stcprm, lat_lon xltlng[], x_y xypts[], int npts,
             int index[],int kndx) ;
int kcwrap ( maparam * stcprm, lat_lon xltlng[], x_y xypts[] );

/*int n_quad(vector_3d map) ;*/
#ifdef __cplusplus
}
#endif
#ifndef RADPDEG
#define RADPDEG (M_PI/180.)
#define DEGPRAD (180./M_PI)
#endif /*RADPDEG*/
#endif /*CMAPF_H*/


