
/************************************************************************
 * proto_cgr.h                                                      	*
 *                                                                      *
 * This include file contains private function prototypes for the       *
 * c files in the CGR library.						*
 *									*
 * Log:									*
 **                                                                     *
 * D.W.Plummer/NCEP	11/03						*
 * D.W.Plummer/NCEP	 2/04	Added polyp_getmap			*
 ***********************************************************************/

#ifndef PROTO_CGR
#define PROTO_CGR

/*
 *  CGR prototypes
 */

POLYPOINT * 	polyp_create ( 	int 		*npin, 
				float 		*xin, 
				float 		*yin );

void		polyp_destroy (	POLYPOINT 	*poly );

void		polyp_dump (	POLYPOINT 	*start );

void		polyp_dup (	POLYGON 	*poly0,
       				POLYGON		*poly1,
				float		tol );

POLYPOINT *	polyp_findpt ( 	POINT 		*point, 
				POLYPOINT 	*first );

void 		polyp_freepts ( int 		*iret );

void		polyp_getpts (	POLYPOINT 	*poly,
       				int		*npo,
				float		*xo,
				float		*yo );

void    	polyp_getmap ( 	MPOLYGON 	*mpoly0, 
				MPOLYGON 	*mpoly1,
				int 		nmap,
				float 		*xmap0, 
				float 		*ymap0,
				float 		*xmap1, 
				float 		*ymap1 );


void 		polyp_link ( 	POLYGON 	*poly0, 
				POLYGON 	*poly1, 
				POLYGON 	*poly_out, 
				int 		*iret ); 

POINT *		polyp_newpt (	float		xo,
				float		yo );

int 		polyp_polyint ( POLYGON 	*p0, 
				POLYGON 	*p1 );

void    	polyp_rmpts ( 	POLYPOINT 	*p0, 
				POLYPOINT 	*p1 );

void 		polyp_segint ( 	SEGMENT 	*s0, 
				SEGMENT 	*s1, 
				int 		*intrsct, 
				double 		*xint, 
				double 		*yint, 
				int 		*iret );

void 		polyp_scale ( 	POLYPOINT 	*start );

double 		polyp_tdist ( 	POLYPOINT 	*start,
       				POLYPOINT	*end );

void 		polyp_union ( 	int 		*process_type, 
				POLYGON 	*poly0, 
				POLYGON 	*poly1, 
				POLYGON 	*poly_out, 
				int 		*iret ); 

void 		polyp_unscale ( POLYPOINT 	*start );


#endif /* PROTO_CGR */
