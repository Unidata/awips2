/************************************************************************
 * proto_gpc.h                                                      	*
 *                                                                      *
 * This include file contains function prototypes for the gpc library.	*
 *									*
 **                                                                     *
 * D.W.Plummer/NCEP	 2/04						*
 * F. J. Yen/NCEP	 1/31	Add clo_blasso				*
 * T. Piper/SAIC	01/07	Added gpc_trpoly			*
 ***********************************************************************/

#ifndef PROTO_GPC
#define PROTO_GPC

/*
 *  gpc prototypes
 */
void	gpc_cvlist ( 	int 		npoly, 
			float 		*px, 
			float 		*py, 
			gpc_vertex_list *contour, 
			int 		*iret );

void	gpc_gvlist ( 	gpc_vertex_list	*contour,
			int 		*npoly, 
			float 		*px, 
			float 		*py, 
			int 		*iret );

float 	gpc_gvarea ( 	gpc_vertex_list *contour );

void	gpc_trpoly (	gpc_polygon	*poly,
			int		scal,
			int		*iret );

void	clo_blasso (    char    	*bndtype,
                        char    	*key,
                        int     	*npts,
                        char    	*btags,
			gpc_polygon     *union_poly,
                        int     	*iret );

#endif /* PROTO_GPC */
