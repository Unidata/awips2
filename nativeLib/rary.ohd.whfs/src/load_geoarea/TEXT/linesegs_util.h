#ifndef LINESEGS_UTIL_H
#define LINESEGS_UTIL_h

#define MAX_NUMBOUND_PTS    5000
#define MAX_ROWS	500

void get_load_linesegs_options(const	int argc,
			       char	**argv,
			       char 	*area_id);

void build_linesegs(const char *area_id);

void write_lineseg(const char *area_id, 
		   const int  numrows,
		   const float area,
		   const long rows[], 
		   const long bcols[], 
		   const long ecols[]);

/* Prototypes for Fortran RFC routines. */
void sfbdrv ( float * x , float * y , float * flat , float * flon ,
              int * iy , int * ixb , int * ixe , int * msegs ,
              int * nbpts , int * lfactr , float * area ,
              float * uarea , float * carea , float * xc ,
              float * yc , char * units , int * nsegs ,
              int * istat ) ;
 

#endif
