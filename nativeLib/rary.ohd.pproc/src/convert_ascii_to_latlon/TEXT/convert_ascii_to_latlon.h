#ifndef CONVERT_ASCII_TO_LATLON_H
#define CONVERT_ASCII_TO_LATLON_H

#include "GeneralUtil.h"  /* for get_apps_defaults */
#include "stage3.h"       /* for HRAP definition */


/* defines */

#define  TRUE    1
#define  FALSE   0
#define  MAXCHAR 121
#define  MAXPOINTS 500


/* prototypes */

void convert_ascii_to_latlon(char *ascii_fname, 
			  char *bin_fname);


int write_basin(char 	*id, 
		char 	*name, 
		int 	order, 
		int 	num_pairs, 
		double 	*lat, 
		double 	*lon, 
		FILE 	*outfile);

void set_variables(int *count, 
		   int *num_pairs, 
		   int *first_line,
		   int *end_of_pts);

char *build_filename(char *, char *);  /* where is this */

#endif
