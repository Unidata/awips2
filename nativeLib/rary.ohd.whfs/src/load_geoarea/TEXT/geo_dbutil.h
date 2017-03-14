#ifndef GEO_DBUTIL_H
#define GEO_DBUTIL_H

#include "GeoArea.h"
#include "geo_header_file.h"

#define MAX_RECORD_LEN 240
#define MAX_PTS 12000


/* arbitrary identifiers for tables; used for the geo tables */

#define GEOLINE 14
#define GEOAREA 15

#define UNASSIGNED -1.


/* status settings for database write */

#define INSERT_SUCCESS    24
#define INSERT_FAILED     26
#define DUPLICATE_IGNORED 27
#define MALLOC_FAILED     28




int load_geodata(FILE 	*infile,
		 char 	*geotype,
		 int	rank,
		 int	geotable);

void log_geomsg(char *msgstr,
		FILE *log_file);

void open_geolog(int	geotable,
		 char	*geotype,
		 int	rank,
		 FILE	**log_file);

void check_for_int_latlon(FILE 		*log_file,
			  FILE		*infile,
			  char 		*str,
			  int		linenum,
			  double	*intlat,
			  double	*intlon,
			  int		*save_data_block);


/* for load geoline */

int put_geoline(char 	*line_id,
		char	*name,
		char	*vector_type,
		int	feature_rank,
		int	num_points,
		double	*lat,
		double	*lon);


/* for load geoarea */

int put_geoarea ( const GeoAreaData * pGeoAreaDataNode ) ;

/* for unload geoline */


void export_geoline(FILE	*outfile,
		    char	*vector_type,
		    int		rank);

void write_geoline(FILE		*outfile,
		   char		*id,
		   char 	*name,
		   int		num_points,
		   double 	*lat,
		   double 	*lon);


/* for unload geoarea */

void export_geoarea(FILE	*outfile,
		     char	*boundary_type);


void write_geoarea(FILE		*outfile,
		   char 	*id,
		   char 	*name,
		   double	interior_lat,
		   double	interior_lon,
		   int		num_points,
		   double 	*lat,
		   double 	*lon);


/* utility */

void strip_tblanks(char    *str);
void exit_geoutil(FILE *filePtr);


#endif
