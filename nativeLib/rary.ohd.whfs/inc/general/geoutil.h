#ifndef GEOUTIL_H
#define GEOUTIL_H

#include <stdio.h>

#include "DbmsDefs.h"
#include "GeoArea.h"

/* Structure for "get_area_linesegs" routine. */
typedef struct GeoAreaLineSegs
{
   Node node ;  /* The node. */
   char area_id [ LOC_ID_LEN + 1 ] ; /* The id of the area being processed. */
   char name [ LOC_AREANAME_LEN + 1 ] ; /* The name of the area being 
                                           processed. */
   int numrows ; /* The number of rows in the geoarea being processed. */
   long * rows ; /* The numbers of the HRAP rows in the area being
                    processed. */
   long * beg_cols ; /* The beginning HRAP column corresponding to each row. */
   long * end_cols ; /* The ending HRAP column corresponding to each row. */
   List list ;       /* The linked list. */
} GeoAreaLineSegs ;

#ifdef __cplusplus
extern "C" {
#endif


void exit_geoutil ( FILE * filePtr ) ;

void extract_geoarea(GeoArea	*gaPtr,
		     double	**lat,
		     double	**lon);


void * get_area_linesegs ( const char * area_type ,
                           size_t size_of_structure ) ;
void free_area_linesegs_list ( void * pHead ) ;

#ifdef __cplusplus
}
#endif

#endif
