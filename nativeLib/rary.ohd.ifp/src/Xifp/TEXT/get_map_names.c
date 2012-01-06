/* File: get_map_names.c
 *
 *  Given the forcast point, obtains the the mean area
 *  precipitation variables.
 *
 *  Input: seg_id
 *
 *  Output: nmap
 *          mapids
 *
 *  Change:  Added MAPX as a datatype for find_datatype_in_ts to search for.
 *           dp - 27 Oct. 97
 */
#include "c_call_f/read_ts_array.h"/* -- added by AV --*/

#define MAX_TS_ARRAY 10000



int     get_map_names(seg_id, nmap, mapids)

char *  seg_id;    /* segment id, forcast point */
int  *  nmap;      /* number of mean area precipitation points */
char ** mapids;    /* mean area precipitation id */
{
 int    nts;        /* number of time series data */
 float  ts[MAX_TS_ARRAY]; /* time series floating point data */
 char   map_datatype[4], map_datatype2[4];

 strncpy(map_datatype, "MAP ", 4);
 strncpy(map_datatype2, "MAPX", 4);
 
 nts = MAX_TS_ARRAY;

 READ_TS_ARRAY(seg_id, &nts, ts);

 find_datatype_in_ts(mapids, nmap, map_datatype, ts, ts, map_datatype2);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/get_map_names.c,v $";
 static char rcs_id2[] = "$Id: get_map_names.c,v 1.4 2002/02/11 19:10:43 dws Exp $";}
/*  ===================================================  */

}
