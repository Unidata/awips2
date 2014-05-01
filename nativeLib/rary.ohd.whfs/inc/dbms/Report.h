#ifndef REPORT_H
#define REPORT_H

#include <time.h>
#include "DbmsDefs.h"

typedef struct Report
{
 	char	pe[SHEF_PE_LEN + 1];
 	long	dur; 		
	char 	ts[SHEF_TS_LEN + 1];
        char	extremum[SHEF_EX_LEN + 1];
	float	probability;
        
        char 	shef_qual_code[SHEF_QC_LEN + 1];
        long	quality_code;
        
        double 	value;
        time_t 	validtime;
	time_t 	basistime;
} Report;


#endif
