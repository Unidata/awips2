#ifndef READ_RADARGRIDS_H
#define READ_RADARGRIDS_H 


/* defines */

#define MISSING_STAGE2	-99.

#include "ArealDataAttr.h"
#include "DPARadar.h"


/* protos */

int read_stage1grid(char        *radid,
                    time_t      gridtime,
                    PrecipType  precipType,
                    int         *zero_flag,
                    float       *bias,
                    float       *grid_vals);
                    
int read_stage1file(DPARadar	*dpaHead,
		    float 	*grid_vals);

int read_latestgrid(char 	*radid,
		    char	*pe,
		    time_t	duration,
		    char	*use_ts,
		    int		*zero_flag,
		    time_t	*gridtime, 
		    float 	*grid_vals);

#endif
