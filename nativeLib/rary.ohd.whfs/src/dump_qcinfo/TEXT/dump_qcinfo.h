#ifndef CHECK_DUMP_QCINFO_H
#define CHECK_DUMP_QCINFO_H

#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "time_convert.h"

#include "Observation.h"

#include "QualityCode.h"

#define MISSING_VAL -9999.

#define DEFAULT_HOURS 24

#define INFORMIX_DUPLICATE           -239
#define INFORMIX_DUPLICATEL          -268

typedef struct _options{
	  char		tabname[20];
          time_t	end_timet;     
          int		numhrs;
	  int		show_errs_only;
	  char		lid[LOC_ID_LEN +1];
	  char		pe[SHEF_PE_LEN + 1];
          int           test_qcode;
	  }
 options;


/************************************
  Declaration of function prototypes 
 ************************************/

void get_dump_options(int		argc,
		       char		**argv,
                       char             *dbname,
		       options          *Options);

void get_timestamp(char *timestamp);

void do_qcinfo_dump(options    Options,
		    time_t     *tnow,
                    FILE       *logfilePtr);


Observation * get_dump_data(options   Options,
                            FILE      *logfilePtr); 
			    
void dump_qc_bit_descr(long quality_code,
                       FILE *logfilePtr);

void dump_qcbits(long quality_code,
		 FILE *logfilePtr);
			
void list_bitdescr(FILE *logfilePtr);

#endif
