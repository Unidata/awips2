#ifndef PDC_PRECIP_PP_H_
#define PDC_PRECIP_PP_H_

#include "instantaneous_precip.h"
#include "accum_precip.h"
#include "load_PCPP_data.h" 


char pdcprecip_name [ ] =  "PDC Precip Preprocessor" ;
char pdcprecip_ver [ ] = "OB8.1" ;
char pdcprecip_date [ ] ="March 27, 2007" ;


void process_command_line_arguments( int argc, char *argv[], time_t *end_timet, int * num_hours_to_process, int *debug );

//void retrieve_data( CurPC **pcHead, CurPP **ppHead, time_t end_timet, int debug );
void retrieve_data( RawPC **pcHead, RawPP **ppHead, time_t end_timet, int hours_to_process, int debug );

void retrieve_hourly_data( HourlyPC **pcHead, HourlyPP **ppHead, time_t end_timet, int debug );

#endif /*PDC_PRECIP_PP_H_*/
