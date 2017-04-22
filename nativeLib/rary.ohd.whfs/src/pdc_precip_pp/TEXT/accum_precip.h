#ifndef ACCUM_PRECIP_H_
#define ACCUM_PRECIP_H_

#include "load_PCPP_data.h" 
#include <time.h>
#include <stdlib.h>


void create_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int hours_to_process, int debug );
void create_hourly_precip_output_file_using_hourly( HourlyPC *pcHead, HourlyPP *ppHead,
                                                    time_t end_timet, int hours_to_process, int debug );

void create_three_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int hours_to_process, int debug );

void create_six_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int hours_to_process, int debug );
void create_twenty_four_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int hours_to_process, int debug );

void create_old_six_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int debug );
void create_old_twenty_four_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int debug );


time_t get_1_hour_loop_end_timet( time_t end_timet );
time_t get_3_hour_loop_end_timet( time_t end_timet );
time_t get_6_hour_loop_end_timet( time_t end_timet );

time_t get_loop_end_timet(time_t end_timet, int hours_to_accum);

time_t get_24_hour_loop_end_timet( time_t end_timet );


 
//typedef float(*pt2Func)(float, float);

typedef void (*blah)( );

typedef time_t  (* GetLoopEndTimeFunctionPtr) ( time_t );

//void create_multi_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,
//											int hours_to_accum, time_t end_timet,
//											GetLoopEndTimeFunctionPtr getLoopEndTimeFunctionPtr, 
//											int debug );
                                            
 void create_multi_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,
                                             int hours_to_accum,
                                             time_t end_timet,
                                             int hours_to_process,
                                             GetLoopEndTimeFunctionPtr getLoopEndTimeFunctionPtr, 
                                             int debug ) ;                                        

#endif /*ACCUM_PRECIP_H_*/
