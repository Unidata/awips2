#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

#include "load_PCPP_data.h" 
#include "get_total_precip.h"
#include "accum_precip.h"
#include "CodeTimer.h"
#include "print_value_to_file.h"

enum PDCprocessPrecip { ProcessPC, ProcessPP };

void create_old_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, 
                                        time_t end_timet, int hours_to_process, int debug )
{
    RawPC ** pcPtr = &pcHead;
    RawPP ** ppPtr = &ppHead;
    FILE *outputFilePtr = NULL;
    time_t start_timet;
    time_t loopend_timet;
    time_t indextime;
    struct total_precip total_precipitation;
    short int exact_match_window = CLOSEST_ENDINGTIME_MATCH;
    
   // #define EXACT_ENDINGTIME_MATCH   -1
	//#define CLOSEST_ENDINGTIME_MATCH -2
	//#define LATEST_ENDINGTIME_MATCH  -3
    
    short int advance = 0;
    int num_pc_records;
    int num_pp_records;
   // int hours_to_process = 0;
 //   char num_hours_to_process_string[5];
    int len;
    char output_file_name[120];
    float min_percent = 0.0;
    unsigned char precip_settings = PRECIP_PE_BEST & PRECIP_PC;
    
    char process_time_string[ANSI_TIME_LEN + 1];
    char start_time_string[ANSI_TIME_LEN + 1];
    char end_time_string[ANSI_TIME_LEN + 1];
    
    int newline = 0;
    codetimer totaltimer;
    int counter = 0;

    loopend_timet = get_1_hour_loop_end_timet( end_timet );
    
  //  len = strlen( "pdc_precip_hours" );
  //  get_apps_defaults( "pdc_precip_hours", &len, num_hours_to_process_string, &len );
 //   hours_to_process = atoi( num_hours_to_process_string );
    
    start_timet = loopend_timet - hours_to_process*SECONDS_PER_HOUR;
    
    len = strlen( "pdc_pp_dir" );
    get_apps_defaults( "pdc_pp_dir", &len, output_file_name, &len );
    
    sprintf( output_file_name, "%s/gen1hourprecip.out", output_file_name );

    outputFilePtr = fopen( output_file_name, "w" );
    
    if ( outputFilePtr == NULL )
    {
        fprintf( stderr, "Unable to open the output file for writing!" );
        exit( -1 );
    }
    
    time_t real_starttimet = start_timet + SECONDS_PER_HOUR;
    time_t real_endtimet = loopend_timet + SECONDS_PER_HOUR;
    
    timet_to_yearsec_ansi( end_timet, process_time_string );
    timet_to_yearsec_ansi( real_endtimet, end_time_string );
    timet_to_yearsec_ansi( real_starttimet, start_time_string );
  
   
    
    fprintf( outputFilePtr, "File generated on %s\n####LID PE TS\n####."
                            "..4_hr_ago_value 3_hr_ago_value" 
                            " 2_hr_ago_value 1_hr_ago_value\n"
                            "#### start time = %s,  end time = %s\n"
                            "%ld %ld\n",
                             process_time_string, 
                             start_time_string, end_time_string,
                             real_starttimet, real_endtimet
                            );

 
    while ( ( *pcPtr != NULL ) || ( *ppPtr != NULL ) )
    {
        advance = 0;
        newline = 0;
        for ( indextime = start_timet; indextime < loopend_timet; indextime+=SECONDS_PER_HOUR )
        {
            if ( indextime == ( loopend_timet - SECONDS_PER_HOUR ) )
            {
                advance = 1;
                newline = 1;
            }
            counter++;
            restart_timer( &totaltimer );
            total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
                                                        ( RawPP ** ) ppPtr, 
                                                        indextime,
                                                        (indextime + SECONDS_PER_HOUR),
                                                        exact_match_window,
                                                        min_percent,
                                                        precip_settings,
                                                        advance,
                                                        & num_pc_records,
                                                        & num_pp_records );

            stop_timer( &totaltimer );
            if ( indextime == start_timet )
            {
                fprintf( outputFilePtr,"%s %s %s\n", total_precipitation.lid, total_precipitation.PE, total_precipitation.TS );
            }

            print_value_to_file( total_precipitation.value, indextime, (indextime + SECONDS_PER_HOUR), &outputFilePtr, debug, newline );
        }
    }                                   
    
    print_elapsed_time( &totaltimer, "Total time in get_total_raw_precip() was ", NULL );
    printf( "HourlyPrecip: get_total_raw_precip() called %d times\n", counter );
    if ( fclose (outputFilePtr) != 0 )
    {
        fprintf( stderr, "Error encountered upon closure of the output file!" );
        exit( -1 );
    }
}

/* --------------------------------------------------------------------------------------------- */


/* --------------------------------------------------------------------------------------------- */


void create_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,  
                                            time_t end_timet, int hours_to_process,
                                            int debug )
{
    
    create_multi_hourly_precip_output_file(pcHead, ppHead,
                                           1, end_timet, hours_to_process,
                                           get_1_hour_loop_end_timet, 
                                           debug);
        
}

void create_three_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,  
                                            time_t end_timet, int hours_to_process,
                                            int debug )
{
	
	create_multi_hourly_precip_output_file(pcHead, ppHead,
										   3, end_timet, hours_to_process,
										   get_3_hour_loop_end_timet, 
										   debug);
    	
}

void create_six_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,
                                           time_t end_timet, int hours_to_process,
                                           int debug )
{
	
	//create_old_six_hourly_precip_output_file(pcHead, ppHead,  end_timet, debug );
	create_multi_hourly_precip_output_file(pcHead, ppHead,
										   6, end_timet, hours_to_process,
										   get_6_hour_loop_end_timet,
										   debug);
}

void create_twenty_four_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,
                                                   time_t end_timet,  int hours_to_process,
                                                   int debug )
{

   // create_old_twenty_four_hourly_precip_output_file(pcHead, ppHead,  end_timet, debug );
    create_multi_hourly_precip_output_file(pcHead, ppHead, 
                                          24, end_timet, hours_to_process,
                                          get_24_hour_loop_end_timet, 
                                          debug);

	
}




void create_multi_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead,
											 int time_step_in_hours, time_t end_timet, int hours_to_process,
											 GetLoopEndTimeFunctionPtr getLoopEndTimeFunctionPtr, 
											 int debug )

{ 
    enum PDCprocessPrecip precip_source;
    char * pcLid = NULL;
    char * ppLid = NULL;
    
    RawPC ** pcPtr = &pcHead;
	RawPP ** ppPtr = &ppHead;
    
    struct total_precip total_precipitation;
    unsigned char precip_settings = PRECIP_TS_SINGLE;
    
    short int exact_match_window = EXACT_ENDINGTIME_MATCH;
    
    short int advance = 0;
        
    
	FILE *outputFilePtr = NULL;
    
    time_t start_timet;
    time_t loopend_timet;
    time_t indextime;
    
    char process_time_string[ANSI_TIME_LEN + 1];
    char start_time_string[ANSI_TIME_LEN + 1];
    char end_time_string[ANSI_TIME_LEN + 1];
    

    int string_compare_result = 0;
    
	int num_pc_records;
	int num_pp_records;
	int len;
	char output_file_name[120];
	float min_percent = 0.0;
	
    int newline = 0;
    int counter = 0;

    int indexTimeIncrement = time_step_in_hours*SECONDS_PER_HOUR;

	loopend_timet = getLoopEndTimeFunctionPtr( end_timet );
	 
	start_timet = loopend_timet - hours_to_process*SECONDS_PER_HOUR;
	
    len = strlen( "pdc_pp_dir" );
    get_apps_defaults( "pdc_pp_dir", &len, output_file_name, &len );
 	
 	sprintf( output_file_name, "%s/gen%dhourprecip.out", output_file_name, time_step_in_hours );

	outputFilePtr = fopen( output_file_name, "w" );
	
	if ( outputFilePtr == NULL )
	{
		fprintf( stderr, "Unable to open the output file for writing!" );
		exit( -1 );
	}
	
    time_t real_starttimet = start_timet + indexTimeIncrement;
    time_t real_endtimet = loopend_timet + indexTimeIncrement;
    
	timet_to_yearsec_ansi( end_timet, process_time_string );
    
    timet_to_yearsec_ansi( real_starttimet, start_time_string );
    timet_to_yearsec_ansi( real_endtimet, end_time_string );
	
	fprintf( outputFilePtr, "File generated on %s\n####LID PE TS\n####...%d_hour_value(%d hours ago)" 
	                       " %d_hour_value(%d hours ago) %d_hour_value\n##### start time = %s end time = %s" 
                           " current_value\n%ld %ld\n",
	                        process_time_string,
	                        time_step_in_hours, 2*time_step_in_hours,
	                        time_step_in_hours, time_step_in_hours,
	                        time_step_in_hours, 
                            start_time_string, end_time_string,
	                        real_starttimet, real_endtimet );

	while ( ( *pcPtr != NULL ) || ( *ppPtr != NULL ) )
	{
		advance = 0;
		newline = 0;
        
        pcLid = NULL;
        ppLid = NULL;

        if ( *pcPtr != NULL )
        {
            pcLid = (*pcPtr)->lid;
        }

        if ( *ppPtr != NULL )
        {
            ppLid = (*ppPtr)->lid;
        }

        if ( pcLid == NULL )
        {
            precip_source = ProcessPP;
        }
        else if ( ppLid == NULL )
        {
            precip_source = ProcessPC;
        }
        else
        {
            string_compare_result = strcmp ( pcLid, ppLid );

            if ( string_compare_result <= 0 )
            {
                precip_source = ProcessPC;
            }
            else
            {
                precip_source = ProcessPP;
            }
        }
        
		for ( indextime = start_timet; indextime < loopend_timet; indextime += indexTimeIncrement )
		{ 
            counter++;
            
            if ( precip_source == ProcessPC )
            {
                total_precipitation = get_total_raw_precip ((RawPC **) pcPtr,
                                                        NULL,
                                                        indextime,
                                                        (indextime + indexTimeIncrement),
                                                        exact_match_window,
                                                        min_percent,
                                                        precip_settings,
                                                        advance,
                                                        &num_pc_records,
                                                        &num_pp_records);
            }
            else
            {
                total_precipitation = get_total_raw_precip (NULL,
                                                        (RawPP **) ppPtr,
                                                        indextime,
                                                        (indextime + indexTimeIncrement),
                                                        exact_match_window,
                                                        min_percent,
                                                        precip_settings,
                                                        advance,
                                                        &num_pc_records,
                                                        &num_pp_records);
            }
            
			/*
			total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										( RawPP ** ) ppPtr, 
										indextime,
										(indextime + indexTimeIncrement ),
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );
            */
            
            
			if ( indextime == start_timet )
			{
				fprintf( outputFilePtr,"%s %s %s\n", total_precipitation.lid,
                                                     total_precipitation.PE, 
                                                     total_precipitation.TS );
			}

            print_value_to_file( total_precipitation.value,
                                 indextime, 
                                (indextime + indexTimeIncrement),
                                 &outputFilePtr, debug, newline );
			
			if ( indextime == loopend_timet -  indexTimeIncrement )
			{
				advance = 1;
                newline = 1;
                
                if ( precip_source == ProcessPC )
                {
                     total_precipitation = get_total_raw_precip ((RawPC **) pcPtr,
                                                        NULL,
                                                        loopend_timet,
                                                        end_timet,
                                                        exact_match_window,
                                                        min_percent,
                                                        precip_settings,
                                                        advance,
                                                        &num_pc_records,
                                                        &num_pp_records);
                }
                else
                {
                     total_precipitation = get_total_raw_precip (NULL,
                                                        (RawPP **) ppPtr,
                                                        loopend_timet,
                                                        end_timet,
                                                        exact_match_window,
                                                        min_percent,
                                                        precip_settings,
                                                        advance,
                                                        &num_pc_records,
                                                        &num_pp_records);
                }
            
                
				/*
				total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										( RawPP ** ) ppPtr, 
										loopend_timet,
										end_timet,
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );
                */
                
                if ( loopend_timet != end_timet )
                {
                    print_value_to_file( total_precipitation.value,
                                         loopend_timet, end_timet,
                                          &outputFilePtr, debug, newline );
                                         
                }
                else
                {
                    fprintf( outputFilePtr, "OTH\n" );
                }
			}
		}
		

	}									

		
	printf( "create_multi_hourly_precip_output_file() for hours_to_accum = %d:"  
            " get_total_raw_precip called %d times\n",time_step_in_hours, counter );
    
	if ( fclose (outputFilePtr) != 0 )
	{
		fprintf( stderr, "Error encountered upon closure of the output file!" );
		exit( -1 );
	}
}

time_t get_loop_end_timet(time_t end_timet, int time_step_interval_in_hours)
{
	
	time_t loop_end_timet = end_timet;
    int rounding_seconds = time_step_interval_in_hours * SECONDS_PER_HOUR;

	loop_end_timet /= rounding_seconds; 
	loop_end_timet *= rounding_seconds; 
	
	return loop_end_timet;
} 


time_t get_1_hour_loop_end_timet( time_t end_timet )
{
	return get_loop_end_timet(end_timet, 1);
}

time_t get_3_hour_loop_end_timet( time_t end_timet )
{
	return get_loop_end_timet(end_timet, 3);
}

time_t get_6_hour_loop_end_timet( time_t end_timet )
{
	return get_loop_end_timet(end_timet, 6);
}


time_t get_24_hour_loop_end_timet( time_t end_timet )
{
	time_t loop_end_timet = end_timet;
	
	loop_end_timet /= 24*SECONDS_PER_HOUR; //24 hours * 3600 seconds per hour = 1 day
	loop_end_timet *= 24*SECONDS_PER_HOUR;
	loop_end_timet += 12*SECONDS_PER_HOUR; //sets the datetime to date/12z
	
	//We want the loop_end_timet to end before the final end time,
	//because there is a final, partial period calculation for the last incomplete, period, if one exists.
	//This calculation is outside the loop.
	if ( loop_end_timet  > end_timet  ) 
	{
		loop_end_timet -= 24*SECONDS_PER_HOUR;
	}
	return loop_end_timet;
}



time_t get_old_oth_hour_loop_end_timet( time_t end_timet )
{
    time_t loop_end_timet = end_timet;
    
    loop_end_timet /= SECONDS_PER_HOUR; //24 hours * 3600 seconds per hour = 1 day
    loop_end_timet *= SECONDS_PER_HOUR; // gives the 0Z datetime e.g. 01-02-2006 0000Z

    return loop_end_timet;    
}


time_t get_old_three_hour_loop_end_timet( time_t end_timet )
{
	time_t hours_to_accum = 3;
	time_t loop_end_timet = end_timet;
	time_t time_diff;
	loop_end_timet /= SECONDS_PER_DAY; //24 hours * 3600 seconds per hour = 1 day
	loop_end_timet *= SECONDS_PER_DAY; // gives the 0Z datetime e.g. 01-02-2006 0000Z
	
	time_diff = end_timet - loop_end_timet;
	
	time_t max_diff = 24*SECONDS_PER_HOUR;
	time_t check_diff = max_diff;
	
	// for 3 hours, checks 21, 18,15,12,9,6,3,hours
	// for 6 hours checks 18,12,6,
	for (check_diff = max_diff; check_diff > 0; check_diff -= (hours_to_accum *  SECONDS_PER_HOUR))
	{
		if (time_diff >= check_diff)
		{
		    loop_end_timet += check_diff;
		    break;	
		}
		
	}
	

	return loop_end_timet;
}


time_t get_old_six_hour_loop_end_timet( time_t end_timet )
{
	time_t loop_end_timet = end_timet;
	time_t time_diff;
	loop_end_timet /= SECONDS_PER_DAY; //24 hours * 3600 seconds per hour = 1 day
	loop_end_timet *= SECONDS_PER_DAY; // gives the 0Z datetime e.g. 01-02-2006 0000Z
	
	time_diff = end_timet - loop_end_timet;
	if ( time_diff >= 18*SECONDS_PER_HOUR )
	{
		loop_end_timet += 18*SECONDS_PER_HOUR;
	}
	else if ( time_diff >= 12*SECONDS_PER_HOUR )
	{
		loop_end_timet += 12*SECONDS_PER_HOUR;
	}
	else if ( time_diff >= 6*SECONDS_PER_HOUR )
	{
		loop_end_timet += 6*SECONDS_PER_HOUR;
	}

	return loop_end_timet;
}






void create_old_six_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int debug )
{
    RawPC ** pcPtr = &pcHead;
	RawPP ** ppPtr = &ppHead;
	FILE *outputFilePtr = NULL;
    time_t start_timet, loopend_timet, indextime;
	struct total_precip total_precipitation;
	short int exact_match_window = CLOSEST_ENDINGTIME_MATCH;
	short int advance = 0;
	int num_pc_records;
	int num_pp_records;
	int hours_to_process = 0;
	char num_hours_to_process_string[5];
	int len;
	char output_file_name[120];
	float min_percent = 0.0;
	unsigned char precip_settings = PRECIP_PC;
	char process_time_string[ANSI_TIME_LEN + 1];
    int newline = 0;
    int counter = 0;


	loopend_timet = get_6_hour_loop_end_timet( end_timet );
	
    len = strlen( "pdc_precip_hours" );
    get_apps_defaults( "pdc_precip_hours", &len, num_hours_to_process_string, &len );
	hours_to_process = atoi( num_hours_to_process_string );
	start_timet = loopend_timet - hours_to_process*SECONDS_PER_HOUR;
	
    len = strlen( "pdc_pp_dir" );
    get_apps_defaults( "pdc_pp_dir", &len, output_file_name, &len );
 	
 	sprintf( output_file_name, "%s/gen6hourprecip.out", output_file_name );

	outputFilePtr = fopen( output_file_name, "w" );
	
	if ( outputFilePtr == NULL )
	{
		fprintf( stderr, "Unable to open the output file for writing!" );
		exit( -1 );
	}
	
	timet_to_yearsec_ansi( end_timet, process_time_string );
	
	fprintf( outputFilePtr, "File generated on %s\n####LID PE TS\n####..."
                            "6_hour_value(12 hours ago) 6_hour_value(6 hours ago)"
                            " 6_hour_value current_value\n%ld %ld\n",
                             process_time_string, start_timet, end_timet );

	while ( ( *pcPtr != NULL ) || ( *ppPtr != NULL ) )
	{
		advance = 0;
		newline = 0;
        
		for ( indextime = start_timet; indextime < loopend_timet; indextime+=6*SECONDS_PER_HOUR )
		{
			counter++;
			total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										( RawPP ** ) ppPtr, 
										indextime,
										(indextime + 6*SECONDS_PER_HOUR),
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );

			if ( indextime == start_timet )
			{
				fprintf( outputFilePtr,"%s %s %s\n", total_precipitation.lid,
                           total_precipitation.PE, total_precipitation.TS );
			}

            print_value_to_file( total_precipitation.value, indextime,
                                (indextime + 6*SECONDS_PER_HOUR), &outputFilePtr, debug, newline );
			
			if ( indextime == loopend_timet - ( 6 * SECONDS_PER_HOUR ) )
			{
				advance = 1;
                newline = 1;
				
				total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										( RawPP ** ) ppPtr, 
										loopend_timet,
										end_timet,
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );
                if ( loopend_timet != end_timet )
                {
                    print_value_to_file( total_precipitation.value, loopend_timet, 
                                         end_timet, &outputFilePtr, debug, newline );
                }
                else
                {
                    fprintf( outputFilePtr, "OTH\n" );
                }
			}
		}
		

	}									

		
	printf( "create_six_hourly_precip_output_file(): get_total_raw_precip called %d times\n", counter );
    
	if ( fclose (outputFilePtr) != 0 )
	{
		fprintf( stderr, "Error encountered upon closure of the output file!" );
		exit( -1 );
	}
}


void create_old_twenty_four_hourly_precip_output_file( RawPC *pcHead, RawPP *ppHead, time_t end_timet, int debug )
{
    RawPC ** pcPtr = &pcHead;
    RawPP ** ppPtr = &ppHead;
	FILE *outputFilePtr = NULL;
	time_t start_timet, loopend_timet, indextime;
	struct total_precip total_precipitation;
	short int exact_match_window = CLOSEST_ENDINGTIME_MATCH;
	short int advance = 0;
	int num_pc_records;
	int num_pp_records;
	int len;
	char output_file_name[120];
	float min_percent = 0.0;
	unsigned char precip_settings = PRECIP_PC;
	char process_time_string[ANSI_TIME_LEN + 1];
    int hours_to_process = 0;
    char num_hours_to_process_string[5];
    int newline = 0;
    int counter = 0;
    
	loopend_timet = get_24_hour_loop_end_timet( end_timet );
    
    len = strlen( "pdc_precip_hours" );
    get_apps_defaults( "pdc_precip_hours", &len, num_hours_to_process_string, &len );
    hours_to_process = atoi( num_hours_to_process_string );
    start_timet = loopend_timet - hours_to_process*SECONDS_PER_HOUR;
	
    len = strlen( "pdc_pp_dir" );
    get_apps_defaults( "pdc_pp_dir", &len, output_file_name, &len );
 	
 	sprintf( output_file_name, "%s/gen24hourprecip.out", output_file_name );

	outputFilePtr = fopen( output_file_name, "w" );
	
	if ( outputFilePtr == NULL )
	{
		fprintf( stderr, "Unable to open the output file for writing!" );
		exit( -1 );
	}
	
	timet_to_yearsec_ansi( end_timet, process_time_string );
	
	fprintf( outputFilePtr, "File generated on %s\n####LID PE TS\n####..." 
                            "3_day_value 2_day_value 1_day_value current_value\n%ld %ld\n",
                             process_time_string, start_timet, end_timet );

	while ( ( *pcPtr != NULL ) ||  ( *ppPtr != NULL ) )
	{
		advance = 0;
        newline = 0;
		
		for ( indextime = start_timet; indextime < loopend_timet; indextime+=24*SECONDS_PER_HOUR )
		{
			total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										( RawPP ** ) ppPtr, 
										indextime,
										(indextime + SECONDS_PER_DAY ),
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );

			if ( indextime == start_timet )
			{
				fprintf( outputFilePtr,"%s %s %s\n", 
                         total_precipitation.lid, total_precipitation.PE,
                          total_precipitation.TS );
			}

            print_value_to_file( total_precipitation.value, indextime, (indextime + SECONDS_PER_DAY ), &outputFilePtr, debug, newline );		
            
			if ( indextime == ( loopend_timet - ( 24 * SECONDS_PER_HOUR ) ) )
			{
				advance = 1;
                newline = 1;
				
				counter++;
   				total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
   										( RawPP ** ) ppPtr, 
   										loopend_timet,
   										end_timet,
   										exact_match_window,
   										min_percent,
   										precip_settings,
   										advance,
    									& num_pc_records,
    									& num_pp_records );
                    
                if ( loopend_timet != end_timet )
                {
                    print_value_to_file( total_precipitation.value, loopend_timet, end_timet, &outputFilePtr, debug, newline );     
                }
                else 
                {
                    fprintf( outputFilePtr, "OTH\n" );
                }
            
			}
		}
		
		
	}									

		
	printf( "create_twenty_four_hourly_precip_output_file(): get_total_raw_precip called %d times\n", counter );	
									
	if ( fclose (outputFilePtr) != 0 )
	{
		fprintf( stderr, "Error encountered upon closure of the output file!" );
		exit( -1 );
	}
}
