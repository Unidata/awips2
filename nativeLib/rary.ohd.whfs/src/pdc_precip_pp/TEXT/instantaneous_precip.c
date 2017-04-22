#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

#include "load_PCPP_data.h" 
#include "get_total_precip.h"
#include "instantaneous_precip.h"
#include "print_value_to_file.h"

void create_instantaneous_precip_output_file( CurPC *pcHead, time_t end_timet, int debug )
{
    CurPC ** pcPtr = &pcHead;
	FILE *outputFilePtr = NULL;
	time_t start_timet, indextime;
	struct total_precip total_precipitation;
	short int exact_match_window = CLOSEST_ENDINGTIME_MATCH;
	short int advance = 0;
	int num_pc_records;
	int num_pp_records;
	int len;
	char dbname[30];
	char output_file_name[120];
	float min_percent = 0.0;
	unsigned char precip_settings = PRECIP_PC;
	char process_time_string[ANSI_TIME_LEN + 1];
    int newline = 0;
	
	start_timet = end_timet - SECONDS_PER_DAY;
	
	len = strlen( "db_name" );
	get_apps_defaults( "db_name", &len, dbname, &len );
    
    len = strlen( "pdc_pp_dir" );
    get_apps_defaults( "pdc_pp_dir", &len, output_file_name, &len );
 	
 	sprintf( output_file_name, "%s/geninstantprecip.out", output_file_name );

	outputFilePtr = fopen( output_file_name, "w" );
	
	if ( outputFilePtr == NULL )
	{
		fprintf( stderr, "Unable to open the output file for writing!" );
		exit( -1 );
	}

	timet_to_yearsec_ansi( end_timet, process_time_string );
	
	fprintf( outputFilePtr, "File generated on %s\n####LID PE TS\n####...3_hr_ago_value, 2_hr_ago_value, 1_hr_ago_value, 30_min_ago_value\n%ld %ld\n", process_time_string, start_timet, end_timet );

	while ( *pcPtr != NULL )
	{
		advance = 0;
        newline = 0;
		
		for ( indextime = start_timet; indextime < end_timet; indextime+=SECONDS_PER_HOUR )
		{
			total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										NULL, 
										indextime,
										end_timet,
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );

			if ( indextime == start_timet )
			{
				fprintf( outputFilePtr,"%s %s %s\n", total_precipitation.lid, total_precipitation.PE, total_precipitation.TS );
			}
			
            print_value_to_file( total_precipitation.value, indextime, end_timet, &outputFilePtr, debug, newline );

			if ( indextime == end_timet - 3600 )
			{
				advance = 1;
				newline = 1;
                
				time_t thirty_minute_start_time = end_timet - 1800;
				total_precipitation = get_total_raw_precip( ( RawPC ** ) pcPtr, 
										NULL, 
										thirty_minute_start_time,
										end_timet,
										exact_match_window,
										min_percent,
										precip_settings,
										advance,
										& num_pc_records,
										& num_pp_records );

                print_value_to_file( total_precipitation.value, thirty_minute_start_time, end_timet, &outputFilePtr, debug, newline );

			}
		}
	}									

	if ( fclose (outputFilePtr) != 0 )
	{
		fprintf( stderr, "Error encountered upon closure of the output file!" );
		exit( -1 );
	}
}

/*
void print_value_to_file( float value, time_t time_value, time_t end_timet, FILE **outputFilePtr, int debug )
{
    char process_time_string[ANSI_TIME_LEN + 1];
    char end_time_string[ANSI_TIME_LEN + 1];
    
    timet_to_yearsec_ansi( end_timet, end_time_string );
    
    if ( debug )
    {
        timet_to_yearsec_ansi( time_value, process_time_string );
        
        if (value == -9999 )
        {
            fprintf( *outputFilePtr, "%s - %s|-9999\n", process_time_string, end_time_string );
        }
        else
        {
            fprintf( *outputFilePtr, "%s - %s|%.2f\n", process_time_string, end_time_string, value );
        }
    }
    else
    {
        if (value == -9999 )
        {
            fprintf( *outputFilePtr, "-9999 " );
        }
        else
        {
            fprintf( *outputFilePtr, "%.2f ", value );
        }
    }
}

*/
