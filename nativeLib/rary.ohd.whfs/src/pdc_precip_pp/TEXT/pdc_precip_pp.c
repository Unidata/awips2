#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

#include "pdc_precip_pp.h"
#include "Location.h"
#include "load_PCPP_data.h"
#include "get_total_precip.h"
#include "CodeTimer.h"
#include "time_convert.h"


int pdc_precip_main(int argc, const char ** argv)
{
    time_t end_timet;
    int hours_to_process = 0;

    codetimer overall_timer;
    codetimer get_data_timer;
    codetimer instanttimer;

    codetimer three_hour_timer;
    codetimer six_hour_timer;
    codetimer twenty_four_hour_timer;
    codetimer raw_hourly_timer;

    RawPC * pcHead = NULL;
    RawPP * ppHead = NULL;

    int debug = 0;

    start_timer( &overall_timer );

    process_command_line_arguments( argc, argv, &end_timet, &hours_to_process,  &debug );


    printf( "\n%s %s - %s\n", pdcprecip_name, pdcprecip_ver, pdcprecip_date );

    printf( "debug = %d\n", debug );
    fflush(stdout);

    start_timer( &get_data_timer );
    retrieve_data( &pcHead, &ppHead, end_timet, hours_to_process, debug );
    stop_timer_and_print_elapsed_time( &get_data_timer, "Finished reading RawPC and RawPP precip data in", NULL );

    start_timer( &instanttimer );
    create_instantaneous_precip_output_file( pcHead, end_timet, debug );
    stop_timer_and_print_elapsed_time( &instanttimer, "Finished instantaneous precip in", NULL );

    start_timer( &raw_hourly_timer );
    create_hourly_precip_output_file( pcHead, ppHead, end_timet, hours_to_process,  debug );
    stop_timer_and_print_elapsed_time( &raw_hourly_timer, "Finished 1 hour precip in", NULL );

    start_timer( &three_hour_timer );
	create_three_hourly_precip_output_file( pcHead, ppHead, end_timet, hours_to_process, debug );
    stop_timer_and_print_elapsed_time( &three_hour_timer, "Finished 3 hourly precip in", NULL );

    start_timer( &six_hour_timer );
	create_six_hourly_precip_output_file( pcHead, ppHead, end_timet, hours_to_process, debug );
    stop_timer_and_print_elapsed_time( &six_hour_timer, "Finished 6 hourly precip in", NULL );

    start_timer( &twenty_four_hour_timer );
	create_twenty_four_hourly_precip_output_file( pcHead, ppHead, end_timet, hours_to_process, debug );
    stop_timer_and_print_elapsed_time( &twenty_four_hour_timer, "Finished 24 hourly precip in", NULL );

   // overall timer
    stop_timer_and_print_elapsed_time( &overall_timer, "\nOverall pdc_precip_pp time was ", NULL );


  //  codetimer *dt_timer = NULL;
  //  dt_timer = get_timer();
  //  print_elapsed_time( dt_timer, "\n yearsec_dt_to_timet's portion of that time was ", NULL );


    exit( 1 );
}


void process_command_line_arguments( int argc, char *argv[],
                                     time_t *end_timet,
                                     int * num_hours_to_process, int *debug )
{
 	int c;
	time_t ending_time;

	if (argc <= 1)
    {
       printf( "Usage: pdc_precip_pp -e<end time> -h<hours_to_process> [-t (testing mode) ]\n" );
       exit(-1);
    }

    while ( (c = getopt( argc, argv, ":e:h:td" ) ) != -1)
    {
    	switch (c)
    	{
    		case 'e':
    			*end_timet = (time_t) atol( optarg );
    			break;
            case 'h':
                *num_hours_to_process = (int) atoi(optarg);
                break;
    		case 't':
    		    time( &ending_time );
    			*end_timet = ending_time;
    			break;

            case 'd':
                *debug = 1;
                break;
    		case ':':
				printf( "Must specify a valid ending_time\n" );
    		    exit( -1 );
    	}
    }

    //ensure minimum hours to process
    if (*num_hours_to_process < 24)
    {
        printf( "original num_hours_to_process = %d\n", *num_hours_to_process );
        *num_hours_to_process = 24;
        printf( "adjusted num_hours_to_process = %d\n", *num_hours_to_process );
    }


    char end_time_string[ANSI_TIME_LEN + 1];

    timet_to_yearsec_ansi( *end_timet, end_time_string );

    printf( "end_timet = %ld end_time_string = %s\n", *end_timet, end_time_string );
}

void retrieve_data( RawPC **pcHead, RawPP **ppHead, time_t end_timet, int hours_to_process, int debug )
{
    time_t start_timet;
    char * pLid = NULL;
    const char ** pTs = NULL;
    int num_ts = 0;
    int record_count = 0;
   // char num_hours_to_process_string[5];
    int len;
    int dbstatus = 0;
    char dbname[30];
    codetimer querytimer;

    int extra_hours = 4; //extra hours to ensure data receipt


    len = strlen( "db_name" );
    get_apps_defaults( "db_name", &len, dbname, &len );
    if ( ( dbstatus = OpenDbms( dbname ) ) != 0)
    {
       printf("Error %d opening database: %s\n", dbstatus, dbname);
       exit(-1);
    }

  //  len = strlen( "pdc_precip_hours" );
  //  get_apps_defaults( "pdc_precip_hours", &len, num_hours_to_process_string, &len );

  //  hours_to_process = atoi( num_hours_to_process_string );

    if ( hours_to_process > 0 )
    {
        start_timet = end_timet - ( hours_to_process + extra_hours) * SECONDS_PER_HOUR;
    //       start_timet = end_timet - ( hours_to_process + 24 ) * SECONDS_PER_HOUR;

        if ( debug )
        {
            restart_timer( &querytimer );
        }

        *pcHead = load_PC_raw( start_timet, end_timet, pLid,
                                        pTs, num_ts, RawPrecip,
                                        &record_count );
        if ( debug )
        {
            printf( "PC record count = %d\n", record_count );
        }
        *ppHead = load_PP_raw( start_timet, end_timet, pLid,
                                        pTs, num_ts, RawPrecip,
                                        &record_count );
        if ( debug )
        {
            printf( "PP record count = %d\n", record_count );
            fflush(stdout);
            stop_timer_and_print_elapsed_time( &querytimer, "Finished PC and PP query in ", NULL );
        }
    }
    else
    {
        printf( "pdc_precip_hours is not greater than 0\nExiting...\n" );
        exit( 1 );
    }
}

/* -------------------------------------------------------------------------------------*/

void retrieve_hourly_data( HourlyPC **pcHead, HourlyPP **ppHead, time_t end_timet, int debug )
{
    time_t start_timet;
    char * pLid = NULL;
    const char ** pTs = NULL;
    int num_ts = 0;
    int record_count = 0;
    char num_hours_to_process_string[5];
    int len;
    int hours_to_process = 0;
    int dbstatus = 0;
    char dbname[30];
    codetimer querytimer;

    len = strlen( "db_name" );
    get_apps_defaults( "db_name", &len, dbname, &len );
    if ( ( dbstatus = OpenDbms( dbname ) ) != 0)
    {
       printf("Error %d opening database: %s\n", dbstatus, dbname);
       exit(-1);
    }

    len = strlen( "pdc_precip_hours" );
    get_apps_defaults( "pdc_precip_hours", &len, num_hours_to_process_string, &len );

    hours_to_process = atoi( num_hours_to_process_string );

    if ( hours_to_process > 0 )
    {
        start_timet = end_timet - ( hours_to_process) * SECONDS_PER_HOUR;

        if ( debug )
        {
            restart_timer( &querytimer );
        }

        *pcHead =  load_PC_hourly( start_timet, end_timet, pLid,
                                        pTs, num_ts, &record_count );

        printf( "Hourly PC record count = %d\n", record_count );
        if ( debug )
        {
            printf( "Hourly PC record count = %d\n", record_count );
        }
        *ppHead = load_PP_hourly( start_timet, end_timet, pLid,
                                        pTs, num_ts, &record_count );

        printf( "Hourly PP record count = %d\n", record_count );
        if ( debug )
        {
            printf( "Hourly PP record count = %d\n", record_count );
            fflush(stdout);
            stop_timer_and_print_elapsed_time( &querytimer, "Finished Hourly PC and PP query in ", NULL );
        }
    }
    else
    {
        printf( "pdc_precip_hours is not greater than 0\nExiting...\n" );
        exit( 1 );
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
