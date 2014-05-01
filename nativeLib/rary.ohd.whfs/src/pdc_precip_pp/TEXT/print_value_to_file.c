 #include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

#include "load_PCPP_data.h" 

void print_value_to_file( float value, time_t start_timet, time_t end_timet, FILE **outputFilePtr, int debug, int newline )
{
    char process_time_string[ANSI_TIME_LEN + 1];
    char end_time_string[ANSI_TIME_LEN + 1];
    
    timet_to_yearsec_ansi( end_timet, end_time_string );
    
    if ( debug )
    {
        timet_to_yearsec_ansi( start_timet, process_time_string );
        
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
        if ( newline )
        {
            if (value == -9999 )
            {
                fprintf( *outputFilePtr, "-9999\n" );
            }
            else
            {
                fprintf( *outputFilePtr, "%.2f\n", value );
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
}
