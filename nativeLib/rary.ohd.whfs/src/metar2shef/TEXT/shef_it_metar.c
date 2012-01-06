/*-------------------------------------------------------------------

   Title:    shef_it_metar                                                 
                                                                     
   Purpose:  To encode METAR reports into SHEF.                        
                                                                     
   Author:   David G. Brandon, CBRFC, Salt Lake City, UT             
                                                                     
   Version:  
             1.0   MAR 21, 96 DGB
                   original version 
             1.1   APR 15  96 DGB
                   Add a line feed at the end of the 
                   ZCZC wmo header line.
                   Remove format specifier, %-9.9s, from ZCZC line.
                   Add the -a option which, when set to on, will
                   strip off the first character of the id in 
                   a metar ob.  For example, KPIT becomes PIT.                   
                   Add sea level pressure as an element.
                   Change preciptation type from 'XW' to 'PT' to
                   correspond to shef rules.
                   Add pe codes, PD (pressure characteristic),
                   PE (pressure tendency) and XV (surface visibility).
              1.2  APR 17 96 DGB
                   Change shef code 'PD' to 'PDIR' to make it unique
                   in the shefstr.pecodes.  Before it would print out if the
                   PPD was set, since it found PD in the PPD string.
              1.3  APR 23 96 DGB
                   Change code 'PT' to 'PTIR" to make it unique 
                   in the shefstr.pecodes.  Before it would print out if the
                   PPT code was set, since if found PT in the PPT string.
                   Add 'TSRA' to the type checking for computing the PT 
                   shef code.
              1.4  MAY 7 96 DGB
                   Change code so that hourly preciptation values of 
                   0 are transmitted if the station is an automatic
                   site which reports precipitation, and the observation
                   is a regular hourly ob (not a special) and that 
                   not hourly precipitation was reported.
              1.5  MAY 28 96 DGB
                   Change window tolerance for 3 and 6 hourly observations
                   from, for example, 11:40 to 11:30.  Request made by
                   the NWRFC.
              1.6  MAY 30 96 DGB
                   Change output units for METRIC (Canadian Obs) for
                   precipitation, water equivalent and snow depth.
                   The output for American obs will remain in English units
                   and Canadian obs will be converted from metric to
                   english units.
              1.7  JUL 9 96
                   Include correct type of report, METAR or SPECI.
              1.8  JUL 12 96
                   Check for bad observation time and do not process
                   if found.
                   Change shef code from "TA" to "TAIRZZ" for 
                   determining instantaneous temperature.

                   Check for 3/6 hour precip if missing flag is set.
                   If it is, transmitt M for missing.  Only do this
                   If -p6 option is set on, and not precip is sent,
                   transmit a zero amount, only if the station is
                   an automatic site expected to provide precip.

              1.9  JUL 18 96
                   Rework the 6 hourly precipitation.
                   There are now three cases/options:

                      no -p6 or -pall6 switches
                      -p6    switch set on
                      -pall6 switch set on

                   no -p6 or pall6 switches on
                   If no switches are set, the following will occur:
                   (a) if a precip value is indicated, it will
                       be transmitted as given
                   (b) an 'M' will be sent if the ob indicates a missing
                       value
                       (No zero values will be assumed or transmitted
                        with this option.)

                   -p6 switch set on
                   If the -p6 switch is on, the following will occur:
                   (a) if a precip value is indicated, it will
                       be transmitted as given
                   (b) an 'M' will be sent if the ob indicates a missing value
                   (c) an 'M' will be sent if the ob is an automatic site
                       and PNO is sent in the message,
                   (d) a zero amount of precipitation will be sent if the
                       site is an automatic site, PNO is not present, and
                       no value is transmitted in the ob.

                   -pall6 switch set on
                   If the -pall6 switch is on, the following will occur:
                   (a) if a precip value is indicated, it will
                       be transmitted as given
                   (b) an 'M' will be sent if the ob indicates a missing
                       value
                   (c) a zero amount will be transmitted in all other cases

                    For hourly obs, check the PNO flag.  If present,
                    transmit 'M' in the shef message.
              2.0   JUL 19 96 DGB
                    Add the -pall24 and -round options.
              2.1   JUL 26 96 DGB
                    Add the -asosts switch.
              2.2   JUL 28 96 DGB
                    Fix problem for the -pall6 and -pall24 switches.
                    Before the fix, zero precip would not be generated
                    for (asos) sites.  This has been fixed.
              2.3   AUG 10 96 dgb
                    Fix autoIndicator to output type source as 'RO'.
              2.4   AUG 16 96 dgb
                    Change check of 'Mptr->autoIndicator[0] != '\0'
                    to determine if station is automatic.
                    Change to look for A02 as well as AO2 for automated
                    sites.
              2.5   OCT 27 96 DGB
                    Add present weather code, XW and cloud cover XC.
              2.6   DEC  2 96 DGB
                    Accomodate changes for SDO and SCD type reports.
              2.7   DEC  8 96 DGB
                    Correct the algorithm for converting centigrade 
                    to Fahrenheit...the formual was incorrect.
                    DEC 10 96 DGB
                    Give another shot...C to F.
              2.8   DEC 16 96 DGB
                    Only generate PPH for routine observations...
                    not specials.
              2.9   JAN 2 97 DGB
                    Fix output order of WX codes so that the most
                    important ( according to the list ) will output
                    first.
              3.0   JAN 9 97 DGB
                    Check the NO_SPECI flag...if on and the ob is
                    a SPECI, bypass and do not print.
              3.1   JAN  1 97 DGB
                    Add capabilities to check and output the 931nnn
                    group located in SDO/SCD obs.  This group is 
                    new snow depth in the past 6 hours, with nnn
                    tens, units and tenths.  The shef code is: SDQ.
                    
                    Add a blank at the end of the string of shef pe codes
                    so that the check can be, for exqmple, "TA " instead
                    of "TA". 
              3.2   JAN 14 97 dgb
                    Fix hourly temperature, TAIRZZZ.  I had deleted
                    a Z at the end.
              3.3   FEB 27 97 DGB
                    Change peak wind gust from YU to UP.
              3.4   MAR 27 97 DGB
                    Change to look for A01 as well as A02 for detecting
                    automated rain gages.
              3.5   MAY 8 97 DGB
                    Change to check for a 7//// group...if found
                    encode as -9999 or missing.
              3.6   OCT 10 97 DGB
                    Add -p1 option...generate a zero report for automated
                    rain gage sites if no value is reported and the 
                    there is no PNO { Precip Not Operating } token.

                    Change autoindicator from AO1 and AO2 to A01 and A02
                    and put check to change AO1 to A01 and AO2 to A02
                    just in case they are entered.
              3.7   NOV 10 97 DGB
                    Change sky cover translations.  See chart in code.
              3.8   NOV 28 97 DGB
                    Check for endless while loops.                    
              3.9   APR 4 98 DGB
                    Fix Indeterminate_24HrPrecip...If it is set to
                    TRUE set value to missing.
              
                    Comment out some of the code in the 24 hour 
                    precipitation section.  If the -p12z option
                    was not set, a 24 hour amount of 0 was generated
                    for every site.  Now it will generate 0 amounts 
                    only for the 12Z report if the -p12 option is
                    is set, otherwise, it will transmit the 24 hour
                    report only if it exists.
              4.0   JUL 1 98 DGB
                    Enclude -e switch which checks to see if temp
                    data, originally sent in metric should be converted
                    to english.  If the -e ( ENGL ) is set to one
                    do not convert to english.
              4.1   AUG 5 98 DGB
                    Add -y2k switch.
              4.2   JAN 3 98 DGB
                    Add new switch, '-kt'.  When included, units of
                    wind speed will be in knots.  If not include,
                    units will be in miles per hour.
                    
                    Store a value of 0 if the wind direction is variable.
                    
                    Change peak wind shef code from a 'UP' to 'UG'.
                    The gust is the peak gust at the time of 
                    observation.  Add 'UP' and 'UR'.
		    
              4.3   OCT 26 1999
                    Add capabilities to output a 'T' for 3, 6 and
                    24hr if a zero is reported in the group.

              4.4   JAN 06 00 DGB
	            Change it so that if the T group exists, the
		    decoder will output the temp and dew point 
		    using degrees and tenths of degrees in the 
		    shef value in lieu of the values in the body 
		    of the observation ( requested by Jeff Zimmerman,
		    Office of Hydrology).
	      4.5   FEB 04 00 DGB
	            Add check for GLOBAL_SPECI flag included in
		    collectives.
		    
	      4.6   OCT 31 00 DGB
	            Switch pe codes of PD and PE.  PD should be
		    pressure change/tendency and PE presure
		    characteristic.
		    
		    Add WIND_LEN which is set from the -q1 switch.
		    When 1, the output of the direction will be in
		    hundreds and not tens.
		    
	      4.7   FEB 03 01 DGB
	            Change from putting out: Mptr->SFC_VSBY to
		    the prevailing visibility: Mptr->prevail_vsbySM
		    
                    Remove the +.5 for rounding wind speeds for
		    the UQ since the speed is reported to ss.s

	      4.8   SEP 16 01 DGB
		    Change <metar.h> to "metar.h"

	      4.9   JUN 02 02 DGB
	            Loosen tolerance times for decoding 3 and 6 hour
		    precip groups.  They are now:
		    
		   code as a 6 hour total 
                   if ( (HRMN_time  >  531 && HRMN_time <  830 ) ||
                      (HRMN_time  > 1131 && HRMN_time < 1430 ) ||
                      (HRMN_time  > 1731 && HRMN_time < 2030 ) ||
                      (HRMN_time  > 2331 && HRMN_time < 2400 ) ||
                      (HRMN_time  > 0000 && HRMN_time < 0230 )
                      )
 
                   code as a 3 hour total 
                   if ( (HRMN_time  >  231 && HRMN_time < 530  ) ||
                      (HRMN_time  >  831 && HRMN_time < 1130 ) ||
                      (HRMN_time  > 1431 && HRMN_time < 1730 ) ||
                      (HRMN_time  > 2031 && HRMN_time < 2330 )
		      
	      5.0   APR 04 03 DGB
	            Redo section of code using the -p12z, -p24 and -pall24
		    switches. 
		     
                    The following logic now applies:
		    -p12z only applies to seven groups ( e.g. 70020 )
		    that exist.  If the -p12z switch exists (true) and
		    a seven group exists, the 24 data will ONLY be 
		    encoded for obs in the time window ( 1140 - 1230Z ).
		    If the -p12z switch does not exist (false ) and
		    a seven group exists, the 24 hour data will be 
		    encoded for any observation time.  Note: the -p12z
		    has no affect on determining if zero observations
		    will be generated or not.
		    
		    The generation of 24 hour amounts of zero is determined
		    by the existence of the -pall24 and/or -p24 switches.
		    NOTE: a zero amount will ONLY be generated for an
		    observation that exists in the 1140-1230Z window.
		    There is no way to distinguish in a METAR ob which
		    stations report or do not report 24 hour precipitation
		    data.
		    
		    If the -pall24 switch is true, a zero amount for the
		    24 hours ending at 12z will be generated for every
		    site ( as long as the site does not explicitly have
		    a 7 group..in which case the data in the 7 group will
		    be transmitted.)
		    
		    If the -p24 switch is true, a zero amount for 24 hours
		    ending at 12Z will be generated for ONLY automated sites
		    ( that have a A01 or AO2 indicator) ( as long as the site 
		    does not explicitly have a 7 group..in which case the 
		    data in the 7 group will be transmitted.) 
		      
	      5.1   NOV 20 04 DGB
                    Add 24 new snow depth
		    change the 6 hour new snow to be shef code 'SFQ'
		    It was previously 'SD' or snow on the ground
		    and it shoud be SFQ, new snow in the past 6 or 24 hours.
		    New 24 hour snowfall is coded as 'SFD'.
		    Change VAL_CHARS to 33.
		    
		    Check for 24 hour indeterminant precipitation, i.e., 7////
		    If True, then code as M.
		    
	      5.2   JAN 17 05 DGB
                    Fix so that if the hourly precip exists and this is 
		    a pc_reset site, do not round observation time.
		    Fix so that if the hourly precip exists and the -p1
		    flag is set, that the actual value gets writen out
		    and not a zero.
		    Fix so that if the hourly precip exists and the -p6
		    flags i set, a zero value for the 6 hourly ob is
		    not written.

	      5.3   May 25 05 RAE
                    Modified the change described in 5.2 above to now
         set the values[7] variable to -9999 instead of -99.
		    
	      5.4   JUN 30 05 DGB
	            Fix so that hourly precipitation will print our for SPECI
		    reports.

	      5.5   FEB 01 06 RAE
	            Initialize HRMN_time to zero.

              5.6   MAY 18 06 GS
                    Changed time duration calculation algorithm.  Now it
                      calculates the top of the hour values based on the 
                      tolerance correctly (tolerance = 3, reset time = 55
                      top of the hour = 52 - 58)
                    If nospeci flag is enabled, will still process non top
                      of the hour values if they are precip
		    
 ---------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"
#include "metar.h"

#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
#define VAL_CHARS  33                                      /* dgb:11/20/04 */

extern int METRIC, DEBUG, VERBOSE, WMO, SM, P24, P6, SWITCH_SOURCE, ID, PALL6;     /* dgb:07/18/96 */
extern int PALL24, ROUND, P12Z;                                                    /* dgb:07/19/96 */
extern int ASOSTS; 
extern int PEDTSEP;                                        /* dgb:07/26/96 */
extern int SDO, SCD;                                       /* dgb:12/02/96 */
extern int GET_PC_NAMES;                                   /* dgb:11/20/04 */
extern int PC_TOLERANCE;                                   /* dgb:11/20/04 */
extern int NO_SPECI;                                       /* dgb:01/09/97 */ 
extern int P1;                                             /* dgb:10/10/97 */
extern int ENGL;                                           /* dgb:07/01/98 */
extern int Y2K;                                            /* dgb:08/05 98 */
extern int KNOTS;                                          /* dgb:01/03/98 */
extern int GLOBAL_SPECI;                                   /* dgb:02/04/00 */
extern int WIND_LEN;                                       /* dgb:10/31/00 */
extern int metar_error_handler( char function[] );
extern long ouptime(int,int,int,int,int,int);               /* dgb:12/16/96 */
extern int metar_ob_time( Decoded_METAR *Mptr );
extern void round_it();
extern void xw_and_xc( Decoded_METAR *Mptr, int *xw_code, int *xc_code ); 
extern int print_obs( FILE *fp, int comment, int header );
int shef_it_metar( Decoded_METAR *Mptr )


{


   float values[VAL_CHARS];

   int dur_time;                                           /* dgb:11/20/04 */
   int ii,k;                                               /* dgb:11/28/97 */
   int pe_flag[VAL_CHARS], HRMN_time=0 ;
   int j, i;                                               /* dgb:07/19/96 */
   int P24_min = 1140;
   int P24_max = 1230;                                     /* dgb:09/05/96 */
   int P1_min  = 50;                                       /* dgb:03/29/97 */
   int P2_max  =  5;                                       /* dgb:03/29/97 */
   int xw_code,  xc_code;                                  /* dgb:10/27/96 */
   int tempyear, tempyeardc;                               /* dgb:08/05/98 */
   int tmp_ob_minute;                                      /* dgb:11/20/04 */
   int found_id = 0;
   char buff_temp[80], dota[4];
   char buff_temp1[80];                                    /* dgb:11/20/04 */ 
   char s_source  = 'V';   /* ASCII V */ 
   char s_sourceo = 'O';                                   /* dgb:07/26/96 */ 
   char hb5_id[10], pc[9];
   /* 
      When adding addtional PEDTSEP codes, ensure that the VAL_CHARS
      is updated, and the check for pe_buffer is updated 
   */

   char pe_buffer[VAL_CHARS][8] = 
   {                                     /* order   description               */
     {'T','A','I','R','Z','Z','Z','0'},  /*  0      Temperature Instantaneous */
     {'T','D','I','R','Z','Z','Z','0'},  /*  1      Dew Point                 */
     {'U','S','I','R','Z','Z','Z','0'},  /*  2      Wind Speed                */
     {'U','D','I','R','Z','Z','Z','0'},  /*  3      Wind Direction            */
     {'U','G','I','R','Z','Z','Z','0'},  /*  4      Peak Wind Gust            */ /* dgb:02/27/96 */
     {'U','Q','I','R','Z','Z','Z','0'},  /*  5      Speed/Direction sss.sddd  */
     {'P','A','I','R','Z','Z','Z','0'},  /*  6      Altimeter                 */
     {'P','P','Q','R','Z','Z','Z','0'},  /*  7      Precip 6 hour             */
     {'P','P','D','R','Z','Z','Z','0'},  /*  8      Precip 24 hour            */
     {'T','A','I','R','Z','X','Z','0'},  /*  9      Temperature Max 24 hour   */
     {'T','A','I','R','Z','N','Z','0'},  /* 10      Temperature Min 24 hour   */
     {'P','P','H','R','Z','Z','Z','0'},  /* 11      Precip 1 hour             */
     {'S','D','I','R','Z','Z','Z','0'},  /* 12      Snow depth on ground      */
     {'S','W','I','R','Z','Z','Z','0'},  /* 13      Water Equivalent          */
     {'R','T','I','R','Z','Z','Z','0'},  /* 14      Sunshine                  */
     {'T','A','I','R','Z','R','Z','0'},  /* 15      Max Temp past 6 hours     */
     {'T','A','I','R','Z','H','Z','0'},  /* 16      Min Temp past 6 hours     */
     {'P','P','T','R','Z','Z','Z','0'},  /* 17      Precip past 3 hours       */
     {'T','A','I','R','Z','Y','Z','0'},  /* 18      Max Temp past 12 hours    */
     {'T','A','I','R','Z','P','Z','0'},  /* 19      Min Temp past 12 hours    */
     {'P','C','I','R','Z','Z','Z','0'},  /* 20      PRECIP accumulator        */
     {'P','T','I','R','Z','Z','Z','0'},  /* 21      Precipitation Type        */ /* dgb:04/15/96 */
     {'P','L','I','R','Z','Z','Z','0'},  /* 22      Pressure Sea Level        */ /* dgb:04/15/96 */
     {'P','D','I','R','Z','Z','Z','0'},  /* 23      Pressure tendency         */ /* dgb:10/31/00 */
     {'P','E','I','R','Z','Z','Z','0'},  /* 24      Pressure charactersitic   */ /* dgb:10/31/00 */
     {'X','V','I','R','Z','Z','Z','0'},  /* 25      surface visibility        */ /* dgb:04/15/96 */
     {'H','G','I','R','Z','Z','Z','0'},  /* 26      river stage               */ /* dgb:04/15/96 */
     {'X','W','I','R','Z','Z','Z','0'},  /* 27      current weather code      */ /* dgb:10/27/96 */
     {'X','C','I','R','Z','Z','Z','0'},  /* 28      cloud sky cover tenths    */ /* dgb:10/27/96 */
     {'S','F','Q','R','Z','Z','Z','0'},  /* 29      snow depth past 6 hours   */ /* dgb:11/20/04 */  
     {'U','P','I','R','Z','Z','Z','0'},  /* 30      peak wind speed past hour */ /* dgb:01/03/98 */                
     {'U','R','I','R','Z','Z','Z','0'},  /* 31      peak wind dir   past hour */ /* dgb:01/03/98 */                
     {'S','F','D','R','Z','Z','Z','0'}   /* 32      snow depth past 24 hours  */ /* dgb:11/22/04 */                

   };

      if ( DEBUG ) fprintf(stdout,"\nbegin:shef_it");
      if ( DEBUG )
           if ( METRIC || SM ) fprintf(stdout,"\nMETRIC switch is on");

   /* Change AO1 and AO2 to A01 and A02 just in case */

      if ( strstr(Mptr->autoIndicator,"AO1") != NULL )      /* dgb:10/10/97 */
           memset( &Mptr->autoIndicator[1],48,1);           /* dgb:10/10/97 */
      if ( strstr(Mptr->autoIndicator,"AO2") != NULL )      /* dgb:10/10/97 */
           memset( &Mptr->autoIndicator[1],48,1);           /* dgb:10/10/97 */


   /* check codename - if NULL set to routine METAR */
      if ( strlen(Mptr->codeName) == 0 )
           strcpy(Mptr->codeName,"METAR");

   /* check for GLOBAL_SPECI flag.  If  'TRUE' set code to SPECI */
      if ( GLOBAL_SPECI )                                   /* dgb:02/04/00 */
           strcpy(Mptr->codeName,"SPECI");                  /* dgb:02/04/00 */

   /* if -nospeci is set on and the ob is a special - bypass */

   /* In addition to checking for the SPECI token, check
      the time window.  Consider the ob a special if it does
      not lie in the window P1_min < obmin < 0:00 and
      0:00 < obmin < P2_max and isn't precip.  Some automatic obs are transmitted
      with METAR and AUTO, anytime during the hour.
   */
      if ( NO_SPECI )
      {
         if ( Mptr->ob_minute > P2_max && Mptr->ob_minute < P1_min )
         {
              if ( Mptr->hourlyPrecip == (float) MAXINT  )
              {
                   return(0);
              }
         }
         if ( strcmp(Mptr->codeName,"METAR") != 0 )
              return(0);
      }


   /* Construct the handbook 5 id  */ 

      memset(hb5_id,0,sizeof(hb5_id));
      if ( Mptr->stnid[ 0 ] != '\0' ) 
      {
         if ( !ID )
           strcpy(hb5_id,Mptr->stnid);
         else
           strcpy(hb5_id,&Mptr->stnid[1]);
      }

      if ( NO_SPECI )
      {
          k = 0;

          while ( k < MAX_NUM_PC_NAMES )
          {
             if ( strcmp(&pc_names[k][0],hb5_id) == 0 )
             {
                found_id = 1;
             }
             else
             {
                if ( ( ID ) && ( strcmp(&pc_names[k][0],hb5_id) == 0 ) )
                {
                    found_id = 1;
                }
             }
             k++;
          }
      
          if ( ! found_id )
          {
             return ( 0 );
          }

      }


   /* Store/Compute data values into value array */
      for ( i = 0; i < VAL_CHARS; i++ ) 
        values[i] = -9999.0;


   /* store surface temperature - original in CELCIUS -> F */
      if ( Mptr->temp != MAXINT ) 
      {

         if ( !ENGL )                                      /* dgb:07/01/98 */
         {
           values[0] = 1.8 * Mptr->temp + 32;              /* dgb:12/08/96 */
           if ( values[0] >= 0 ) 
                values[0] = values[0] + .5;                /* dgb:12/10/96 */
           else
                values[0] = values[0] - .5;                /* dgb:12/10/96 */
         }
         else
           values[0] = Mptr->temp;                         /* dgb:07/01/98 */
      }
   /* store surface temp from T group and overwrite - original in CELCIUS -> F */
   
      if ( Mptr->Temp_2_tenths != MAXINT )                 /* dgb:01/06/00 */    
      {

         if ( !ENGL )                                      /* dgb:01/06/00 */
         {
           values[0] = 1.8 * Mptr->Temp_2_tenths + 32;     /* dgb:01/06/00 */
           if ( values[0] >= 0 ) 
                values[0] = values[0] + .5;                /* dgb:01/06/00 */
           else
                values[0] = values[0] - .5;                /* dgb:01/06/00 */
         }
         else
           values[0] = Mptr->Temp_2_tenths;                /* dgb:01/06/00 */
      }

   /* store surface dew point - original in CELCIUS -> F */
      if ( Mptr->dew_pt_temp != MAXINT ) 
      {
         if ( !ENGL )                                      /* dgb:07/01/98 */
         {
           values[1] = 1.8 * Mptr->dew_pt_temp + 32;       /* dgb:12/08/96 */
           if ( values[1] >= 0 ) 
                values[1] = values[1] + .5;                /* dgb:12/10/96 */
           else
                values[1] = values[1] - .5;                /* dgb:12/10/96 */
         }
         else
           values[1] = Mptr->dew_pt_temp;                  /* dgb:07/01/98 */
      }

   /* store surface dew point from T group and overwrite - original in CELCIUS -> F */
      if ( Mptr->DP_Temp_2_tenths != MAXINT ) 
      {
         if ( !ENGL )                                      /* dgb:01/06/00 */
         {
           values[1] = 1.8 * Mptr->DP_Temp_2_tenths + 32;  /* dgb:01/06/00 */
           if ( values[1] >= 0 ) 
                values[1] = values[1] + .5;                /* dgb:01/06/00 */
           else
                values[1] = values[1] - .5;                /* dgb:01/06/00 */
         }
         else
           values[1] = Mptr->DP_Temp_2_tenths;             /* dgb:01/06/00 */
      }

   /* store wind speed in KNOTS */
      if ( Mptr->winData.windSpeed != MAXINT )
      {
         if ( KNOTS )                                      /* dgb:01/03/98 */
           values[2] = Mptr->winData.windSpeed;
         else
           values[2] = Mptr->winData.windSpeed * 1.151555 +.5;  /* dgb:01/03/98 */
      }
      
   /* store wind direction in tens of DEGREES */
      if ( WIND_LEN == 0 )                                  /* dgb:10/31/00 */
      {                                                     /* dgb:10/31/00 */
         if ( Mptr->winData.windDir != MAXINT ) 
              values[3] = (Mptr->winData.windDir + 5) / 10; /* dgb:01/03/98 */
      }                                                     /* dgb:10/31/00 */
      else                                                  /* dgb:10/31/00 */
      {
         if ( Mptr->winData.windDir != MAXINT )             /* dgb:10/31/00 */ 
              values[3] = (Mptr->winData.windDir);          /* dgb:10/31/00 */
      
      }                                                     /* dgb:10/31/00 */
           
      /* if wind direction is variable store 0 */
      if( Mptr->winData.windVRB )
           values[3] = 0;                                   /* dgb:01/03/98 */
      
   /* store wind gust in KNOTS */
      if ( Mptr->winData.windGust != MAXINT ) 
      {   
          if ( KNOTS )                                      /* dgb:01/03/98 */
             values[4] = Mptr->winData.windGust;
          else
             values[4] = Mptr->winData.windGust * 1.15155 +.5; /* dgb:01/03/98 */
      }

   /* construct & store wind speed/direction sss.sddd - SHEF UQ */
         if ( Mptr->winData.windDir != MAXINT )
         {
            memset(buff_temp,0,sizeof(buff_temp));
            if ( KNOTS )                                    /* dgb:01/03 98 */
               sprintf(buff_temp,"%5.1f%03d", 
                    (float) Mptr->winData.windSpeed, Mptr->winData.windDir);
            else
               sprintf(buff_temp,"%5.1f%03d",
                    (float) Mptr->winData.windSpeed * 1.15155, Mptr->winData.windDir);
                                                            /* dgb:02/02/01 */
            values[5] = atof(buff_temp);
         }        
     
   /* store peak wind speed for past hour */
      if( Mptr->PKWND_dir !=  MAXINT )
      {
         if ( KNOTS )
            values[30] = Mptr->PKWND_speed;                 /* dgb:01/03/98 */
         else
            values[30] = Mptr->PKWND_speed * 1.151555 + .5; /* dgb:01/03/98 */
      }
      
   /* store peak wind direction for past hour */
      if( Mptr->PKWND_dir !=  MAXINT )
          values[31] = ( Mptr->PKWND_dir + 5 ) / 10 ;       /* dgb:01/03/98 */

   /* store altimeter setting ( INCHES ) */
      if ( Mptr->A_altstng ) 
           values[6] =  Mptr->inches_altstng;

   /* Construct integer time - HRMN */
      if ( ( Mptr->ob_hour != MAXINT ) && ( Mptr->ob_minute != MAXINT ) )
           HRMN_time = ( Mptr->ob_hour * 100 ) + Mptr->ob_minute;


   /* 
      store 3/6 HOURLY precipitation ( INCHES ) 
      only for regular (non special obs within the 3/6 hour window 
   */
      /* check if regular ob */
      if ( strcmp(Mptr->codeName,"METAR") == 0 )
            {

         /* check if 3/6 hour value is present */
         if ( Mptr->precip_amt != (float) MAXINT  ) 
         {
            /* check if in 6 hour window */
            if ( (HRMN_time  >  531 && HRMN_time <  830 ) ||
                 (HRMN_time  > 1131 && HRMN_time < 1430 ) ||
                 (HRMN_time  > 1731 && HRMN_time < 2030 ) ||
                 (HRMN_time  > 2331 && HRMN_time < 2400 ) ||
                 (HRMN_time  > 0000 && HRMN_time < 0230 ) 
               )                                                      /* dgb:06/02/02 */
               {
               if ( METRIC )
                   values[7] = Mptr->precip_amt * 100 * .03937;  /* dgb:05/30/96 */
               else    
                   values[7] = Mptr->precip_amt;
               }
            /* check if in 3 hour window */
            if ( (HRMN_time  >  231 && HRMN_time <  530 ) ||
                 (HRMN_time  >  831 && HRMN_time < 1130 ) ||
                 (HRMN_time  > 1431 && HRMN_time < 1730 ) ||
                 (HRMN_time  > 2031 && HRMN_time < 2330 )
               )                                                      /* dgb:06/02/02 */
               {
               if ( METRIC )
                   values[17] = Mptr->precip_amt * 100 * .03937;      /* dgb:05/30/96 */
               else    
                   values[17] = Mptr->precip_amt;

               }   
         }
         else
         {
            /* 
               no 3/6 hour value present...now decide what to
               do based on the existence or non existence of the
               -p6 and -pall6 flags
            */
   
            /* check if in 6 hour window  */
            if ( (HRMN_time  >  530 && HRMN_time <  610 ) ||
                 (HRMN_time  > 1130 && HRMN_time < 1210 ) ||
                 (HRMN_time  > 1730 && HRMN_time < 1810 ) ||
                 (HRMN_time  > 2330 && HRMN_time < 2400 ) ||
                 (HRMN_time  > 0000 && HRMN_time < 0010 ) 
                )
            {
               if ( PALL6 )
               {
                   if ( (strstr(Mptr->autoIndicator,"A01") != NULL) ||
                        (strstr(Mptr->autoIndicator,"A02") != NULL)) /* dgb:10/10/97 */ 
                   {
                      if ( Mptr->PNO )
                           values[7] = -99;
                      else
                           values[7] = 0;                  /* dgb:07/28/96 */
                   }
                   else
             
                   /* check if the value was sent as missing */
                   if ( Mptr->Indeterminant3_6HrPrecip )   /* dgb:07/12/96 */    
                              values[7] = -99;
                   else
                              values[7] = 0;
               }
               else
               if ( P6 )
               {
                   /* check if the value was sent as missing */
                   if ( Mptr->Indeterminant3_6HrPrecip )   /* dgb:07/12/96 */    
                              values[7] = -99;
                   else
                   if ( (strstr(Mptr->autoIndicator,"A01") != NULL) ||
                        (strstr(Mptr->autoIndicator,"A02") != NULL) )  /* dgb:10/10/97 */
                   {
                      if ( Mptr->PNO )
                           values[7] = -99;
                      else
                           values[7] = 0;
                   }
               }
               else
               {
                   /* check if the value was sent as missing */
                   if ( Mptr->Indeterminant3_6HrPrecip )   /* dgb:07/12/96 */    
                           values[7] = -99;
               }
            }
         }
      } 

         if ( Mptr->Indeterminant3_6HrPrecip == TRUE )        /* dgb:11/23/97 */     
              values[7] = -99;



   /* store 24 HOURLY precipitation ( INCHES )          */
   /* if P24 option is on only post data for 12Z window */
   /* this whole section redone on April 4, 2003 */


   /* check if regular ob */
   if ( strcmp(Mptr->codeName,"METAR") == 0 )
   {
            /* check for the existence of a 24 hour amount */
      if ( Mptr->precip_24_amt != (float) MAXINT  ) 
      {         
         /* check if 12z only is set */
         if ( P12Z )
         {
            /* is ob in time window */
            if ( (HRMN_time > P24_min && HRMN_time < P24_max) )
            {

               if ( Mptr->precip_24_amt < 0 )              /* dgb:05/08/97 */
                    values[8] = -99;
               else
               {
                  if ( METRIC )
                       values[8] = Mptr->precip_24_amt * 100 * .03937; 
                   else    
                       values[8] = Mptr->precip_24_amt;
               }
            }  
          } 
	  else
	  {

               if ( Mptr->precip_24_amt < 0 )              /* dgb:05/08/97 */
                    values[8] = -99;
               else
               {
                  if ( METRIC )
                       values[8] = Mptr->precip_24_amt * 100 * .03937; 
                   else    
                       values[8] = Mptr->precip_24_amt;
               }

          }
       } 
       else
       /* now check case where 24 value does not exist..and a zero may need to be generated */
       /* first....is the ob in the time window...no need to generate zeroes for everyhour */
       if ( (HRMN_time > P24_min && HRMN_time < P24_max) )
       {
                /* if all is set..generate for automatic and non automatic sites */
                if ( PALL24 )
                {
                   values[8] = 0;
		}
		else
		/* just check automatic sites */
                if ( P24 )
                {
                   if ( (strstr(Mptr->autoIndicator,"A01") != NULL) ||
                        (strstr(Mptr->autoIndicator,"A02") != NULL)  )  /* dgb:10/10/97 */ 
                   {
                      if ( Mptr->PNO )
                           values[8] = -99;
                      else
                           values[8] = 0;
                   }
		}
       

       }
                   /* check if the value was sent as missing */
                   if ( Mptr->Indeterminant_24HrPrecip )   /* dgb:11/20/05 */    
                           values[8] = -99;                /* dgb:11/20/05 */   

     }
        


  /* Max Temp past 24 hours ( CELCIUS -> F ) */
      if ( Mptr->max24temp != (float) MAXINT ) 
      {
         if ( !ENGL )                                     /* dgb:07/01/98 */
         {
          values[9] = 1.8 * Mptr->max24temp + 32;         /* dgb:12/08/96 */

           if ( values[9] >= 0 )  
                values[9] = values[9] + .5;                /* dgb:12/10/96 */
           else
                values[9] = values[9] - .5;                /* dgb:12/10/96 */
          }
          else
             values[9] = Mptr->max24temp;                  /* dgb:07/01/98 */
      }


   /* Min Temp past 24 hours ( CELCIUS -> F ) */
      if ( Mptr->min24temp != (float) MAXINT ) 
      {
         if ( !ENGL )                                      /* dgb:07/01/98 */
         {
           values[10] = 1.8 * Mptr->min24temp + 32;        /* dgb:12/08/96 */
           if ( values[10] >= 0 ) 
                values[10] = values[10] + .5;              /* dgb:12/10/96 */
           else
                values[10] = values[10] - .5;              /* dgb:12/10/96 */
         }
         else
            values[10] = Mptr->min24temp;                  /* dgb:07/01/98 */
      }


   /* store HOURLY precipitation ( INCHES ) 
      If hourly precip is indicated store that value...
      If it is not indicated first check if the station is
      and automatic site with precip ( i.e. A01 or A02 ) and then 
      check if it is a regular observation (i.e. not a
      special ( SPECI ).  Also check if PNO is set false.
      If so generate a zero value.
   */

   if ( strcmp(Mptr->codeName,"METAR") == 0 )              /* dgb:12/16/96 */
   {
      if ( Mptr->hourlyPrecip != (float) MAXINT  )
      {
          if ( METRIC )
               values[11] = Mptr->hourlyPrecip * 100 * .03937;      /* dgb:05/30/96 */
          else    
               values[11] = Mptr->hourlyPrecip;
      }
      else    
      if ( P1 )                                            /* dgb:10/10/97 */
      {
         if ( (strstr(Mptr->autoIndicator,"A01") != NULL) ||
              (strstr(Mptr->autoIndicator,"A02") != NULL) ) /* dgb:03/27/97 */      
         {
            if ( strcmp(Mptr->codeName,"METAR") == 0 )
            {
               if ( Mptr->PNO )                             /* dgb:07/18/96 */
                    values[11] = -99;
               else
                    values[11] = 0;
            }
         }
      }                                                    /* dgb:10/10/97 */
   }
   else                                                    /* dgb:06/30/2005 */
   if ( strcmp(Mptr->codeName,"SPECI") == 0 )              /* dgb:06/30/2005 */
   {
      if ( Mptr->hourlyPrecip != (float) MAXINT  )         /* dgb:06/30/2005 */
      {
          if ( METRIC )                                    /* dgb:06/30/2005 */
               values[11] = Mptr->hourlyPrecip * 100 * .03937;      /* dgb:06/30/2005 */
          else                                             /* dgb:06/30/2005 */ 
               values[11] = Mptr->hourlyPrecip;            /* dgb:06/30/2005 */
      }
    }   

      dur_time = -1;  
  /* now check for sites that may have a reset time read in from the cfg file */
      if ( GET_PC_NAMES ) 
      {
         if ( Mptr->hourlyPrecip != (float) MAXINT  )
         {
            if ( METRIC )
               values[11] = Mptr->hourlyPrecip * 100 * .03937;      /* dgb:05/30/96 */
            else    
               values[11] = Mptr->hourlyPrecip;
         }
         /* compute duration using reset time Absolute_Value(data_time - reset_time) */
         /* look in list for id */
         k = 0;
         ii = 0;                                     /* dgb:11/20/04 */
         while ( k < MAX_NUM_PC_NAMES )
         {
            if ( strcmp(&pc_names[k][0],hb5_id) == 0 ) 
	    {
               tmp_ob_minute = Mptr->ob_minute;
               dur_time = ( tmp_ob_minute + 60 - atoi( pc_times[k] ) ) % 60;
	       
               if ( ( dur_time <= PC_TOLERANCE ) || ( abs( dur_time - 60 ) <= PC_TOLERANCE ) )
	          dur_time = 60.;
	       if ( P6 )                             /* dgb:01/17/05 */
	          values[7] = -9999;                         /* rae:05/25/05 */
               break;	
	     } 
             else 
             {    
               if ( ID ) 
  	       {
                 if ( strcmp(&pc_names[k][0],hb5_id) == 0 )
	         {
                   tmp_ob_minute = Mptr->ob_minute;
                   dur_time = ( tmp_ob_minute + 60 - atoi( pc_times[k] ) ) % 60;
	       
	           if ( ( dur_time <= PC_TOLERANCE ) || ( abs( dur_time - 60 ) <= PC_TOLERANCE ) )
	              dur_time = 60.;
                   break;
	           if ( P6 )                           // dgb:01/17/05 
	            values[7] = -9999;                  // rae:05/25/05 
	
	         }
	       }    
               else
               {
                 return(0);
               }
             }
	  k++;
          ii++;                                             /* dgb:11/28/97 */
          if ( ii > MAX_LOOP ) metar_error_handler("shef_it_metar"); /* dgb:11/28/97 */
         }
       }
     
      
      
   /* store snow depth ( INCHES ) */
      if ( Mptr->snow_depth != MAXINT  ) 
      {
         if ( METRIC )
                values[12] = Mptr->snow_depth * 10 * 3.937;      /* dgb:05/30/96 */
         else    
                values[12] = Mptr->snow_depth;
      }

   /* store snow water uquivalent ( INCHES ) */
      if ( Mptr->WaterEquivSnow != (float) MAXINT  ) 
      {
            if ( METRIC )
                values[13] = Mptr->WaterEquivSnow * 100 * .03937;  /* dgb:05/30/96 */
            else    
                values[13] = Mptr->WaterEquivSnow;
      }

   /* store sunshine duration ( MINUTES ) */
      if ( Mptr->SunshineDur !=  MAXINT  ) 
           values[14] = Mptr->SunshineDur;

  
   /* store maxtemp past 6 hours ( CELCIUS -> F ) */
      if ( Mptr->maxtemp !=  (float) MAXINT  )
      {
         if ( !ENGL )                                      /* dgb:07/01/98 */
         {
           values[15] = 1.8 * Mptr->maxtemp + 32;          /* dgb:12/08/96 */
           if ( values[15] >= 0 ) 
                values[15] = values[15] + .5;              /* dgb:12/10/96 */
           else
                values[15] = values[15] - .5;              /* dgb:12/10/96 */
         }
         else
            values[15] = Mptr->maxtemp;                    /* dgb:07/01/98 */
      }
 
   /* store mintemp past 6 hours ( CELCIUS -> F ) */
      if ( Mptr->mintemp !=  (float) MAXINT  )
      {
          if ( !ENGL )                                     /* dgb:07/01/98 */
          {
           values[16] = 1.8 * Mptr->mintemp + 32;          /* dgb:12/08/96 */
           if ( values[16] >= 0 ) 
                values[16] = values[16] + .5;              /* dgb:12/10/96 */
           else
                values[16] = values[16] - .5;              /* dgb:12/10/96 */
           }
           else
               values[16] = Mptr->mintemp;                 /* dgb:07/01/98 */
      }


   /* store precipitation type for non-special hourly observations only */
   /*  0 = ice prism        RA SHRA    */   
   /*  1 = rain             RA SHRA    */
   /*  2 = freezing rain    FZRA       */
   /*  3 = drizzle          DZ         */
   /*  4 = freezing drizzle FZDZ       */
   /*  5 = snow             SQ SN      */
   /*  7 = snow grains      SG         */ 

       values[21] = -1;                                    /* dgb:04/15/96 */
       i = 0;
       ii = 0;                                             /* dgb:11/28/97 */

       while ( Mptr->WxObstruct[i][0] != '\0' && i < 5 )
       {
          if ( strstr(Mptr->WxObstruct[i],"RA") != 0 )
               values[21] = 1;
          else
          if ( strstr(Mptr->WxObstruct[i],"DZ") != 0 )
               values[21] = 3;
          else
          if ( strstr(Mptr->WxObstruct[i],"SHRA") != 0 )
               values[21] = 1;
          else
          if ( strstr(Mptr->WxObstruct[i],"TSRA") != 0 )  /* dgb:04/22/96 */
               values[21] = 1;
          else
          if ( strstr(Mptr->WxObstruct[i],"SQ") != 0 )
               values[21] = 5;
          else
          if ( strstr(Mptr->WxObstruct[i],"SG") != 0 )
               values[21] = 7;
          else
          if ( strstr(Mptr->WxObstruct[i],"SN") != 0 )
               values[21] = 5;
          else
          if ( strstr(Mptr->WxObstruct[i],"FZRA") != 0 )
               values[21] = 2;
          else
          if ( strstr(Mptr->WxObstruct[i],"FZDZ") != 0 )
               values[21] = 4;
         i++;
         ii++;                                             /* dgb:11/28/97 */
         if ( ii > MAX_LOOP ) metar_error_handler("shef_it_metar"); /* dgb:11/28/97 */
       }
  

   /* determine if AUTO or MANUAL - forms_.type_sensor */
      if ( Mptr->autoIndicator[0] != '\0' )                /* dgb:08/16/96 */
           strcpy(forms_.type_sensor,"AUTO");
      else
           strcpy(forms_.type_sensor,"MANUAL");

   /* determine if SPECIAL or NON-SPECIAL ob */
      if ( strcmp(Mptr->codeName,"METAR") == 0 )
           strcpy(forms_.type_report,"METAR");             /* dgb:07/09/96 */
      else
           strcpy(forms_.type_report,"SPECI");             /* dgb:07/09/96 */

   /* check if SDO or SCD report */
      if ( SDO )                                           /* dgb:12/02/96 */
          strcpy(forms_.type_report,"SDO");  
      else
      if ( SCD )
          strcpy(forms_.type_report,"SCD");
          

   /* store sea level pressure */                          /* dgb:04/15/96 */
      if ( Mptr->SLP != (float) MAXINT )
           values[22] = Mptr->SLP;
 
   /* store pressure characteristic */                     /* dgb:04/15/96 */
      if ( Mptr->char_prestndcy != MAXINT )
           values[24] = Mptr->char_prestndcy;                  /* dgb:10/31/00 */

   /* store pressure tendency */                           /* dgb:04/15/96 */
      if ( Mptr->prestndcy != (float) MAXINT )
           values[23] = Mptr->prestndcy;                       /* dgb:10/31/00 */ 

   /* store prevailing visibility */                          /* dgb:02/03/01 */
      if (  Mptr->prevail_vsbySM != (float) MAXINT)
           values[25] = Mptr->prevail_vsbySM;
	   
   /* check if obs is a CORrected ob - if so set revision flag */
      if ( Mptr->COR )
           strcpy(dota,".AR");
      else
           strcpy(dota,".A");

   /* get obs time */                            /* dgb:07/12/96 */
      if ( metar_ob_time( Mptr ) == 1 )
           return(1);

   /* check if HRMN shoule be rounded to nearest whole hour for non specials */
      if ( strcmp(Mptr->codeName,"METAR") == 0 )
      {
          if ( ROUND && dur_time == -1 )            /* dgb:01/17/05 */
               round_it();
      }

   /* Get Current Wx & cloud cover */
      xw_and_xc( Mptr, &xw_code, &xc_code );       /* dgb:10/27/96 */
      values[27] = xw_code;                        /* dgb:10/27/96 */
      values[28] = xc_code;                        /* dgb:10/27/96 */
      if ( SDO || SCD )                            /* dgb:12/02/96 */
      {
         if ( xw_code == 0 ) values[27] = -9999;
         values[28] = -9999;
      }

   /* Snow Depth for past 6 hours */
      if ( Mptr->DepthNewSnow != (float) MAXINT )
           values[29] = Mptr->DepthNewSnow;                 /* dgb:01/10/97 */

   /* Snow Depth for past 24 hours */
      if ( Mptr->DepthNewSnow24 != (float) MAXINT )
           values[32] = Mptr->DepthNewSnow24;               /* dgb:11/20/04 */


/* -------------------------------------------------------------- */

 
	for( i = 0; i < VAL_CHARS; i++ )
		 pe_flag[i] = 0;

		/* NOTE: these must be in the same order as the 
		   array pe_buffer above
		*/
                strncpy(&shefstr_.pecodes[strlen(shefstr_.pecodes)-1]," ",1); /* dgb:01/10/97 */
		if( strstr( shefstr_.pecodes, "TAIRZZ " ) != 0 ) /* dgb:01/13/97 */
			pe_flag[0] = 1;
		if( strstr( shefstr_.pecodes, "TD " ) != 0 )
			pe_flag[1] = 1;
		if( strstr( shefstr_.pecodes, "US " ) != 0 )
			pe_flag[2] = 1;
		if( strstr( shefstr_.pecodes, "UD " ) != 0 )
			pe_flag[3] = 1;
		if( strstr( shefstr_.pecodes, "UG " ) != 0 )      /* dgb:02/27/97 */
			pe_flag[4] = 1;
		if( strstr( shefstr_.pecodes, "UQ " ) != 0 )
			pe_flag[5] = 1;
		if( strstr( shefstr_.pecodes, "PA " ) != 0 )
			pe_flag[6] = 1;
		if( strstr( shefstr_.pecodes, "PPQ ") != 0 )
			pe_flag[7] = 1;
		if( strstr( shefstr_.pecodes, "PPD ") != 0 )
			pe_flag[8] = 1;
		if( strstr( shefstr_.pecodes, "TX " ) != 0 )
			pe_flag[9] = 1;
		if( strstr( shefstr_.pecodes, "TN " ) != 0 )
			pe_flag[10] = 1;
		if(strstr( shefstr_.pecodes,  "PPH ") != 0 )
			pe_flag[11] = 1;
		if( strstr( shefstr_.pecodes, "SD " ) != 0 )
			pe_flag[12] = 1;
		if( strstr( shefstr_.pecodes, "SW " ) != 0 )
			pe_flag[13] = 1;
		if( strstr( shefstr_.pecodes, "RT " ) != 0 )
			pe_flag[14] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZR " ) != 0 )
			pe_flag[15] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZH " ) != 0 )
			pe_flag[16] = 1;
		if( strstr( shefstr_.pecodes, "PPT " ) != 0 )
			pe_flag[17] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZY " ) != 0 )
			pe_flag[18] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZP " ) != 0 )
			pe_flag[19] = 1;
		if( strstr( shefstr_.pecodes, "PC " ) != 0 )
			pe_flag[20] = 1;
		if( strstr( shefstr_.pecodes, "PTIR " ) != 0 )      /* dgb:04/23/96 */
			pe_flag[21] = 1;
		if( strstr( shefstr_.pecodes, "PL " ) != 0 )        /* dgb:04/15/96 */
			pe_flag[22] = 1;
		if( strstr( shefstr_.pecodes, "PDIR " ) != 0 )      /* dgb:04/17/96 */
			pe_flag[23] = 1;
		if( strstr( shefstr_.pecodes, "PE " ) != 0 )        /* dgb:04/15/96 */
			pe_flag[24] = 1;
		if( strstr( shefstr_.pecodes, "XV " ) != 0 )        /* dgb:04/15/96 */
			pe_flag[25] = 1;
		if( strstr( shefstr_.pecodes, "HG " ) != 0 )        /* dgb:10/27/96 */
			pe_flag[26] = 1;
		if( strstr( shefstr_.pecodes, "XW " ) != 0 )        /* dgb:10/27/96 */
			pe_flag[27] = 1;
		if( strstr( shefstr_.pecodes, "XC " ) != 0 )        /* dgb:10/27/96 */
			pe_flag[28] = 1; 
		if( strstr( shefstr_.pecodes, "SFQ " ) != 0 )       /* dgb:01/10/97 */
			pe_flag[29] = 1;
		if( strstr( shefstr_.pecodes, "UP " ) != 0 )        /* dgb:01/03/98 */
			pe_flag[30] = 1;                            /* dgb:01/03/98 */
		if( strstr( shefstr_.pecodes, "UR " ) != 0 )        /* dgb:01/03/98 */
			pe_flag[31] = 1;                            /* dgb:01/03/98 */
		if( strstr( shefstr_.pecodes, "SFD " ) != 0 )        /* dgb:11/20/04 */
			pe_flag[32] = 1;                            /* dgb:11/20/04 */

      /* write header to error log */
         if ( luns_.icher != NULL )
         {
            fprintf(luns_.icher,"\n----------------------------");
            fprintf(luns_.icher,"\n ENCODED: SHEF OBSERVATION");
            fprintf(luns_.icher,"\n----------------------------\n\n");
         }

      /* write header to stdout */
      if ( VERBOSE ) 
      {
            fprintf(stdout,"\n----------------------------");
            fprintf(stdout,"\n ENCODED: SHEF OBSERVATION");
            fprintf(stdout,"\n----------------------------\n\n");
      }

      if ( WMO )
         fprintf(luns_.jchn,"ZCZC %s\n%s\n",stats_.product_name,buffer_.wmo_line); /* dgb:04/15/96 */

      if ( luns_.jchn != NULL )
         print_obs( luns_.jchn, 1, 0 );

      for ( j = 0; j < VAL_CHARS; j++ )
      {  
/*      if ( pe_flag[j] && values[j] > -998)    */   /* process this pe code */

        if ( pe_flag[j] && values[j] >= -9998 )      /* dgb:05/08 97 */     /* process this pe code */
        {

          for (i=0; i < 7; ++i)
               pc[i] = pe_buffer[j][i]; /* build PEDSTEP string */

          if ( SWITCH_SOURCE )
             pc[4] = s_source;

          if ( ASOSTS )                                        /* dgb:07/26/96 */
          {
             pc[4] = s_source;
             if ( Mptr->autoIndicator[0] != '\0' )             /* dgb:08/10/96 */
                pc[4] = s_sourceo;
          }

          pc[7] =  0;                   /* string terminator    */
          pc[8] =  0;


          if ( PEDTSEP )
          {
              fprintf(luns_.mrec,"%s %s\n",hb5_id,pc);
          }          

              /* write report to output file */
              if ( !Y2K )                                   /* dgb:08/05/98 */
              {
                 if ( buffer_.idate1[0] < 2000 )
                      tempyear = buffer_.idate1[0]  - 1900;  /* dgb:08/05/98 */
                 else
                      tempyear = buffer_.idate1[0] - 2000;  /* dgb:08/05/98 */
                 if ( buffer_.idate[0] < 2000 )             /* dgb:08/05/98 */
                      tempyeardc = buffer_.idate[0] - 1900; /* dgb:08/05/98 */
                 else
                      tempyeardc = buffer_.idate[0] - 2000; /* dgb:08/05/98 */
                 sprintf(buff_temp,
                 "%s %s :%5s %6s %2s: %02d%02d%02d Z DH%02d%02d/DC%02d%02d%02d%02d%02d/%s ",
                 dota,
                 hb5_id,
                 forms_.type_report,
                 forms_.type_sensor,
                 forms_.type_format,
                 tempyear,  
                 buffer_.idate1[1],
                 buffer_.idate1[2],
                 buffer_.idate1[3],
                 buffer_.idate1[4],
                 tempyeardc,
                 buffer_.idate[1],
                 buffer_.idate[2],
                 buffer_.idate[3],
                 buffer_.idate[4],
                 pc);
		 /* dgb: 11/20/04 add buff_temp1 */
                 sprintf(buff_temp1,
                 "%s %s :%5s %6s %2s: %02d%02d%02d Z DH%02d%02d/DC%02d%02d%02d%02d%02d/DVN%02d/PPVRZZZ  ",
                 dota,
                 hb5_id,
                 forms_.type_report,
                 forms_.type_sensor,
                 forms_.type_format,
                 tempyear,  
                 buffer_.idate1[1],
                 buffer_.idate1[2],
                 buffer_.idate1[3],
                 buffer_.idate1[4],
                 tempyeardc,
                 buffer_.idate[1],
                 buffer_.idate[2],
                 buffer_.idate[3],
                 buffer_.idate[4],
		 dur_time);
              }
              else
              {
                 sprintf(buff_temp,
                 "%s %s :%5s %6s %2s: %04d%02d%02d Z DH%02d%02d/DC%04d%02d%02d%02d%02d/%s ",
                 dota,
                 hb5_id,
                 forms_.type_report,
                 forms_.type_sensor,
                 forms_.type_format,
                 buffer_.idate1[0],  
                 buffer_.idate1[1],
                 buffer_.idate1[2],
                 buffer_.idate1[3],
                 buffer_.idate1[4],
                 buffer_.idate[0],
                 buffer_.idate[1],
                 buffer_.idate[2],
                 buffer_.idate[3],
                 buffer_.idate[4],
                 pc);

              }

                switch (j)
                {
                    case 0 : /* temp */
                    {

                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 1: /* dew */

                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                    case 2 : /* wind sp */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 3: /* dir */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }

                    case 4 : /* gust */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 5: /* speed/dir */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s  %7.4f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s  %7.4f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s  %7.4f\n",buff_temp,values[j]);
                        break;
                     }
                      case 6: /* altimeter */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                        break;
                     }

                    case  7: /* precip 6 */
                    {
                       
                     if ( Mptr->precip_amt != (float) MAXINT  )   /*dgb 10/26/99 */ 
                     {
                        if ( values[j] > 0 )
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                        }
                        else
                        if ( values[j] == 0 )
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s   T\n",buff_temp);   /*dgb:10/26/99  */
                          fprintf(luns_.jchn,"%s   T\n",buff_temp);  /* dgb:10/26/99 */
                          fprintf(luns_.icher,"%s   T\n",buff_temp); /* dgb:10/26/99 */
                        }
                        else
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s   M\n",buff_temp);
                          fprintf(luns_.jchn,"%s   M\n",buff_temp);
                          fprintf(luns_.icher,"%s   M\n",buff_temp);
                        }
                        break;
                      }
                      else
                      {
                        if ( values[j] >= 0 )
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                        }
                        else
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s   M\n",buff_temp);
                          fprintf(luns_.jchn,"%s   M\n",buff_temp);
                          fprintf(luns_.icher,"%s   M\n",buff_temp);
                        }
                        break;

                      }                                           /* dgb:10/26/99 */
                     }
                     case 8: /* precip 24 */
                     {
                        if ( Mptr->precip_24_amt != (float) MAXINT  ) 
                        {
                           if ( values[j] > 0 )
                           {
                             if ( VERBOSE )
                             fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                             fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                             fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                           }
                           else
                           if ( values[j] == 0 )
                           {
                              if ( VERBOSE )
                              fprintf(stdout,"%s   T\n",buff_temp);   /*dgb:10/26/99  */
                              fprintf(luns_.jchn,"%s   T\n",buff_temp);  /* dgb:10/26/99 */
                              fprintf(luns_.icher,"%s   T\n",buff_temp); /* dgb:10/26/99 */
                           }
                           else
                           {
                              if ( VERBOSE )
                              fprintf(stdout,"%s   M\n",buff_temp);
                              fprintf(luns_.jchn,"%s   M\n",buff_temp);
                              fprintf(luns_.icher,"%s   M\n",buff_temp);
                           }
                           break;
                         }
                         else
                         {
                            if ( values[j] >= 0 )
                            {
                               if ( VERBOSE )
                               fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                               fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                               fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                            }
                            else
                            {
                               if ( VERBOSE )
                               fprintf(stdout,"%s   M\n",buff_temp);
                               fprintf(luns_.jchn,"%s   M\n",buff_temp);
                               fprintf(luns_.icher,"%s   M\n",buff_temp);
                            }
                            break;

                         }                                           /* dgb:10/26/99 */
                      }

                    
                     
                    case 9: /* temp mx */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }

                     case 10: /* temp mn */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }

                     case 11: /* hourly precip */
                     {
		        /* if dur_time is > 0 it means that there was a reset time value
			      and a variable duration shef line is needed
			   */
		        if ( dur_time > 0 ) 
			{
			
			
                           if ( VERBOSE )
                              fprintf(stdout,"%s  %5.2f\n",buff_temp1,values[j]);
                           fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp1,values[j]);
                           fprintf(luns_.icher,"%s  %5.2f\n",buff_temp1,values[j]);
			
			
			}
			else
                        if ( values[j] >= 0 )               /* dgb:07/18/96 */
                        {
                           if ( VERBOSE )
                              fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                           fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                           fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                         } 
                         else
                         {
                             if ( VERBOSE )
                                fprintf(stdout,"%s   M\n",buff_temp);
                             fprintf(luns_.jchn,"%s   M\n",buff_temp);
                             fprintf(luns_.icher,"%s   M\n",buff_temp);
                          }
			 
                        break;
			
                     }
 
                     case 12: /* snow depth */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 13: /* water equivalent */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                        break;
                     }
                     case 14: /* sunshine */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }

                    case 15: /* temp mx past 6 hours */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 16: /* temp mn past 6 hours */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                    case  17: /* precip 3 hour */
                    {

                     if ( Mptr->precip_amt != (float) MAXINT  )   /*dgb 10/26/99 */ 
                     {
                        if ( values[j] > 0 )
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                        }
                        else
                        if ( values[j] == 0 )
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s   T\n",buff_temp);   /*dgb:10/26/99  */
                          fprintf(luns_.jchn,"%s   T\n",buff_temp);  /* dgb:10/26/99 */
                          fprintf(luns_.icher,"%s   T\n",buff_temp); /* dgb:10/26/99 */
                        }
                        else
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s   M\n",buff_temp);
                          fprintf(luns_.jchn,"%s   M\n",buff_temp);
                          fprintf(luns_.icher,"%s   M\n",buff_temp);
                        }
                        break;
                      }
                      else
                      {
                        if ( values[j] >= 0 )
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.jchn,"%s  %5.2f\n",buff_temp,values[j]);
                          fprintf(luns_.icher,"%s  %5.2f\n",buff_temp,values[j]);
                        }
                        else
                        {
                          if ( VERBOSE )
                             fprintf(stdout,"%s   M\n",buff_temp);
                          fprintf(luns_.jchn,"%s   M\n",buff_temp);
                          fprintf(luns_.icher,"%s   M\n",buff_temp);
                        }
                        break;

                      }                                           /* dgb:10/26/99 */
                     }

                     

                    case 18: /* temp mx past 12 hours */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 19: /* temp mn past 12 hours */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 20: /* PC accumulator */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 21: /* PT precip type */
                     {
		        if ( (int) values[j] >= 0 ) {                                   /* dgb:11/20/04 */
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);  
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]); 
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]); 
                        break;
			} 
			break;                                                         /* dgb:11/20/04 */
                     }

                      case 22: /* PE sea level pressure */    /* dgb:04/15/96 */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s%7.2f\n",buff_temp,values[j]);
                        break;
                     }

                      case 23: /* pressure characteristic */    /* dgb:04/15/96 */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s%7.2f\n",buff_temp,values[j]);
                        break;
                     }
                      case 24: /* pressure tendency */    /* dgb:04/15/96 */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s%7.2f\n",buff_temp,values[j]);
                        break;
                     }
                      case 25: /* XV surface visibility */    /* dgb:04/15/96 */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s%7.2f\n",buff_temp,values[j]);
                        break;
                     }

                     case 26: /* HG river stage */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 27: /* XW present weather code */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     case 28: /* XC sky cover in tenths  */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                     
 
                     case 29: /* New snowfall depth past 6 hours */ /* dgb:01/10/97 */
                     {
                        if ( VERBOSE ) 
                             fprintf(stdout,"%s  %4.1f\n",buff_temp,values[j]);
                          fprintf(luns_.jchn,"%s  %4.1f\n",buff_temp,values[j]);
                          fprintf(luns_.icher,"%s  %4.1f\n",buff_temp,values[j]);
 
                        break;
                     }

                     case 30 : /* peak wind sp */ 
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }
                      
                     case 31: /* peak wind dir */
                     {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                     }

                     case 32: /* New snowfall depth past 24 hours */
                     {
                        if ( VERBOSE )                                              /* dgb:11/20/04 */
                             fprintf(stdout,"%s  %4.1f\n",buff_temp,values[j]);     /* dgb:11/20/04 */
                          fprintf(luns_.jchn,"%s  %4.1f\n",buff_temp,values[j]);    /* dgb:11/20/04 */
                          fprintf(luns_.icher,"%s  %4.1f\n",buff_temp,values[j]);
                     }                                                              /* dgb:11/20/04 */



          }
        } 
      }  

      if ( WMO )
         fprintf(luns_.jchn,"NNNN\n");


  return(0);
}

void round_it()
{



   struct tm tmm;
   long time_seconds;
   static short int iday, imonth, iyear, ihour, imin, isec; 

   iyear  = buffer_.idate1[0];
   imonth = buffer_.idate1[1];
   iday   = buffer_.idate1[2];
   ihour  = buffer_.idate1[3];
   imin   = buffer_.idate1[4];
   isec   = 0;
    
   time_seconds = ouptime(iyear,imonth,iday,ihour,imin,isec);
   tmm=*gmtime(&time_seconds);


   /* find number of seconds that needs to be added or subtracted from 
      original time to bring it to the nearest hour
   */
   
   
   if ( buffer_.idate1[4] <= 30 )

      time_seconds = time_seconds - ( buffer_.idate1[4] * 60 );
   
   else
   
      time_seconds = time_seconds  + ( 60 - buffer_.idate1[4]) *60;
   
   
    tmm = *gmtime(&time_seconds);
   
    buffer_.idate1[0] = tmm.tm_year + 1900;
    buffer_.idate1[1] = tmm.tm_mon+ 1;
    buffer_.idate1[2] = tmm.tm_mday;
    buffer_.idate1[3] = tmm.tm_hour;
    buffer_.idate1[4] = tmm.tm_min;
    buffer_.idate1[5] = 0;


}

/*-------------------------------------------------------------------

   Title:    xw_and_xc                                                 
                                                                     
   Purpose:  To change metar present weather tokens to synoptic
             codes and metar sky conditions to tenths of sky cover
             and eventually into shef.                        

   Date:     OCT 31, 1996                                                                     
------------------------------------------------------------------- */


void xw_and_xc( Decoded_METAR *Mptr, int *xw_code, int *xc_code )

{

   int i, j, k, max_xw, plus, minus, xw_five[6],xw_temp;  /* dgb:01/02/97 */
   int ii;                                                /* dgb:11/28/97 */

   /* look for the following tokens and if found equate them to  

   */ 

       max_xw = 6;

       /* clear out array of xw flags */
       for ( i=0; i < max_xw; i++)
             xw_five[i] = -1;       
       ii = 0;                                             /* dgb:11/28/97 */
       i = 0;
       while ( Mptr->WxObstruct[i][0] != '\0' && i < max_xw )
       {
          j     = 0;
          k     = 0;
          minus = 0;
          plus  = 0;


          for ( j = 0; j < SIZE_XW+1; j++)
          {

             if ( strcmp(Mptr->WxObstruct[i],xw[j].t) == 0 )
             {
                xw_five[i] = j;
             }

          }
          i++;
          ii++;                                            /* dgb:11/28/97 */
          if ( ii > MAX_LOOP ) metar_error_handler("xw_and_xc"); /* dgb:11/28/97 */
       }
   
       /* find most important present wx..i.e. smallest array value */
       *xw_code = 999;
        xw_temp = 999;                                      /* dgb:01/02/97 */
        
       for ( i = 0; i < max_xw; i++)
       {
  
          if ( xw_five[i] != -1 )
          {
             if ( xw_five[i] < xw_temp )                    /* dgb:02/02/97 */
             {
                  xw_temp = xw_five[i];                     /* dgb:01/02/97 */
                 *xw_code = xw[xw_five[i]].s;
             }
          }
       }

       if ( *xw_code == 999 )
            *xw_code = 0;

   /* computer cloud cover in tenths of coverage */
   /*
      scale is:
                OLD     NEW After dgb:11/10/97
      CLR     =  0          0( tenths )
      SKC     =  0          0
      FEW     =  3          1
      SCT     =  5          3
      BKN     =  7          6
      OVC     = 10         10
       VV     = 10         10
   */
   
   ii = 0;                                                 /* dgb:11/28/97 */
   i = 0;
   *xc_code = 0;
   while ( Mptr->cldTypHgt[ i ].cloud_type[0] != '\0' &&
                     i < 6 )
   {
      if( Mptr->cldTypHgt[ i ].cloud_type[0] != '\0' )
      {
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"CLR") == 0 )
               *xc_code = 0;
          else
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"SKC") == 0 )
               *xc_code = 0;
          else
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"FEW") == 0 )
               *xc_code = 1;                                /* dgb:11/10/97 */
          else
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"SCT") == 0 )
               *xc_code = 3;                                /* dgb:11/09/97 */
          else
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"BKN") == 0 )
               *xc_code = 6;                                /* dgb:11/10/97 */                    
          else
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"OVC") == 0 )
               *xc_code = 10;
          else
          if ( strcmp(Mptr->cldTypHgt[i].cloud_type,"VV") == 0 )
               *xc_code = 10;
      }
                             
      i++;
      ii++;                                                /* dgb:11/28/97 */
      if ( ii > MAX_LOOP ) metar_error_handler("xw_and_xc"); /* dgb:11/28/97 */
   }

}
