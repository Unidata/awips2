/*
---------------------------------------------------------------------
																	   
	title:    main_metar													   
																	   
	purpose:  Main function used to drive the METAR decoder. 

    execution:The function is called by sao_decode. 

	Author:   David G. Brandon, CBRFC, Salt Lake City, UT			   
																	   
	Version:  1.0  FEB 18, 95  DGB										   
				   Original											   
              1.1  FEB 27, 96  DGB
                   Change TESTM and TESTS into METAR and SPECI
                   for METAR test period.
              1.2  JUL 8 96
                   Add check decoding/not decoding special obs.		
              1.3  JUL 12 96 DGB
                   Move the section of code that prints the input
                   buffer (observation) before the decoding portion
                   starts.  If there is a core dump problem, the
                   error file will at least contain the 'bad' ob.					
                   Remove the section of code that was used during
                   the test phase (TESTM, etc.).																	   
                   Check for missing/bad time in metar ob and do not
                   print out shef message if found.
              1.4  JUL 15 96 DGB
                   Replace '=' sign in ob with a blank.
              1.5  JAN 24 96 dgb
                   If collectives switch in not on, remove file 
                   before decoding.  If there was a memory error
                   the file would never be deleted.
              1.6  JAN 28 97 dgb
                   Do not remove input file if the decoder is run
                   in test mode.
              1.7  AUG 13 1998 dgb
                   Turn on if -strip switch is present:

                   If value > 0 and value < 10
                   or
                   If value > 10 and value < 32
                   convert to 32.
                   or
                   If value == 61
                   convert to 32.
              1.8  FEB 04 2000 DGB
                   Add capability to screen output by specific ids
                   read in from config file.
              1.9  OCT 31 2000 DGB
	           add just_output for normal output
              2.0  SEP 16 2001 DGB
	           Change <metar.h> to "metar.h"
              2.1  FEB 01 2006 RAE
                   Added function prototype for strpat().
---------------------------------------------------------------------
*/ 


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"
#include "metar.h"
extern void shef_it_normal( Decoded_METAR *Mptr );
extern int shef_it_metar( Decoded_METAR *Mptr );
extern int check_name( Decoded_METAR *Mptr );
extern int DcdMETAR( char *string , Decoded_METAR *Mptr );
extern int print_obs( FILE *fp, int comment, int header );
extern void metar_log( );
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);


extern int VERBOSE, DEBUG;
extern int AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM,  REMARKS; 
extern int NO_SPECI;                                       /* dgb:07/08/96 */
extern int COLLECTIVES;                                    /* dgb:01/24/97 */
extern int STRIP;                                          /* dgb:08/18/98 */
extern int GET_MTR_NAMES;                                  /* dgb:02/04 00 */
extern int JUST_OUTPUT;                                    /* dgb:10/31/00 */
extern int LOGIT;                                          /* dgb:11/30/04 */
void prtDMETR( Decoded_METAR *, FILE * );

void main_metar()
{ 
   char *c;                                                /* dgb:07/15/96 */
   char observation_string[SIZE_OF_MSG]; 
   Decoded_METAR Metar;
   Decoded_METAR *Mptr = &Metar;
   int  ErReturn, shef_err;                           /* dgb:07/12/96 */
   int  i;                                                 /* dgb:08/13/98 */

   /* if log is on call metar log */
   if ( LOGIT )                                           /* dgb:11/20/04 */
      metar_log();                                        /* dgb:11/20/04 */

   /* print obs */                                          /* dgb:07/12/96 */
   if ( luns_.icher != NULL )
        print_obs( luns_.icher, 0, 1 );
   if ( VERBOSE )
        print_obs( stdout, 0, 1 );


   if ( NO_SPECI )                                         /* dgb:07/08/96 */
   {
          if ( strcmp(forms_.type_report,"SPL METAR") == 0 )
               return;
   }


   if ( !COLLECTIVES )                                     /* dgb:01/24/97 */
   {
      if ( !test_.test_flag )                              /* dgb:01/28/97 */
      {
         fclose( luns_.lchn );                             /* dgb:01/24/97 */
         remove( files_.shef_in);                          /* dgb:01/24/97 */      
      }
   }
      

   strcpy(observation_string,databuf_.sdata);
   if ( (c = strchr(observation_string,'=')) != NULL )     /* dgb:07/15/95 */
         memset(c,32,1);


   if ( STRIP )                                            /* dgb:08/18/98 */
   {
      i = 0;                                               /* dgb:08/18/98 */  
      while ( observation_string[i] != 0 )                 /* dgb:08/18/98 */
      {                                                    /* dgb:08/18/98 */ 
         if ( observation_string[i] == 61 )                /* dgb:08/18/98 */
              memset(&observation_string[i],32,1);         /* dgb:08/18/98 */ 
         if ( observation_string[i] > 0 &&                 /* dgb:08/18/98 */
              observation_string[i] < 10 )
              memset(&observation_string[i],32,1);         /* dgb:08/18/98 */ 
         if ( observation_string[i] > 10 &&                /* dgb:08/18/98 */
              observation_string[i] < 32 )
              memset(&observation_string[i],32,1);         /* dgb:08/18/98 */ 
         i++;
      }                                                    /* dgb:08/18/98 */         
   }
   

   /* decode METAR report */
   if ( (ErReturn = DcdMETAR( observation_string, Mptr )) != 0 )
   {

      if ( ErReturn == 12 )
      { 
         if ( VERBOSE )
         {
             if ( luns_.icher != NULL )
             {
                fprintf(stdout,"\n ----------------------------");
                fprintf(stdout,"\n  ERROR: ");                                   
                fprintf(stdout,"\n ----------------------------\n\n");
                fprintf(stdout,"\nMETAR:bad station name");

             }
         }

         if ( VERBOSE )
         if ( luns_.icher != NULL )
              fprintf(luns_.icher,"\nMETAR:bad station name");
      }
      else
      if ( ErReturn == 13 )
      { 
         if ( VERBOSE )
              fprintf(stdout,"\nMETAR:missing or error on time value UTC");
         if ( luns_.icher != NULL )
              fprintf(luns_.icher,"\nMETAR:missing or error on time value UTC");
      }
   }


   /* print decoded data */
   if ( luns_.icher != NULL )
        prtDMETR( Mptr, luns_.icher );

   if ( VERBOSE )
        prtDMETR( Mptr, stdout );

   if ( JUST_OUTPUT == 0 )
   {
      /* turn into SHEF */
      if ( ErReturn == 0 )
      {
         if ( GET_MTR_NAMES )
         {
            if ( check_name( Mptr ) )
            {
              shef_err = shef_it_metar( Mptr );
              if ( shef_err == 1 )
              {
              if ( VERBOSE )
                 fprintf(stdout,"\nMETAR:missing or error on time value UTC");
              if ( luns_.icher != NULL )
                 fprintf(luns_.icher,"\nMETAR:missing or error on time value UTC");
              }
            }
         }
         else
         {     
            shef_err = shef_it_metar( Mptr );
           if ( shef_err == 1 )
           {
             if ( VERBOSE )
               fprintf(stdout,"\nMETAR:missing or error on time value UTC");
             if ( luns_.icher != NULL )
              fprintf(luns_.icher,"\nMETAR:missing or error on time value UTC");
           }
         } 
      }
   }
   else
   if ( JUST_OUTPUT > 0 )
   {
      /* output observation in indiviudal file */   

            shef_it_normal( Mptr ); 
   
   }
}

/* entire routine below entered on dgb:02/04/00 */
int check_name( Decoded_METAR *Mptr )                           /* dgb:02/04/00 */
{
int i;


   for ( i=0; i < MAX_NUM_MTR_NAMES; i++)
   {

     if ( strlen(&mtr_names[i][0]) == 0 ) 
          break;
     if ( strpat(&mtr_names[i][0],&Mptr->stnid[1],(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(mtr_names[i])) >= 0 )
     {
           return(1);
           break;
     }  
   }
   return(0);
}
