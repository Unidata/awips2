/*
---------------------------------------------------------------------
																	   
	title:    metardrive													   
																	   
	purpose:  Main function used to drive the metar decoder. 

    execution:The function is called by metar_decode. 

	Author:   David G. Brandon, CBRFC, Salt Lake City, UT			   
																	   
	Version:  
	      1.0  FEB 95  DGB	
	           Original														   
              2.0  JAN 8 96 DGB
                   Restructure driver to handle SM reports, and 
                   METAR when it is available.
              2.1  FEB 22, 1996 dgb
                   Added 'At end of message." message to error
                   file and out file.                   											   
                   Include function to print parsed off values of 
                   sao.										
              2.2  APR 1 96 DGB
                   Added shef_it_sm function to make it easier to
                   make changes in the shef_it routines for either
                   SA or SM reports.							   
              2.3  APR 15 96 DGB
                   Include extract_init for sm reports.
              2.4  JUL 8 96 DGB
                   Include check for decoding/not decoding specials 
                   in function main_sao.
              2.5  JUL 9 96 DGB
                   Removed "At end of message file" .
              2.6  JUL 12 96 DGB
                   To aleviate core dumps, after the file is read, 
                   but before decoding occurs, delete the input file.
                   Also, before decoding begins write the input buffer
                   to the error file so that the observation encoding
                   errors can be looked at.  However, if processing
                   collectives, only the first ob will be decoded.
              2.7  DEC 2 96 DGB
                   At SDO and SCD checks.
              2.8  DEC 23 96 DGB
                   Change code to flush buffer before closing...and
                   change the logic a little about flushing and
                   closing.  Change the location of the counter ii.
              2.9  NOV 28 97 DGB
                   Add check for endless while loops.
              3.0  NOV 17 98 DGB
                   Change MAX_LOOP from 5000 to 50000.
              3.1  DEC 13 99 dgb
                   Add i_total to be the counter that will
                   assist in the extension of the file.
	      3.2  01/10/00 DGB
	           Remove code for sao's.
	      3.3  Change the call to function strpat...the argument list
	           was changed to accomodate the newer strpat
	      3.4  FEB 04 2000 DGB
	           Add GLOBAL_SPECI check.  SPECI is not included
		   in each ob in the collective, but as a single
		   token at the beginning of the ob.  If the 
		   token is found all obs are considered SPECI.
	      3.5  FEB 26 2002 DGB
                   Add a blank before the "SPECI" string, making it
		   " SPECI".  Before this change, NOSPECI was incorrectly
		   detected as SPECI.
	      3.6  AUG 09 2002 DGB
	           Make changes in functions: metardrive, main_sdo,
		   make_sdo_ob.  Check if there is anything in the obs.
		   Prior to this there would be a core dump if there
		   was no part of the observation.
	      3.7  FEB 01 2006 RAE
	           Added a return(0) if you get to the end of make_sdo_ob().
              Added function prototypes for strpat() and main_metar().
		   
------------------------------------------------------------------------
*/ 

#define MAX_LOOP 50000                                      /* dgb:11/17/98 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"

extern int VERBOSE, DEBUG;
extern int AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, METAR, REMARKS; 
extern int GLOBAL_EOF, GLOBAL_TYPE, NO_SPECI;             /* dgb:07/08/96 */
extern int GLOBAL_SPECI;                                  /* dgb:02/04/00 */
extern int SDO, SCD;                                      /* dgb:12/02/96 */
extern int COLLECTIVES;                                   /* dgb:12/07/96 */
extern int i_total, ii_total;                             /* dgb:13/12/99 */
extern void extract_init();
extern int  metar_read();
extern void housekeeping();
extern void main_sm();
extern int  main_sdo();
extern void pause_it( char *msg);
extern int  metar_error_handler( char function[] );
extern int print_obs( FILE *fp, int comment, int header );
extern int extract_smsection();
extern int extract_smdata();
extern void smtime();
extern void prtsao( FILE *fp );
extern void shef_it_sm();
extern int make_sdo_ob(); 
extern int is_it_wx( char wxbuff[] );
extern void global_speci();
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);
extern void main_metar();

int metardrive()

{
   void date_time();
   int rtn;
   int jj;                                                 /* dgb:11/28/97 */
   int ier;                                                 /* dgb:08/09/02 */
   /*  get system date/time 
       date_time( &year, &month, &day, &hour, &minute, &second); 

       check_dtg( &year, &month, &day, &hour, &minute, &second);    
   */
    /* init arrays */


       extract_init();

            SA    = 0;
            SM    = 0;
            METAR = 0;
            RS    = 0;
            SP    = 0;
            SDO   = 0;                                     /* dgb:12/02/96 */
            SCD   = 0;                                     /* dgb:12/02/96 */
            GLOBAL_TYPE  = 0;
            GLOBAL_SPECI = 0;                              /* dgb:02/04/00 */
   
    /* read first few lines and see if SPECI or METAR token is found 
       if SPECI is found, set GLOBAL_SPECI flag to on and rewind file
    */
       global_speci();                                      /* dgb:02/04/00 */

    /* loop thru looking for reports */
       jj = 0;                                              /* dgb:11/28/97 */
       while ( ( rtn = metar_read() ) < 2 )
       {

            
      	    if ( rtn == 1 ) continue;
      	    
            if ( METAR )             /* decode METAR */
            {
               main_metar();
               ii_total++;                                  /* dgb:12/13/99 */
               i_total++;                                   /* dgb:12/13/99 */
               if ( i_total == 99 ) i_total = 0;            /* dgb:12/13/99 */
               housekeeping();                              /* dgb:12/23/96 */
            }
            
            else

            if ( SM )                /* decode SM */
            {              
               main_sm();
               ii_total++;                                  /* dgb:12/13/99 */
               i_total++;                                   /* dgb:12/13/99 */
               if ( i_total == 99 ) i_total = 0;            /* dgb:12/13/99 */
               housekeeping();                              /* dgb:12/24/96 */
            }
            
            else
            if ( SDO || SCD )        /* decode SDO/SCD */  /* dgb:12/02/96 */
            {
               ier = main_sdo();                            /* dgb:08/09/02 */  
	       if ( ier == 0 )                              /* dgb:08/09/02 */
	       {            
                  main_metar();                             /* dgb:12/02/96 */
                  ii_total++;                               /* dgb:12/13/99 */
                  i_total++;                                /* dgb:12/13/99 */
                  if ( i_total == 99 ) i_total = 0;         /* dgb:12/13/99 */
	        }  
		else                                     /* dgb:08/09/02 */
		{
                   if ( VERBOSE )
                   {
                         fprintf(stdout,"\n ----------------------------");
                         fprintf(stdout,"\n  ERROR: ");                                   
                         fprintf(stdout,"\n ----------------------------\n\n");
                         fprintf(stdout,"\nMETAR:no data in SDO/SCD osbservation");
                         fprintf(stdout,"\n%s",databuf_.remarks);

                      
                   }


                   if ( luns_.icher != NULL )
                   {

		         fprintf(luns_.icher,"\nMETAR:no data in SDO/SCD observation ");
                         fprintf(luns_.icher,"\n%s",databuf_.remarks);
		   }

                }		
		
		
		
               housekeeping();                              /* dgb:12/23/96 */

            }
                 
	       if ( DEBUG ) pause_it("continue with next obs");
           if ( GLOBAL_EOF ) break;
          jj++;                                             /* dgb:11/28/97 */
          if ( jj > MAX_LOOP ) metar_error_handler("metardrive"); /* dgb:11/28/97 */      
       }

       if ( VERBOSE ) printf("\n Number of observations processed-> = %d",ii_total);

         return(0);

}

void global_speci()                                              /*dgb:02/04/00 */
{                                                           /*dgb:02/04/00 */
int i;                                                      /*dgb:02/04/00 */
char temp_rec[SIZE_OF_RECORD];                              /*dgb:02/04/00 */

    i = 0;                                                  /*dgb:02/04/00 */
    while ( i < 4 )                                         /*dgb:02/04/00 */
    {                                                       /*dgb:02/04/00 */
       fgets(temp_rec,sizeof(temp_rec),luns_.lchn);         /*dgb:02/04/00 */
       if ( (strpat(temp_rec," SPECI",'$','%',(int)NULL,(int)NULL,strlen(temp_rec) )) >= 0 ) /*dgb:02/26/02 */
          {                                                 /*dgb:02/04/00 */
	  GLOBAL_SPECI = 1;                                 /*dgb:02/04/00 */
	  break;                                            /*dgb:02/04/00 */
	  }                                                 /*dgb:02/04/00 */
      i++;	                                            /*dgb:02/04/00 */     
    }                                                       /*dgb:02/04/00 */
    rewind(luns_.lchn);                                     /*dgb:02/04/00 */
}                                                           /*dgb:02/04/00 */

void housekeeping()                                              /* dgb:12/24/96 */
{                                                        
               SA    = 0;
               SM    = 0;
               METAR = 0;              
               RS    = 0;
               SP    = 0;
               SDO   = 0;                                   /* dgb:12/02/96 */
               SCD   = 0;                                   /* dgb:12/02/96 */
               if ( !COLLECTIVES )                          /* dgb:12/07/96 */
               {
                  if ( !test_.test_flag )
                  {
                  
                     if ( luns_.lchn )  
                     {
                        fflush(luns_.lchn);
                        fclose(luns_.lchn);
                     }
                  }
               }
}


void main_sm()
{

            /* print ob */                  /* dgb:07/12/96 */
               if ( VERBOSE ) print_obs(stdout, 0, 1 );
               if ( luns_.icher != NULL )
                    print_obs(luns_.icher, 0, 1);

            /* parse off sections of the sm report */
               extract_smsection();

            /* get data fields */
               extract_smdata();

   		 	/* construct times */
               smtime();


            /* print parsed values */
               if ( VERBOSE ) prtsao( stdout );
               if ( luns_.icher != NULL )
                    prtsao( luns_.icher );

    		/* convert to shef */
	   		   shef_it_sm();                   /* dgb:04/01/96 */

            /* init arrays */
               extract_init();                             /* dgb:04/15/96 */

}

int main_sdo( )                                                 /* dgb:12/02/96 */
{

int ier;                                                   /* dgb:08/09/02 */
           /* construct pseudo metar ob from SDO so that it 
              can be passed through the metar decoder...but
              the wind, temp, and dew point will not be used
              in the output...its only placed in the pseudo ob
              so that it can go thru the decoder.  Only the 
              remarks section will be used as output for SDO
              type observations.
           */

              ier = make_sdo_ob();                          /* dgb:08/09/02 */
	      if ( ier == -1 )                              /* dgb:08/09/02 */
	         return (-1);                               /* dgb:08/09/02 */
	      else                                          /* dgb:08/09/02 */
	         return (0);                                /* dgb:08/09/02 */

}

int make_sdo_ob()                                              /* dgb:12/02/96 */
{
char temp_metar[6], temp_id[5], temp_time[5];
char temp_wind[8], temp_tatd[8],temp_buff[SIZE_OF_REMARKS], buff[8];
char *c, *d=NULL;
int i, k, notfound;
int ii;                                                     /* dgb:11/28/97 */
int ilen;                                                   /* dgb:08/09/02 */
   memset(databuf_.remarks,0,sizeof(databuf_.remarks));
   strcpy(databuf_.remarks,databuf_.sdata);
   memset(databuf_.sdata,0,sizeof(databuf_.sdata));
   memset(temp_time,0,sizeof(temp_time));
   memset(temp_id,0,sizeof(temp_id));
   strncpy(temp_metar,"METAR",5);
   strcpy(temp_wind,"00000KT");
   strcpy(temp_tatd,"0/0 RMK");

   if ( (c = strchr(databuf_.remarks,'/')) == NULL )        /* dgb:08/09/02 */
   {
      return (-1);                                          /* dgb:08/09/02 */  
   }

   ilen = strlen (databuf_.remarks );

   if ( ilen < 14 )                                         /* dgb:08/09/02 */
   {
       return (-1 );                                        /* dgb:08/09/02 */
   }
   
   if 
   ( 
      (i = strpat(databuf_.remarks,"%%%% SDO $$$$ ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.remarks) ))  >= 0 /*dgb:01/16/00 */
      ||
      (i = strpat(databuf_.remarks,"%%%% SCD $$$$ ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.remarks) ))  >= 0 /*dgb:01/16/00 */
   )
 
  {

          strncpy(temp_id,&databuf_.remarks[i],4);
     
	
      
       strncpy(temp_time,&databuf_.remarks[i+9],4);

      if 
      ( 
     
      ( i = strpat(databuf_.remarks,"8%%%%%%",'$','%',(int)NULL,(int)NULL,strlen(databuf_.remarks) ))  >= 0 /*dgb:01/16/00 */
      )
      {
        printf("\n found cloud");
      }
      /* check if present WX is there */
      strcpy(temp_buff,&databuf_.remarks[13]);
      if ( (c = strchr(temp_buff,'/')) != NULL )
      {
         d = c;
         notfound = 1;
         k = 0;
         ii = 0;                                           /* dgb:11/28/97 */
         while ( notfound )
         {
           c--;
           k++;
           if ( k > 30 )   notfound = 0;  /* protection for endless loop */
           if ( *c == 32 ) 
           {
              strncpy(buff,c+1,d-c-1);
              notfound = 0;
           }
           ii++;                                           /* dgb:11/28/97 */
           if ( ii > MAX_LOOP ) metar_error_handler("metardrive"); /* dgb:11/28/97 */
         }
      }
     
      if ( is_it_wx(buff) == 0 ) 
      {

         /* construct pseudo ob without present wd */
         sprintf(databuf_.sdata,"%s %s %s %s %s %s",
              temp_metar,
              temp_id,
              temp_time,
              temp_wind,
              temp_tatd,
              &databuf_.remarks[13]);

      }
      else
      {

         /* construct pseudo ob with present wd */
         sprintf(databuf_.sdata,"%s %s %s %s %s %s %s",
              temp_metar,
              temp_id,
              temp_time,
              temp_wind,
              buff,
              temp_tatd,
              d+2);
      }
   }
   
   return (0);
}

/* main_metar()
{
   see function main_metar 
  
}
*/
/*-------------------------------------------------------------------

   Title:    is_it_wx                                                 
                                                                     
   Purpose:  To check if the present wx in an SDO or SDC ob
             is valid for inclusion into the pseudo ob.                        

   Date:     DEC 7, 1996                                                                     
------------------------------------------------------------------- */


int is_it_wx( char wxbuff[] )

{

   int j;

         if ( wxbuff[0] != '\0' )
         {
           for ( j = 0; j < SIZE_XW+1; j++)
            {

             if ( strcmp(wxbuff,xw[j].t) == 0 )
                return(1);
            }
         }
   
        return(0);


}
