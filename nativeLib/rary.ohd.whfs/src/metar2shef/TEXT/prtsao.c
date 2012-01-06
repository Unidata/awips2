/* --------------------------------------------------------------------
  
       FUNCTION
         prtsao

       PURPOSE  

         Print sao values.        

       VERSION and UPDATES (for this function only)
         1.0    FEB 22 96 David G. Brandon
                Original Version
         1.1    APR 15 96 DGB
                Minor change in lables of output for SM reports.
                Precip for 3/6 hours show (MM.M) indicating decimal
                to tenths.  Also for all temperatures, (CC.C) indicating
                readings to tenths.
                Add printout of river stage for alaska.   
 *--------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"

extern int DEBUG, VERBOSE;
extern int AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, REMARKS, METAR; 


void prtsao( FILE *fp )
{

int x = -998;

     fprintf(fp,"\n----------------------------");
     fprintf(fp,"\n DECODED: %s",forms_.type_format);
     fprintf(fp,"\n----------------------------\n");


           fprintf(fp, "\nSTATION ID          :    %s",buffer_.tempid);
           fprintf(fp, "\nOBSERVATION TIME    :   %s",buffer_.time);
           fprintf(fp, "\nTYPE OF REPORT      :     %s",forms_.type_report);
      if ( SM )
      {
           if ( buffer_.ibuf[35] > x )
             fprintf(fp, "\nTEMP (C.C)          : %6d  ",buffer_.ibuf[35]);
           if ( buffer_.ibuf[36] > x )
             fprintf(fp, "\nD.P. TEMP (C.C)     : %6d  ",buffer_.ibuf[36]);
           if ( buffer_.ibuf[38] > x )
             fprintf(fp, "\nWIND DIRECTION (DEG): %6d  ",buffer_.ibuf[38]);
           if ( buffer_.ibuf[39] > x )
             fprintf(fp, "\nWIND SPEED (KTS)    : %6d  ",buffer_.ibuf[39]);
           if ( buffer_.ibuf[40] > x )
             fprintf(fp, "\nWIND GUST  (KTS)    : %6d  ",buffer_.ibuf[40]);
           if ( buffer_.ibuf[41] > x )
             fprintf(fp, "\nALTIMETER           : %6d  ",buffer_.ibuf[41]);
           if ( buffer_.ibuf[14] > x )
             fprintf(fp, "\n6HrMAXTMP (C.C)     : %6d  ",buffer_.ibuf[14]);
           if ( buffer_.ibuf[15] > x )
             fprintf(fp, "\n12HrMINTMP(C.C)     : %6d  ",buffer_.ibuf[15]);
           if ( buffer_.ibuf[58] > x )
             fprintf(fp, "\n24HrMAXTMP(C.C)     : %6d  ",buffer_.ibuf[58]);
           if ( buffer_.ibuf[59] > x )
             fprintf(fp, "\n24HrMINTMP(C.C)     : %6d  ",buffer_.ibuf[59]);
           if ( buffer_.ibuf[57] > x )
             fprintf(fp, "\n24HR PRECIP (MM.M)  : %6d  ",buffer_.ibuf[57]);
           if ( buffer_.ibuf[16] > x )
             fprintf(fp, "\nSNOW DEPTH          : %6d  ",buffer_.ibuf[16]);
           if ( buffer_.ibuf[64] > x )
             fprintf(fp, "\n6HrMAXTMP (C.C)     : %6d  ",buffer_.ibuf[64]);
           if ( buffer_.ibuf[65] > x )
             fprintf(fp, "\n6HrMINTMP (C.C)     : %6d  ",buffer_.ibuf[65]);
           if ( buffer_.ibuf[52] > x )
             fprintf(fp, "\n3/6HR PRECIP (MM)   : %6d  ",buffer_.ibuf[52]);
           if ( buffer_.ibuf[16] > x )
             fprintf(fp, "\nSNOW DEPTH          : %6d  ",buffer_.ibuf[16]);
           if ( buffer_.ibuf[62] > x )
             fprintf(fp, "\nH2O EQUIV SNOW      : %6d ",buffer_.ibuf[62]);
           if ( buffer_.ibuf[63] > x )
             fprintf(fp, "\nSUNSHINE            : %6d  ",buffer_.ibuf[63]);
           if ( buffer_.ibuf[41] > x )
             fprintf(fp, "\nSTA PRESSURE        : %6d  ",buffer_.ibuf[41]);
           if ( buffer_.ibuf[34] > x )
             fprintf(fp, "\nSEA LEVEL PRESSURE  : %6d  ",buffer_.ibuf[34]);
           if ( buffer_.ibuf[50] > x )
             fprintf(fp, "\nPRES. CHARACTER     : %6d  ",buffer_.ibuf[50]);
           if ( buffer_.ibuf[51] > x )
             fprintf(fp, "\nPRES CHANGE         : %6d  ",buffer_.ibuf[51]);
           if ( buffer_.ibuf[53] > x )
             fprintf(fp, "\nPEAK WIND SPD (KTS) : %6d  ",buffer_.ibuf[53]);
           if ( buffer_.ibuf[54] > x )
             fprintf(fp, "\nPEAK WIND DIR       : %6d  ",buffer_.ibuf[54]);
           if ( buffer_.ibuf[55] > x )
             fprintf(fp, "\nPEAK WIND TIME      : %6d  ",buffer_.ibuf[55]);
           if ( buffer_.ibuf[60] > x )
             fprintf(fp, "\nHRLY PRECIP (MM)    : %6d  ",buffer_.ibuf[60]);
           if ( buffer_.ibuf[61] > x )
             fprintf(fp, "\nPRECIP ACCUMULATOR  : %6d  ",buffer_.ibuf[61]);
           if ( buffer_.ibufchar[1] > x )
             fprintf(fp, "\nVISIBILITY (MI)     : %6.2f   ",buffer_.ibufchar[1]);

      }
      else
      { 
           if ( buffer_.ibuf[34] > x )
             fprintf(fp, "\nPRESSURE (MB X.X)   : %6d  ",buffer_.ibuf[34]);
           if ( buffer_.ibuf[35] > x )
             fprintf(fp, "\nTEMP (F)            : %6d  ",buffer_.ibuf[35]);
           if ( buffer_.ibuf[36] > x )
             fprintf(fp, "\nD.P. TEMP  (F)      : %6d  ",buffer_.ibuf[36]);
           if ( buffer_.ibuf[38] > x )
             fprintf(fp, "\nWIND DIRECTION      : %6d  ",buffer_.ibuf[38]);
           if ( buffer_.ibuf[39] > x )
             fprintf(fp, "\nWIND SPEED  (KTS)   : %6d  ",buffer_.ibuf[39]);
           if ( buffer_.ibuf[40] > x )
             fprintf(fp, "\nWIND GUST (KTS)     : %6d  ",buffer_.ibuf[40]);
           if ( buffer_.ibuf[41] > x )
             fprintf(fp, "\nALTIMETER (IN X.XX) : %6d  ",buffer_.ibuf[41]);
           if ( buffer_.ibuf[14] > x )
             fprintf(fp, "\n6HrMAXTMP (F)       : %6d  ",buffer_.ibuf[14]);
           if ( buffer_.ibuf[15] > x )
             fprintf(fp, "\n12HrMINTMP (F)      : %6d  ",buffer_.ibuf[15]);
           if ( buffer_.ibuf[58] > x )
             fprintf(fp, "\n24HrMAXTMP (F)      : %6d  ",buffer_.ibuf[58]);
           if ( buffer_.ibuf[59] > x )
             fprintf(fp, "\n24HrMINTMP (F)      : %6d  ",buffer_.ibuf[59]);
           if ( buffer_.ibuf[57] > x )
             fprintf(fp, "\n24HR PRECIP (IN)    : %6d  ",buffer_.ibuf[57]);
           if ( buffer_.ibuf[16] > x )
             fprintf(fp, "\nSNOW DEPTH (IN)     : %6d  ",buffer_.ibuf[16]);
           if ( buffer_.ibuf[64] > x )
             fprintf(fp, "\n6HrMAXTMP (F)       : %6d  ",buffer_.ibuf[64]);
           if ( buffer_.ibuf[65] > x )
             fprintf(fp, "\n6HrMINTMP (F)       : %6d  ",buffer_.ibuf[65]);
           if ( buffer_.ibuf[52] > x )
             fprintf(fp, "\n3/6HR PRECIP (IN)   : %6d  ",buffer_.ibuf[52]);
           if ( buffer_.ibuf[16] > x )
             fprintf(fp, "\nSNOW DEPTH (IN)     : %6d  ",buffer_.ibuf[16]);
           if ( buffer_.ibuf[62] > x )
             fprintf(fp, "\nH2O EQUIV SNOW (IN) : %6d ",buffer_.ibuf[62]);
           if ( buffer_.ibuf[63] > x )
             fprintf(fp, "\nSUNSHINE (MIN)      : %6d  ",buffer_.ibuf[63]);
           if ( buffer_.ibuf[50] > x )
             fprintf(fp, "\nPRES. CHARACTER     : %6d  ",buffer_.ibuf[50]);
           if ( buffer_.ibuf[51] > x )
             fprintf(fp, "\nPRES CHANGE(IN XX.X): %6d  ",buffer_.ibuf[51]);
           if ( buffer_.ibuf[53] > x )
             fprintf(fp, "\nPEAK WIND SPD (KTS) : %6d  ",buffer_.ibuf[53]);
           if ( buffer_.ibuf[54] > x )
             fprintf(fp, "\nPEAK WIND DIR (DEG) : %6d  ",buffer_.ibuf[54]);
           if ( buffer_.ibuf[55] > x )
             fprintf(fp, "\nPEAK WIND TIME      : %6d  ",buffer_.ibuf[55]);
           if ( buffer_.ibuf[60] > x )
             fprintf(fp, "\nHRLY PRECIP (IN)    : %6d  ",buffer_.ibuf[60]);
           if ( buffer_.ibuf[61] > x )
             fprintf(fp, "\nPRECIP ACCUMULATOR  : %6d  ",buffer_.ibuf[61]);
           if ( buffer_.ibufchar[1] > x )
             fprintf(fp, "\nVISIBILITY (MI)     : %6.2f   ",buffer_.ibufchar[1]);
           if ( buffer_.ibufchar[0] > x )
             fprintf(fp, "\nSTAGE (FT)          : %6.2f   ",buffer_.ibufchar[0]);
        }
}
