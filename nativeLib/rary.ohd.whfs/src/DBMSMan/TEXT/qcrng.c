#include <stdio.h>
#include "LocDataLimits.h"
#include "DataLimits.h"
#include <sqlca.h>

/* extern long int SQLCODE; */

      void qcrng(char id[9], char pe[3], char strdt[22],
	double *value, short int *idurat, int *qcpass,
        long int *irc1, long int *irc2)
{
/*
   this function checks the THE DATA VALUE AGAINST THE MAX AND MIN VALUES
     from the LocDataLimits table for a specific station
   if no record is found in the LocDataLimits table, then it CHECKS THE DATA
     VALUE AGAINST THE MAX AND MIN VALUES FROM the DataLimits table
   if no record is found in either table, then data passes qc check

   the max and min values in both tables depend on date and duration

   CALLING SUBROUTINES: pc2pp(ofsde),wr2dbcal(siipp)

   qcpass = 0 -- data passed quality control test
	  = 1 --  "   did not pass qc test
*/

      float max=0.0, min=0.0;
      char dttm[6];
      char whereloc[150],wheredef[150];

      LocDataLimits *locHead,*locPtr;
      DataLimits *defHead,*defPtr;

      *irc2 = -99;

      sprintf(dttm, "%c%c-%c%c",strdt[5],strdt[6],strdt[8],strdt[9]);

sprintf(whereloc,"WHERE lid='%s' AND pe='%s' AND dur='%d' AND (monthdaystart <= '%s' AND monthdayend >= '%s')",
               id,pe,*idurat,dttm,dttm);
      locHead = GetLocDataLimits(whereloc);
      *irc1 = SQLCODE;

      if(locHead)
      {

        locPtr = (LocDataLimits*) ListFirst(&locHead->list);

        min = locPtr->gross_range_min;
        max = locPtr->gross_range_max;

        FreeLocDataLimits(locHead);

      }
      else
        *irc1 = 100;  /* set irc1 to 100 because SQLCODE returned from GetLocDataLimits for case of no
                         records found = 0  */

      if(*irc1 == 100)
      {

        sprintf(wheredef,"WHERE pe='%s' AND dur='%d' AND (monthdaystart <= '%s' AND monthdayend >= '%s')",
               pe,*idurat,dttm,dttm);
        defHead = GetDataLimits(wheredef);
        *irc2 = SQLCODE;

        if(defHead)
        {

          defPtr = (DataLimits*) ListFirst(&defHead->list);

          min = defPtr->gross_range_min;
          max = defPtr->gross_range_max;

          FreeDataLimits(defHead);

        }
        else
          *irc2 = 100;

      }

      if(*irc1 == 0 || *irc2 == 0)
      {
        if(*value >= min && *value <= max) *qcpass=0;
        else *qcpass=1;
      }

      else *qcpass=0;

}  /* end function qcrng  */
