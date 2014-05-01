#include <stdio.h>
#include "LocDataLimits.h"
#include "DataLimits.h"
#include <sqlca.h>

      void qc_range_values(char id[9], char pe[3], char strdt[22],
	short int *idurat, float *rng_min, float *rng_max)
{
/*
   this function returns the MAX AND MIN VALUES used for the qc range check

   the LocDataLimits table is checked first for a record
   if no record is found, then the DataLimits table is checked
   if no record is found in either table, then values =-99. are returned

   the max and min values in both tables depend on date and duration

   CALLING SUBROUTINE: pc2pp(ofsde)

*/

      long int irc;
      char dttm[6];
      char whereloc[128],wheredef[128];

      LocDataLimits *locHead,*locPtr;
      DataLimits *defHead,*defPtr;

      /*--------------------------------------------*/
      /*  check LocDataLimits table for records     */
      /*--------------------------------------------*/

      sprintf(dttm, "%c%c-%c%c",strdt[5],strdt[6],strdt[8],strdt[9]);

sprintf(whereloc,"WHERE lid='%s' AND pe='%s' AND dur='%d' AND (monthdaystart <= '%s' AND monthdayend >= '%s')",
               id,pe,*idurat,dttm,dttm);
      locHead = GetLocDataLimits(whereloc);
      irc = SQLCODE;

      if(locHead)
      {

        locPtr = (LocDataLimits*) ListFirst(&locHead->list);

        *rng_min = locPtr->gross_range_min;
        *rng_max = locPtr->gross_range_max;

        FreeLocDataLimits(locHead);

      }
      else
        irc = 100;  /* set irc to 100 because SQLCODE returned from GetLocDataLimits for case of no
                         records found = 0  */

      if(irc == 100)
      {

        /*--------------------------------------------*/
        /*  check DataLimits table for records */
        /*--------------------------------------------*/

        sprintf(wheredef,"WHERE pe='%s' AND dur='%d' AND (monthdaystart <= '%s' AND monthdayend >= '%s')",
               pe,*idurat,dttm,dttm);
        defHead = GetDataLimits(wheredef);

        if(defHead)
        {

          defPtr = (DataLimits*) ListFirst(&defHead->list);

          *rng_min = defPtr->gross_range_min;
          *rng_max = defPtr->gross_range_max;

          FreeDataLimits(defHead);

        }
        else
        {

           /*--------------------------------------------*/
           /*  records not found in either table         */
           /*--------------------------------------------*/

          *rng_min = -99.;
          *rng_max = -99.;
        }

      }

}  /* end function qc_range_values  */
