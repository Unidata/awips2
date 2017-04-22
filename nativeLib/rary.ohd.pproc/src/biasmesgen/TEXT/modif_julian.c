#include <stdio.h>
#include "string.h"
#include "mesg_hdr.h"

/*--------------------------------------------------------------*/
/*       Function to determine Julian Date since 1/1/70         */
/*--------------------------------------------------------------*/
  
short int modif_julian( short int gen_date_hour[6] )
{
    short int  ix_yr = 0, ix_mo = 1, ix_da = 2;
    short int  ref_year = 1970, delta_yrs, elapsd_mos;
    short int  days_in_mo[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}; 
    short int  days_thru_mo[12];

    int   n, elapsd_days;

/*---------------------------------------------------------*/
/*   Determine and print days-thru-end of previous month   */
/*---------------------------------------------------------*/

    days_thru_mo[0] = 0;
    for( n=1; n<12; n++)
    {
       days_thru_mo[n] = days_thru_mo[n-1] + days_in_mo[n-1];
    }
/*
    printf("Days in Month:\n");
    for( n=0; n<12; n++)
    {
       printf("%d ",days_in_mo[n]);
    }
    printf("\nDays thru Month:\n");
    for( n=0; n<12; n++)
    {
       printf("%d ",days_thru_mo[n]);
    }
    printf("\n");
*/

/*------------------------------------------------------------*/
/*   Determine modified Julian day no. since day one 1/1/70   */
/*   (accounting for one extra day in leap years after 2/29   */
/*------------------------------------------------------------*/

    delta_yrs = (gen_date_hour[ix_yr] - ref_year);
    elapsd_mos = (gen_date_hour[ix_mo] - 1);
    
    elapsd_days = (delta_yrs * 365.25 + 0.250001) + days_thru_mo[elapsd_mos] + gen_date_hour[ix_da];
    if ( (gen_date_hour[ix_yr] % 4) == 0 && gen_date_hour[ix_mo] > 2)  elapsd_days += 1;

    return( elapsd_days);
}
