/*******************************************************************************
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
*   Jan 15 2008  Paul Tilles, 
*                Ram Varma         New Code
*
*********************************************************************************/


/*-------------------------------------------------------------------------*/
/*  Function to write 1hr values resulting from disagg to the HourlyPP table */
/*  These 1hr values are the results from disagging 6hr values             */
/*                                                                         */
/*  PK fields in the HourlyPP table are lid, ts, obsdate                   */
/*  Each record in the HourlyPP table contains 24 1hr values               */
/*  Units of these values are inches x 100                                 */
/*  Records are written to the HourlyPP table with ts set to 'PZ' and      */
/*     hourlyqc flags all set to 'D'                                       */
/*                                                                         */
/*  Input to function:                                                     */
/*    structure containing the disagg station identifiers                  */
/*       (disagg_station_6hr[j*num_disagg_stations+i].hb5)                 */
/*    structure containing the 1hr values resulting from disagg            */
/*       (disaggValues.HourlyValues)                                       */
/*    array of obsdates (obsdates array)                                   */
/*    number of days of data to disagg = num_days                          */
/*    number of 6hr stations to disagg = num_records                       */
/*    flag indicating if current hour is > or < 12z  = hrgt12z             */
/*       hrgt12z = 1 -- current hour gt 12z                                */
/*       hrgt12z = 0 -- current hour lt 12z                                */
/*                                                                         */
/*  Output from function:                                                  */
/*    Write to log file:                                                   */
/*        lid, hourly values for all stations                              */
/*-------------------------------------------------------------------------*/
/*  Array definitions:                                                     */
/*                                                                         /
/*  disaggValues[ind1].HourlyValues[ind2]                                  */
/*     ind1 = j*num_disagg_stations+i                                      */
/*     ind2 = 6*k+l                                                        */
/*     where                                                               */
/*      j = index on days to QC = 0 to num_days                            */
/*      i = index on disagg stations = 0 to num_disagg_stations            */
/*      k = index on 6hr periods = 0,1,2,3                                 */
/*      l = index on 1hr slots = 0,1,2,3,4,5                               */
/*                                                                         */
/*  disagg_station_6hr[ind1].hb5                                           */
/*      where ind1 defined as above                                        */   
/*-------------------------------------------------------------------------*/

#include "DbmsDefs.h"
#include "HourlyPP.h"
#include "disagg.h"
#include "gageqc_types.h"

extern struct station * disagg_station_6hr;
extern struct Values_1hr * disaggValues;
extern time_t end_time_temp;
extern FILE *disagg_log_fd;
extern date_t * obsdate_date_t;
extern char** obsdate;
extern int hrgt12z;
extern struct Values_6hr * values6hr;

void Write1hrValuesFor6hrGages()
{

   char ts[SHEF_TS_LEN + 1];
   char where[100];
   int  ret, i, j, k, jj;
   int num_insert=0, num_update=0;
   int index = -1;

   HourlyPP hourlyPP;
   HourlyPP *pHourlyPP = NULL;

   int num_days_to_qc = get_num_days_to_qc();

   ts[SHEF_TS_LEN] = '\0';
   ts[0] = 'P';
   ts[1] = 'Z';

   strcpy(hourlyPP.ts,ts);
   strcpy(hourlyPP.minute_offset, "------------------------");
   strcpy(hourlyPP.hourly_qc, "DDDDDDDDDDDDDDDDDDDDDDDD");

   hourlyPP.sixhr06 = -9999;
   hourlyPP.sixhr12 = -9999;
   hourlyPP.sixhr18 = -9999;
   hourlyPP.sixhr24 = -9999;
   strcpy(hourlyPP.sixhrqc, "ZZZZ");
   strcpy(hourlyPP.sixhroffset, "----");

   fflush(disagg_log_fd);
   
/*-----------------------------------------*/
/*  populate the HourlyPP structure        */
/*-----------------------------------------*/

for (j=0;j<num_days_to_qc+1;j++)
{
   fprintf(disagg_log_fd, "\n");
   fprintf(disagg_log_fd, " \t Day %d\n", j);
   hourlyPP.obsdate = obsdate_date_t[j];

   jj = j;
   if(j == num_days_to_qc) jj = jj - 1;

   for (i=0;i<num_disagg_stations;i++)
   {

      index = jj*num_disagg_stations+i; // do not allow jj to be > num_days_to_qc

      strcpy(disaggValues[index].ID, values6hr[index].ID);
      strcpy(hourlyPP.lid, disaggValues[index].ID);
	 
      fprintf(disagg_log_fd, "\n%s\n", disaggValues[index].ID);

      /*----------------------------------------------*/
      /*   Store values in hourlyPP structure         */
      /*   hourlyPP.hourxx is a smallint              */
      /*   hourlyPP.hourxx has units of inches x 100  */
      /*----------------------------------------------*/

      if(j == 0)
      {

           /*----------------------------------------------*/
           /*  first day - partial day defined             */
           /*  second half of day is missing               */
           /*----------------------------------------------*/

	    hourlyPP.hour1 = (int)disaggValues[index].HourlyValues[12];
	    hourlyPP.hour2 = (int)disaggValues[index].HourlyValues[13];
	    hourlyPP.hour3 = (int)disaggValues[index].HourlyValues[14];
	    hourlyPP.hour4 = (int)disaggValues[index].HourlyValues[15];
	    hourlyPP.hour5 = (int)disaggValues[index].HourlyValues[16];
	    hourlyPP.hour6 = (int)disaggValues[index].HourlyValues[17];
	    hourlyPP.hour7 = (int)disaggValues[index].HourlyValues[18];
	    hourlyPP.hour8 = (int)disaggValues[index].HourlyValues[19];
	    hourlyPP.hour9 = (int)disaggValues[index].HourlyValues[20];
	    hourlyPP.hour10 = (int)disaggValues[index].HourlyValues[21];
	    hourlyPP.hour11 = (int)disaggValues[index].HourlyValues[22];
	    hourlyPP.hour12 = (int)disaggValues[index].HourlyValues[23];
	    hourlyPP.hour13 = -9999;
	    hourlyPP.hour14 = -9999;
	    hourlyPP.hour15 = -9999;
	    hourlyPP.hour16 = -9999;
	    hourlyPP.hour17 = -9999;
	    hourlyPP.hour18 = -9999;
	    hourlyPP.hour19 = -9999;
	    hourlyPP.hour20 = -9999;
	    hourlyPP.hour21 = -9999;
	    hourlyPP.hour22 = -9999;
	    hourlyPP.hour23 = -9999;
	    hourlyPP.hour24 = -9999;
       }
       else if(j == jj)
       {

            /*----------------------------------------------*/
            /*  middle days - full day defined              */
            /*  values are from different hydrologic days   */
            /*----------------------------------------------*/

            index = jj*num_disagg_stations+i;

	    hourlyPP.hour1 = (int)disaggValues[index].HourlyValues[12];
	    hourlyPP.hour2 = (int)disaggValues[index].HourlyValues[13];
	    hourlyPP.hour3 = (int)disaggValues[index].HourlyValues[14];
	    hourlyPP.hour4 = (int)disaggValues[index].HourlyValues[15];
	    hourlyPP.hour5 = (int)disaggValues[index].HourlyValues[16];
	    hourlyPP.hour6 = (int)disaggValues[index].HourlyValues[17];
	    hourlyPP.hour7 = (int)disaggValues[index].HourlyValues[18];
	    hourlyPP.hour8 = (int)disaggValues[index].HourlyValues[19];
	    hourlyPP.hour9 = (int)disaggValues[index].HourlyValues[20];
	    hourlyPP.hour10 = (int)disaggValues[index].HourlyValues[21];
	    hourlyPP.hour11 = (int)disaggValues[index].HourlyValues[22];
	    hourlyPP.hour12 = (int)disaggValues[index].HourlyValues[23];

            index = (jj-1)*num_disagg_stations+i; // values from previous hyd day

	    hourlyPP.hour13 = (int)disaggValues[index].HourlyValues[0];
	    hourlyPP.hour14 = (int)disaggValues[index].HourlyValues[1];
	    hourlyPP.hour15 = (int)disaggValues[index].HourlyValues[2];
	    hourlyPP.hour16 = (int)disaggValues[index].HourlyValues[3];
	    hourlyPP.hour17 = (int)disaggValues[index].HourlyValues[4];
	    hourlyPP.hour18 = (int)disaggValues[index].HourlyValues[5];
	    hourlyPP.hour19 = (int)disaggValues[index].HourlyValues[6];
	    hourlyPP.hour20 = (int)disaggValues[index].HourlyValues[7];
	    hourlyPP.hour21 = (int)disaggValues[index].HourlyValues[8];
	    hourlyPP.hour22 = (int)disaggValues[index].HourlyValues[9];
	    hourlyPP.hour23 = (int)disaggValues[index].HourlyValues[10];
	    hourlyPP.hour24 = (int)disaggValues[index].HourlyValues[11];

        }
        else
        {

            /*----------------------------------------------*/
            /*  last day - partial day defined              */
            /*  second half of day is updated               */
            /*  first half of day is previously defined     */
            /*----------------------------------------------*/

	    hourlyPP.hour13 = (int)disaggValues[index].HourlyValues[0];
	    hourlyPP.hour14 = (int)disaggValues[index].HourlyValues[1];
	    hourlyPP.hour15 = (int)disaggValues[index].HourlyValues[2];
	    hourlyPP.hour16 = (int)disaggValues[index].HourlyValues[3];
	    hourlyPP.hour17 = (int)disaggValues[index].HourlyValues[4];
	    hourlyPP.hour18 = (int)disaggValues[index].HourlyValues[5];
	    hourlyPP.hour19 = (int)disaggValues[index].HourlyValues[6];
	    hourlyPP.hour20 = (int)disaggValues[index].HourlyValues[7];
	    hourlyPP.hour21 = (int)disaggValues[index].HourlyValues[8];
	    hourlyPP.hour22 = (int)disaggValues[index].HourlyValues[9];
	    hourlyPP.hour23 = (int)disaggValues[index].HourlyValues[10];
	    hourlyPP.hour24 = (int)disaggValues[index].HourlyValues[11];
        }
	 
       memset(where, '\0', 100);
       sprintf ( where, "WHERE lid='%s' AND ts='PZ' AND obsdate='%s'",
                     hourlyPP.lid, obsdate[j]) ;
       fprintf(disagg_log_fd, "%s\n", where);
       fprintf(disagg_log_fd, "\n");
	    
       if(j == num_days_to_qc)
       {
          fprintf(disagg_log_fd, 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
           hourlyPP.hour13,
           hourlyPP.hour14,
           hourlyPP.hour15,
           hourlyPP.hour16,
           hourlyPP.hour17,
           hourlyPP.hour18,
           hourlyPP.hour19,
           hourlyPP.hour20,
           hourlyPP.hour21,
           hourlyPP.hour22,
           hourlyPP.hour23,
           hourlyPP.hour24
           );
       }
       else
       {
          fprintf(disagg_log_fd, 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
           hourlyPP.hour1,
           hourlyPP.hour2,
           hourlyPP.hour3,
           hourlyPP.hour4,
           hourlyPP.hour5,
           hourlyPP.hour6,
           hourlyPP.hour7,
           hourlyPP.hour8,
           hourlyPP.hour9,
           hourlyPP.hour10,
           hourlyPP.hour11,
           hourlyPP.hour12,
           hourlyPP.hour13,
           hourlyPP.hour14,
           hourlyPP.hour15,
           hourlyPP.hour16,
           hourlyPP.hour17,
           hourlyPP.hour18,
           hourlyPP.hour19,
           hourlyPP.hour20,
           hourlyPP.hour21,
           hourlyPP.hour22,
           hourlyPP.hour23,
           hourlyPP.hour24
           );
        }


        fprintf(disagg_log_fd, "\n");
	 
        pHourlyPP = GetHourlyPP ( where ) ;

        if ( pHourlyPP != NULL )
        {
            ret = UpdateHourlyPP ( &hourlyPP, where ) ;
        }
        else
        {
           ret = PutHourlyPP(&hourlyPP);
        }

    } /* end for (i=0;i<num_disagg_stations;i++) */

    FreeHourlyPP( pHourlyPP );

} /* end for (j=0;j<num_days_to_qc+1;j++) */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
