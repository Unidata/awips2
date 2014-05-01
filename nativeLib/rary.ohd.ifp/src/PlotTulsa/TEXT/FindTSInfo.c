#include "cex25.h"
#include "Mods_info.h"

/* File: FindTSInfo.c
 *
 * Obtains time series data by reading the time series arrays
 *
 *
 */
void FindTSInfo(novrsn, ts_float, ts_char, ts_info, p_char, p_float, locp, tul_locp, num_ts,
		dt)
   int      novrsn;         /* Tulsa Plot version number */
   float    ts_float[];     /* Time series floating point data */
   int      *locp;          /* pointer to the location of the beginning of
			       parameter array. */
   int      tul_locp;       /* location of the first member of the Tulsa
			       parameter array  */
   int      num_ts;         /* Number of time series used in the current
			       forcast program. */
   char     ts_char[][4];   /* Time series character data */
   char     p_char[][4];    /* Parameter character data   */
   float    p_float[];      /* Parameter float data */
   TS_INFO  ts_info[];      /* Time series information structure  */
   int      dt;             /* Sample time interval   */
{
   int      i=0;            /* counter        */
   int      locts=1;        /* location of the time series array  */
   int      temp_dt;        /* temperature data associated with the time
			       interval samples. */
   int      increment;      /* increment 12 postions for version 1 and 18 for version 2 of
                               the Tulsa Plot */
   int      dt_temp, tul_locp2;     

tul_locp2 = tul_locp;

for(i=0; i<num_ts; i++)
   while(locts > 0)
   {
      temp_dt = ts_float[locts-1 + 5];
      dt_temp = dt;
      if (novrsn == 2) 
      {
         dt_temp = p_float[(*locp-1)+tul_locp2+12];
         if (i > 0)
         {
            dt_temp = p_float[(*locp-1)+tul_locp2+12+(i*18)];
         }
      }

      if((strncmp(p_char[*locp-1 + tul_locp], ts_char[locts-1 + 2], 8) == 0)
	 &&
	 (strncmp(p_char[*locp-1 + tul_locp + 2], ts_char[locts-1 + 4], 4) == 0)
	 &&
	 (dt_temp == temp_dt)) 
      {
/*       printf(" in FindTSInfo.c i num_ts dt_temp dt = %d %d %d %d\n",i,num_ts,dt_temp,dt); */
	 ts_info[i].ts_type = ts_float[locts-1];
	 memset (ts_info[i].ts_id, '\0', 9);
	 strncpy (ts_info[i].ts_id, ts_char[locts-1 + 2], 8);
	 memset (ts_info[i].data_type, '\0', 5);
	 strncpy (ts_info[i].data_type, ts_char[locts-1 + 4], 4);
	 ts_info[i].delta_t = ts_float[locts-1 + 5];

/*       printf("i=%d  ts_type=%d  ts_id=%s  data_type=%s  delta_t=%d\n",
	       i, ts_info[i].ts_type, ts_info[i].ts_id, ts_info[i].data_type,
	       ts_info[i].delta_t); */
       
	 locts = 1;
         increment = 12;
         if (novrsn == 2)
         {
            increment = 18;
         }
/*	 tul_locp = tul_locp + 12; */
         tul_locp = tul_locp + increment;
	 break;
      }
      else
	 locts = ts_float[locts-1 + 1];
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/FindTSInfo.c,v $";
 static char rcs_id2[] = "$Id: FindTSInfo.c,v 1.3 1996/12/10 20:10:24 dws Exp $";}
/*  ===================================================  */

}  /* end of FindTSInfo */

