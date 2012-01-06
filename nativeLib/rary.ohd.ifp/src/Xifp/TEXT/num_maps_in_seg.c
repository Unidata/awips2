/* int num_maps_in_seg.c
 *
 *  Given the segment name, finds the number of 
 *  MAP and MAPX time series in the segment.
 *
 *  This was put together with code from get_map_names,
 *  find_datatype_in_ts, 
 *
 *  Input: seg_id
 *
 *  Returns:  number of MAP and MAPX ts in the segment.
 *  
 *  Written:  Nov. 1997 - D. Page - HRL        
 */
#include <stdio.h>
#include "c_call_f/read_ts_array.h"

#define MAX_TS_ARRAY 10000

int num_maps_in_seg(char *seg_id)
{
   int      i, nmap=0;
   int      nts;               /* number of time series data */
   float    ts[MAX_TS_ARRAY];  /* time series floating point data */
   char     map_datatype[4], map_datatype2[4];  
   int      locts;             /* location of time series array */
   int      loc_id;            /* location of time series id */
   int      loc_datatype;      /* location of time series data type */ 
   float    *ts_float;
   //char     (*ts_char)[4];
   int      *ts_char;
   char     pad_seg_id[8];     /* padded segment id array */
   
   strncpy(map_datatype, "MAP ", 4);
   strncpy(map_datatype2, "MAPX", 4);
   nts = MAX_TS_ARRAY; 
   
   memset(pad_seg_id, ' ', 8);
   strncpy(pad_seg_id, seg_id, strlen(seg_id));
 
   READ_TS_ARRAY(pad_seg_id, &nts, ts);
   
   ts_float = ts;
   ts_char  = (int *)ts;
   locts = 1;   
   while (locts > 0)
   {
      if(ts_float[locts-1] >= 1.0)
      {                              /*  There are remaining t.s.      */
         loc_id       = locts-1 + 2;
         loc_datatype = locts-1 + 4;
         if(strncmp((char *)&ts_char[loc_datatype], map_datatype, 4) == 0)
            nmap++;
         /*else if(strcmp(map_datatype2, (char *) NULL) != 0)--Changed by AV -- */
         else if(map_datatype2 != NULL)
         {
            if(strncmp((char *)&ts_char[loc_datatype], map_datatype2, 4) == 0)
               nmap++;
         }
            
         locts = ts_float[locts-1 + 1];
        
      }
      else
         locts = 0;                      /*  no more t.s.       */
   }                                     /*  end while          */
   
   return(nmap);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/num_maps_in_seg.c,v $";
 static char rcs_id2[] = "$Id: num_maps_in_seg.c,v 1.4 2006/03/28 19:46:45 aivo Exp $";}
/*  ===================================================  */

}
