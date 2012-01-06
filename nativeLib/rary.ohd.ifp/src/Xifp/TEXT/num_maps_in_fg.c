/* int num_maps_in_fg.c
 *
 *  Given the list of segments in the forecast group, 
 *  finds the number of MAP and MAPX time series.
 * *
 *  Input: seg_ids, nsegs
 *
 *  Returns:  number of MAP and MAPX ts in the fg.
 * 
 *  Written by:  D. Page 21 Nov. 1997
 *  Written to replace some hard coded assumptions
 *  on the number of map areas in a fg.         
 */
#define MAX_TS_ARRAY 3000

int num_maps_in_fg(char **seg_ids, int nsegs)
{
   int      i;
   int      total_num_maps=0;    

   
   for(i=0; i<nsegs; i++)
   {
      total_num_maps += num_maps_in_seg(seg_ids[i]);
   }
   
   return(total_num_maps);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/num_maps_in_fg.c,v $";
 static char rcs_id2[] = "$Id: num_maps_in_fg.c,v 1.1 1997/12/31 19:08:38 page Exp $";}
/*  ===================================================  */

}
            

            
