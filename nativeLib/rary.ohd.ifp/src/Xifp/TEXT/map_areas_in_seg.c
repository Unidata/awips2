/* File: map_areas_in_seg.c
 *
 *  Returns the mean area precipitation id given the segment id.
 *
 *  Change:  Added call to num_maps_in_seg to correctly allocate 
 *           space for loc_mapids.  dp - Nov. 1997
 */

char ** map_areas_in_seg(seg_id, nmap)

char *  seg_id;   /* segment id */
int  *  nmap;     /* number of mean area precipitation data */
{
 char **      mapids;        /* address of mean area precipitation id pointer */
 char         loc_seg_id[8]; /* location of segment id array */
 int          i, j;          /* counter */
 char **      loc_mapids;    /* address of mean area precipitation id pointer */

 memset(loc_seg_id, ' ', 8);
 strncpy(loc_seg_id, seg_id, strlen(seg_id));
 
 *nmap = num_maps_in_seg(seg_id);
 
 loc_mapids = (char **)malloc(*nmap * sizeof(char *));

 for (i = 0; i < *nmap; i++)
     {
      loc_mapids[i] = (char *)malloc(8 * sizeof(char));
      memset(loc_mapids[i], '\0', 8);
     }
 
 *nmap = 0;
 get_map_names(loc_seg_id, nmap, loc_mapids);

 if(*nmap > 0)
   {
    mapids = (char **)malloc(*nmap * sizeof(char *));
    for (i = 0; i < *nmap; i++)
	{
	 mapids[i] = (char *)malloc(9 * sizeof(char));
	 mapids[i][8] = '\0';
	 strncpy(mapids[i], loc_mapids[i], 8);
	 for (j = 0; j < 8; j++)
	      if(mapids[i][j] == ' ')
		{
		 mapids[i][j] = '\0';
		 break;
		}
	}
   }
 else
    mapids = 0L;

 free(loc_mapids);
 return(mapids);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/map_areas_in_seg.c,v $";
 static char rcs_id2[] = "$Id: map_areas_in_seg.c,v 1.2 1997/12/31 19:08:18 page Exp $";}
/*  ===================================================  */

}

