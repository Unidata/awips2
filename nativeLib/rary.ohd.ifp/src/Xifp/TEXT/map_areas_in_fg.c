/* File: map_areas_in_fg.c
 *
 * Returns the mean area precipitation ids given the forcast group id.
 *
 * Change:  Add call to num_maps_in_fg so that space for mapsids can be
 *          correctly allocated. dp - Nov. 1997
 */

char ** map_areas_in_fg(char *fg_id, int *total_nmaps)

/* char *  fg_id;             forcast group id */
/* int  *  total_nmaps;       total number of mean area precipitation data */
{
 char ** idsegn;            /* address of segment id number pointer */
 char ** mapids_per_seg;    /* mean area precipitation id per segment */
 char ** mapids;            /* mean area precipitation ids */
 int nseg;                  /* number of segments */
 int nmap;                  /* number of mean area precipitations data */
 int i, j;                  /* counters */
 char ** seg_names_in_fg(); /* address of segment names in forcast group pointer */
 char ** map_areas_in_seg();/* address of mean area precipitations areas in segment pointer */
 *total_nmaps = 0;

 idsegn = seg_names_in_fg(fg_id, &nseg);
 
 *total_nmaps = num_maps_in_fg(idsegn, nseg);

 mapids = (char **)malloc(*total_nmaps * sizeof(char *));
 for(i = 0; i < *total_nmaps; i++)
    mapids[i] = (char *)malloc(9 * sizeof(char));

 *total_nmaps = 0;
 for(i = 0; i < nseg; i++)
 {
     nmap = 0;
     mapids_per_seg = map_areas_in_seg(idsegn[i], &nmap);
     for(j = 0; j < nmap; j++)
        strcpy(mapids[(*total_nmaps)++], mapids_per_seg[j]);
     if(mapids_per_seg != 0L) free(mapids_per_seg);
 }
 free(idsegn);
 return(mapids);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/map_areas_in_fg.c,v $";
 static char rcs_id2[] = "$Id: map_areas_in_fg.c,v 1.4 2006/04/27 15:02:55 aivo Exp $";}
/*  ===================================================  */

}
