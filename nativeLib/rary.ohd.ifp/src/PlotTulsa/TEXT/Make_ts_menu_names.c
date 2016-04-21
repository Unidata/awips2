/* File: Make_ts_menu_names.c
 *
 *  Makes the selectable menu of listed time series names.
 *  This is done after the list_mask is created.
 *
 *
 */
void Make_ts_menu_names(num_ts, tsid, list_mask, ts_menu_names,
			       ts_index)
      int               num_ts;        /* Number of data points in the time series */
      int               list_mask[];   /* When this flag equals to 1 the list for the menu
					  view, of the time series name array is created. */
      int               ts_index[];    /* Time series index  */
      char              **tsid;        /* Time series id pointer address */
      char              **ts_menu_names; /* Time series menu names */
	 {
	 int   i,k;                    /* counters */

	    /* create time series name array for the menus view
	       and an index array for storing the number of each
	       listed time series in the ts_array */
	    /*ts_menu_names[0] = "Day Hr";*/
	    i=0;
	    for (k=0; k<num_ts; k++)
	    {
	       /*if(k<num_ts) printf("tsid = %s\n", tsid[k]);*/
	       if (list_mask[k] == 1)
	       {
		  ts_menu_names[i] = tsid[k];
		  ts_index[i]      = k;
		  /*printf("%s\n", ts_menu_names[i]);*/
		  i++;
	       }
	    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/Make_ts_menu_names.c,v $";
 static char rcs_id2[] = "$Id: Make_ts_menu_names.c,v 1.1 1995/09/08 14:56:53 page Exp $";}
/*  ===================================================  */

	 }  /* end of Make_ts_menu_names */
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */
