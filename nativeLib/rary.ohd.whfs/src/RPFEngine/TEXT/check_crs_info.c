/*********************************************************************
   check_crs_info.c
      
   check_loc_in_tower()
   check_grp_in_tower()
   check_numloc_in_tower()
   check_locs_covered()
   county_unique_prodcodes()

   
   *******************************************************************/

#include <stdio.h>             /* standard io library functions */
#include <string.h>            /* library string functions */
#include <stdlib.h>            /* standard library functions */

#include "check_crs_info.h" 

extern char paramdir[];
extern char productdir[];
extern char file_suffix[];
  

/*********************************************************************
   check_loc_in_tower()
   
   PURPOSE
   Checks if the given location is one of the locations
   associated with the given tower, as determined by scanning
   the complete list of location-tower associations.
   
   *******************************************************************/

int check_loc_in_tower(const char 	        *lid,
		             NWRTransmitter	*ntransPtr,
		             LocTransmit	*loctransPtr)
{
   int 		match = FALSE;
   LocTransmit	*ltPtr = NULL;
   
   
   if (loctransPtr != NULL)
      ltPtr = (LocTransmit *) ListFirst(&loctransPtr->list);
   
   while(ltPtr)
   {
      if ((strcmp(lid,                  ltPtr->lid)       == 0) &&
	  (strcmp(ntransPtr->call_sign, ltPtr->call_sign) == 0))
      {
	 match = TRUE;
	 break;
      }
      
      ltPtr = (LocTransmit *) ListNext(&ltPtr->node);
   }
   
   return(match);
   
}


/*********************************************************************
   check_grp_in_tower()
   
   PURPOSE
   Checks if the given grp has at least one of its locations
   associated with the given tower, as determined by scanning
   the complete list of location-tower associations.
   
   *******************************************************************/

int check_grp_in_tower(const grp_struct		*grp,
		       const int		grpindex,
		       const fp_struct		*fp,
		             misc_struct	*misc)
{
   int	match;
   int	j, fpindex;
   int	include_fp;
   
   match = FALSE;
   
   for (j = 0; j < grp[grpindex].numfps; j++)
   {
      fpindex = grp[grpindex].fpindex[j];
      
      if (misc->fps_included[fpindex] == TRUE)
      {	 
	 include_fp = check_loc_in_tower(fp[fpindex].id,
					 misc->nwrtransPtr, 
					 misc->loctransPtr);
	 if (include_fp)
	 {
	    match = TRUE;
	    break;
	 }
      }
   }
   
   return(match);
}


/*********************************************************************
   check_numloc_in_tower()
   
   PURPOSE
   Given a set of forecast points and towers, checks to see
   how many of the forecast points are within the towers
   area of coverage.
   
   This function also supports the logging of the ids in the 
   tower area to the output file as comment information.
   The tower id is also written, which is used later (i.e. not
   a comment line!).
   
   *******************************************************************/

int check_numloc_in_tower(const 	int		numfps,
		          const 	fp_struct	*fp,
		    	  		misc_struct	*misc,
					NWRTransmitter	*ntransPtr, 
					LocTransmit	*loctransPtr,
					FILE		*outfile_ptr)
{
   int	j;
   int	include_in_product;
   int	cnt = 0;
   int 	first = TRUE;
   int	ids_per_line = 9;
   char	outstr[500];
   
   
   /* always write this comment info regardless of whether no locations
      found in tower area */
   
   sprintf(outstr,
	   "%s\n"
	   "%s ********** start of tower product ***********\n",
	   NWR_COMMENT, NWR_COMMENT);
   write_line_text(outstr, outfile_ptr);
   
   
   
   /* loop on the number of forecast points */
   
   for (j = 0; j < numfps; j++)
   {
      if (misc->fps_included[j] == TRUE)
      {
	 include_in_product =
	    check_loc_in_tower(fp[j].id, ntransPtr, loctransPtr);
	 
	 if (include_in_product)
	 {	    
	    /* upon hitting the first location, write the all-important
	       product start line and other info */
	    
	    if (first)
	    {
	       sprintf(outstr,
		       "%s: %s %s %s\n", NWR_PRODUCT_START,
		       ntransPtr->call_sign, ntransPtr->transmit_prod_code,
		       ntransPtr->wfo);
	       write_line_text(outstr, outfile_ptr);
	       
	       sprintf(outstr, 
		       "%s call_sign, city, wfo: %s, %s, %s\n",
		       NWR_COMMENT, ntransPtr->call_sign, ntransPtr->city,
		       ntransPtr->wfo);
	       write_line_text(outstr, outfile_ptr);
	       
	       sprintf(outstr,
		       "%s locations defined for tower area of coverage:\n",
		       NWR_COMMENT);
	       write_line_text(outstr, outfile_ptr);
	       
	       sprintf(outstr, "%s %s", NWR_COMMENT, fp[j].id);
	       write_line_text(outstr, outfile_ptr);
	       
	       first = FALSE;
	    }
	    
	    else
	    {
	       if ((cnt % ids_per_line) == 0)
		  sprintf(outstr, "\n%s ", NWR_COMMENT);
	       else
		  sprintf(outstr, ", ");
	       write_line_text(outstr, outfile_ptr);
	       
	       sprintf(outstr, "%s", fp[j].id);
	       write_line_text(outstr, outfile_ptr);
	    }
	    
	    cnt++;
	    
	 }    /* if check on location associated with tower. */	 
      }       /* if check on location in product */
   }
   
   
   /* show the count of the number of matching locations. */
   
   
   if (cnt > 0)
   {
      sprintf(outstr, " (%d total)\n", cnt);
      write_line_text(outstr, outfile_ptr);
   }

   else
   {
      sprintf(outstr, 
	      "%s call_sign, city, wfo: %s, %s, %s\n",
	      NWR_COMMENT, ntransPtr->call_sign, ntransPtr->city,
	      ntransPtr->wfo);
      write_line_text(outstr, outfile_ptr);
      
      sprintf(outstr, "%s no locations defined for tower area of coverage.\n",
	      NWR_COMMENT);
      write_line_text(outstr, outfile_ptr);
   }
   
   
   return(cnt);
}


/*********************************************************************
   check_locs_covered()
   
   PURPOSE
   Given a set of forecast points and towers, checks to see
   that each forecast point is covered by at least one tower.
   This is done for logging purposes.
   
   *******************************************************************/

int check_locs_covered(const 	int		numfps,
		       const 	fp_struct	*fp,
		    	  	misc_struct	*misc,
				NWRTransmitter	*nwrtransmitPtr, 
				LocTransmit	*loctransPtr,
				FILE		*outfile_ptr)
{
   int			j;
   int			tower_cnt = 0;
   int			first = TRUE;
   int 			match_otherwfo, match_wfo;
   LocTransmit		*ltPtr = NULL;
   NWRTransmitter	*ntPtr = NULL;
   char			msgstr[80];
   int			ids_per_line = 5;
   char			outstr[500];

   
   /* log info about the number and the towers being considered */
   if (nwrtransmitPtr == NULL)
      tower_cnt = 0;
   else
      tower_cnt = ListCount(&nwrtransmitPtr->list);
   
   sprintf(outstr, "%s\n%s %d NWR towers(controlling wfo) considered:\n%s ",
	   NWR_COMMENT, NWR_COMMENT, tower_cnt, NWR_COMMENT);
   write_line_text(outstr, outfile_ptr);
	    
   if (nwrtransmitPtr != NULL)
      ntPtr = (NWRTransmitter *) ListFirst(&nwrtransmitPtr->list);
   j = 0;
   
   while(ntPtr)
   {
      sprintf(outstr, " %s(%s)", ntPtr->call_sign, ntPtr->wfo);      
      write_line_text(outstr, outfile_ptr);
	    
      ntPtr = (NWRTransmitter *) ListNext(&ntPtr->node);
      j++;
      
      if ((j % ids_per_line) == 0)
      {
	 sprintf(outstr, "\n%s ", NWR_COMMENT);
	 write_line_text(outstr, outfile_ptr);
      }
   }
   
   sprintf(outstr, "\n");
   write_line_text(outstr, outfile_ptr);
   
   
   /* loop on forecast points, and see which ones are not covered */
   
   for (j = 0; j < numfps; j++)
   {      
      if (misc->fps_included[j] == TRUE)
      {
         if (loctransPtr != NULL)
         	ltPtr = (LocTransmit *) ListFirst(&loctransPtr->list);
	 
	 /* check if location is in list for either the current wfo
	    of another wfo.  if so, then all is well */
	 
	 match_otherwfo = match_wfo = FALSE;
	 
	 while(ltPtr)
	 {
	    if (strcmp(fp[j].id, ltPtr->lid) == 0)
	    {	       
	       /* if the ids match, then it matches at least 
		  a wfo besides the host. check further below
		  to see if matces the host wfo. */
	       
	       match_otherwfo = TRUE;
	       
	       
	       /* now check if the match is for the host wfo */
	       
	       if (nwrtransmitPtr != NULL)
	       	   ntPtr = (NWRTransmitter *) ListFirst(&nwrtransmitPtr->list);
	       
	       while(ntPtr)
	       {
		  if (strcmp(ltPtr->call_sign, ntPtr->call_sign) == 0)
		  {
		     if (strcmp(ntPtr->wfo, misc->hsa) == 0)
		     {
			match_wfo = TRUE;
			break;
		     }
		  }
		  ntPtr = (NWRTransmitter *) ListNext(&ntPtr->node);
	       }	       
	    }
	    
	    ltPtr = (LocTransmit *) ListNext(&ltPtr->node);
	 }
	 
	 
	 /* if location not found in list, then issue message */
	 
	 if (!match_wfo)
	 {
	    if (first)
	    {
	       sprintf(outstr,
		       "%s note: locations included in product but...\n",
		       NWR_COMMENT);
	       write_line_text(outstr, outfile_ptr);
	       first = FALSE;
	    }
	    
	    if (match_otherwfo)
	    {
	       sprintf(outstr,
		       "%s %s-%s defined for tower controlled by other wfo.\n",
		       NWR_COMMENT, fp[j].id, fp[j].name);
	       write_line_text(outstr, outfile_ptr);
	       
	       sprintf(msgstr, "%s-%s", fp[j].id, fp[j].name); 
	       log_msg(OTHER_WFO_TOWER, msgstr);
	    }
	    
	    else
	    {
	       sprintf(outstr, "%s %s-%s not defined for any tower.\n",
		       NWR_COMMENT, fp[j].id, fp[j].name);
	       write_line_text(outstr, outfile_ptr);
	       
	       sprintf(msgstr, "%s-%s", fp[j].id, fp[j].name); 
	       log_msg(NO_ASSIGNED_TOWER, msgstr);
	    }
	 }  	 
      }     /* end of if check for whether forecast point included */
   }        /* end of loop of forecast points */
   
   
   return(0);
}


/*********************************************************************

   count_unique_prodcodes() 
   
   *******************************************************************/

int count_unique_prodcodes(char *where)
{
   UniqueList   *ulistPtr = NULL;
   int		cnt;
   
   ulistPtr = LoadUnique("transmit_prod_code", "NWRTransmitter", where, &cnt);
   
   if (ulistPtr != NULL)
	FreeUnique(ulistPtr);
   
   return(cnt);
}
