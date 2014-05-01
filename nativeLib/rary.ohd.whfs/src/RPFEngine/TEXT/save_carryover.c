/************************************************************************
   save_carryover.c
   
   save_carryover()
   save_product()
   store_purge_info()
   save_fpprevprod()
      
   ************************************************************************/

#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "save_carryover.h"


/************************************************************************
   save_carryover()
   
   PURPOSE
   Saves the previous product information into the database.
   
   NOTES
   The info written to the PrevProd table is re-gotten from the database.
   The info written to the FpPrevProd table is loaded into current memory
   inthe fp structure rather than re-getting the data.
   
   ************************************************************************/

int save_carryover(int			numfps,
		    fp_struct		*fp,
		    int			numgrps,
		    grp_struct		*grp,
		    int			numcnty,
		    county_struct	*cnty,
		    pcc_struct		*pcc,
		    misc_struct		*misc,
		    vtecinfo_struct	*vtecinfo,
		    char		product_class,
		    char		*filename)
{   
   char prod_categ[PROD_CATEG_LEN + 1];
   char product_cnx[PRODUCT_LEN + 1];
   int	vtec_set;
   int  status;
   
   /* extract some key fields from the pcc structure */
   
   strcpy(prod_categ,  pcc->product.prod_categ);
   strcpy(product_cnx, pcc->product.product_cnx);
   

   /* log the product to the database  */
   
   status = save_product(misc, product_cnx, product_class, prod_categ, filename);
   
   if (status < 0 )
      return(-1);   
   
   /* if creating a VTEC product, then save the event info 
      to the database */
   
   vtec_set = check_if_vtec(pcc);
   
   if (vtec_set)
      save_vtec_events(numfps, fp, numgrps, grp, numcnty, cnty,
		       pcc, misc, vtecinfo);
   
   /* log the information for each included forecast point 
      into the FpPrevprod table.  this must be done after
      the vtecinfo is saved, because the event info is read back 
      in when reloading the prev prod info into memory. */
   
   save_fpprevprod(misc, numfps, fp, product_cnx, prod_categ, vtecinfo);
   
   return(0);
}


/************************************************************************
   
   save_product()
   
   PURPOSE
   Adds the newly created product to the hydrologic database
   
   **********************************************************************/

int save_product( misc_struct	*misc,
		  char		*product_cnx,
		  char		product_class,
		  char		*prod_categ,
		  char		*filename)

{
 
   char 	statstr[6];
   int  	status;
   TextProduct	 newtextproduct;
   FILE		*infile_ptr  = NULL;
   char 	*fgets_ptr = NULL;
   char 	fileline[MAXLEN_STRING+1];
   char         text_product[MAXLEN_TEXT_PRODUCT] = "";
   char         product_type[2];
   bool         isUnique;
   char         msg[300]="";
    
   /*initialize the product content*/
   
   strcpy(text_product, "");
   
   product_type[0] = product_class;
   product_type[1] = '\0';
   
   /*read the product content from filename*/
        
   infile_ptr = fopen(filename, "r");
   if (infile_ptr == NULL) log_msg(FILE_OPENERR, filename);
   
   for (;;)
   {
      fgets_ptr = fgets(fileline, MAXLEN_STRING, infile_ptr);
      
      if (fgets_ptr == NULL) break;

      if (strlen(text_product) >= MAXLEN_TEXT_PRODUCT )
      {
         sprintf(msg, "Error: exceeding max length (%d char) for product: %s",
	              MAXLEN_TEXT_PRODUCT, filename);
         log_msg("", msg);	

	 fclose(infile_ptr);
	 return(-1);
	
      }
      else	 	
         strcat(text_product, fileline);
      	    
   }
   
   fclose(infile_ptr);
   
   /* write the product to the database;
      use the same system time as used for saving the fpprevprod info.
      this is needed for the determine_issuance_number algorithm, besides
      making sense in its own right. */
   
   strcpy(newtextproduct.product_id, product_cnx);   
   status = timet_to_yearsec_dt(misc->system_time, &newtextproduct.producttime);
   status = timet_to_yearsec_dt(misc->system_time, &newtextproduct.postingtime);
   strcpy(newtextproduct.prodtype, product_type);
   newtextproduct.issnum = misc->issnum;
   
   newtextproduct.product = NULL;
   newtextproduct.product = text_product;
   
   status = InsertIfUniqueTextProduct(&newtextproduct, &isUnique);
   
/*   strcpy(newtextproduct.product, text_product);
        
   status = PutTextProduct(&newtextproduct); */
   
   if (status < 0)
   {
      sprintf(statstr, "%d", status);
      log_msg(PRODUCT_SAVEFAIL, statstr);
      return(-1);
   }
   
   
   /* load the purge product information as appropriate */
   
   else
   {
      store_purgeinfo(product_cnx, misc->system_time, misc->system_time);
   }
   
   return(0);
}


/*******************************************************************
   
   store_purgeinfo()
   
   PURPOSE
   Store the information about the product in the purgeproduct table.
   This means either a new entry is made the first time the product id
   is encountered or the time info is updated for later encounter.
   
   Note that there is no purge of the products to keep them
   in sync with the number of products to keep.  The purge
   is done at regular intervals by the purgeproduct operations.
   
   *****************************************************************/

void store_purgeinfo(char 	*prodid,
		     time_t	product_timet,
		     time_t	post_timet)     	  
   
{
   int  		status;
   PurgeProduct		newproduct;
   PurgeProduct 	*curproduct;
   char			where[120];
   char			msgstr[100];
     
   
   /* try and get the existing info for the product */
   
   sprintf(where, " where PRODUCT_ID = '%s' ", prodid);   
   curproduct = GetPurgeProduct(where);
   
   
   /* if the product is already logged, update the information */
   
   if (curproduct != NULL)
   {
      
      status = timet_to_yearsec_dt(product_timet, &curproduct->producttime);
      status = timet_to_yearsec_dt(post_timet,    &curproduct->postingtime);
      
      status = UpdatePurgeProduct(curproduct, where);
      if (status < 0)
      {
	 sprintf(msgstr,
	         "    Postgres error %d updating %s data in PurgeProduct",
		 status, prodid);
	 fprintf(stderr, msgstr);
	 log_msg("", msgstr);
      }
      
      FreePurgeProduct(curproduct);
   }
   
   
   /* if the product has not been logged, insert the information.
      set the num of versions to keep, which is also stored in
      this table. */
   
   else
   {
      strcpy(newproduct.product_id, prodid);
      newproduct.num_versions = 5;
      status = timet_to_yearsec_dt(product_timet, &newproduct.producttime);
      status = timet_to_yearsec_dt(post_timet,    &newproduct.postingtime);
      
      status = PutPurgeProduct(&newproduct);
      if (status < 0)
      {
	 sprintf(msgstr,
	         "    Postgres error %d putting %s data into PurgeProduct",
		 status, prodid);
	 fprintf(stderr, msgstr);
	 log_msg("", msgstr);
      }
      
   }
   
   return;
}
   

/************************************************************************
   save_fpprevprod()
   
   PURPOSE
   Updates/adds the entries in the FpPrevprod table.
   
   NOTES
   In addition to writing the data to the database, the data values
   in current memory must also be loaded.
   The calling function sequence is responsible for loading the "previous" 
   data via load_fpprev_data().
   
   ************************************************************************/
void save_fpprevprod(misc_struct	*misc,
		     int		numfps,
		     fp_struct		*fp,
		     char		*product_id,
		     char		*prod_categ,
		     vtecinfo_struct	*vtecinfo)
{
   FpPrevProd 	fppp;
   int 		i;
   int		status;
   char 	msgstr[60];
      
   
   /* loop on the number of forecast points and 
      process those that are included */
   
   for (i = 0; i < numfps; i++)
   {
      if (misc->fps_included[i] == TRUE)
      {	 	 	 
	 
	 /* build the record to add */
	 
	 strcpy(fppp.lid, fp[i].id);
	 strcpy(fppp.product_id, product_id);	    
	 strcpy(fppp.prod_categ, prod_categ);	    
	 status = timet_to_yearsec_dt(misc->system_time, &fppp.producttime);
	 strcpy(fppp.office_id, misc->selected_office);
	 
	 	 
	 /* now for the observed and forecast stage info */
	 
	 fppp.obsvalue = fp[i].curobs.value;
	 status = timet_to_yearsec_dt(fp[i].curobs.validtime,
				      &fppp.obstime);	    
	 
	 fppp.max_fcstvalue = fp[i].maxfcst.value;	       
	 status = timet_to_yearsec_dt(fp[i].maxfcst.validtime,
				      &fppp.validtime);
	 status = timet_to_yearsec_dt(fp[i].maxfcst.basistime,
				      &fppp.basistime);
	 
	 
	 /* add the new record */
	 
	 /* Store data in FpPrevProdPractice table if in "Practice" work station
	 mode */
	 
	 if (misc->workstation_mode == PRACTICE_MODE)
	    status = PutFpPrevProdPractice((FpPrevProdPractice *)(&fppp));
	 else	    
	    status = PutFpPrevProd(&fppp);
	    
	 if (status != 0)
	 {
	    sprintf(msgstr, "%s (%d)", fppp.lid, status);
	    log_msg(FPPREVPROD_SAVEFAIL, msgstr);
	 }
      }
   }
	 	 
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

