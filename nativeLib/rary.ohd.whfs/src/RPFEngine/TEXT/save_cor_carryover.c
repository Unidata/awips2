/************************************************************************
   save_cor_carryover.c
   
   save_cor_carryover()
   save_cor_fpprevprod()
      
   ************************************************************************/

#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "save_carryover.h"
#include "process_vtecinfo.h"
#include "save_cor_carryover.h"

/************************************************************************
   save_cor_carryover()
   
   PURPOSE
   Saves the corrected previous product information into the database.
   
   NOTES
   The info written to the PrevProd table is re-gotten from the database.
   The info written to the FpPrevProd table is loaded into current memory
   inthe fp structure rather than re-getting the data.
   
   ************************************************************************/

int save_cor_carryover(int			numfps,
			fp_struct		*fp,
			int			numgrps,
			grp_struct		*grp,
			int			numcnty,
			county_struct   	*cnty,
			pcc_struct		*pcc,
			misc_struct		*misc,
			vtecinfo_struct	        *vtecinfo,
			char		        product_class,
			char		        *filename)
{   
   char prod_categ[PROD_CATEG_LEN + 1];
   char product_cnx[PRODUCT_LEN + 1];
   int  status;
   
   /* extract some key fields from the pcc structure */
   
   strcpy(prod_categ,  misc->cor_prevprod.prodcateg);
   strcpy(product_cnx, misc->cor_prevprod.prodid);
   

   /* log the product to the database  */
   
   status = save_product(misc, product_cnx, product_class, prod_categ, filename);
   
   if (status < 0)
      return(-1);
         
   
   /* if creating a corrected VTEC product, then save the event info 
      to the database */      
   
   if (product_class == 'T' || product_class == 'V')
      save_corvtec_events(numfps, fp, numgrps, grp, numcnty, cnty,
		          pcc, misc, vtecinfo);
   
   /* log the information for each included forecast point 
      into the FpPrevprod table.  this must be done after
      the vtecinfo is saved, because the event info is read back 
      in when reloading the prev prod info into memory. */
   
   save_cor_fpprevprod(misc, numfps, fp, product_cnx, prod_categ, vtecinfo);
   
   return(0);
}


/************************************************************************
   save_cor_fpprevprod()
   
   PURPOSE
   Adds the entries in the FpPrevprod table.
   
   NOTES
   In addition to writing the data to the database, the data values
   in current memory must also be loaded.
   The calling function sequence is responsible for loading the "previous" 
   data via load_fpprev_data().
   
   ************************************************************************/
void save_cor_fpprevprod(misc_struct	 *misc,
			 int		 numfps,
			 fp_struct	 *fp,
			 char		 *product_id,
			 char		 *prod_categ,
			 vtecinfo_struct *vtecinfo)
{
   FpPrevProd 	fppp;
   FpPrevProd   *fppHead = NULL;
   int 		i;
   int		status;
   char 	msgstr[60];
   char         where[300];   
   
   /* loop on the number of corrected events */
   
   for (i = 0; i < misc->cor_prevprod.event_cnt; i++)
   {
      /* The primary key for fpprevprod table is id+producttime, retrive record */
      
       sprintf(where, 
	   " WHERE producttime = '%s' AND lid ='%s' ",
	     misc->cor_prevprod.prod_ansi_time, misc->cor_prevprod.event_id[i]);
      
      if (misc->workstation_mode == PRACTICE_MODE)
         fppHead = (FpPrevProd *)GetFpPrevProdPractice(where);
      else	 
         fppHead = GetFpPrevProd(where);	     
	 	 	 
      if (fppHead != NULL)
      {	 
	 /* build the record to add */
	 
	 strcpy(fppp.lid, fppHead->lid);
	 strcpy(fppp.product_id, fppHead->product_id);	    
	 strcpy(fppp.prod_categ, fppHead->prod_categ);	    
	 status = timet_to_yearsec_dt(misc->system_time, &fppp.producttime);
	 strcpy(fppp.office_id, fppHead->office_id);
	 
	 	 
	 /* now for the observed and forecast stage info */
	 
	 fppp.obsvalue = fppHead->obsvalue;
	 fppp.obstime = fppHead->obstime;
	 
	 fppp.max_fcstvalue = fppHead->max_fcstvalue;
	 fppp.validtime = fppHead->validtime;
	 fppp.basistime = fppHead->basistime;
	 
	 
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
	 
	 /* free space */
	 
	 if (misc->workstation_mode == PRACTICE_MODE)
	    FreeFpPrevProdPractice((FpPrevProdPractice *)fppHead);
	 else   
	    FreeFpPrevProd(fppHead);
	 
      } /* end of valid fppHead */
      
      else /* can't find the record */
      {
         sprintf(msgstr,
	         "Can not find record from FpPrevProd/FpPrevProdPractice\n"
		 "product id, lid, producttime: %s %s %s \n",
		  product_id, misc->cor_prevprod.cor_fpid[i], misc->cor_prevprod.prod_ansi_time);
         log_msg("", msgstr);
      
      }
   }
	 	 
   
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


