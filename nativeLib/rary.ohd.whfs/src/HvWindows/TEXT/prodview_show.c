/************************************************************************
   
   Functions to handle the display of product information.
   
   show_prodview()
   add_prodview_cbs()
   
   init_prodlist_options()
   get_prodlist_options()
   
   load_prodlist()
   load_prodlist_info()
   
   load_textprodCB()
   update_prodlistCB()
   ok_prodviewCB()
   
   
   ***********************************************************************/
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/List.h>
#include <Xm/Text.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "TextProductManual.h"
#include "PurgeProduct.h"
#include "ProductLink.h"

#include "prodview.h"
#include "prodview_show.h"

#include "DbmsUtils.h"
#include "Xtools.h"

#include "time_convert.h"
#include "LoadUnique.h"
#include "TextProduct.h"


/* variables global to this file */

prodview_struct	prodview;
ProductLink	*prodlinkHead = NULL ;
PurgeProduct	*purgeprodHead = NULL ;
UniqueList	*ulHead = NULL ;



/************************************************************************
   
   Load the product view dialog shell.
   
   ***********************************************************************/

void show_prodview(Widget 	w,
		   const char *lid)
{
    
   create_prodviewDS(GetTopShell(w));
   add_prodview_cbs();      
   
   
   /* manage the windows now before doing the set_selections() */
   
  
   XtManageChild(prodviewFM);
   XtManageChild(prodviewDS);
     
   
   /* initialize data memory */
   
   prodlinkHead  = (ProductLink *)(NULL);
   purgeprodHead = (PurgeProduct *)(NULL);
   ulHead        = (UniqueList *)(NULL);
      
   
   /* set the environment to force the display of all products
      for the current location */
   
   strcpy(prodview.curlid, lid);
   init_prodlist_options();
   
   
   /* now load the list of products */
   
   get_prodlist_options();
   load_prodlist();   
   
   
   return;
}


/************************************************************************
    
   Add the stations selection callbacks.
   
   ************************************************************************/

void add_prodview_cbs()
{
   
   Atom	wmAtom;
   
   
   /* callbacks on scrolled list of products */
   
   XtAddCallback(prodlistLS, XmNdefaultActionCallback, load_textprodCB, NULL);
   XtAddCallback(prodlistLS, XmNbrowseSelectionCallback, load_textprodCB, NULL);
   
   
   /* callbacks on display mode and selected location !!! for text */
   
   XtAddCallback(prodsel_loc_PB,    XmNactivateCallback, update_prodlistCB, NULL);
   XtAddCallback(prodsel_latest_PB, XmNactivateCallback, update_prodlistCB, NULL);
   XtAddCallback(prodsel_all_PB,    XmNactivateCallback, update_prodlistCB, NULL);
   
   
   /* callbacks on sort options for text */
   
   XtAddCallback(prodfilterTX, XmNlosingFocusCallback, update_prodlistCB, NULL);
   
   
   /* callbacks on filter options */   
   
   XtAddCallback(prodsort_loc_PB,      XmNactivateCallback, update_prodlistCB, NULL);
   XtAddCallback(prodsort_prodtime_PB, XmNactivateCallback, update_prodlistCB, NULL);
   XtAddCallback(prodsort_posttime_PB, XmNactivateCallback, update_prodlistCB, NULL);
   
   
   /* callbacks on atom widget */   
   
   wmAtom = XmInternAtom(XtDisplay(prodviewDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(prodviewDS, wmAtom, ok_prodviewCB, NULL);
   
   XtAddCallback(ok_prodviewPB, XmNactivateCallback, ok_prodviewCB, NULL);
      
   return;
}

   
/************************************************************************
    
   Initialize the product viewing options.
   
   ************************************************************************/

void init_prodlist_options()
{
   
   
   /* set the option menu to be in show products for single
      location mode */
   
   SetMenuHistory(prodselOM,  "Products for Selected Location");
   
   
   /* set the location text string */
   
   XmTextSetString(prodlocTX, prodview.curlid);
   
   
   /* blank out the product filter string */
   
   XmTextSetString(prodfilterTX, "");
   
   
   /* set the option menu to sort by product id */
   
   SetMenuHistory(prodsortOM, "Product Id");
   
   
   return;   
}


/************************************************************************
   
   Get the user controllable options defined for
   controlling the list of products.
   
   ************************************************************************/

void get_prodlist_options()
{
   char *valstr;

   
   /* get the setting of the option menu for the list mode */
   
   valstr = GetLabel(GetMenuHistory(prodselOM));
   
   if (strstr(valstr, "Location") != (char *) NULL)
      prodview.prodlist_mode = LOC_PRODUCTS;
   
   else if (strstr(valstr, "Latest") != (char *) NULL)
      prodview.prodlist_mode = LATEST_PRODUCTS;
   
   else
      prodview.prodlist_mode = ALL_PRODUCTS;
   
   
   /* get the setting of the option menu for the sort mode */
            
   valstr = GetLabel(GetMenuHistory(prodsortOM));
   
   if (strstr(valstr, "Product Time") != (char *)NULL)
      prodview.prodsort_mode = PRODTIME_SORT;
   
   else if (strstr(valstr, "Posting Time") != (char *)NULL)
      prodview.prodsort_mode = POSTTIME_SORT;
   
   else
      prodview.prodsort_mode = PRODID_SORT;
       
   
   /* get the specified product id filter text */
   
   memset(prodview.prodfilter, 0, PRODUCT_LEN + 1);
   valstr = XmTextGetString(prodfilterTX);
   strcpy(prodview.prodfilter, valstr);
   XtFree(valstr);
   
   
   /* get the specified location text */
   
   memset(prodview.curlid, 0, LOC_ID_LEN + 1);
   valstr = XmTextGetString(prodlocTX);
   strcpy(prodview.curlid, valstr);
   XtFree(valstr);
   
      
   return;   
}


/************************************************************************
   
   Load the list of products based on the current settings.
   
   ***********************************************************************/

void load_prodlist()
{
   char where[240];
   char	sort_clause[50], date_clause[75];
   char	unique_fields[50], filter_str[PRODUCT_LEN + 1];
   int	filter_len;
   int 	i;
   int	statuscnt;
   int	lookback_days = 180;
   
   
   /* free any data previously allocated */
   
   free_prodview();
   
   
   /* load the list of products based on the user settings.
      load from the appropriate table using any filters 
      defined and in the sort order specified. */
   
   
   /* limit the retrieval to products with a postingtime 
      within the specified number of days */
   sprintf(date_clause, " WHERE EXTRACT(DAY FROM CURRENT_TIMESTAMP "
                        " - postingtime) < %d ", lookback_days);
   
   /* set the sort order string for the where clause. 
      when getting for all products, a "unique" query approach is used;
      because of its nature, the sort is accomplished differently */
   
   if (prodview.prodlist_mode != ALL_PRODUCTS)
   {
      if (prodview.prodsort_mode == PRODTIME_SORT)
	 sprintf(sort_clause, " ORDER BY producttime DESC, postingtime DESC ");
      
      else if (prodview.prodsort_mode == POSTTIME_SORT)
	 sprintf(sort_clause, " ORDER BY postingtime DESC, producttime DESC ");
      
      else if (prodview.prodsort_mode == PRODID_SORT)
	 sprintf(sort_clause, " ORDER BY product_id ASC, producttime DESC ");
   }
   
   else
   {
      if (prodview.prodsort_mode == PRODTIME_SORT)
      {
	 sprintf(unique_fields, "producttime||product_id||postingtime");
	 sprintf(sort_clause, " ORDER BY 1 DESC ");
      }
      
      else if (prodview.prodsort_mode == POSTTIME_SORT)
      {
	 sprintf(unique_fields, "postingtime||product_id||producttime");
	 sprintf(sort_clause, " ORDER BY 1 DESC ");
      }
      
      else if (prodview.prodsort_mode == PRODID_SORT)
      {
	 sprintf(unique_fields, "product_id||producttime||postingtime");
	 sprintf(sort_clause, " ORDER BY 1 ASC ");
      }
   }
   
   
   /* set the filter on product id string for the where clause;
      wildcards are allowed using the asterisk.  simply
      substitute the sql wildcare % for the asterisk. */
   
   memset(filter_str, 0, PRODUCT_LEN + 1);
   filter_len = strlen(prodview.prodfilter);
   
   for (i = 0; i < filter_len; i++)
   {
      if (prodview.prodfilter[i] == '*')
	 filter_str[i] = '%';
      else
	 filter_str[i] = prodview.prodfilter[i];
   }
   
   
   /* if loading products for a given location,
      load from the ProductLink table */
   
   if (prodview.prodlist_mode == LOC_PRODUCTS)
   {      
      sprintf(where, "%s AND lid = '%s' ", date_clause, prodview.curlid);
      
      if (strlen(filter_str) > 0)
      {
	 strcat(where, " AND product_id LIKE '");
	 strcat(where, filter_str);
	 strcat(where, "' ");
      }
      
      strcat(where, sort_clause);

      prodlinkHead = GetProductLink(where);      
   }
   
   
   /* if loading the latest of the products, load from the
      PurgeProduct table. */
   
   else if (prodview.prodlist_mode == LATEST_PRODUCTS)
   {
      
      if (strlen(filter_str) > 0)
	 sprintf(where, "%s AND product_id LIKE '%s' ",
		 date_clause, filter_str);
      else
	 strcpy(where, date_clause);
      
      strcat(where, sort_clause);
      
      purgeprodHead = GetPurgeProduct(where);
      
   }
   
   
   /* if loading all products, then load from the TextProduct table.
      because the product text in blobs can potentially be large,
      only load in the pertinent info using the unique utility
      function.  for this "unique" type query, use the field number
      for the sort.  because of this, the "unique" field specified
      must correspond to the sort order.  therefore use a special
      sort string for the retrieved concatenated field, and 
      parse and rearrange the string later */
   
   else if (prodview.prodlist_mode == ALL_PRODUCTS)
   {      
      if (strlen(filter_str) > 0)
	 sprintf(where, "%s AND product_id like '%s' ",
		 date_clause, filter_str);
      else
	 strcpy(where, date_clause);
      
      strcat(where, sort_clause);
      
      ulHead = LoadUnique(unique_fields, "TextProduct", where, &statuscnt);
   }
            
   
   /* call the function that actually loads the list
      using the data just retrieved. */
   
   load_prodlist_info();
        
   
   return;
}


/************************************************************************
   
   Load the list of parameters into scrolled list, and load
   supporting info into memory.
   
   Note the manner in which the string itself is formatted. 
   This is important as the string is read back and used
   to search the database.
   
   ***********************************************************************/

void load_prodlist_info()
{
   XmStringTable        xmStr;
   Arg                  arg[10];
   int                  ac, i;
   int			cnt = 0 ;
   char			liststr[80];
   ProductLink 		*prodlinkPtr = NULL ;
   PurgeProduct		*purgeprodPtr = NULL ;
   UniqueList		*ulPtr = NULL ;
   char			prodid[PRODUCT_LEN + 1];
   int 			status;
   time_t		prod_timet, post_timet;
   char prodtime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
   char posttime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
   struct tm		*tm_ptr;
   
      
   /* get the number of items to be loaded in the list.
      also set the pointer to the first item */
   
   if (prodview.prodlist_mode == LOC_PRODUCTS)
   {
      if (prodlinkHead != NULL)
      {
	 cnt = ListCount(&prodlinkHead->list);
	 prodlinkPtr = (ProductLink *) ListFirst(&prodlinkHead->list);
      }
      else
	 cnt = 0;
   }
   
   else if (prodview.prodlist_mode == LATEST_PRODUCTS)
   {
      if (purgeprodHead != NULL)
      {
	 cnt = ListCount(&purgeprodHead->list);
	 purgeprodPtr = (PurgeProduct *) ListFirst(&purgeprodHead->list);
      }
      else
	 cnt = 0;
   }
   
   else if (prodview.prodlist_mode == ALL_PRODUCTS)
   {
      if (ulHead != NULL)
      {
	 cnt = ListCount(&ulHead->list);
	 ulPtr = (UniqueList *) ListFirst(&ulHead->list);
      }
      else
	 cnt = 0;
   }
   
   /* allocate the Motif list strings */
   
   xmStr = (XmStringTable) XtMalloc(cnt * sizeof(XmString *));
   
   
   /* load the strings into the Motif strings; load from the
      appropriate tables.  the format of the string is important
      as it is read back and used to define the database key
      for retrievals.*/
   
   for (i = 0; i < cnt; i++)
   {
      
      if (prodview.prodlist_mode == LOC_PRODUCTS)
      {
	 status = yearsec_dt_to_ansi(prodlinkPtr->producttime, prodtime);
	 status = yearsec_dt_to_ansi(prodlinkPtr->postingtime, posttime);
	 strcpy(prodid, prodlinkPtr->product_id);
	 	 
	 prodlinkPtr = (ProductLink *) ListNext(&prodlinkPtr->node);
      }
      
      else if (prodview.prodlist_mode == LATEST_PRODUCTS)
      {
	 status = yearsec_dt_to_ansi(purgeprodPtr->producttime, prodtime);
	 status = yearsec_dt_to_ansi(purgeprodPtr->postingtime, posttime);	 
	 strcpy(prodid, purgeprodPtr->product_id);
	 
	 purgeprodPtr = (PurgeProduct *) ListNext(&purgeprodPtr->node);
      }
      
      
      /* extract the strings in the location which depends
	 on the sort order, knowing that the product id is 10, the
	 to the fixed width of the id is 10 and of the times is 19  */
      
      else if (prodview.prodlist_mode == ALL_PRODUCTS)
      {
	 
	 /* extract the info from the string.  this is needed
	    since the order of the fields in the string
	    varies by the sort order */
	 extract_unique_fields(ulPtr, prodid, prodtime, posttime);
	 
	 ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      }
      
      
      /* with the strings that were extracted above, load
	 the Motif string */
      yearsec_ansi_to_timet ( prodtime, & prod_timet );
      yearsec_ansi_to_timet ( posttime, & post_timet );

      tm_ptr = gmtime(&prod_timet);     
      strftime(prodtime, ANSI_TIME_LEN, "%a %m-%d %H:%M:%S", tm_ptr);

      tm_ptr = gmtime(&post_timet);     
      strftime(posttime, ANSI_TIME_LEN, "%a %m-%d %H:%M:%S", tm_ptr);
      
      sprintf(liststr, "%-10s   %s   %s", prodid, prodtime, posttime);
      xmStr[i] = XmStringCreateSimple(liststr);
   }
   
   
   /* load the list in the scrolled list */
   
   ac = 0;
   XtSetArg(arg[ac], XmNitemCount, cnt);  ac++;
   XtSetArg(arg[ac], XmNitems, xmStr);  ac++;
   XtSetValues(prodlistLS, arg, ac);
  
   
   
   /* free the memory */
   
   for (i = 0; i < cnt; i++)
      XmStringFree(xmStr[i]);
   XtFree((char *)xmStr);
   
   
   /* whenever the list of products is updated, then clear
      the product text shown  */
   
   XmTextSetString(productTxt, "");
   
   
   return;
}


/************************************************************************
   
   Extract the fields of interest from the string, where the 
   locationis based upon the sort order.
   
   **********************************************************************/

void extract_unique_fields(UniqueList 	*ulPtr,
			   char 	*prodid,
			   char	        *prodtime,
			   char  	*posttime)

{ 
   char         ** values = NULL;
   int          count;

   /* Set the prodid, prod_timet, and post_timet to default values. */
   memset(prodid, 0, PRODUCT_LEN + 1);
   
   /* these data are ordered by producttime||product_id||postingtime */
   values = ParseUnique ( ulPtr, &count ); 

   if ( values == NULL || count < 3 )
   {
      fprintf ( stderr, "In routine 'extract_unique_fields':\n"
                        "Could not parse the unique string.\n"
                        "prodid, prod_timet, and post_timet set to \n"
                        "default values.\n" );
      return;
   }
   
   if (prodview.prodsort_mode == PRODTIME_SORT)
   {
      strncpy(prodtime, values[0], ANSI_YEARSEC_TIME_LEN);
      strncpy(prodid, values[1], PRODUCT_LEN);
      strncpy(posttime, values[2], ANSI_YEARSEC_TIME_LEN);
   }
   
   
   /* these data are order by postingtime||product_id||producttime */
   
   else if (prodview.prodsort_mode == POSTTIME_SORT)
   {
      strncpy(posttime, values[0], ANSI_YEARSEC_TIME_LEN);
      strncpy(prodid, values[1], PRODUCT_LEN);
      strncpy(prodtime, values[2], ANSI_YEARSEC_TIME_LEN);
   }
   
   
   /* these data are ordered by product_id||producttime||postingtime */
   
   else if (prodview.prodsort_mode == PRODID_SORT)
   {
      strncpy(prodid, values[0], PRODUCT_LEN);
      strncpy(prodtime, values[1], ANSI_YEARSEC_TIME_LEN);
      strncpy(posttime, values[2], ANSI_YEARSEC_TIME_LEN);
   }

   FreeParseUnique ( values );
   values = NULL;
   
   return;
}

/************************************************************************
   
   Callback for loading (or attempting) to load text of selected
   prodcut into scrolled text window.
   
   **********************************************************************/

void load_textprodCB()
{
   ProductLink 		*prodlinkPtr;
   PurgeProduct		*purgeprodPtr;
   UniqueList		*ulPtr;   
   int 		*listitems;
   int 		listcnt;
   int		itemnum;
   int		status;
   char		prodid[PRODUCT_LEN + 1];
   char         msgstr[80];
   char         where [ 100 ];
   char prod_ansi_time [ ANSI_YEARSEC_TIME_LEN + 1 ] = { '\0' };
   char post_ansi_time [ ANSI_YEARSEC_TIME_LEN + 1 ] = { '\0' };
   TextProduct * pTextProduct = NULL;
      
   /* determine which item is selected */
   XmListGetSelectedPos(prodlistLS, &listitems, &listcnt);
   itemnum = listitems[0];
   free(listitems);
   
   
   /* get the item from the appropriate data set */
   
   if (prodview.prodlist_mode == LOC_PRODUCTS)
   {
      prodlinkPtr = (ProductLink *) ListNth(&prodlinkHead->list, itemnum);
      if (prodlinkPtr != NULL)
      {
	 status = 0;
	 strcpy(prodid, prodlinkPtr->product_id);
	 status = yearsec_dt_to_ansi(prodlinkPtr->producttime, prod_ansi_time);
 	 status = yearsec_dt_to_ansi(prodlinkPtr->postingtime, post_ansi_time);
      }
      else
      {
	 fprintf(stderr, "Error getting item from ProductLink list.");
	 status = -1;
      }
   }
   
   else if (prodview.prodlist_mode == LATEST_PRODUCTS)
   {
      purgeprodPtr = (PurgeProduct *) ListNth(&purgeprodHead->list, itemnum);
      if (purgeprodPtr != NULL)
      {
	 status = 0;
	 strcpy(prodid, purgeprodPtr->product_id);
         status = yearsec_dt_to_ansi(purgeprodPtr->producttime, prod_ansi_time);
 	 status = yearsec_dt_to_ansi(purgeprodPtr->postingtime, post_ansi_time);
      }
      else
      {
	 fprintf(stderr, "Error getting item from PurgeProduct list.");
	 status = -1;
      }
   }
   
   else if (prodview.prodlist_mode == ALL_PRODUCTS)
   {
      ulPtr = (UniqueList *) ListNth(&ulHead->list, itemnum);
      if (ulPtr != NULL)
      {
	 extract_unique_fields(ulPtr, prodid, prod_ansi_time, post_ansi_time);
	 status = 0;
      }
      else
      {
	 fprintf(stderr, "Error getting item from TextProduct unique list.");
	 status = -1;
      }
   }
   
   /* get the product text and display it, then free the memory */
   
   /* Build the where clause for retrieving the text product. */
   sprintf ( where, "WHERE product_id = '%s' AND "
                    "producttime = '%s' AND postingtime='%s'",
                    prodid, prod_ansi_time, post_ansi_time );
    
   printf ( "Calling GetTextProduct for query: %s\n", where );
   pTextProduct = GetTextProduct ( where );

   if ( pTextProduct != NULL )
   {
      XmTextSetString(productTxt, pTextProduct->product); 
      FreeTextProduct ( pTextProduct ); 
      pTextProduct = NULL;
   }
   else
   { 
      sprintf(msgstr, "\n\nSelected %s product not available in database.\n",
              prodid);
      XmTextSetString(productTxt, msgstr);
   }
   
   return;
}


/************************************************************************
   
   Free any memory allocated for the data.
   
   **********************************************************************/

void free_prodview()
{
   
   
   if (prodlinkHead != NULL)
   {
      free(prodlinkHead);
      prodlinkHead = (ProductLink *)NULL;
   }
   
   if (purgeprodHead != NULL)
   {
      free(purgeprodHead);
      purgeprodHead = (PurgeProduct *)NULL;
   }
   
   if (ulHead != NULL)
   {
      free(ulHead);
      ulHead = (UniqueList *)NULL;
   }
   
   
   return;
}


/************************************************************************
   
   Callback for update of the list of products.
   
   **********************************************************************/

void update_prodlistCB()
{
   
   /* get the current defined options, then apply them to the 
      loading of the list */
   
   get_prodlist_options();
   load_prodlist();   
   
   return;
}


/************************************************************************
   
   Close the window.
   
   **********************************************************************/

void ok_prodviewCB()
{
   
   XtDestroyWidget(prodviewDS);
   prodviewDS = NULL;

   
   /* free any allocated memory */
   
   free_prodview();
   
   return;
}




