#ifdef  __HPUX__
#define _INCLUDE_POSIX_SOURCE
#endif
 
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h> 


#include "read_pccnames.h"             /* function prototypes */

extern char paramdir[];
extern char outputdir[];

char SEQUENCE_SORT[] = "SEQUENCE";
char OTHER_SORT[]    = "OTHER";

extern int errno;

/*********************************************************************
   read_pccnames()
   
   PURPOSE
   To handle the reading of the pcc file names and then
   loading the names into the structure.
   
   NOTES
   Note that a linked list approach is used because during the 
   course of Riverpro, new pcc data sets may be added.
   
   This function reads entry into pccname, then incorporates
   it into tempnames, then tempnames is sorted by an external 
   function to yeild pccnames, which is what is returned.
   
   ********************************************************************/

pccnames_struct * read_pccnames(misc_struct *misc)
{
   FILE 		*pccfile_ptr;
   char 		filename[BUFSIZ];
   char 		descr_str[MAXLEN_STRING];
   char                 msg_str[MAXLEN_STRING];
   int 			cnt, i, j;
   int 			first = 1;  
   int 			curout;
   int 			*order_index;   
   DIR 			*dirp;
   struct dirent 	*dp;
   char 		*fgets_ptr, *keyword, *keyword_value;
   char 		fileline[MAXLEN_STRING];
   int			type_specified, id_specified, nwrflag_specified;
   char			*lbracket_str, *rbracket_str;
   int			lbracket_pos, rbracket_pos;
   char			*lastdot_loc;
   int			suffix_len;
   char			office_suffix[HYD_SERV_LEN + 1];
   
   pccnames_struct 	*pccname   = NULL, *tempname = NULL; /* scratch list */
   pccnames_struct 	*tempnames = NULL;              /* unsorted list */
   pccnames_struct 	*pccnames  = NULL;              /* sorted list */
   
   
   /* log message */
   
   sprintf(msg_str, "Reading pcc file list for selected office: %s", 
	   misc->selected_office);
   log_msg("", msg_str);
   
    
   /* open the directory containing the pcc files;
      if an error occurs, the log_msg aborts the program */
   
   dirp = opendir(paramdir);
   if (dirp == NULL)
      log_msg(PCCFILELIST_ERR, paramdir);
   

   /* loop on the entries in the file; only consider *.pcc files */
   
   cnt = 0;
   while ((dp = readdir(dirp)) != NULL)
   {   
      
      /* only consider pcc files, of course */
      
      if (strstr(dp->d_name, ".pcc.") != NULL)
      {	 
	 
	 /* check if the office matches. if it doesn't, 
	    use a continue (dear god, forgive me!) to skip to the next file */
	 
	 lastdot_loc = strrchr(dp->d_name, '.');
	 suffix_len = strlen(lastdot_loc) - 1;
	 
	 if (suffix_len > 0 && suffix_len <= HYD_SERV_LEN)
	 {	    	    
	    strcpy(office_suffix, lastdot_loc + 1);
	    convert_str_to_upcase(office_suffix);

	    if (strcmp(misc->selected_office, office_suffix) != 0)
	       continue;
	 }
	 
	 
	 /* if reached here, then office must match so
	    open the file for reading */
	    
         sprintf(msg_str, "Reading pcc file: %s", dp->d_name);
         log_msg("", msg_str);
	    
	 
	 sprintf(filename, "%s/%s", paramdir, dp->d_name);
	 pccfile_ptr = fopen(filename, "r");
	 if (pccfile_ptr == NULL) 
	    log_msg(FILE_OPENWARN, dp->d_name);
	 	 
	 else
	 {
	    /* allocate space for the new data */
	    
	    pccname = (pccnames_struct *)malloc(sizeof(pccnames_struct));
	    if (pccname == NULL)
	    {
	       log_msg(FAILED_MALLOC, "of pccname in read_pccnames");
	       return(pccname);
	    }
	    memset(pccname, '\0', sizeof(pccnames_struct));
	    
	    
	    /* initialize. ensure that the consistent/default product 
	       id and nwr_flags are used by converting bad values
	       on purpose via the convert function. */
	    
	    type_specified = id_specified = nwrflag_specified = FALSE;
	    
	    strcpy(pccname->product_cnx, "CCCCNNNXXX");
	    strcpy(pccname->prod_categ, "RVS");
	    pccname->nwr_flag = convert_nwrflag_to_index("NO");
	    
	    
	    /* read the description from the first line and
	       skip the leading # sign and strip the newline */
	    
	    fgets(descr_str, MAXLEN_STRING, pccfile_ptr);	 
	    strtok(&descr_str[1],"\n");
	    
	    
	    /* loop thru the file and find the lines that specify
	       the product type (nnn), product id, the crs switch */
	    
	    for(;;)
	    {
	       /* get a line from the input pcc file; if end-of-file
		  then log error */
	       
	       fgets_ptr = fgets(fileline, MAXLEN_STRING, pccfile_ptr);
	       if (fgets_ptr == NULL)
	       {
		  break;
	       }
	       convert_str_to_upcase(fileline);
	       
	       keyword = strtok(fileline, ":\n");

               if (keyword != NULL )
               {
	       
	          /* load in the product id - i.e. cccnnnxxx
		     insist that the id be between 8-10 chars. */
	       
	          if (strcmp(keyword, "PRODUCT_ID") == 0)
	          {
		     keyword_value = strtok(NULL, ", \t\n");
		     if (keyword_value != NULL && strlen(keyword_value) >= 8 && 
                         strlen(keyword_value) <= 10)
		     {
		        strcpy(pccname->product_cnx, keyword_value);
		        id_specified = TRUE;
		     }
		     else
		        log_msg(BADPRODCNX, keyword_value);
	          }
	       
	       
	          /* load in the product category */
	       
	          else if (strcmp(keyword, "PRODUCT_TYPE") == 0)
	          {
		     keyword_value = strtok(NULL, ", \t\n");
		     if (keyword_value != NULL)
		     {
		       if (strlen(keyword_value) != 3)
		         log_msg(BADPRODID, keyword_value);
		       else
		       {
		         strcpy(pccname->prod_categ, keyword_value);
		         type_specified = TRUE;
		       }
		     }  
	          }
	       
	       
	          /* load in the nwr flag */
	       
	          else if (strcmp(keyword, "NWR_FLAG") == 0)
	          {
		     keyword_value = strtok(NULL, ", \t\n");
		     if (keyword_value != NULL)
		     {
		       if (strlen(keyword_value) < 0)
		          log_msg(BADNWRFLAG, keyword_value);
		       else
		       {
		          pccname->nwr_flag = 
                               convert_nwrflag_to_index(keyword_value);
		          nwrflag_specified = TRUE;
		       }
		     }  
	          }
               }
	    }   /* end of loop on reading file lines */
	    
	    
	    /* close the input file */

	    fclose(pccfile_ptr);
	    
	    
	    /* log messages if critical fields not specified */
	    
	    if (type_specified == FALSE)
	       log_msg(NO_PRODUCTTYPE, "");
	    
	    if (id_specified == FALSE)
	       log_msg(NO_PRODUCTID, "");
	    
	    if (nwrflag_specified == FALSE)
	       log_msg(NO_NWRFLAG, "");
   
	    
	    /* copy the strings.  if the beginning of the description
	       string has a number enclosed in brackets following the 
	       first character, then that is the optional
	       sequence number for the pcc file. store the number and
	       don't preserve it as part of the description */

	    strcpy(pccname->filename, filename);
	    pccname->sequence = MISSINGVAL;
	    
	    if ((lbracket_str = strstr(descr_str, "[")) != NULL &&
		(rbracket_str = strstr(descr_str, "]")) != NULL)
	    {
	       lbracket_pos = strlen(descr_str) - strlen(lbracket_str);
	       rbracket_pos = strlen(descr_str) - strlen(rbracket_str);
	       if (lbracket_pos < rbracket_pos && rbracket_pos < 10)
	       {
		  i = rbracket_pos - lbracket_pos -1;
		  if (i > 0)
		  {
		     memset(fileline, 0, MAXLEN_STRING);
		     strncpy(fileline, &descr_str[lbracket_pos + 1], 
			     rbracket_pos - lbracket_pos -1);
		     i = atoi(fileline);
		     if (i > 0)
		     {
			strcpy(pccname->descr, &descr_str[rbracket_pos + 1]);
			pccname->sequence = i;
		     }
		  }
	       }
	    }
	    
	    if (pccname->sequence == MISSINGVAL)
	    {
	       strcpy(pccname->descr, &descr_str[1]);
	    }
	    
	    
	    /* initialize the list with the first entry */
	    
	    if (first)
	    {
	       tempnames = pccname;
	       ListInit(&tempnames->list);
	       first = 0;
	    }
	    
	    
	    /* load the local structure entry into the linked list */
	    
	    ListAdd(&tempnames->list, &pccname->node);
	    cnt++;
	    
	 } /* if file opened ok */
      }    /* if pcc file being processed */
   }       /* end while more file to read */
   
   (void) closedir(dirp);   
   first = 1;
   if (cnt == 0) return(NULL);
   
   
   /*-----------------------------------------------------
      at this point, the unsorted list is completed 
      ----------------------------------------------------*/
     
   /* allocate space for any array that contains the indices
      that define the determined order and initialize them */
   
   order_index = (int *)malloc(sizeof(int) * cnt);
   if (order_index == NULL)
   {
      log_msg(FAILED_MALLOC, "of order_index in read_pccnames");
      return(pccname);
   }
   for (i = 0; i < cnt; i++)
      order_index[i] = MISSINGVAL;
   curout = 0;
   
   
   /* first try and sort the pcc files by any explicit sequence number that may
      be defined.  then, sort the items by the product categ and the nwr flag.
      this function updates the order_index array and current output index */
   
   sort_pccproduct(tempnames, SEQUENCE_SORT, 0, cnt, &curout, order_index);
      
   nwrflag_specified = FALSE;
   
   sort_pccproduct(tempnames, "RVS", nwrflag_specified, cnt, &curout, order_index); 
   sort_pccproduct(tempnames, "FLS", nwrflag_specified, cnt, &curout, order_index);  
   sort_pccproduct(tempnames, "FLW", nwrflag_specified, cnt, &curout, order_index);   
   sort_pccproduct(tempnames, OTHER_SORT, nwrflag_specified, cnt, &curout, order_index);
   
   nwrflag_specified = TRUE;

   sort_pccproduct(tempnames, "RVS", nwrflag_specified, cnt, &curout, order_index); 
   sort_pccproduct(tempnames, "FLS", nwrflag_specified, cnt, &curout, order_index);  
   sort_pccproduct(tempnames, "FLW", nwrflag_specified, cnt, &curout, order_index);  
   sort_pccproduct(tempnames, OTHER_SORT, nwrflag_specified, cnt, &curout, order_index);
      
   
   /*-----------------------------------------------------
      at this point, the order_index array indicates the
      proper sorted order of the lis
      ----------------------------------------------------*/
   
   /* now copy the unsorted information from the temporary
      list into the sorted list, using the determined
      indices that specify the order. */
   
   for (i = 0; i < cnt; i++)
   {
      /* allocate space for the new data;
	 can't just copy the entire structure */
      
      pccname = (pccnames_struct *)malloc(sizeof(pccnames_struct));
      if (pccname == NULL)
      {
	 log_msg(FAILED_MALLOC, "of newname in read_pccnames"); 
	 return(pccname);
      }
      memset(pccname, '\0', sizeof(pccnames_struct));
      
      
      /* get the entry for the currently pointed to index
	 and copy the information into the new linked list */

      if (tempnames != NULL)
	tempname = (pccnames_struct *)ListFirst(&tempnames->list);
      for (j = 0; tempname; j++)
      {
	 if (j == order_index[i])
	 {
	    strcpy(pccname->filename,    tempname->filename);
	    strcpy(pccname->descr,       tempname->descr);
	    strcpy(pccname->product_cnx, tempname->product_cnx);
	    strcpy(pccname->prod_categ,  tempname->prod_categ);
	    pccname->nwr_flag =          tempname->nwr_flag;
	    pccname->sequence =          tempname->sequence;
	    
	    if (first)
	    {
	       pccnames = pccname;
	       ListInit(&pccnames->list);
	       first = 0;
	    }
	    
	    ListAdd(&pccnames->list, &pccname->node);
	    break;
	 }
	 
	 tempname = (pccnames_struct *)ListNext(&tempname->node);
      }
   }  
  
   
   /* free the temporary memory */

   free(order_index);
   free_pccnames(tempnames);
   
   return(pccnames);
}


/*********************************************************************
   sort_pccproduct()
   
   PURPOSE
   Checks all the pcc sort strings for the particular product and
   sorts them and returns the sorted order in an index array
   and a count of the currently indexed items.
   
   ********************************************************************/
void sort_pccproduct(pccnames_struct 	*tempnames, 
		     char 		*prod_categ,
		     int		nwr_flag,
		     int		cnt,
		     int		*curout, 
		     int		*order_index)
{
   int 			i;
   int 			curindex;
   char 		tempstr[MAXLEN_STRING];
   pccnames_struct 	*tempname = NULL;
   int			consider_item;
   int			highest_rank;

   
   /* debug statements ------------------
   if (first)
   {
      if (tempnames != NULL)
	tempname = (pccnames_struct *)ListFirst(&tempnames->list);
      i = 0;
      while (tempname)
      {
	 printf("%d:%s:%s:%d:\n", i,
	 tempname->prod_categ, tempname->descr, tempname->sequence);
	 tempname = (pccnames_struct *)ListNext(&tempname->node);
	 i++;
      }
      first = 0;
   }
   */
   

   
   /* loop on the entries for the given product category and
      nwr flag, then sort them by the description. start with the
      first entry and store its index, then look for more entries;
      continue until no more entries */
   
   do
   {
      curindex = MISSINGVAL;
      memset(tempstr, 0, MAXLEN_STRING);
      if (tempnames != NULL)
	tempname = (pccnames_struct *)ListFirst(&tempnames->list);
      highest_rank = 32000;
      
      for (i = 0; tempname; i++)
      {      
	 /* determine whether the item should be considered.
	    first check if the item has already been sorted. */
	 
	 if (check_if_sorted(order_index, cnt, i) == FALSE)
	 {	    
	    if (strcmp(prod_categ, SEQUENCE_SORT) == 0)
	       consider_item = TRUE;
	    
	    else if (strcmp(prod_categ, OTHER_SORT) == 0)
	    {
	       if (tempname->nwr_flag == nwr_flag)
		  consider_item = TRUE;
	       else
		  consider_item = FALSE;
	    }
	    
	    else 
	    {
	       if ((strcmp(tempname->prod_categ, prod_categ) == 0) &&
		   tempname->nwr_flag == nwr_flag)
		  consider_item = TRUE;
	       else
		  consider_item = FALSE;
	    }
	 }
	 else
	    consider_item = FALSE;
	 
	 
	 /* now find the next item to insert into the sorted
	    list, using either by the sequence number or the description. */
	 
	 if (consider_item)
	 {
	    if (strcmp(prod_categ, SEQUENCE_SORT) == 0)
	    {
	       if (tempname->sequence != MISSINGVAL)
	       {
		  if (tempname->sequence < highest_rank)
		  {
		     highest_rank = tempname->sequence;
		     curindex = i;
		  }
	       }
	    }
	    
	    else
	    {
	       if (strlen(tempstr) > 0)
	       {
		  if (strcmp(tempname->descr, tempstr) < 0)
		  {
		     strcpy(tempstr, tempname->descr);
		     curindex = i;
		  }
	       }
	       else
	       {
		  strcpy(tempstr, tempname->descr);
		  curindex = i;
	       }
	    }
	 }
	 
	 tempname = (pccnames_struct *)ListNext(&tempname->node);
      }
      
      
      /* if an item was found from this pass thru the list,
	 then load an entry into the index and increment the count */
      
      if (curindex != MISSINGVAL)
      {
	 order_index[*curout] = curindex;
	 (*curout)++;
      }
   }
   while (curindex != MISSINGVAL);  /* continue until no more matches found */
   
   
   return;
}


/*********************************************************************
   check_if_sorted()
   
   PURPOSE
   Check whether the given item is already logged into the 
   list of indices used for the sort.
   
   ********************************************************************/
int check_if_sorted(int 	*order_index,
		    int		cnt,
		    int 	indexnum)
{  
   int i;
   int found;
   
   
   found = FALSE;
   
   for (i = 0; i < cnt; i++)
   {
      if (order_index[i] == indexnum)
      {
	 found = TRUE;
	 break;
      }
   }
   
   return(found);
}

   
