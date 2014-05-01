#ifndef PRODVIEW_SHOW_H
#define PRODVIEW_SHOW_H

#include "DbmsDefs.h"
#include "ProductLink.h"
#include "LoadUnique.h"

/* structure of internal state of prodview interface */

typedef struct
{
   int		prodlist_mode;
   char 	curlid[LOC_ID_LEN + 1];
   int		prodsort_mode;
   char		prodfilter[PRODUCT_LEN + 1];
} prodview_struct;


/* arbitrary values for option settings values for internal use;
   set defined for prodlist_mode and prodsort_mode */

#define  LOC_PRODUCTS    50
#define  LATEST_PRODUCTS 51
#define  ALL_PRODUCTS    52

#define  PRODID_SORT    60
#define  PRODTIME_SORT  61
#define  POSTTIME_SORT  62


/* prototypes */

void show_prodview(Widget w, 
		   const char *lid);
void add_prodview_cbs();
void init_prodlist_options();
void get_prodlist_options();
void load_prodlist();
void load_prodlist_info();
void extract_unique_fields(UniqueList 	*ulPtr,
			   char 	*prodid,
			   char 	*prod_timet,
			   char 	*post_timet);
void free_prodview();
void load_textprodCB();
void update_prodlistCB();
void ok_prodviewCB();


#endif 
