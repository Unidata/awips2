/* 

Defines prototypes for product retrieval
and storage to the hydrologic database.

*/

#ifndef TextProductManual_h
#define TextProductManual_h

char *  ReadProduct(const char          *prodid,
                    const time_t        prod_timet,
                    const time_t        post_timet);

		   
int	WriteProduct(const char 	*prodid,
		     const char 	prodtype,
		     const time_t 	prod_timet, 
		     const time_t	post_timet,
		     const int  	issnum, 
		     const char 	*filename);

#endif /* #ifndef TextProductManual_h */
