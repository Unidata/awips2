/*

	File:	

	Purpose:	Provides methods for accessing
		PostgreSQL database for the purpose of
		selecting, inserting, updating, and
		deleting fields from the table.

*/


#ifndef GTextProduct_h
#define GTextProduct_h


#include "datetime.h"
#include "List.h"

typedef struct _GTextProduct {

	Node		node;
	char		product_id[11];
	dtime_t		producttime;
	dtime_t		postingtime;
	char		prodtype[2];
	long		issnum;
	char		*product;
	List		list;

} GTextProduct;

/*
	Function Prototype(s)
*/
GTextProduct	 * GetGTextProduct(char *where);
int	PutGTextProduct(GTextProduct *sp);
int	DeleteGTextProduct(const char *where);
int	UpdateGTextProduct(GTextProduct *sp, const char *where);
void	FreeGTextProduct(GTextProduct *sp);


#endif
