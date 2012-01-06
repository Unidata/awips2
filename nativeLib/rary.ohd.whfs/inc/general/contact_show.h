/*
	File:		contact_show.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef contact_show_h
#define contact_show_h


#include <Xm/Xm.h>
#include "Contacts.h"


/*
	Function prototypes.
*/
void	contact_show(Widget w, const char *lid, Boolean editable);
int	contact_load(const char *lid);
void	contact_key(char *key, int *pos);

void	contact_callbacks(void);
void	contact_add_callbacks(void);
void	contact_remove_callbacks(void);

int	contact_save(void);
void	find_contact_pos(Contacts *cPtr, int *pos);


/*
	Callback prototypes.
*/
void	contact_import();
void	contact_ok();
void	contact_apply();
void	contact_close();
void	contact_new();
void	contact_delete();
void	contact_del_conf();


#endif
