/*
	File:		lwstmt_show.h
	Date:		Jan. 20, 2006
	Author:		Jingtao Deng
	
	Purpose:
	
*/


#ifndef lwstmt_show_h
#define lwstmt_show_h


#include <Xm/Xm.h>
#include "LWstmt.h"


/*
	Function prototypes.
*/
void	ShowLwstmtDs(Widget w, const char *lid, Boolean editable);
int	lwstmt_load(const char *lid);
void	lwstmt_key(char *key, int *pos);

void	lwstmt_callbacks(void);
void	lwstmt_add_callbacks(void);
void	lwstmt_remove_callbacks(void);

int	lwstmt_save(void);
void	find_lwstmt_pos(LWstmt *lwPtr, int *pos);


/*
	Callback prototypes.
*/
void	lwstmt_import();
void	lwstmt_ok();
void	lwstmt_apply();
void	lwstmt_close();
void	lwstmt_new();
void	lwstmt_delete();
void	lwstmt_del_conf();


#endif
