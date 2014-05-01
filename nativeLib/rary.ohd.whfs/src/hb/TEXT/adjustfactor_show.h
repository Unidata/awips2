/*
	File:		adjust_show.h
	Date:		November 2002
	Author:		Russell Erb
	
	Purpose:	Provide support for the Adjustment Factor DS.
	
*/


#ifndef adjust_show_h
#define adjust_show_h

/*
	Includes.
*/
#include <Xm/Xm.h>
#include "AdjustFactor.h"
#include "ShefPe.h"
#include "ShefDur.h"
#include "ShefTs.h"
#include "ShefEx.h"

/*
	Constants.
*/

#define IN_MAX_TABLENAME	50
#define IN_MAX_ORDERBY_PHRASE	100
/*
	Structures.
*/
typedef struct adj_info_st
{
   AdjustFactor	*adjustPtr;		/* current list of adjustfactor info */
   AdjustFactor	*item_adjustPtr;	/* contents of item_adjPtr structure */
   char		*item_where_adjustPtr;	/* keep for when doing updates */
   
   
   ShefDur		*shefdurPtr;	/* complete list of shefdurs */
   ShefPe		*shefpePtr;	/* complete list of shefpes */
   ShefTs		*sheftsPtr;	/* complete list of shefts */
   ShefEx		*shefexPtr;	/* complete list of shefex */
   
   char		table[IN_MAX_TABLENAME];		/* for reference */
   char		orderby_phrase[IN_MAX_ORDERBY_PHRASE];	/* for reference */
   
   
   int			apply_successful;
   int			aflistLI_selpos;  /* previous pos before NEW pressed */
   int			delete_flag;  /* indicates delete was just performed */
   
} adj_info, *adj_info_Ptr;



/*
	Function prototypes.
*/
void	adjust_loaddisplay(Widget w, XtPointer ptr, XtPointer cbs);
void	adjust_loadDataIntoDisplay(void);

void 	afdata_setSensitivity(Boolean set);

char*	af_get_shefdur_name(int dur);
char*	af_create_shefdur_name(ShefDur *shefdurPtr);
void	af_create_shefdur_btns(adj_info *adjPtr);

char*	af_get_shefts_name(char *ts);
char*	af_create_shefts_name(ShefTs *sheftsPtr);
void	af_create_shefts_btns(adj_info *adjPtr);

char*	af_get_shefex_name(char *ex);
char*	af_create_shefex_name(ShefEx *shefexPtr);
void	af_create_shefex_btns(adj_info *adjPtr);


char*	af_get_shefpe_name(char *pe);
char*	af_getpe_from_peLI(Widget peLI);
void	in_loadpe_into_peLI(Widget peLI, char* pe);
void	adjust_loadpeLI(Widget peLI, ShefPe *shefpePtr);

void	in_recompute_aflistLI_selpos(adj_info *adjPtr);

void	adjust_callbacks(void);


void	adjust_ok	(Widget w, XtPointer ptr, XtPointer cbs);
void	adjust_apply	(Widget w, XtPointer ptr, XtPointer cbs);
int	in_apply_adjustItemData (adj_info *adjPtr);


void	adjust_close	(Widget w, XtPointer ptr, XtPointer cbs);
void	adjust_del_conf	(Widget w, XtPointer ptr, XtPointer cbs);
void	adjust_delete	(Widget w, XtPointer ptr, XtPointer cbs);

void	adjust_clearDisplay(void);

void	af_getItemData(adj_info *adjPtr);
void	af_getAdjustItemData(AdjustFactor *adjustPtr);


void	af_setItemData (Widget w, XtPointer ptr, XtPointer cbs);
void	in_setAdjustItemData(AdjustFactor *adjustPtr);

void	in_setSwitches_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	in_setSwitches(Widget w, XtPointer ptr, XtPointer cbs);

char*	adjust_build_where(AdjustFactor *adjustPtr);


void	adjust_loadadjust(void);

void	in_select_new_adjust_item(adj_info *adjPtr);


adj_info*		getAdjustFactorInfo(int init_flag);
AdjustFactor*		adjust_getadjust(void);
ShefPe*			adjust_getshefpes(void);


void	freeAdjustFactorInfo(void);
void	adjust_freeadjust(adj_info *adjPtr);
void	adjust_freeshefdur(adj_info *adjPtr);
void	adjust_freeshefpes(adj_info *adjPtr);
void	adjust_freeshefts(adj_info *adjPtr);
void	adjust_freeshefex(adj_info *adjPtr);

void    ShowAdjustDs ( Widget w ) ;



#endif
