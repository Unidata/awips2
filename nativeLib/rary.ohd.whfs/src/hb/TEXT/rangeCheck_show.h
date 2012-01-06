/*
	File:		rangeCheck_show.h
	Date:		February 1997
	Author:		Paul Taylor
	
	Purpose:	Provide support for the Range Check DS.
	
*/


#ifndef rcheck_show_h
#define rcheck_show_h

/*
	Includes.
*/
#include <Xm/Xm.h>

#include "DataLimits.h"
#include "LocDataLimits.h"
#include "ShefPe.h"
#include "ShefDur.h"

/*
	Constants.
*/
#define	RC_DEFAULT_RANGES	0  /* constants for current OM selection */
#define RC_LOCATION_RANGES	1

#define RC_NORMAL_ENTRY_MODE	0  /* "data_entry_mode" constants */
#define RC_NEW_ENTRY_MODE	1

#define RC_MAX_TABLENAME	50
#define RC_MAX_LOC_PHRASE	20	/* e.g. "lid = 'BLUO2'" */
#define RC_MAX_PE_PHRASE	2500	/* e.g. "pe in ('HG','PC')" */
#define RC_MAX_WHERE_PHRASE	RC_MAX_LOC_PHRASE+RC_MAX_PE_PHRASE+100
#define RC_MAX_ORDERBY_PHRASE	100

/*
	Structures.
*/
typedef struct rc_info_st
{
   DataLimits		*drcPtr;	/* current list of drc info */
   LocDataLimits	*lrcPtr;	/* current list of lrc info */
   
   DataLimits		*item_drcPtr; /* ptr to static memory (don't free) */
   LocDataLimits	*item_lrcPtr; /* ptr to static memory (don't free) */

   char			*item_where_drcPtr;  /* keep for when doing updates */
   char			*item_where_lrcPtr;  /* keep for when doing updates */   
   
   
   ShefDur		*shefdurPtr;	/* complete list of shefdurs */
   ShefPe		*shefpePtr;	/* complete list of shefpes */
   int			*filterpe_poslist;
   int			filterpe_poslist_cnt;
   int			filterpe_poslist_okayfree;
    
   
   char			table[RC_MAX_TABLENAME];		/* for reference */
   char			orderby_phrase[RC_MAX_ORDERBY_PHRASE];	/* for reference */
   
   
   char			*loc_phrase;		/* for filter */
   char			*pe_phrase;		/* for filter */
   char			*where_phrase;		/* for filter */
   char			*whereorderby_phrase;	/* for filter */
   
   
   int			apply_successful;
   int			data_entry_mode;	/* "NORMAL" vs. "NEW" */
   int			rcmainLI_selpos;  /* current pos of selected item */
   int			delete_flag;  /* indicates delete was just performed */

} rc_info, *rc_info_Ptr;


/*
	Function prototypes.
*/
void	rcheck_loaddisplay(Widget w, XtPointer ptr, XtPointer cbs);
void	rcheck_loadDataIntoDisplay(int menu_pos);

void 	rcloc_setSensitivity(Boolean set);
void 	rcdata_setSensitivity(Boolean set);

char*	rc_cvttext_slash_to_hyphen(char *input);
void	rc_cvttext_lwr_to_upr(Widget w, XtPointer ptr, XtPointer cbs);

char*	rc_get_shefdur_name(int dur);
char*	rc_create_shefdur_name(ShefDur *shefdurPtr);
void	rc_create_shefdur_btns(rc_info *rcPtr);

char*	rc_get_shefpe_name(char *pe);
char*	rc_getpe_from_peLI(Widget peLI);
void	rc_loadpe_into_peLI(Widget peLI, char* pe);
void	rcheck_loadpeLI(Widget peLI, ShefPe *shefpePtr);

void	rc_recompute_rcmainLI_selpos(rc_info *rcPtr);

void	rcheck_callbacks(void);

void	rcheck_ok	(Widget w, XtPointer ptr, XtPointer cbs);
void	rcheck_apply	(Widget w, XtPointer ptr, XtPointer cbs);
int	rc_apply_defItemData (rc_info *rcPtr);
int	rc_apply_locItemData (rc_info *rcPtr);


void	rcheck_close	(Widget w, XtPointer ptr, XtPointer cbs);
void	rcheck_del_conf	(Widget w, XtPointer ptr, XtPointer cbs);
void	rcheck_delete	(Widget w, XtPointer ptr, XtPointer cbs);
void	rcheck_new	(Widget w, XtPointer ptr, XtPointer cbs);
void	rcheck_help	(Widget w, XtPointer ptr, XtPointer cbs);

void 	rc_Sense_LocTB	(Widget w, XtPointer ptr, XtPointer cbs);
void 	rc_Sense_PeTB	(Widget w, XtPointer ptr, XtPointer cbs);

void	rcheck_clearDisplay(void);
void	rc_useFilter	(Widget w, XtPointer ptr, XtPointer cbs);

void	rc_filter_grabLocPe(void);
void	rc_selectposlist(void);
void	rc_deselectposlist(void);

void	rc_filter_buildPhrases(char *loc_phrase, char *pe_phrase,
			       char *orderby_phrase);
void	rcheck_setLocPhrase(char *loc_phrase);
void	rcheck_setPePhrase(char *pe_phrase);
void	rcheck_obtainLocPhrase(void);
void	rcheck_obtainPePhrase(void);


int	rc_getItemData(rc_info *rcPtr);
int	rc_getDefItemData(DataLimits *drcPtr);
int	rc_getLocItemData(LocDataLimits *lrcPtr);


void	rc_setItemData (Widget w, XtPointer ptr, XtPointer cbs);
void	rc_setDefItemData(DataLimits *drcPtr);
void	rc_setLocItemData(LocDataLimits *lrcPtr);


char*	rc_build_def_where(DataLimits    *drcPtr);
char*	rc_build_loc_where(LocDataLimits *lrcPtr);


void	rcheck_loaddefault(void);
void	rcheck_loadlocation(void);

void	rc_select_new_def_item(rc_info *rcPtr);
void	rc_select_new_loc_item(rc_info *rcPtr);


rc_info*		GetDataLimitsInfo(int init_flag);
DataLimits*		rcheck_getdefault(void);
LocDataLimits*		rcheck_getlocation(void);
ShefPe*			rcheck_getshefpes(void);

void	freeDataLimitsInfo(void);
void	rcheck_freedefault(rc_info *rcPtr);
void	rcheck_freelocation(rc_info *rcPtr);
void	rcheck_freeshefdur(rc_info *rcPtr);
void	rcheck_freeshefpes(rc_info *rcPtr);
void	rcheck_freeposlist(rc_info *rcPtr);  /* free the filterpe_poslist */
void	decode_datalimit_str   ( DataLimits     *drcPtr, char buf[] );
void	decode_locdatalimit_str( LocDataLimits  *drcPtr, char buf[] );
void    ShowRangeCheckDs ( Widget w ) ;

#endif
