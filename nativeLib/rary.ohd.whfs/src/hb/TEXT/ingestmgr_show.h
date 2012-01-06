/*
	File:		ingest_show.h
	Date:		February 1997
	Author:		Paul Taylor
	
	Purpose:	Provide support for the Ingest Filter DS.
	
*/


#ifndef ingest_show_h
#define ingest_show_h

/*
	Includes.
*/
#include <Xm/Xm.h>
#include "IngestFilter.h"
#include "ShefPe.h"
#include "ShefDur.h"
#include "ShefTs.h"
#include "ShefEx.h"

/*
	Constants.
*/
#define IN_NORMAL_ENTRY_MODE	0
#define IN_NEW_ENTRY_MODE	1

#define IN_MAX_TABLENAME	50
#define IN_MAX_LOC_PHRASE	20	/* e.g. "lid = 'BLUO2'" */
#define IN_MAX_PE_PHRASE	2500	/* e.g. "pe in ('HG','PC')" */
#define IN_MAX_TS_PHRASE	10
#define IN_MAX_SW_PHRASE	50

#define IN_MAX_WHERE_PHRASE	IN_MAX_LOC_PHRASE+IN_MAX_PE_PHRASE+100
#define IN_MAX_ORDERBY_PHRASE	100

#define INGEST_FMT_1	"%-8s %s %-s %2s"
			/* lid," ",pe," " */

#define INGEST_FMT_2	"%4s %4s %s %4s %s %3s %i"
			/* dur," ",ts," ",extremum," ",ts_rank */

#define INGEST_FMT_3	"%7s %s %3s %s %3s %s"
			/* " ",mast," ",ofs," ",stg2 */

#define INGEST_FMT_STR  INGEST_FMT_1 INGEST_FMT_2 INGEST_FMT_3


/*
	Structures.
*/
typedef struct in_info_st
{
   IngestFilter	*ingestPtr;		/* current list of ingestfilter info */
   IngestFilter	*item_ingestPtr;	/* contents of item_inPtr structure */
   char		*item_where_ingestPtr;	/* keep for when doing updates */
   
   
   ShefDur		*shefdurPtr;	/* complete list of shefdurs */
   ShefPe		*shefpePtr;	/* complete list of shefpes */
   ShefTs		*sheftsPtr;	/* complete list of shefts */
   ShefEx		*shefexPtr;	/* complete list of shefex */
   int			*filterpe_poslist;
   int			filterpe_poslist_cnt;
   int			filterpe_poslist_okayfree;
   
   int			switches_master;	/* for filter */
   int			switches_ofs;		/* for filter */
   int			switches_stg2;		/* for filter */
   
   int			setswitches_master;	/* for setSwitches */
   int			setswitches_ofs;	/* for setSwitches */
   int			setswitches_stg2;	/* for setSwitches */
   
   
   char		table[IN_MAX_TABLENAME];		/* for reference */
   char		orderby_phrase[IN_MAX_ORDERBY_PHRASE];	/* for reference */
   
   
   char			*loc_phrase;		/* for filter */
   char			*pe_phrase;		/* for filter */
   char			*ts_phrase;		/* for filter */
   char			*sw_phrase;		/* for filter */
   char			*where_phrase;		/* for filter */
   char			*whereorderby_phrase;	/* for filter */
   
   
   int			apply_successful;
   int			data_entry_mode;	/* "NORMAL" vs. "NEW" */
   int			inlistLI_selpos;  /* previous pos before NEW pressed */
   int			delete_flag;  /* indicates delete was just performed */
   
} in_info, *in_info_Ptr;



/*
	Function prototypes.
*/
void	ingest_loaddisplay(Widget w, XtPointer ptr, XtPointer cbs);
void	ingest_loadDataIntoDisplay(void);

void 	indata_setSensitivity(Boolean set);

char*	in_get_shefdur_name(int dur);
char*	in_create_shefdur_name(ShefDur *shefdurPtr);
void	in_create_shefdur_btns(in_info *inPtr);

char*	in_get_shefts_name(char *ts);
char*	in_create_shefts_name(ShefTs *sheftsPtr);
void	in_create_shefts_btns(in_info *inPtr);

char*	in_get_shefex_name(char *ex);
char*	in_create_shefex_name(ShefEx *shefexPtr);
void	in_create_shefex_btns(in_info *inPtr);


char*	in_get_shefpe_name(char *pe);
char*	in_getpe_from_peLI(Widget peLI);
void	in_loadpe_into_peLI(Widget peLI, char* pe);
void	ingest_loadpeLI(Widget peLI, ShefPe *shefpePtr);

void	in_recompute_inlistLI_selpos(in_info *inPtr);

void	ingest_callbacks(void);


void	ingest_ok	(Widget w, XtPointer ptr, XtPointer cbs);
void	ingest_apply	(Widget w, XtPointer ptr, XtPointer cbs);
int	in_apply_ingestItemData (in_info *inPtr);


void	ingest_close	(Widget w, XtPointer ptr, XtPointer cbs);
void	ingest_del_conf	(Widget w, XtPointer ptr, XtPointer cbs);
void	ingest_delete	(Widget w, XtPointer ptr, XtPointer cbs);
void	ingest_new	(Widget w, XtPointer ptr, XtPointer cbs);
void	ingest_help	(Widget w, XtPointer ptr, XtPointer cbs);


void 	in_Sense_LocTB	(Widget w, XtPointer ptr, XtPointer cbs);
void 	in_Sense_PeTB	(Widget w, XtPointer ptr, XtPointer cbs);
void	in_Sense_TsTB	(Widget w, XtPointer ptr, XtPointer cbs);
void	in_Sense_SwTB	(Widget w, XtPointer ptr, XtPointer cbs);

void	ingest_clearDisplay(void);
void	in_useFilter	(Widget w, XtPointer ptr, XtPointer cbs);


void	in_filter_grabLocPeTsSw(void);
void	in_select_filterpe(void);	/* show hidden options */
void	in_deselect_filterpe(void);	/* hide chosen options */

void	in_select_switches(void);	/* show hidden options */
void	in_deselect_switches(void);	/* hide chosen options */


void	in_filter_buildPhrases(char *loc_phrase, char *pe_phrase,
			       char *ts_phrase,  char *sw_phrase,
			       char *orderby_phrase);
void	ingest_setLocPhrase(char *loc_phrase);
void	ingest_setPePhrase(char *pe_phrase);
void	ingest_setTsPhrase(char *ts_phrase);
void	ingest_setSwPhrase(char *sw_phrase);
void	ingest_obtainLocPhrase(void);
void	ingest_obtainPePhrase(void);
void	ingest_obtainTsPhrase(void);
void	ingest_obtainSwPhrase(void);


int	in_getItemData(in_info *inPtr);
int	in_getIngestItemData(IngestFilter *ingestPtr);


void	in_setItemData (Widget w, XtPointer ptr, XtPointer cbs);
void	in_setIngestItemData(IngestFilter *ingestPtr);

void	in_setSwitches_conf(Widget w, XtPointer ptr, XtPointer cbs);
void	in_setSwitches(Widget w, XtPointer ptr, XtPointer cbs);

char*	ingest_build_where(IngestFilter *ingestPtr);


void	ingest_loadingest(void);

void	in_select_new_ingest_item(in_info *inPtr);


in_info*		getIngestFilterInfo(int init_flag);
IngestFilter*		ingest_getingest(void);
ShefPe*			ingest_getshefpes(void);


void	freeIngestFilterInfo(void);
void	ingest_freeingest(in_info *inPtr);
void	ingest_freeshefdur(in_info *inPtr);
void	ingest_freeshefpes(in_info *inPtr);
void	ingest_freeshefts(in_info *inPtr);
void	ingest_freeshefex(in_info *inPtr);

void	ingest_freeposlist(in_info *inPtr);  /* free the filterpe_poslist */
void    ShowIngestDs ( Widget w ) ;



#endif
