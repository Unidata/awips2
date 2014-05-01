/*
	File:		grpinfo_cbs.h
	Date:		11/08/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/


#ifndef grpinfo_cbs_h
#define grpinfo_cbs_h


#include "RpfFcstGroup.h"
#include "RpfFcstPoint.h"
#include "ShefPe.h"


/*
	Function prototypes.
*/
void	grpinfo_show(Widget w);
int	grpinfo_load(void);
void	grpinfo_key(char *key, int *pos);
void	grpinfo_callbacks(void);
void	grpinfo_clearfg(void);
void	grpinfo_clearfp(void);

int	grpinfo_save(void);


/*
	Callback prototypes.
*/
void	grpinfo_importfg();	/* for importing group data into LIs, Txts */
void    grpinfo_importfp();	/* for importing point data into LI, Txt */

void	grpinfo_applyfg();	/* for saving forecast group info */
void	grpinfo_applyfp();	/* for saving point info (within the group) */

void	grpinfo_close();
void	grpinfo_add();
void	grpinfo_delete();
void	grpinfo_del_conf();

void	grpinfo_show_new_group(RpfFcstGroup *newgrpPtr);
void	grpinfo_show_new_point(RpfFcstPoint *newptPtr);

RpfFcstGroup*	grpinfo_getFcstGroupInfo(RpfFcstGroup *fginfo, Boolean init);
RpfFcstPoint*	grpinfo_getFcstPointInfo(RpfFcstPoint *fpinfo, Boolean init);

void		grpinfo_updateFcstGroupInfo(RpfFcstGroup *fginfo,
					    char *id, 
					    char *name, 
					    long ordinal,
					    char *rec_all_str);

void grpinfo_updateFcstPointInfo(RpfFcstPoint	*fpinfo, 
				 long 	        ordinal,
				 char		*rectype_str,
				 double	        chg_window,
                                 char		*primary,
                                 char		*secondary,
				 int            backhrs,
				 int            fwdhrs,				
				 double         ajtendhrs);

void rec_petypeCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);
void rec_npetypeCB(Widget w, XtPointer xtptr1, XtPointer xtptr2);
void load_rec_petype(Widget w);
void load_rec_npetype(Widget w);

#endif

