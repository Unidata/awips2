
#ifndef fstmt_cbs_h
#define fstmt_cbs_h

#include "Floodstmt.h"

/********************************
 Definition of symbolic constants
 ********************************/

#define SUCCESS            0
#define NO_DATA_AVAILABLE  1
#define FILE_PROBLEM       2

/*******************
 Function Prototypes
 *******************/

void	ShowFstmtDs(Widget w, char *lid, Boolean editable);	
void	fstmt_callbacks(void);
void	close_fstmt();
void	select_fstmt();
void	apply_fstmt();
void	defaults_fstmt();
void 	fstmt_del_conf();
void 	fstmt_delete();
void 	new_fstmt();
void 	ok_fstmt();
void 	clear_fstmt();

void    print_fstmt(Widget w, XtPointer ptr, XtPointer cbs); 
void    save_fstmt(Widget w, XtPointer ptr, XtPointer cbs);

int     populate_output_file(); 

void    text_wrapper(char *statement, FILE *filePtr); 

void    FSTMTselectfile_CB(Widget w, XtPointer ptr, XtPointer cbs);
void    FSTMTclosefile_CB(Widget w, XtPointer ptr, XtPointer cbs);

void    unload_fstmt(Floodstmt *fsRec);
void	fstmt_key(char *key, int *pos);

void	fstmt_load_stglist(char *lid);
void	fstmt_load_nth(char *lid, int item_num);

char * CurrentLid();

#endif
