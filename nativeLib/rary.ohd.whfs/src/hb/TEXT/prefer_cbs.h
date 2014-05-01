/*
	File:		prefer_cbs.h
	Date:		May 1995
	Author:		Dale Shelton
	
	Purpose:	Defines the function prototypes,
			definitions, and data declarations
			for the user preferences dialog.
			
*/


#ifndef pref_cbs_h
#define pref_cbs_h

/*
	Definitions.
*/
#define FLDS_OFF	0
#define FLDS_ON		1

/*
	Function prototypes.
*/
void	ShowPrefDs(Widget w);	
void	prefer_sensitize_flds(const long state);
void	prefer_callbacks(void);
void	prefer_clear(void);
void	prefer_load(void);
void	prefer_fields();
void	prefer_close();
void	prefer_save();


#endif
