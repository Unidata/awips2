/*
	File:		user_prefs.h
	Date:		11/28/1994
	Author:		Dale Shelton
	
	Purpose:
	
*/

#ifndef user_prefs_h
#define user_prefs_h


/*
	Preference identifiers.
*/
#define TITLE		"TITLE"
#define SORT_LIST	"SORT_LIST"
#define FIELD_LIST	"FIELD_LIST"


/*
	Title preference identifiers.
*/
#define TITLE_HB5	0x01
#define TITLE_NAME	0x02


/*
	Field preference identifiers.
*/
#define FLD_COUNTY	0x01
#define FLD_BASIN	0x02
#define FLD_STREAM	0x04
#define FLD_LATLON	0x08


/*
	Function prototypes.
*/
long	get_preference(const char *pref);
void	set_preference(const char *pref, long code);

long	get_title_preference(void);
void	set_title_preference(long code);
void	set_window_title(Widget w, char *title, char *lid);
void	set_main_window_title(Widget w, const char *appName);

long	get_sort_preference(void);
void	set_sort_preference(long code);

long	get_field_preference(void);
void	set_field_preference(long code);

#endif
