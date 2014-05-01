/*
	File:		geoline_show.h
	Date:		May 1997
	Author:		Mark Glaudemans
	
	Purpose:	Provide support for GeoLine DS.
*/


#ifndef GEOLINE_SHOW_H
#define GEOLINE_SHOW_H


#define READFILE  1
#define WRITEFILE 2

/* prototypes */

void geoline_show(Widget 	w);
void add_geoline_cbs();

void load_line_import();

void geoline_importCB();
void import_geoline();
void geoline_editCB();

void getline_filename(int  	readwrite_flag,
		      int	*default_match,
		      int	*status);


void geoline_typeCB();

void geoline_readlog_importCB();

void remove_ok_line_callbacks(Widget widgetPB);

void ok_geolineCB();

#endif 
