/*
	File:		geoarea_show.h
	Date:		May 1997
	Author:		Mark Glaudemans
	
	Purpose:	Provide support for GeoArea DS.
*/


#ifndef GEOAREA_SHOW_H
#define GEOAREA_SHOW_H

#include "GeoArea.h"

#define READFILE  1
#define WRITEFILE 2
      
/* prototypes */

void geoarea_show(Widget 	w);
void add_geoarea_cbs();

void load_area_list();
void load_area_import();

void geoarea_importCB();
void import_geoarea();
void geoarea_editCB();

void getarea_filename(int  	readwrite_flag,
		      int	*default_match,
		      int	*status);

void geoarea_select(GeoArea *areaPtr);

void free_geoarea();
void geoarea_typeCB();

void geoarea_readlog_importCB();

void remove_ok_callbacks(Widget widgetPB);

void ok_geoareaCB();

#endif 
