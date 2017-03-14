
#ifndef COLOR_THRESHOLD_SHOW_H
#define COLOR_THRESHOLD_SHOW_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "ColorName.h"
#include "ColorValue.h"
#include "color_threshold.h"
#include "time_defs.h"
#include "NamedColorSetGroup.h"

#define MISSING_VALUE -9999
#define LESS_THAN_MIN_VALUE -8888

typedef struct ColorWindowSetup
{
  char ** color_use_names;
  char ** color_use_strings;
  int num_color_use_names;
  char * application_name;
  char threshold_unit;
  char * user_id;
} ColorWindowSetup;

typedef enum ThresholdType
   {
        MISSING_TYPE,
        LESS_MIN_TYPE,
	GREATER_EQUAL_TYPE
   } ThresholdType;

typedef enum QueryType
{
   COLOR_DELETE ,
   COLOR_SELECT ,
   COLOR_SELECT_ALL_DUR ,
   COLOR_UPDATE

}  QueryType ;

void	color_threshold_show(Widget w);
void	add_color_threshold_callbacks(void);
void	color_threshold_apply(Widget w, XtPointer ptr, XtPointer cbs);
int	color_threshold_save( ColorValue * pColorValue );
void	color_threshold_close(Widget w, XtPointer ptr, XtPointer cbs);
void	color_threshold_new(Widget w, XtPointer ptr, XtPointer cbs);


void	color_threshold_confirm_delete(Widget w, XtPointer ptr, XtPointer cbs);
void	color_threshold_delete(Widget w, XtPointer ptr, XtPointer cbs);

void    color_threshold_confirm_default ( Widget w , XtPointer ptr ,
                                          XtPointer cbs ) ;
void    color_threshold_default ( Widget w , XtPointer ptr , XtPointer cbs ) ;

void	color_threshold_unload_widgets(ColorValue *colorValue);
void	color_threshold_save_hvColorValue(ColorValue *colorValue);
void    color_threshold_load_widgets(ColorValue *colorValue);

void    color_threshold_load_widgets_callback(Widget w, XtPointer ptr,
				     XtPointer cbs);

void    color_threshold_load_main_list ( ColorValue * pColorValue ) ;
void    loadColorNameList(void);

void    hvColorValueCreateKeyWhere(ColorValue *colorValue, char *where,
                                   QueryType query_type ) ;
int     hvColorValueIsEqual(ColorValue *value1, ColorValue *value2);
void    hvColorValuePrint(ColorValue *colorValue);

int     getColorValueListPos(ColorValue *head, ColorValue *value);

void    pickColorCallback(Widget w, XtPointer ptr,  XtPointer cbs);

int     initialize_color_threshold_window ( 
                                    NamedColorSetGroup * pColorSetGroup,
                                    const char * application_name,
                                    const char * user_id,
                                    char threshold_unit );
void setApplicationName ( const char * app_name );
const char * getApplicationName ( );
void FreeColorWindowSetupMemory ( );

#endif
