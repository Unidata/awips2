#ifndef POINTCONTROL_SHOW_H
#define POINTCONTROL_SHOW_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/ComboBox.h>

#include "pointcontrol.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "pointcontrol_report.h"
#include "pointservice_backup_show.h"
#include "pointcontrol_dispatch.h"

#include "DbmsUtils.h"
#include "DbmsAccess.h"
#include "DbmsDefs.h"
#include "Xtools.h"
#include "List.h"
#include "time_convert.h"
#include "PointDataPresets.h"
#include "precip_total.h"
#include "LoadUnique.h"
#include "QualityCode.h"
#include "TSControl_show.h"
#include "GeneralUtil.h"
#include "TokenizeOptionString.h" /* For the OptionValuePair structure
                                     definition. */

/* database tables, views */

#include "LatestObsValue.h"
#include "ShefPe.h"
#include "ShefTs.h"
#include "LocView.h"
#include "TelmType.h"
#include "IngestFilter.h"
#include "StnClass.h"
#include "LocWfo.h"
#include "LoadUnique.h"

#define RIVER_STRING "River"
#define RAIN_STRING "Rain"
#define SNOW_STRING "Snow"
#define TEMP_STRING "Temperature"
#define OTHER_STRING "Other"


typedef enum  RiverValueDisplayMode
{
	RAW_VALUE_ONLY = 0,
	RAW_VALUE_FLOOD_LEVEL, /*1*/
	RAW_VALUE_STAGE_FLOW,/*2*/
	FLOOD_DEPARTURE, /*3*/
	FLOOD_DEPARTURE_FLOOD_LEVEL/*4*/

} RiverValueDisplayMode;



/* gui functions ------------------------------------------------ */
char * pc_getTimeStepElementStringFromElement(TimeStepDataElement element);
TimeStepDataElement pc_getTimeStepElementFromString(const char * timeStepElementString);



extern void    show_pointcontrolDS ( Widget w , Widget map ,
                                     XtAppContext app  ) ;
extern void    pointcontrol_add_callbacks ( ) ;
void pc_add_common_callbacks();
void pc_add_adhoc_callbacks();
void pc_add_timestep_callbacks();
void pc_set_query_mode(QueryMode queryMode);

void pc_PredefinedOptionSelectCB ( Widget w, XtPointer ptr, XtPointer cbs);
void pc_QueryModeCB(Widget w, XtPointer ptr, XtPointer cbs);

void pc_TimeModeCB(Widget w, XtPointer ptr, XtPointer cbs);

void pc_dataSourceToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_showMissingToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_showForecastPointsToggleButtonCB (Widget w, XtPointer ptr, XtPointer cbs);

void pc_ToggleButtonCB(Widget w, XtPointer ptr, XtPointer cbs);
int reversePolarity(int booleanValue);
void pc_ShowCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_AdHocElementTypeCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_TimeStepElementTypeListCB(Widget w, XtPointer ptr, XtPointer cbs);
int getElementTypeFromString(char * elementTypeString);
void pc_setElementTypeByOppositeModeElementType(QueryMode previousMode, QueryMode newMode);
void pc_setElementType(int element_type);

void pc_riverStationFilterCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_precipPeFilterCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_valueFilterOperationCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_elevFilterOperationCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_valueTextFilterCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_elevTextFilterCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_hoursTextFilterCB (Widget w, XtPointer ptr, XtPointer cbs);


// text filtering

double filterTextAndGetDecimal(Widget textField);
long filterTextAndGetLong(Widget textField);
char * removeNonNumericChars(char * text);
char * removeNonDigits(char * text);

//void pc_SetDisplay( void ( * display_routine ) ( ) ) ;
void pc_StageDataToDisplayCB(Widget w, XtPointer ptr, XtPointer cbs);
RiverValueDisplayMode getRiverValueDisplayMode(pc_options_struct * pc_options);
void enableDisableValueFilterText(FilterOperation filterOperation, Widget textField);
void pc_StageBasisCB (Widget w, XtPointer ptr, XtPointer cbs);

void pc_OtherListCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_AdHocPhysElementCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_TimeStepElementListCB(Widget w, XtPointer ptr, XtPointer cbs);
TimeStepDataElement pc_getTimeStepElement(TimeStepDataElementType elementType,
									  	int arrayPosition);
void pc_TypesrcListCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_DataSourceListCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_AdHocDataSourceItemChooserCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_TimeStepTypeSourceItemChooserCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_AdHocTypeSourceItemChooserCB(Widget w, XtPointer ptr, XtPointer cbs);

// custom callback signatures for the ItemChooser dialog
void pc_AdHocDataSourceApplyCB(int * selectedPositionArray, int itemCount);
void pc_AdHocDataSourceCloseCB(int * selectedPositionArray);
void pc_TimeStepTypeSourceApplyCB(int * selectedPositionArray, int itemCount);
void pc_TimeStepTypeSourceCloseCB(int * selectedPositionArray);
void pc_TypeSourceApplyCB(int * selectedPositionArray, int itemCount);
void pc_TypeSourceCloseCB(int * selectedPositionArray);

void pc_hsaToggleCB (Widget w, XtPointer ptr, XtPointer cbs);
void pc_hsaButtonCB (Widget w, XtPointer ptr, XtPointer cbs);

void pc_loadComboBox(Widget comboBoxList, char** stringArray, int itemCount);
void pc_loadElementTypeCBX();


void pc_loadTimeStepElementXmListByElementType(TimeStepDataElementType elementType);
void pc_loadTimeStepElementCBX(int * elementArray, int elementCount);

void pc_loadAdHocElementXmList(Widget w, PhysTypeSrcText *textbuf, int nitems);

void pc_LoadOtherXmListByPos(int pos);

void pc_TypeSrcToggleCB(Widget w, XtPointer ptr, XtPointer cbs);

void pc_instPrecipOptionMenuCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_instPrecipSimpleSpinBoxCB(Widget w, XtPointer ptr, XtPointer cbs);

void increaseEndDayCB( Widget w, XtPointer ptr, XtPointer cbs);
void decreaseEndDayCB( Widget w, XtPointer ptr, XtPointer cbs);
void increaseEndHourCB( Widget w, XtPointer ptr, XtPointer cbs);
void decreaseEndHourCB( Widget w, XtPointer ptr, XtPointer cbs);
int getAdjustmentHours(pc_options_struct * pc_options);

void arrow_key_press_handler ( Widget w , XtPointer clientdata ,
                         XEvent * event ,
                         Boolean * continue_to_dispatch_event );

void pc_load_timeinfo();
void pc_SetGUIFromOptionsStruct();

void read_end_pctimestr();

int    	save_PeTsIngestfilter();
void   	count_PhysElemTypes();
void   	pc_loadAdHocTypeSrcListByElementPos(int pos);
void   	load_DatasrcList();

int load_presetOptionList ( Boolean refresh_preset_option_list ,
                             char * preset_id ,
                             Boolean update_pcoptions);
void selectFirstPreset();

void	checkOtherList();

void 	desensitize_type_source_widgets();
void 	sensitize_type_source_widgets();
void 	set_timefields();

void pc_ClearCB ( Widget w , XtPointer clientdata , XtPointer calldata );
void pc_drawMap(Widget widget, Boolean forceRetrieveData);
void pc_drawMapIfNotDisabled(Widget widget, Boolean forceRetrieveData);
void pc_MapButtonCB (Widget w, XtPointer ptr, XtPointer cbs);
//void pc_RetrieveAndMapData (Widget w , int retrieval_required);
void pc_TableCB (Widget w, XtPointer ptr, XtPointer cbs);

void pc_CloseCB(Widget w, XtPointer ptr, XtPointer cbs);

void pc_SaveAsNewCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_UpdateCB(Widget w, XtPointer ptr, XtPointer cbs);
void pc_DeleteCB(Widget w, XtPointer ptr, XtPointer cbs);

void show_pointcontrol_tableDS(Widget 		w,
			       ReportList	*obsrepHead);
void pc_table_callbacks();

void load_pointtable(ReportList		*obsrepHead);

void pc_invoke_timeseries( Widget w , XtPointer ptr , XtPointer call_data );

void pc_table_print(Widget w, XtPointer ptr, XtPointer cbs);
int create_pctable_file(void);
void pc_table_get_filename(Widget w, XtPointer ptr, XtPointer cbs);
void pc_table_save_table(Widget 				w ,
			 XtPointer 				ptr ,
			 XtPointer                              call_data ) ;
void pc_table_close_filesb(Widget w, XtPointer ptr, XtPointer cbs);


void pc_table_CloseCB(Widget w, XtPointer ptr, XtPointer cbs);


/* service backup filter functions ------------------------------ */
void show_serv_bkup(Widget w);
void load_serv_bkup_lists();
void serv_bkup_toggles(Widget w, XtPointer ptr, XtPointer cbs);
void serv_bkup_apply(Widget w, XtPointer ptr, XtPointer cbs);
void serv_bkup_close(Widget w, XtPointer ptr, XtPointer cbs);
void serv_bkup_save();
void FreeReportList(ReportList *sp);

/* The generic map redraw utility -------------------------------------- */
void redrawMap ( ) ;

/* This function preserves the file scope of the pc_petsdata structure.
   It returns a copy of the structure to the calling routine. */

#endif
