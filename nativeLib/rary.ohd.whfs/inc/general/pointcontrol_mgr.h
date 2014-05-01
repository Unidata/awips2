
/*******************************************************************************
* FILENAME:             pointcontrol_mgr.h
* GENERAL INFORMATION:
* DESCRIPTION:          Contains prototypes for routines defined in the 
*                       pointcontrol_mgr.c file.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        April 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   4/11/02      Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef POINTCONTROL_MGR_H
#define POINTCONTROL_MGR_H

#include "CurPC.h"
#include "CurPP.h"
#include "LatestObsValue.h"
#include "Observation.h"
#include "pointcontrol_options.h"
#include "pointcontrol_report.h"
#include "RiverStatus.h"
#include "TokenizeOptionString.h"
#include "pointcontrol_timestep.h"

#define ACTION_TEXT_LENGTH 40
#define ALL_AREAS          "A-L-L_A-R-E-A-S"
#define DEBUG              1
#define FCSTPT_STNCLASS    'F'
#define LENGTH_OF_FILE_NAME 15
#define MISSING_VAL        -9999.


void printReportObs(ReportList * repPtr, char * header, char * message);
void printRiverStatus(RiverStatus * rsPtr, char *header, char * message);

char * build_pointdata_preset_string_from_options ( ) ;
void build_pointdata_table_entry ( ReportList * rPtr , char * buf ,
	                           int is_file ) ;
inline void clear_pointdata_update_flag ( ) ;
void disableStationDrawing ( ) ;
void enableStationDrawing ( ) ;
inline int get_pointdata_update_flag ( ) ;
int getStationDrawingState ( ) ;
int is_riverstatus_displayed ( ) ;
inline void set_pointdata_update_flag ( ) ;




ReportList * getReportListHead();
void setReportListHead(ReportList * newReportListHead);

ReportList * getIconReportListHead();
void setIconReportListHead(ReportList * newReportListHead);


void pc_process_request(int     retrieval_required);
void pc_adhoc_process_request(int retrieval_required);

ReportList * pc_process_onetime(pc_options_struct       pc_options);

void free_rawPtrs(Observation    *obshHead,
                  Observation    *obsdHead,
                  RiverStatus    *rsHead,
                  CurPP          *ppHead,
                  CurPC          *pcHead,
                  LatestObsValue *lHead,
                  Observation    *obsHead);

void free_derivedPtrs();

void free_finalPtr();

/* filter station functions -------------------------------------- */

void filter_reports_and_add_info ( pc_options_struct    * pc_options,
                                   ReportList   *obsrepHead ) ;
int isFilteredOutByValue(FilterOperation operation, double comparisonValue,  double valueToBeExamined);                                

ReportList * copy_replist(ReportList    *inputHead);

void get_serv_bkup_info ( char * reponsible_hsa_list ) ;
void set_serv_bkup_info ( const char * responsible_hsa_list ) ;

#endif
