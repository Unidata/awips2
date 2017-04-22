/*  cex25.h:  headers information for cex25 program  */

#ifndef cex25_h
#define cex25_h
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>

#define  ChooseTStypestruct
#define  MAX_MODS   100
#define  MAX_SEG_TS  50
#define  MAX_TS  20
#define  MAXDATA 50
#define  MAXLEN  10
#define  NUM_DATATYPES 94
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */
typedef struct
{
   char   datatype[5];
   int    num_dp;
}   DecPlaces;

/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

/* Function Declarations */
extern  void          Create_mods();
extern  void          Create_ts_array();
extern  void          date_hr();
extern  void          FillDecPlaceArray();
extern  void          fill_px_ro_ts();
extern  void          FindMDH();
extern  void          FindTSInfo();
extern  void          find_rrm_io_ts();
	float         linear_int();
	float         log_int();
extern  void          MakeTSInfoIndex();
extern  void          Make_ts_menu_names();
extern  void          Make_ts_name();
extern  void          rc_info();
static  void          restore();
static  void          stg_axis();
	void          TimeLabelFunction();
extern  void          tulplot();
extern  void          tultable();
extern  void          Write_mods();
	void          x_axis();
/* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> */

/* Variable Declarations */
  static char              **day_hrs, **day_hour;
  static int               type_check[MAX_TS];
  static int               run_start_hr = 0, file_start_hr = 0;
  static float             **ts_array, **orig_ts_array;
  int                      currentModIndex;
  DecPlaces                decimal_places[NUM_DATATYPES];
  
#endif  
