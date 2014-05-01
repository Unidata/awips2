/* extra_e19.h */

#ifndef extra_e19_h
#define extra_e19_h


typedef struct
	{
	 char   RCName[9]; /*Name of forecast point*/
	 char   river_name[21];
	 char   station_name[21];
	 float  latitude;
	 float  longitude;
	 char   forecast_point_type[21];
	 float  total_area;
	 float  local_area;
	 float  flood_stage;
	 float  flood_flow;
	 float  secondary_stage;
	 float  warning_stage;
	 float  warning_flow;
	 float  gage_zero;
	 float  record_stage;
	 float  record_flow;
	 int    date_of_record;
	 char   record_flood_comment[21];
	 float  rating_curve_limit;
/*	 char   f_group[9];
	 char   c_group[9];
	 char   upstream[5][9];
	 char   downstream[2][9];
*/
/*this 4 variables have moved to e19_data struct in strcut_defs.h
* and added RCName variable. For bug r20-33---kwz[4/29/02]
*/
	char    seg_status[8];         /* segment flow status - "Unknown",    */
				       /*   "Normal", "Alert", and "Flood"    */
	}       extra_e19;

#endif
