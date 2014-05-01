/*******************************************************************************
 * FILENAME:            empe_data_structs.h
 *
 * DESCRIPTION:         This file contains user-defined types 
 *                      for the empe_fieldgen main function.
 *
 * ORIGINAL AUTHOR:       Guoxian Zhou
 * CREATION DATE:         Feb. 2008
 * ORGANIZATION:          HSEB / OHD
 * MACHINE:               Dell-Redhat Linux
 * MODIFICATION HISTORY:
 *   DATE         PROGRAMMER        DESCRIPTION/REASON
 *
 ********************************************************************************
 */

#ifndef EMPE_DATA_STRUCTS_
#define EMPE_DATA_STRUCTS_

#include "DbmsDefs.h"

typedef struct
{
    int hourNum; /* Number of hours to be run. */

    time_t tRunTime; /* ending date & time of runs. */

} run_date_struct;

typedef struct
{
    int hrap_x; /* The HRAP x-coordinate. */

    int hrap_y; /* The HRAP y-coordinate. .*/

    int num_cols; /* The number of columns.*/

    int num_rows; /* The number of rows.*/

} geo_data_struct;

typedef struct _gage_radar_pair_struct
{

    int hrap_x; /* HRAP x-coordinates of positive radar-gage pair */

    int hrap_y; /* HRAP y-coordinates of positive radar-gage pair */

    double gageValue; /* positive gage data */

    double radarValue; /* positive radar data */

} gage_radar_pair_struct;

typedef struct _gage_radar_pair_table_struct
{
    gage_radar_pair_struct * ptrGageRadarPair;

    int pairNum; /* number of positive radar-gage pairs */

} gage_radar_pair_table_struct;

typedef struct _radar_result_struct
{
    char radID[RADAR_ID_LEN + 1];
    short edit_bias;
    short ignore_radar;
    float bias;

} radar_result_struct;

typedef struct _neighbor_list_struct
{
    short listSize;

    short * pIndex;
    float * pDistance;

} neighbor_list_struct;

#endif /*EMPE_DATA_STRUCTS_*/
