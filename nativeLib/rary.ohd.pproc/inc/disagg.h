#ifndef DISAGG_H
#define DISAGG_H

#include <time.h>

#include "HourlyPP.h"
#include "HourlyPC.h"
#include "load_PCPP_data.h"
#include "time_convert.h"
#include "get_total_precip.h"

#define MAX_STATION_RECORD_LEN 200

int num_disagg_stations;
int mpe_dqc_max_precip_neighbors;

void mergeSort(int numbers[], int temp[], int array_size);
void m_sort(int numbers[], int temp[], int left, int right);
void merge(int numbers[], int temp[], int left, int mid, int right);

void sort_without_duplicates(int numbers[], int temp[], int array_size, int * cnt);
void compute_1hr_station_list ();

typedef struct Values_1hr
{
   int dqc_day;
   int index_in_1hr_list;
   char ID[MAX_STATION_RECORD_LEN];
   float HourlyValues[24];
}Values_1hr;

typedef struct Values_6hr
{
   char ID[MAX_STATION_RECORD_LEN];
   float value[4];
   int hrapx_local;
   int hrapy_local;
}Values_6hr;

typedef struct Dist
{
   double * distances_to_neighbors;
}Dist;

void Read1hrGageVals();
void Disagg6hr();
void DisaggPointMethod();
void DisaggGridMethod();
void Delete1hrDisaggValues();
void Write1hrValuesFor6hrGages();
int GetQPEGrids(int, int);
void free_1hr_station_list();
void disagg_cleanup();
void grid_cleanup();

#endif /* ifndef DISAGG_H */
