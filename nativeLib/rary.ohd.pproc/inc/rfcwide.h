/*=========================================================================*/
/*                              FILE NAME:   rfcwide.h                     */
/*                                                                         */
/*                              HEADER FILE                                */
/*=========================================================================*/

#ifndef RFCWIDE_H
#define RFCWIDE_H

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>

#include "ColorValue.h"
#include "DbmsDefs.h"
#include "HydroStatus.h"

/*----------------------------------*/
/* global definitions               */
/*----------------------------------*/
#define MAX_RFC_LEN 9
#define MAX_USER_ID_LEN 100
#define MAX_MPE_STRING_LEN 1024
#define APPLICATION_NAME "hmapmpe"

/*----------------------------------*/
/* Coorinate reading function       */
/*----------------------------------*/
HydroStatus read_mpe_coordinate_file ( int * xor, int * yor, int * maxx,
                                       int * maxy );

/*----------------------------------*/
/* main window functions            */
/*----------------------------------*/

void   popup_rfcwide_workingDialog();
Widget create_working_dialog_RFCW();

void hour_sensitive_RFCW();

/*----------------------------------*/
/* save data definitions            */
/*----------------------------------*/

char date_form[4];   /*   date format for xmrg filename  */
int daa_min_coverage_dur;  /* NOTE: this variable is same as field in mpe_params_struct */

/*------------------------------------------*/
/* radar related data                       */
/*------------------------------------------*/

short int    ***stage1u;
short int    ***radcov;

char  **datetime_radar_prod;
char  **datetime_daaradar_prod;

/*--------------------------------------------------------*/
/* routines for reading data from PseudoGageRadarVal and  */
/*  ProcPrecip tables                                     */
/*--------------------------------------------------------*/

void init_cur_pse_RFCW();

void fetch_loc();

/*--------------------------------------------------*/
/*  reading preferences and parameters              */
/*   and initializing data                          */
/*--------------------------------------------------*/

void read_rwparams();

char default_display_type[MOSAIC_TYPE_LEN + 1];

/*----------------------------------*/
/* grid display                     */
/*----------------------------------*/

void read_field_data_RFCW(int);

int first_display ;

/*----------------------------------*/
/* date related routines            */
/*----------------------------------*/

void display_rfcwide_date_window();
void show_dates_RFCW();

void get_dates_RFCW();
void GetCurrentDate_RFCW();
void GetEarlierDate_RFCW();

/*----------------------------------*/
/* supplemental data handling       */
/*----------------------------------*/

typedef struct         {
                       int nisolbin;
                       int noutint;
                       int noutrep;
                       int nbadscan;
                       int nhourout;
                       int volcovpat;
                       int opermode;
                       int minoff;
                       int supplmess;
                       float maxvald;
                       float maxvalh;
                       float bias;
                       float areared;
                       float biscanr;
                       char gentime[22];
                       } suppl_data_struct; 

suppl_data_struct      suppldata;

/*------------------------*/
/* overlay                */
/*------------------------*/

typedef struct         {
		       int irfc;
                       int istate;
                       int icity;
                       int icounty;
                       int iradring;
                       int iriver;
                       int ibasbound;
		       int igage;
                       } overlay_default; 

overlay_default        overlay_def;

/*----------------------------------*/
/* update gage values               */
/*----------------------------------*/

void update_gval_RFCW(char [], char [], short, float, int *, char [10][4], long int *);
void update_pseudo_RFCW ( const char * gid, const char * dt, 
		          float gval, long int * nupd );
void update_procprecip(char [], char [], short, float, short int *, char [], char [],
                       char [], long int *);
void update_precip_RFCW(char [], char [], float, short int, char [], char [], char [],
                        float *, float *, int *);
void insert_reject_RFCW(char [], char [], short int, char [], char [], float *);

/*----------------------------------*/
/* single site window               */
/*----------------------------------*/

void label_unbrad();
void label_rawrad();
void label_radclim();
void label_radcov();
void edit_radcov();
void locate_ss_RFCW();
void initialize_draw_data_rfcwide();
void missing_data_rfcwide();

void write_editbias_RFCW();
void edit_bias_value_RFCW();
void destroy_biasShell();

void update_flig_RFCW();

char cv_use_mainwin[16];
int cv_duration_mainwin;

void read_radcov_grids();

void show_ss_gages_RFCW();

/*----------------------------------*/
/* draw precip                      */
/*----------------------------------*/


void delete_poly_RFCW();
void setup_edit_poly_RFCW();
void display_prev_poly_RFCW();

/*----------------------------------*/
/* units factor, scale factor       */
/*----------------------------------*/

int scale_factor;
float units_factor;

/*----------------------------------*/
/* bias table data handling         */
/*----------------------------------*/

void read_radar_results_table();

typedef struct         {
                       float mem_span[20];
                       float num_pairs[20];
                       float sumgag[20];
                       float sumrad[20];
                       float bias[20];
                       } bias_data_struct; 

bias_data_struct       biasdata;

typedef struct         {
                       float rw_bias_val_used;
                       char  edit_bias_values[2];
                       } result_data_struct; 

result_data_struct     radarresultdata;

typedef struct         {
                       float mlt_zrcoef;
                       float pwr_zrcoef;
                       } zerocoef_data_struct; 

zerocoef_data_struct   abzerocoef;

float                  memspan_values[10];

/*---------------------------*/
/*  levels,colors for legend */
/*---------------------------*/

float       level_value[30];
char     color_list_levels[40][26];

void    set_colorvalues();
void    set_coloroverlays();

#endif /* #ifndef RFCWIDE_H */
