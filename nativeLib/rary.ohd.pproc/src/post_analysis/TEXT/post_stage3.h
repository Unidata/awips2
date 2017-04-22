/*=========================================================================*/
/*                    FILE NAME:   stage3.h                                */
/*=========================================================================*/

#ifndef POST_STAGE3_H
#define POST_STAGE3_H

#include <stdio.h>
#include <time.h>
#include <math.h>
#include <X11/Intrinsic.h>

#include "hrap.h"
#include "post_siii_shared.h"
#include "post_stage3_interface.h"

#define PI 3.141593

typedef struct         {
			int   month,day,year,hour;
			char  cdate[11];
			char  ldate[20];
			char  lldate[20];
			char  dttm[22];    
		       } date_struct;


typedef struct         {
			char id[9],rid[4],edit[10];
			float gval,mval,ms,mslow,mshi,s2,s2low,s2hi;
			point hrap;
			short td;
                        short manedit;
		       } gage_struct;


typedef struct         {
			char id[4];
			float xlat;
			float xlong;
			int   on;
			point  ctr;
			int   ngrd;
			int   ngage;
		       } nex_struct;

typedef struct {
   HRAP hrap;
   float value;
   Widget scale_widget;
   } pseudo_struct;

typedef struct {
   float value;
   Widget scale_widget;
   } editbias_struct;

typedef struct
  {
   int num;
   Widget w;
  } gage_edit_struct;

pseudo_struct pseudo;

struct tm      argdate;

Widget          dateshell;
Widget          swin;
int             numarg;
char            cdate[11];
char            datetime[22];
char          **dstring;
date_struct     curdate;
date_struct     xdate;
date_struct    *dates;
date_struct     date_st3;
date_struct     date_prev;
gage_struct    *gage;
nex_struct     *nexrad;
void            get_dates();
void            GetCurrentDate();
void            get_upd_time();
date_struct     GetEarlierDate();
void            initialize_data();
void            create_gage_table();
void            read_radarloc(int *);
void            Mosaic();
void            ReadGageData();
void            ReadStageiiData();
void            ReadParameters();
void            wr2s3res();
void            writemosaic();
void            read_fldtype(char *);
void            update_flig(char *, char *);
void            read_igbias(char *);
void            update_gval(char *, char *, short, float, int *,
                            char  ridlist[10][4], long int *);
void            update_fledml(char *, char *);
void            update_fldtype();
void            update_numpseudo();
void            update_bias();
void            read_siibias();
void            read_s3pref();
void            read_s3res();
void            read_s3res_mos();
void            rddpas2data();
void            clselcur();
void            init_cur_gag();
void            init_cur_pse();
void            fetch_gagrad();
void            fetch_gagradloc();
void            fetch_pseudo();
void            startdb(long int *);
void            closedb(long int *);
int             InOut();
int             get_apps_defaults();
Widget          create_working_dialog();
void            read_gage_edit_scale();
void            create_gage_edit_popup();
void            gage_edit_missing();

/*-------------------------------------*/
/*  draw precip polygon functions      */
/*-------------------------------------*/

void            setup_draw_precip();
void            apply_exit_drawpr();
void            noapply_exit_drawpr();
void            read_draw_precip_value();
void            read_draw_precip_value_RFCW();
void            setSubPrecipValueRFCW();
void            draw_precip_value_popup();
void            draw_precip_value_popup_RFCW();
void            popdown_draw_precip_value();
void            popdown_draw_precip_value_RFCW();
void            write_draw_precip_data();
int             InOutPoly();
void            read_drawpr_poly();
void            setup_edit_poly();
void            display_prev_poly();
void            select_poly_for_edit();
void            edit_precip_value_popup();
void            read_edit_precip_value();
void            popdown_edit_precip_value();
void            delete_poly();
void            read_s3res_drawpr(char[]);

/*-------------------------------------*/

char            RFC[9];
char           *HOME;
int             NRADARS;
int           **datafile;
int             dbg;
int             ngages;
int             NUMHRS;
int             XOR;
int             YOR;
int             MAXX, mxx;
int             MAXY, mxy;
int             num_gage_edit, num_rad_edit;
int             numpseudo;
int             zoom_flag;
int             istate, icity, iriver, ibound, iring;
int             numcounty;
short int    ***gageonly;
short int     **MosBuf;
short int    ***Multi;
short int    ***stage1i;
short int    ***stage1ii;
int             DataSaved;
int             projection;
short int       maxmosval;
short int       nmosaichr;
float          *siibiasu;
float          *siibiass;
float          *siibiasl;
float          *bias_slope;
float          *bias_inter;
float          *sibias;
short int      *iflarad;
short int      *iflgg;
short int      *iflml;
short int      *iflign;
char            mosaic_type[10];
char            default_mosaic_type[10];
int             iselect;
short int      *precip;     /* s3 precip array for use in save_netcdf function*/
char            st1fname[17]; /*  filename in st2_st1 dir for stage1 data */

/*----------------------------*/
/*   adaptable parameters     */
/*----------------------------*/

float           ad_params[37];
char            ad_param46[2];

/*------------------------------*/
/*   draw precip polygon info   */
/*------------------------------*/

int             drawprecip_flag;
int             applyprecip_flag;
int             deletepoly_flag;
int             num_draw_precip_poly;
int             num_prev_poly;
int            *draw_codes;
int            draw_precip_value_popup_RFCW_Up;
float          *draw_precip_values;
short int     **precip_for_draw;
char            drawprecip_filename[128];

/*------------------------------*/
/*   Stage3Result table info    */
/*------------------------------*/

char        datetime_save_xmrg[20];
char        filename_xmrg[30];

/*------------------------------*/
/*   colorvalue table info      */
/*------------------------------*/

char cv_use[16];
char cv_use_tmp[16];
char cv_lastUsedField[16];
int cv_duration;

/*------------------------------*/
/*   Bias Table GUI Info        */
/*------------------------------*/

int         biasShellUp;
Widget      biasTableDate;

#endif /* #ifndef POST_STAGE3_H */
