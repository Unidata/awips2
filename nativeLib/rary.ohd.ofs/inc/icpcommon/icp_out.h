/*   icp_out.h   this code will write data needed by the icp gui
               at present (9/14/94) that is only for the wy-plot
                                 hs                  9/14/94     1005
               converted from top of large file composed of this file without
               the extern definitions and several .c files included at the
               bottom TO a usual, i.e. normal, .h file,  9/18/95*/
/* Note: this file is in directory
        /usr/apps/nwsrfs/ofs/inc/icpcommon
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#define FLIT(A) for(i=0;i<4;i++)jnflt.cs[i]=*pin++;A=jnflt.flt;
#define FXIT(A) for(i=0;i<4;i++)jnflt.cs[i]=*pin++;A=jnflt.fxd;
#define ROIMP 0
#define SDRO  1
#define SSUR  2
#define SIF   3
#define BFS   4
#define BFP   5

/* these numbers are larger than needed.  at least one is needed as it is
   used to keep a count rather than holding data  */
#define WYP_VARS   15
#define SAC_VARS   15
#define SNO17_VARS   20
#define PLOTTS_VARS   8

typedef union joint
 {
  char  cs[4];
  float flt;
  int   fxd;
 } Joint;

extern void mdyh1();

#ifdef MAIN_ICP_C
int ifirst,first_wyp, wyp_leap_yr, sac_leap_yr, sno17_leap_yr, plotts_leap_yr;
float tot_obs;

char *pin, *chrptr;
char shed_name[9], file_suffix[6], file_name_card[150];
char text_out_sac[];//="icp_sac_out.txt";
char text_out_crd[];//="icp_crd_out.txt";
char text_out_wyp[];//="icp_wyp_out.txt";
char text_out_sno17[];//="icp_sno17_out.txt";
char text_out_plotts[];//="icp_plotts_out.txt";
char tok_intrfc_dir[];//="mcp3_icp_iface";
char intrfc_dir[250];
char work_file[300];
char **file_name_plotts,**plotts_opnames;
char **file_name_sno17,**sno17_opnames;
char **file_name_sac,**sac_opnames;
char **file_name_wyp,**wyp_opnames;
int num_sac, num_wyp, num_sno17, num_plotts;
char envvar[250];
int   varlen, toklen;
float **plotts_data, **sno17_data, **sac_data, **wyp_data;
char seven_2[], seven_3[];
int ascii_out, sac_idt,   sac_time_steps, sac_frze,
                 sno17_itpx, sno17_idt, sno17_time_steps,
                 plotts_idt;
int mons[2][13];

FILE *icp_incrd, *icp_in_plotts, *icp_in_sno17, *icp_in_sac, *icp_in_wyp;
FILE *txt_out, *plotts_scr, *sno17_scr, *sac_scr, *wyp_scr;

#else

extern int ifirst, first_wyp, wyp_leap_yr, sac_leap_yr, sno17_leap_yr,
                                                          plotts_leap_yr;
extern float tot_obs;

extern char *pin, *chrptr;
extern char shed_name[9], file_suffix[6], file_name_card[150];
extern char text_out_sac[];
extern char text_out_crd[];
extern char text_out_wyp[];
extern char text_out_sno17[];
extern char text_out_plotts[];
extern char tok_intrfc_dir[];
extern char intrfc_dir[250];
extern char work_file[300];
extern char **file_name_plotts,**plotts_opnames;
extern char **file_name_sno17,**sno17_opnames;
extern char **file_name_sac,**sac_opnames;
extern char **file_name_wyp,**wyp_opnames;
extern int num_sac, num_wyp, num_sno17, num_plotts;
extern char envvar[250];
extern int   varlen, toklen;
extern float **plotts_data, **sno17_data, **sac_data, **wyp_data;
extern char seven_2[], seven_3[];
extern int ascii_out, sac_idt,   sac_time_steps, sac_frze,
                 sno17_itpx, sno17_idt, sno17_time_steps,
                 plotts_idt;
extern int mons[2][13];

extern FILE *icp_incrd, *icp_in_plotts, *icp_in_sno17, *icp_in_sac, *icp_in_wyp;
extern FILE *txt_out, *plotts_scr, *sno17_scr, *sac_scr, *wyp_scr;

#endif

