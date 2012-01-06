#include "gemprm.h"
#include "xwprm.h"

#define ROCP .28571428

#define OBS_SND_TABLE "nsharp_observed.tbl"
#define PFC_SND_TABLE "nsharp_pfc.tbl"
#define MOD_SND_TABLE "nsharp_models.tbl"

/* maximum number of times in a scrolled list*/
#define MAX_TIME_LIST 20000 

/* maximum length of file list */
#define MAX_LIST_LEN 2000000

struct _skewt
{
  short tlx;
  short tly;
  short brx;
  short bry;
  short hspread;
  short vspread;
  short brtemp;
  short type;
};

struct _hodog
{
  short tlx;
  short tly;
  short brx;
  short bry;
  short xshift;
  short yshift;
  short hodomag;
  short scale;
};

struct _lplvalues
{
  char desc[40];
  float pres;
  float temp;
  float dwpt;
  short flag;
};

struct _parcel
{
  float lplpres;
  float lpltemp;
  float lpldwpt;
  float blayer;
  float tlayer;
  float entrain;
  float lclpres;
  float lfcpres;
  float elpres;
  float mplpres;
  float bplus;
  float bminus;
  float bfzl;
  float li5;
  float li3;
  float brn;
  float limax;
  float limaxpres;
  float cap;
  float cappres;
};


struct sndg_parms
{
  float omega;
  float pres;
  float hght;
  float temp;
  float dwpt;
  float drct;
  float sped;
};

struct sndg_struct
{
  int numlev;
  struct sndg_parms sndg[LLMXLV];
  int origlev;
  struct sndg_parms orig[LLMXLV];
  int ovrlev;
  struct sndg_parms ovrl[LLMXLV];
  float st_dir, st_spd;
  struct _lplvalues lplvals;
  char title[80];
};



struct _configure
{
  char filename[200];
  char prntype[15];
  char lptname[45];
};

struct obs_file_times
{
  char *filstr;
  int *posindex;
  char path[256];
  int nitems;
  int *selected_items;
};

struct modfile_struct
{
  char *model;
  char *cycle;
  char *fhour;
};

struct mlist_struct
{
  int ftim;
  int *posindex;
  int nselect;
  struct modfile_struct *modlist;
};

	/* Global Variable Definitions */

#ifdef MAINMOD
short xwdth, xhght, xdpth, raob_mod=0;
Pixel pixels[40];
GC gc, gc_cursor;		/* graphic context */
Widget draw_reg, cursor_out;
Pixmap canvas, canvas_cursor;
struct sndg_struct *sndgs[MAX_PIXMAP], *sndgp;
short mode = 1, pagenum, overlay_previous = 0;
char raobtitle[80], stn_abbrev[4], raob_type[80], mdl_type[80];
char *gemdata_env, *gemhds_env;
char gemsoundfile[200], gemsoundtime[20];
struct _skewt skv = { 40, 25, 500, 500, 100, 70, 60, 1 };
struct _hodog hov = { 40, 25, 500, 500, -20, -20, 120, 20 };
struct _configure config;
#else
extern short xwdth, xhght, xdpth, raob_mod;
extern Pixel pixels[40];
extern GC gc, gc_cursor;	/* graphic context */
extern Widget draw_reg, cursor_out;
extern Pixmap canvas, canvas_cursor;
extern struct sndg_struct *sndgs[MAX_PIXMAP], *sndgp;
extern short mode, pagenum, overlay_previous;
extern char raobtitle[80], stn_abbrev[4], raob_type[80], mdl_type[80];
extern char *gemdata_env, *gemhds_env;
extern char gemsoundfile[200], gemsoundtime[20];
extern struct _configure config;
#endif

#ifdef VIDEO
extern struct _skewt skv;
extern struct _hodog hov;
#endif

/* Function Prototype definitions for public functions */
#include "basics.h"
#include "decoder.h"
#include "readdata.h"
#include "thermo.h"
#include "winds.h"
#include "skparams.h"

#include "nsharp_proto.h"

