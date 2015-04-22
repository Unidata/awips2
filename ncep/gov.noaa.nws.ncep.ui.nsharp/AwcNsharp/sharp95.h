	#define ROCP .28571428

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
	   float bplus3000;
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

	struct _configure
	{
	char filename[200];
	char prntype[15];
	char lptname[45];
	};

	#include <readdata.h>
	#include <basics.h>
	#include <thermo.h>
	#include <xwvideo.h>
	#include <skparams.h>
	#include <skparams.h>
	#include <decoder.h>
	#include <winds.h>
	#include <hpgl.h>
	#include <epson.h>
	#include <pcl.h>

	/* Global Variable Definitions */

#ifdef MAINMOD
        int             mode = 1, xwdth, xhght, xdpth, raob_mod;
        Pixel           pixels[40];
        GC              gc;       /* graphic context */
        Widget          draw_reg;
        Pixmap          canvas;
	float sndg[500][7], origsndg[500][7], sndg2[500][7];
	float sndgs[200][7][49];  /* Extend Loaded Soundings from 20 to 49 */
	float st_dir=-999, st_spd=-999;
	short numlvl = 0, origlvl = 0, pagenum, nobanner, overlay_previous=0;
	char raobtitle[80], stn_abbrev[4], raob_type[80], mdl_type[80];
	struct _skewt skv = { 40, 25, 500, 500, 100, 70, 60 , 1};
	struct _hodog hov = { 40, 25, 500, 500, -20, -20, 120, 20 };
	struct _lplvalues lplvals;
	struct _configure config;
        int  g_pfs_model_sel_sw = 0;
#else
        extern int             mode, xwdth, xhght, xdpth, raob_mod;
        extern Pixel           pixels[40];
        extern GC              gc;       /* graphic context */
        extern Widget          draw_reg;
        extern Pixmap          canvas;
	extern float sndg[500][7], origsndg[500][7], sndg2[500][7], sndgs[200][7][49], st_dir, st_spd;
	extern short numlvl, origlvl, pagenum, nobanner, overlay_previous;
	extern char raobtitle[80], stn_abbrev[4], raob_type[80], mdl_type[80];
	extern struct _lplvalues lplvals;
	extern struct _configure config; 
        extern int g_pfs_model_sel_sw;
#endif

#ifdef VIDEO
	extern struct _skewt skv;
	extern struct _hodog hov;
#endif

