/*
 * Chin::Implement necessary APIs, not provided by Nsahrp, to interface with other Nsharp legacy code
 * First code: 05/2010.
 *
 */
#ifndef _WIN32
#include "gui.h"
#endif
#include "sharp95.h"
#include "Sndglib/profile.h"
#include "Sndglib/setsndg.h"
#include "Sndglib/sndglib.h"
#define cavesars            cavesars_
#define cavespnsharp        cavespnsharp_
#define hailcast1       hailcast1_
void cavesars(float *, float *, float * , float *, float *, float *, float *, float *, float *, int *, float *, float *, float *, float *, char *[15], float *[15], char *[80],int *);
void cavespnsharp(float *, float *, float * , float *, float *, float *, int *, float *, float *, char *[15], float *[15], char *[80],
float *, float *, float *,int *);

/*
 * In Cave: parameters are defined dynamically in  "populateSndgDataStatic()" in this file. Its index are defined
 * as followings.
 */
#define NPARM 7 /* Assume always using 7 parameters "PRES;HGHT;TEMP;DWPT;DRCT;SPED;OMEG".*/
#define NLEVEL 150 /* assume max sounding layers 150 */
#define PPRESS 0
#define PHGHT  1
#define PTEMP 2
#define PDEW  3
#define PWDIR 4
#define PWSPED 5
#define POMEG  6
//Chin's NOTE: AWC Nsharp source code parameters definitions are different from BigNsharp
/* They defined as,
 * sndg[numlvl][0] = omega
 * sndg[numlvl][1] = pressure
 * sndg[numlvl][2] = height
 * sndg[numlvl][3] = temperature
 * sndg[numlvl][4] = dew point
 * sndg[numlvl][5] = wind direction
 * sndg[numlvl][6] = wind speed
 * Therefore, when porting AWC Nsharp code, make sure convert to right parameters in CAVE.
 */
//this is copy from sndg_parms..but adding typedef for our implementation
typedef struct Sndg_parms
{
  float omega;
  float pres;
  float hght;
  float temp;
  float dwpt;
  float drct;
  float sped;
} CaveSndgParms;

typedef struct stormSlinkyStr
{
	int size;
	float tottim;
	float angl;
	float tsuv[200][2];
	int color[200];
} StormSlinkyStr;

/* cloud amount definition for FM algorithm */
#define OVC 1
#define BKN 2
#define SCT 3
#define FEW 4
#define MAX_CLOUD_LAYER 20
typedef struct  cloudInfoStr
{
	/* FM: Fred Mosher's Algorithm */
	int sizeFM;
	float preStartFM[MAX_CLOUD_LAYER];
	float preEndFM[MAX_CLOUD_LAYER];
	int   cloudTypeFM[MAX_CLOUD_LAYER];
	/* CE: Chernykh and Eskridge Algorithm */
	int sizeCE;
	float preStartCE[MAX_CLOUD_LAYER];
	float preEndCE[MAX_CLOUD_LAYER];
}CloudInfoStr;
/*
typedef struct stpStatsStr
{
	int stpcColor;
	int pwColor;
	int blMaxColor;
	int fosbergColor;
	float stpCin;
	char sfcRh[60];
	char sfc[60];
	char zeroOneKmRh[60];
	char zeroOneKmMean[60];
	char blMeanRh[60];
	char blMean[60];
	char pw[60];
	char blMax[60];
	char fosberg[60];
} StpStatsStr;
*/
#define WINTER_STRING_LEN	60
typedef struct  winterInfoStr
{
	float mopw;
	/*float htop;
	float hbot;
	float mrh;
	float mq;
	float mo;
	float pw;
	float pLevel;*/
	char oprh[WINTER_STRING_LEN];
	char layerDepth[WINTER_STRING_LEN];
	char meanLayerRh[WINTER_STRING_LEN];
	char meanLayerMixRat[WINTER_STRING_LEN];
	char meanLayerPw[WINTER_STRING_LEN];
	char meanLayerOmega[WINTER_STRING_LEN];
	char initPhase[100];
	char tempProfile1[WINTER_STRING_LEN];
	char tempProfile2[WINTER_STRING_LEN];
	char tempProfile3[WINTER_STRING_LEN];
	char wetbulbProfile1[WINTER_STRING_LEN];
	char wetbulbProfile2[WINTER_STRING_LEN];
	char wetbulbProfile3[WINTER_STRING_LEN];
	char bestGuess1[WINTER_STRING_LEN];
	char bestGuess2[WINTER_STRING_LEN];
} WinterInfoStr;

#define FIRE_STRING_LEN	60
typedef struct fireInfoStr
{
	int sfcRhColor;
	int pwColor;
	int blMaxColor;
	int fosbergColor;
	char sfcRh[FIRE_STRING_LEN];
	char sfc[FIRE_STRING_LEN];
	char zeroOneKmRh[FIRE_STRING_LEN];
	char zeroOneKmMean[FIRE_STRING_LEN];
	char blMeanRh[FIRE_STRING_LEN];
	char blMean[FIRE_STRING_LEN];
	char pw[FIRE_STRING_LEN];
	char blMax[FIRE_STRING_LEN];
	char fosberg[FIRE_STRING_LEN];
} FireInfoStr;

#define SARS_STRING_LEN  40
#define SARS_STRING_LINES  12
typedef struct sarsInfoStr
{
	int numHailstr; //max=12
	char hailStr[SARS_STRING_LINES][SARS_STRING_LEN];
	int hailStrColor[SARS_STRING_LINES];
	//char sighailStr[2][SARS_STRING_LEN];
	//int sighailStrColor;
	int numsupcellstr; //max=12
	char supcellStr[SARS_STRING_LINES][SARS_STRING_LEN];
	int supcellStrColor[SARS_STRING_LINES];
	//char torStr[2][SARS_STRING_LEN];
	//int torStrColor;
} SarsInfoStr;

struct Sounding staticSounding;

float * dataPtr[NLEVEL];
float * origdataPtr[NLEVEL];
float data[NLEVEL][NPARM];
float origdata[NLEVEL][NPARM];
char * parmsPtr[NPARM];
char parms[NPARM][8];
float snd[NLEVEL][NPARM];
/*
 * Chin: change from profile.c "Sounding *newSounding(short nparms, short nlev)" to use static memory
 *
 *
 */
void initStaticGlobalsMem(){
	int       i, j;

	Sounding *new=&staticSounding;
	new->data =(float **)  & dataPtr;
	for(j=0;j<NLEVEL;j++) {
		 new->data[j] = (float *) &data[j][0];
	}
	new->origdata =(float **)  & origdataPtr;
	for(j=0;j<NLEVEL;j++) {
		new->origdata[j] = (float *) &origdata[j][0];

	}
	new->parms = (char **) &parmsPtr;
	strcpy(&parms[PPRESS][0], "PRES");
	strcpy(&parms[PHGHT][0], "HGHT");
	strcpy(&parms[PTEMP][0], "TEMP");
	strcpy(&parms[PDEW][0], "DWPT");
	strcpy(&parms[PWDIR][0], "DRCT");
	strcpy(&parms[PWSPED][0], "SPED");
	strcpy(&parms[POMEG][0], "OMEG");
	new->nparms = NPARM;
	for(j=0;j<NLEVEL;j++) {
		new->parms[j] = (char **)&parms[j][0];
	}
	strcpy(new->stid, "ARCH"); // dont care for us
	strcpy(new->dattim, "ARCH");// dont care for us



}
void setSarsSupcellFileName(char sarsFlName[],int sarsLen, char supercellFlName[], int supLen){
	//init GLOBALS sars_filename and  sup_filename (for supercell)
	//the following statements are copied from read_nsharp_config();
	//printf("input super file = %s\n",supercellFlName);
	int       i;
	for(i=0; i < sizeof(sars_filename); i++){
		sars_filename[i]=0x0;
	}
	if(sarsLen >0 && sarsLen<= sizeof(sars_filename))
		strncpy(sars_filename, sarsFlName, sarsLen); //"/export/cdbsrv/cchen/Desktop/bigsharp9-original/nlist.txt");
	for(i=0; i < sizeof(sup_filename); i++){
		sup_filename[i]=0x0;
	}
	if(supLen>0 && supLen<= sizeof(sup_filename))
		strncpy(sup_filename, supercellFlName, supLen);

	//printf("copied super file = %s\n",sup_filename);
}
Sounding *getSoundingAndInit( short nlev)
{
	Sounding *new=&staticSounding;
	int       i, j;
	/*
	 * Initialize everything
	 */

	strcpy(new->stid,"UNKNOWN");
	new->datatype = -1;
	new->nlev     =  nlev;
	new->noriglev =  nlev;
	new->sfct     = RMISSD;
	new->sfctd    = RMISSD;
	new->sfcspd   = RMISSD;
	new->sfcdir   = RMISSD;

	for(j=0;j<nlev;j++) {
	  for(i=0;i<NPARM;i++) {
	    new->data[j][i]     = RMISSD;
	    new->origdata[j][i] = RMISSD;
	  }
	}

	return new;
}
int  populateSndgDataStatic(CaveSndgParms snDataArray[], int arraySize, int datatype) {
	float ix1, ix2;
	int i,j, minArraySize;
	Sounding  *s = NULL;
	static int inited=0;

	//if(inited==0){
	//	initStaticGlobalsMem();
	//	inited=1;
	//}
	if(arraySize > NLEVEL)
		minArraySize = NLEVEL;
	else
		minArraySize = arraySize;
	s = getSoundingAndInit( minArraySize);
	s->datatype = datatype;


	/* Populate data*/
	for (i=0;i<minArraySize;i++)
	{
		s->data[i][PPRESS]     = snDataArray[i].pres;
		s->origdata[i][PPRESS] = snDataArray[i].pres;
		s->data[i][PHGHT]     = snDataArray[i].hght;
		s->origdata[i][PHGHT] = snDataArray[i].hght;
		s->data[i][PTEMP]     = snDataArray[i].temp;
		s->origdata[i][PTEMP] = snDataArray[i].temp;
		s->data[i][PDEW]     = snDataArray[i].dwpt;
		s->origdata[i][PDEW] = snDataArray[i].dwpt;
		s->data[i][PWDIR]     = snDataArray[i].drct;
		s->origdata[i][PWDIR] = snDataArray[i].drct;
		s->data[i][PWSPED]     = snDataArray[i].sped;
		s->origdata[i][PWSPED] = snDataArray[i].sped;
		s->data[i][POMEG]     = snDataArray[i].omega;
		s->origdata[i][POMEG] = snDataArray[i].omega;

	}

	changeGlobalSounding(s);

	xtnd_sndg();

	/* Reset levels in sounding since the global var numlvl is updated in xtnd_sndg() */
	s->nlev     = numlvl;
	s->noriglev = numlvl;
	/* Chin::
	 * the following is copy from load_sounding() in xwvid6.c. This is how BigNsharp decide storm motion when it loads a
	 * new sounding data. st_dir and st_spd are globals and used to store current storm motion wind direction and speed.
	 * Calling bunkers_storm_motion() to set them.
	 */
	/* First guess at storm motion. User can modify on hodograph */
	bunkers_storm_motion(&ix1, &ix2, &st_dir, &st_spd);
	//st_dir = winDir;
	//st_spd = winSpd;
    return 1;
}


int  populateSndgData(CaveSndgParms snDataArray[], int arraySize, int datatype) {
	float snd[arraySize][7],ix1, ix2;
	int i,j;
	Sounding  *s = NULL, *curS=NULL;
	short  nparms = 0;
	char parms[128], **parmlist = NULL;
	/*
	switch (datatype) {
		  case SNDG_OBS:
		    strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED");
		  break;
		  case SNDG_MDL:
		    strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED;OMEG");
		  break;
		  case SNDG_PFC:
		    strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED;OMEG");//;TKEL");
		  break;
		  case SNDG_ACARS:
		  case SNDG_ARCH:
		  default:
		    return -1;
		  break;
	}*/
	strcpy(parms, "PRES;HGHT;TEMP;DWPT;DRCT;SPED;OMEG");
    for (i=0;i<arraySize;i++)
    {
    	snd[i][PPRESS]= snDataArray[i].pres;
    	snd[i][PHGHT]= snDataArray[i].hght;
    	snd[i][PTEMP]= snDataArray[i].temp;
    	snd[i][PDEW]= snDataArray[i].dwpt;
    	snd[i][PWDIR]= snDataArray[i].drct;
    	snd[i][PWSPED]= snDataArray[i].sped;
    	snd[i][POMEG]= snDataArray[i].omega; //copy it anyway

    }

	parmlist = defineParms(parms, &nparms);
	/*printf("datatype = %d nparm=%d\n",datatype, nparms);
	for( i=0; i< nparms; i++){
		printf("parm= %s\n",*(parmlist+i) );
	}*/
	s = newSounding(nparms, arraySize);
	s->parms    = parmlist;
	s->nparms   = nparms;
	s->datatype = datatype;
	s->nlev     = arraySize;
	s->noriglev = s->nlev;
	strcpy(s->stid, "ARCH"); // dont care for us
	strcpy(s->dattim, "ARCH");// dont care for us

	/* Populate */
	for (i=0;i<arraySize;i++)
	{
		for (j=0;j<nparms;j++)
		{
			s->data[i][j]     = snd[i][j];
			s->origdata[i][j] = snd[i][j];
		}
	}

	curS = getGlobalSounding();
	freeSounding(curS);

	changeGlobalSounding(s);

	xtnd_sndg();

	/* Reset levels in sounding since the global var numlvl is updated in xtnd_sndg() */
	s->nlev     = numlvl;
	s->noriglev = numlvl;

	/* First guess at storm motion. User can modify on hodograph */
	bunkers_storm_motion(&ix1, &ix2, &st_dir, &st_spd);
	//st_dir = winDir;
	//st_spd = winSpd;
    return 1;
}

void get_lpvaluesData(struct _lplvalues *pParcel){
	pParcel->temp = lplvals.temp;
	pParcel->dwpt = lplvals.dwpt;
	pParcel->pres = lplvals.pres;
	pParcel->flag = lplvals.flag;
	strcpy(pParcel->desc, lplvals.desc);
	//printf("desc %s\n", pParcel->desc);
}

/*
 * This function is used to get surface pressure and temp directly from sndgp. As some other api need
 * them as input parameters and legacy code API does not provide such.
 */
void get_surface(float* pressure, float * temp, float * dewpt){

	int idx;
	idx = getParmIndex("PRES");
	if(idx==-1)
		*pressure = 0;
	else
		*pressure = sndg[sfc()][idx];
	idx = getParmIndex("TEMP");
	if(idx==-1)
		*temp = 0;
	else
		*temp = sndg[sfc()][idx];
	idx = getParmIndex("DWPT");
	if(idx==-1)
		*dewpt = 0;
	else
		*dewpt = sndg[sfc()][idx];

}
void get_surfaceWind(float* windSp, float * windDir){
	int idx;
	idx = getParmIndex("SPED");
	if(idx==-1)
		*windSp = 0;
	else
		*windSp = sndg[sfc()][idx];
	idx = getParmIndex("DRCT");
	if(idx==-1)
		*windDir = 0;
	else
		*windDir = sndg[sfc()][idx];

}

void get_effectLayertopBotPres(float *topP, float * botP){
	//get p_top and p_bot by calling effective_inflow_layer, 100 and -250 are used by BigNsharp
	effective_inflow_layer(100,-250, &p_bot,&p_top);

	*topP = 	p_top;
	*botP =		p_bot;
}
/*
 * This function is used to get storm speed and direction directly from sndgp. As some other api need
 * them as input parameters and legacy code API does not provide such.
 */
void get_storm(float* speed, float * direction){
	*direction =  st_dir;
	*speed = st_spd;
}
void set_storm(float speed, float  direction){
	st_dir = direction;
	st_spd = speed;
}
float cave_ship(){
	/* this code is ported from show_hail_new() of xwvid3.c, part of Compute SARS Data code*/
	float lr75, shr6, fzlh, mucinh, ship,  t500, mumixr, mucape,mlcape,ix1, ix2, ix3, ix4,pres,el,depth;
	Parcel pcl;
	short  oldlplchoice, pIndex;
	pIndex = getParmIndex("PRES");
	oldlplchoice = lplvals.flag;
	define_parcel(4, 100);
	mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	define_parcel(3, 400);
	mucape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	mucinh = pcl.bminus;
	mumixr = mixratio(lplvals.pres, lplvals.dwpt);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &shr6);
	el = agl(i_hght(pcl.elpres, I_PRES));
	//Chin, for ensuring to set p_top and p_bot globals, call effective_inflow_layer_thermo(100,-250, &p_bot, &p_top); here anyway
	effective_inflow_layer_thermo(100,-250, &p_bot, &p_top);
	if (agl(i_hght(p_bot, I_PRES)) > 0.0){
		depth = el - agl(i_hght(p_bot, I_PRES));
	    wind_shear(p_bot, i_pres(msl(depth*0.5)), &ix1, &ix2, &ix3, &shr6);
	}

	t500 =  i_temp(500, I_PRES);
	lr75 = lapse_rate(&ix1, 700, 500);
	// Chin V9 was:: fzlh = agl(i_hght(temp_lvl( 0, &ix1 ), I_PRES));
	fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));//Chin: according to bignsharpV2013Jun12
	ship = sig_hail(mucape, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, mucinh, 0, 0, 25, mlcape);
	/* ----- Set Parcel Back ----- */
	if (oldlplchoice == 1)
		pres = 0;
	else if (oldlplchoice == 2)
		pres = 0;
	else if (oldlplchoice == 3)
		pres = mu_layer;
	else if (oldlplchoice == 4)
		pres = mml_layer;
	else if (oldlplchoice == 5)
		pres = user_level;
	else if (oldlplchoice == 6)
		pres = mu_layer;
	define_parcel(oldlplchoice, pres);

	return ship;
}
/*
 * This function is to get an array of points for storm slinky plotting
 * original function plot them directly, but CAVE has to plot on its own
 * way.
 */
void cave_visual1 ( float lower, float upper, float pres, float temp, float dwpt , struct stormSlinkyStr * stmSlinky)
	/*************************************************************/
	/*  VISUAL1                                                  */
	/*  John Hart  NSSFC KCMO                                    */
	/*                                                           */
	/*  Lifts specified parcel, given an initial 5 m/s push.     */
	/*  parcel trajectory is then calculated, using strict       */
	/*  parcel theory.  Updraft size is assumed 1km dia.         */
	/*                                                           */
	/*  All calculations use the virtual temperature correction. */
	/*                                                           */
	/*  lower       =  Bottom level of layer (mb)  [ -1=SFC]     */
	/*  upper       =  Top level of layer (mb)     [ -1=TOP]     */
	/*  pres        =  LPL pressure (mb)                         */
	/*  temp        =  LPL temperature (c)                       */
	/*  dwpt        =  LPL dew point (c)                         *
	**								*
	* Log:								*
	*
	*************************************************************/
	{
	short i, lptr, uptr, pIndex, zIndex, tIndex, tdIndex;
	float te1, pe1, te2, pe2, h1, h2, lyre, tdef1, tdef2, totp, totn;
	float te3, pe3, h3, tp1, tp2, tp3, tdef3, lyrf;
	float tote, dh, restim, uvv, ix1, ix2, tottim;
	float u, v, du, dv, tsu, tsv, tdist, angl;
	int size=0, colrx;
/*	float tangle; */

	lyre = -1.0F;
	totp = 25.0F;
	totn = 0.0F;
    tote = 0.0F;
    pIndex = getParmIndex("PRES");
    zIndex = getParmIndex("HGHT");
    tIndex = getParmIndex("TEMP");
    tdIndex = getParmIndex("DWPT");
    if (!sndg || pIndex == -1 || zIndex == -1 || tIndex == -1 || tdIndex == -1) return;

	/* ----- See if default layer is specified ----- */
    if (lower == -1) { lower = sndg[sfc()][pIndex]; }
    	if (upper == -1) { upper = sndg[numlvl-1][pIndex]; }

	/* ----- Make sure this is a valid layer ----- */
	if( lower > pres ) { lower = pres; }
	if( !qc( i_vtmp( upper , I_PRES)))  { return; }
	if( !qc( i_vtmp( lower , I_PRES)))  { return; }

	/* ----- Begin with Mixing Layer (LPL-LCL) ----- */
	te1 = i_vtmp(pres , I_PRES);
	pe1 = lower;
	h1 =  i_hght(pe1 , I_PRES);
	tp1 = virtemp( pres, temp, dwpt);

	drylift(pres, temp, dwpt, &pe2, &tp2);
	h2 =  i_hght(pe2 , I_PRES);
	te2 = i_vtmp(pe2 , I_PRES);

	if( lower > pe2 ) { lower = pe2; }

	/* ----- Find lowest observation in layer ----- */
	i = 0;
	while( sndg[i][pIndex] > lower)  { i++; }
	while ( !qc(sndg[i][tdIndex]) ) { i++; }
	lptr = i;
	if( sndg[i][pIndex] == lower ) { lptr++; }

	/* ----- Find highest observation in layer ----- */
	i=numlvl-1;
	while(sndg[i][pIndex] < upper) { i--; }
	uptr = i;
	if( sndg[i][pIndex] == upper ) { uptr--; }

	/* ----- Start with interpolated bottom layer ----- */
	pe1 = lower;
	h1 =  i_hght( pe1 , I_PRES);
	te1 = i_vtmp( pe1 , I_PRES);
	tp1 = wetlift(pe2, tp2, pe1);

	totp = 25.0F;
	totn = 0.0F;
	tsu = 0.0F;
	tsv = 0.0F;
	restim = 0.0F;
	tottim = 0.0F;
	for( i = lptr; i < numlvl; i++)
	{
		if (qc(sndg[i][tIndex]))
		{
			/* ----- Calculate every level that reports a temp ----- */
			pe2 = sndg[i][pIndex];
			h2 =  sndg[i][zIndex];
			te2 = i_vtmp( pe2 , I_PRES);
			tp2 = wetlift(pe1, tp1, pe2);
			tdef1 = (virtemp(pe1, tp1, tp1) - te1) / (te1 + 273.15F);
			tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15F);

			lyre = 9.8F * (tdef1 + tdef2) / 2.0F * (h2 - h1);

			if( lyre > 0.0F ) { totp += lyre; }
			else { if(pe2 > 500.0F) { totn += lyre; } }
			tote += lyre;

			uvv = (float)sqrt( (double)(totp * 2.0F) );
			dh = h2 - h1;
			restim = dh / uvv;
			tottim += restim;

			sr_wind( pe1, pe2, st_dir, st_spd, &u, &v, &ix1, &ix2);
			du = kt_to_mps(u) * restim;
			dv = kt_to_mps(v) * restim;
			tsu -= du;
			tsv += dv;
			tdist = (float)sqrt((double)(tsu*tsu) + (double)(tsv*tsv));
			/*	      tangle = angle(tsu, tsv);  NOT used */

			pe1 = pe2;
			h1 = h2;
			te1 = te2;
			tp1 = tp2;

			/* ----- Is this the top of given layer ----- */
			if(i >= uptr)
			{
				pe3 = pe1;
				h3 = h1;
				te3 = te1;
				tp3 = tp1;
				lyrf = lyre;

				if( lyrf > 0.0F )
				{ totp -= lyrf; }
				else
				{ if(pe2 > 500.0F) { totn -= lyrf; } }

				pe2 = upper;
				h2 = i_hght( pe2 , I_PRES);
				te2 = i_vtmp( pe2 , I_PRES);
				tp2 = wetlift(pe3, tp3, pe2);
				tdef3 = (virtemp(pe3, tp3, tp3) - te3) / (te3 + 273.15F);
				tdef2 = (virtemp(pe2, tp2, tp2) - te2) / (te2 + 273.15F);
				lyrf = 9.8F * (tdef3 + tdef2) / 2.0F * (h2 - h3);
				if( lyrf > 0.0F )
				{ totp += lyrf; }
				else
				{ if(pe2 > 500.0F) { totn -= lyrf; } }

				if( totp == 0.0F ) { totn = 0.0F; }

				uvv = (float)sqrt( (double)(totp * 2.0F) );
				dh = h2 - h1;
				restim = dh / uvv;
				tottim += restim;

				sr_wind( pe1, pe2, st_dir, st_spd, &u, &v, &ix1, &ix2);
				du = kt_to_mps(u) * restim;
				dv = kt_to_mps(v) * restim;
				tsu -= du;
				tsv += dv;
				tdist = (float)sqrt((double)(tsu*tsu) + (double)(tsv*tsv));
				/*		 tangle = angle(tsu, tsv);  NOT used */

				//vis_xy( tsu, tsv);
				stmSlinky->tsuv[size][0]= tsu;
				stmSlinky->tsuv[size][1]= tsv;
				stmSlinky->color[size]= 7; //based on BigNsharp
				//printf( " returning with size %d\n", size+1);

				angl = 90.0F - angle( tdist, agl(h2));

				//write_vis_data( tottim, angl );
				stmSlinky->tottim = tottim;
				stmSlinky->angl = angl;
				stmSlinky->size = size+1;
				return;
			}
			//based on BigNsharp to set color
			colrx = 13;
		    if (h2>msl(3000)) colrx=3;
			if (h2>msl(6000)) colrx=27;
			if (h2>msl(9000)) colrx=20;
			if (h2>msl(12000)) colrx=6;
			stmSlinky->color[size]= colrx;
			//vis_xy( tsu, tsv);
			stmSlinky->tsuv[size][0]= tsu;
			stmSlinky->tsuv[size][1]= tsv;
			stmSlinky->size = size+1;
			size++;

			//printf( "tsu=%f tsv=%f \n", tsu, tsv);
			if(size>= 200)
				return;
			if( sndg[i][pIndex] == 500.0F )
			{
				angl = 90.0F - angle( tdist, agl(sndg[i][zIndex]));
			}
		}
	}
}
float cave_bulk_rich2 (float *brnshear ){
	Parcel pcl;
	float ix1, sfctemp, sfcdwpt, sfcpres;
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;
	/* ----- Calculate Parcel Data ----- */
	parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	bulk_rich( pcl, &ix1 );
	*brnshear = ix1;
}
//calculate temp delta between 2 layer
//l1high should be 0 if it is surface layer
float aglT(int l1high, int l2high){
	short zIndex = getParmIndex("HGHT");

	return (i_temp(i_pres(sndg[sfc()][zIndex]+l1high),I_PRES)-i_temp(i_pres(sndg[sfc()][zIndex]+l2high),I_PRES));
}
/*float cave_bulk_rich ( float lplpres, float bplus,float *brnshear )
/*************************************************************/
/* ported from skparams.c but change its input parameters     */
/* BRN                                                      */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Calculates the Bulk Richardson Number for given parcel.  */
/*  Value is returned both as (param) and as a RETURN.       */
/*************************************************************/
/*{
	short idxp;
	float ptop, pbot, x1, y1, x2, y2, z1, z2, dx, dy;
	*brnshear = -9999;
	if (!sndg)
	  return -9999;
	idxp = getParmIndex("PRES");
	if (idxp == -1)
	  return -9999;

	//* Make sure they've initialized a parcel here
	if (lplvals.flag > 0 && lplvals.flag < 4) {
		   ptop = i_pres(msl(6000.0));
	   pbot = sndg[sfc()][idxp];
	}
	else {
	   pbot = i_pres(i_hght(lplpres, I_PRES) - 500.0);
	   if (!qc(pbot))
	     pbot = sndg[sfc()][idxp];
	   ptop = i_pres(i_hght(pbot, I_PRES) + 6000.0);
	}


	//* ----- First, calculate lowest 500m mean wind -----
	mean_wind(pbot, i_pres(i_hght(pbot, I_PRES)+500.0), &x1, &y1, &z1, &z2);
	/* ----- Next, calculate 6000m mean wind -----
	mean_wind(pbot, ptop, &x2, &y2, &z1, &z2);

	/* ----- Check to make sure CAPE and SHEAR are avbl -----
	if (!qc(bplus)) {return -9999;}
	if (!qc(x1)) {return -9999;}
	if (!qc(x2)) {return -9999;}

	/* ----- Calculate shear between winds -----

	dx = x2 - x1;
	dy = y2 - y1;
	*brnshear = (float)(sqrt((dx * dx) + (dy * dy))) * 0.51479;
	*brnshear = *brnshear * *brnshear / 2.0;
	/* ----- Calculate and return BRN ----- Chin: I dont know why return this value
	/* BRN valus is returned in  *brnshear
	return bplus / *brnshear;
}
*/
float itemp(float level)
{
	return i_temp(level, I_PRES);
}

float idwpt(float level)
{
	return i_dwpt( level, I_PRES);
}

float ihght(float level)
{
	return i_hght(level, I_PRES);
}

float iwndu(float level)
{
	/* return i_var("U", level, itype); */
        return ucomp(i_wdir(level, I_PRES), i_wspd(level, I_PRES));
}

float iwndv(float level)
{
	/* return i_var("V", level, itype); */
        return vcomp(i_wdir(level, I_PRES), i_wspd(level, I_PRES));
}

float iomeg(float level)
{
	return i_omeg(level, I_PRES);
}

float iwdir(float level)
{
	return i_wdir( level, I_PRES);
}

float iwspd(float level)
{
	return i_wspd( level, I_PRES);
}

float ivtmp(float level){
	return i_vtmp( level, I_PRES);
}

float ipres(float hght){
	return i_pres(hght);
}

void low_inv ( float *inv_mb, float *inv_dC )
/************************************************************************
 *  OPC MODIFICATION - J. Morgan 5/2/05					*
 *  LOW_INV - New function						*
 *									*
 *  LOW_INV								*
 *  J. Morgan OPC							*
 *									*
 *  Calculates base height of lowest temperature inversion.		*
 *									*
 *  inv_mb           - Pressure of inversion level (mb)			*
 *  inv_dC           - Change in temperature (C)			*
 *									*
 *  Called by 	xwvid1.c: draw_skewt()					*
 *  Called by 	xwvid3.c: show_inversion()				*
 *  *
 *  Chin:: not in BigNsharp, therefore ported to here. 6/8/2011
 ***********************************************************************/
{
	short ii, tIndex, pIndex;

	*inv_mb = -9999;
	*inv_dC = -9999;

	tIndex = getParmIndex("TEMP");
	pIndex = getParmIndex("PRES");

	if (!sndg) { return; }

	for  ( ii = 0; ii < numlvl-1; ii++) {
		if( qc(sndg[ii+1][tIndex]) && qc(sndg[ii][tIndex]) ) {
			if( sndg[ii+1][tIndex] > sndg[ii][tIndex] ) {
				*inv_mb = sndg[ii][pIndex];
				*inv_dC = sndg[ii+1][tIndex] - sndg[ii][tIndex];
				return;
			}
		}
	}
}
void mix_height ( float *mh_pres, float *mh_drct, float *mh_sped,
                  float *mh_dC, float *mh_lr, float *mh_drct_max,
                  float *mh_sped_max, short flag )
/****************************************************************************
 *  mix_height                                                          	*
 *                                                                      	*
 *  This function modifies original mix_height funtion which            	*
 *  computes hydro meteorological parameters within empirical mix       	*
 *  boundary layer.                    										*
 *                                                                      	*
 *  Calculates the mixing height.                                       	*
 *                                                                      	*
 *  mh_pres          - Pressure at mixing height (mb)                   	*
 *  mh_drct          - Wind direction at mixing height (deg)            	*
 *  mh_sped          - Wind speed at mixing height (kts)                	*
 *  mh_dC       	 - Layer change in temperature (C)                  	*
 *  mh_lr            - Layer lapse rate (C/km)                          	*
 *  mh_drct_max      - Layer maximum wind direction (deg)               	*
 *  mh_sped_max      - Layer maximum wind speed (kts)                   	*
 *  flag                         - 0: Surface-based lapse rate          	*
 *                                 1: Layer-based lapse rate            	*
 *                                                                          *
 *  Chin:: not in BigNsharp, therefore ported to here. 6/8/2011             *                                                      	*
 *  J. Morgan	5/2005		Created											*
 *  T. Lee      2/2014		Cleanup and fixed surface based and layer based	*
 *  						parameters always have same values				*
 ****************************************************************************/
{
        short ii, bb, b_1;
        float thresh, lapse_rate, lapser_1, mxdrct, mxsped, drct, sped;
        float dt, dz;
        short  pIndex, zIndex, tIndex, drIndex, spIndex;
        Boolean first_level = True;

        pIndex = getParmIndex("PRES");
        zIndex = getParmIndex("HGHT");
        tIndex = getParmIndex("TEMP");
        spIndex = getParmIndex("SPED");
        drIndex = getParmIndex("DRCT");
        if (!sndg || pIndex == -1 || zIndex == -1 || tIndex == -1  || drIndex == -1 || spIndex == -1) return;

        *mh_pres = -9999;
        *mh_drct = -9999;
        *mh_sped = -9999;
        *mh_dC = -9999;
        *mh_lr   = -9999;
        *mh_drct_max = -9999;
        *mh_sped_max = -9999;

        thresh   = 8.3;
        lapser_1 = 0.0;
        mxdrct   = 0.0;
        mxsped   = 0.0;
        drct     = 0.0;
        sped     = 0.0;

        for  ( ii = 0; ii < numlvl-1; ii++ ) {//Chin

                if  ( qc( sndg[ii+1][tIndex] ) ) { //TL
                        /* ----- Set Method Values ----- */
                        if( flag == 0 ) {
							bb  = 0;
							b_1 = 0;
                        }
                        else {
                            bb  = ii;
                            b_1 = ii-1;
                    }

                    if ( first_level ) {
                    	mxdrct = sndg[0][drIndex];
                    	mxsped = sndg[0][spIndex];
                    	*mh_drct = sndg[0][drIndex];
                    	*mh_sped = sndg[0][spIndex];
                    }

                    /* ----- Calculate Lapse Rate ----- */
                    if ( qc ( snd[bb][tIndex] ) ) {
                    	dt = sndg[ii+1][tIndex] - sndg[bb][tIndex];
                    	dz = sndg[ii+1][zIndex] - sndg[bb][zIndex];
                    } else {
                    	dt = sndg[ii+1][tIndex] - sndg[0][tIndex];
                    	dz = sndg[ii+1][zIndex] - sndg[0][zIndex];
                    }
                    lapse_rate = (dt / dz)*-1000;

                    /* ----- Test Lapse Rate ----- */
                    if  ( lapse_rate > thresh ) {
                        	first_level = False;

                            /* ----- Store Maximum Wind Data ----- */
                            if ( qc (sndg[ii+1][drIndex] ) ) {
                                drct = sndg[ii+1][drIndex];
                            	sped = sndg[ii+1][spIndex];

                            	if  ( sped >= mxsped ) {		// TL
                                	mxdrct = drct;
                                	mxsped = sped;
                                }
                            }

                    }
                    else if( first_level ) {  /* ----- Surface Test failed, Mixing Height=Surface ----- */
                            *mh_pres = sndg[0][pIndex];
                            *mh_drct = sndg[0][drIndex];
                            *mh_sped = sndg[0][spIndex];
                            *mh_dC   = -(sndg[ii+1][tIndex]  - sndg[0][tIndex]);
                            *mh_lr   = lapse_rate;
                            *mh_drct_max = sndg[0][drIndex];
                            *mh_sped_max = sndg[0][spIndex];

                            return;
                    }
                    else if( ii > 0 ) { /* ----- Above Mixing Height ----- */
                            /* ----- Calculate lapse rate within mixing layer ----- */
                    	    if ( qc ( sndg[b_1][tIndex]) ) {
                    	    	dt = sndg[ii][tIndex] - sndg[b_1][tIndex];
                    	    	dz = sndg[ii][zIndex] - sndg[b_1][zIndex];
                    	    } else {
                    	    	dt = sndg[ii][tIndex] - sndg[0][tIndex];
                    	    	dz = sndg[ii][zIndex] - sndg[0][zIndex];
                    	    }
                    	    *mh_dC = -dt;
                            if ( dz != 0. ) lapser_1 = (dt / dz)*-1000;
                            *mh_pres        = sndg[ii][pIndex];
                            if ( qc (sndg[ii][drIndex] ) ) {
                            	*mh_drct = sndg[ii][drIndex];
                            	*mh_sped = sndg[ii][spIndex];
                            } else {
                                *mh_drct = drct;
                                *mh_sped = sped;
                            }
                            *mh_lr       = lapser_1;
                            *mh_drct_max = mxdrct;
                            *mh_sped_max = mxsped;
                            return;
                    }
            }
    }
}


int cave_ww_type()
        /********************************************************************/
        /*      Watch type guidance                                         */
        /*      A decision tree to help with ww issuance                    */
        /*                                                                  */
        /*      Rich Thompson SPC OUN                                       */
		/* Chin: ported from xwvid5.c and modified for CAVE					*/
		/* input parameters were (short *wwtype, short *dcp). Not used by Cave*/
		/* calling function now. So, remove them.                           */
		/* output meaning:
		 *  0: NONE, color 19 Gold
		 *  1: MRGL SVR, color 26 Sky Blue
		 *  2: SVR, color 6 Cyan
		 *  3: MRGL TOR, color 2 red
		 *  4: TOR, color 2 red
		 *  5: PDS TOR, color 7 Magenta
		 *
		 *
		 *  Chin: 10/2/2013 : modified according to BigNsharpV2013-06-12
		 */
        /********************************************************************/
{
	float ix1, ix2, ix3, ix4, lr75, shr6, t500, fzlh, mumixr, lowrh, midrh, low_mid_rh;
	float mucn, mlcn, mlcp, sbcp, mucp, lr1, lr3, shr6_dir, sr46_dir, sr46_spd, shr6_sr46_diff, mmp;
	float sig_tor, sig_tor_winter, sighail, wind_dmg, rm_scp, cbsig, dncp, srh1, sblcl, mllcl;
	float oldlplpres, pres, pbot, ptop, shr8, bot, top, esrh, lm_scp;
	short oldlplchoice;
	int   ww_choice;
	short pIndex, tIndex, zIndex, tdIndex;
	short x1, y1, x2, y2, tlx, tly, wid;
	struct _parcel pcl;


	//*wwtype = 0;
	//*dcp = 0;

	oldlplchoice = lplvals.flag;

	tIndex = getParmIndex("TEMP");
	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tdIndex = getParmIndex("DWPT");
	/* sb parcel */
	define_parcel(1,0);
	ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	sbcp = pcl.bplus;
	sblcl = agl(i_hght(pcl.lclpres, I_PRES));
	sig_tor_winter = sigtorn_fixed(st_dir, st_spd);

	/* ml parcel */
	define_parcel(4,100);
	ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	mlcn = pcl.bminus;
	mlcp = pcl.bplus;
	mllcl = agl(i_hght(pcl.lclpres, I_PRES));
	sig_tor = sigtorn_cin(st_dir, st_spd);

	/* mu parcel */
	define_parcel(3,400);
	mucp = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	mucn = pcl.bminus;

	dncp = dcape(&ix1, &ix2);

	/* sighail ingredients */
	lr75 = lapse_rate(&ix1, 700, 500);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4);
	shr6 = ix4;
	shr6_dir = ix3;
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(8000)), &ix1, &ix2, &ix3, &ix4);
	shr8 = ix4;
	mumixr = mixratio(lplvals.pres, lplvals.dwpt);
	t500 =  i_temp(500, I_PRES);
	fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
	sighail = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcp);

	rm_scp = scp(st_dir, st_spd);
	wind_dmg = damaging_wind();

	sr_wind( i_pres(msl(4000)), i_pres(msl(6000)), st_dir, st_spd, &ix1, &ix2, &ix3, &ix4);
	sr46_dir = ix3;
	sr46_spd = ix4;
	shr6_sr46_diff = (shr6_dir - sr46_dir);
	srh1 = helicity(0, 1000, st_dir, st_spd, &ix1, &ix2);
	bot = agl(i_hght(p_bot, I_PRES));
	top = agl(i_hght(p_top, I_PRES));
	esrh = helicity(bot, top, st_dir, st_spd, &ix1, &ix2);

	lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+1000));
	lr1 = ix2;
	lapse_rate(&ix2, sndg[sfc()][pIndex], i_pres(sndg[sfc()][zIndex]+3000));
	lr3 = ix2;

	mean_relhum(&ix1, sndg[sfc()][pIndex]-150, sndg[sfc()][pIndex]-350);
	midrh = ix1;
	mean_relhum(&ix1, sndg[sfc()][pIndex], sndg[sfc()][pIndex]-150);
	lowrh = ix1;
	low_mid_rh = ((lowrh + midrh)/2);
	mmp = coniglio1();
	cbsig = (mlcp * kt_to_mps(shr6));
	/*printf("sig_tor=%f sig_tor_winter=%f srh1=%f esrh=%f sr46_spd=%f shr8=%f sblcl=%f\n mllcl=%f lr1=%f bot=%f low_mid_rh=%f rm_scp=%f\n mucn=%f dncp=%f sighail=%f cbsig=%f wind_dmg=%f",
			sig_tor,sig_tor_winter,srh1,esrh,sr46_spd,shr8, sblcl, mllcl, lr1, bot, low_mid_rh, rm_scp,mucn, dncp, sighail, cbsig, wind_dmg);
	Decision tree below is identical to the operational "ww_type" flow chart documentation 9/23/09 RLT */
    if ((sig_tor >= 3.0) && (sig_tor_winter >= 4.0) && (srh1 >=200) && (esrh >= 200) && (sr46_spd >= 15.0) && (shr8 >= 45.0) && (sblcl < 1000) && (mllcl < 1200) && (lr1 >= 5.0) && (mlcn > -50.0) && (bot == 0.0)) {
	// Chin V9 was::if ((sig_tor >= 3.0) && (sig_tor_winter >= 3.0) && (srh1 >= 150) && (esrh >= 150) && (sr46_spd >= 15.0) && (shr8 >= 40.0) && (sblcl < 1000) && (mllcl < 1100) && (lr1 >= 5.0) && (bot == 0.0)) {
		//*dcp = 1;
		//*wwtype = 5;
		ww_choice = 5;

		/*outgtext( "PDS TOR", tlx + 45, tly + 60 );*/
	}
    else if (((sig_tor >= 3.0) || (sig_tor_winter >= 4.0)) && (mlcn > -125.0) && (bot == 0.0)) {
    // Chin V9 was::else if (((sig_tor >= 3.0) || (sig_tor_winter >= 4.0)) && (bot == 0.0)) {
			//*dcp = 3;
			//*wwtype = 4;
			ww_choice = 4;

			/*outgtext( "TOR", tlx + 45, tly + 60 );*/
		}

	else if (((sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && ((sr46_spd >= 15.0) || (shr8 >= 40.0)) && (mlcn > -75.0) && (bot == 0.0)) {
		// Chin V9 was::if (((sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && ((sr46_spd >= 15.0) || (shr8 >= 40.0)) && (bot == 0.0)) {
				//*dcp = 4;
				//*wwtype = 4;
				ww_choice = 4;

				/*outgtext( "TOR", tlx + 45, tly + 60 );*/
			}

	else if (((sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && (low_mid_rh >= 60) && (lr1 >= 5.0) && (mlcn > -50.0) && (bot == 0.0)) {
		// Chin V9 was::if 	if (((sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && (low_mid_rh >= 60) && (lr1 >= 5.0) && (bot == 0.0)) {
					//*dcp = 5;
					//*wwtype = 4;
					ww_choice = 4;

					/*setcolor(2);
                outgtext( "TOR", tlx + 45, tly + 60 );*/
				}

	else if ((( sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && (mlcn > -150.0) && (bot == 0.0)) {
		// Chin V9 was::	if ((( sig_tor >= 1.0) || (sig_tor_winter >= 1.0)) && (bot == 0.0)) {
						//*dcp = 6;
						//*wwtype = 3;
						ww_choice = 3;

						/*setcolor(2);
                outgtext( "mrgl TOR", tlx + 40, tly + 60 );*/
					}

	else  if (((( sig_tor >= 0.5) && (esrh >= 150)) || ((sig_tor_winter >= 0.5) && (srh1 >= 150))) && (mlcn > -50.0) && (bot == 0.0)) {
		// Chin V9 was::    	if (((( sig_tor >= 0.5) && (esrh >= 150)) || ((sig_tor_winter >= 0.5) && (srh1 >= 150))) && (bot == 0.0)) {
							//*dcp = 7;
							//*wwtype = 3;
							ww_choice = 3;

							/*setcolor(2);
                outgtext( "mrgl TOR", tlx + 40, tly + 60 );*/
						}

	else if ((( sig_tor_winter >= 1.0) || (rm_scp >= 4.0) || (sig_tor >= 1.0)) && (mucn >= -50.0)) {
								//*dcp = 8;
								//*wwtype = 2;
								ww_choice = 2;
								/*
                setcolor(6);
                outgtext( "SVR",  tlx + 60, tly + 60 );*/
							}

	else if ((rm_scp >= 2.0) && ((sighail >= 1.0) || (dncp >= 750)) && (mucn >= -50.0)) {
									//*dcp = 9;
									//*wwtype = 2;
									ww_choice = 2;
									/*
                setcolor(6);
                outgtext( "SVR",  tlx + 60, tly + 60 );*/
								}

	else if ((cbsig >= 30000) && (mmp >= 0.6) && (mucn >= -50.0)) {
										//*dcp = 10;
										//*wwtype = 2;
										ww_choice = 2;
										/*
                setcolor(6);
                outgtext( "SVR",  tlx + 60, tly + 60 );*/
									}

	else if ((mucn >= -75.0) && ((wind_dmg >= 0.5) || (sighail >= 0.5) || (rm_scp >= 0.5)))  {
											//*dcp = 11;
											//*wwtype = 1;
											ww_choice = 1;
											/*setcolor(26);
                outgtext( "MRGL SVR",  tlx + 40, tly + 60 );*/
										}
	else    {
											//*dcp = 0;
											/*printf("\n dcp (after logic checks) = %d", *dcp);*/
											//*wwtype = 0;
											ww_choice = 0;
										}
	/*
        if (*wwtype == 0) {
                set_font(6);
                setcolor(19);
                outgtext( "NONE",  tlx + 50, tly + 60 );
                }*/

	/*      define_parcel(oldlplchoice, oldlplpres); */

	/* set parcel back to user selection */
	if (oldlplchoice == 1)
		pres = 0;
	else if (oldlplchoice == 2)
		pres = 0;
	else if (oldlplchoice == 3)
		pres = mu_layer;
	else if (oldlplchoice == 4)
		pres = mml_layer;
	else if (oldlplchoice == 5)
		pres = user_level;
	else if (oldlplchoice == 6)
		pres = mu_layer;
	define_parcel(oldlplchoice, pres);

	return ww_choice;

}
float cave_criticalAngel(){
	/* This function is based on trace_esrh() at xwvid1.c */
	/* critical angle from Guiliano and Esterheld (2007) */
	float  ix1, ix2,  ix4,ca, shr_dir, sr_dir;
	if ((p_bot < 1) || agl(i_hght(p_bot, I_PRES)) > 1) return -9999;
	sr_wind( p_bot, p_bot, st_dir, st_spd, &ix1, &ix2, &sr_dir, &ix4);
	wind_shear( p_bot, i_pres(msl(500)), &ix1, &ix2, &shr_dir, &ix4);
	if ((sr_dir <= 180) && (shr_dir <= 180)){ ca  = (sr_dir + 180) - shr_dir;}
	if ((sr_dir <= 180) && (shr_dir > 180)){ ca  = (sr_dir + 180)  - shr_dir;}
	if ((sr_dir > 180) && (shr_dir <= 180)){ ca  = (sr_dir - 180) - shr_dir;}
	if ((sr_dir > 180) && (shr_dir > 180)){ ca  = 180 - ((shr_dir - 180) - (sr_dir - 180));}
	/*printf("\nsfc DIR = %0.1f \ sfc spd = %0.1f \ sfc u (ucomp) = %0.1f \ sfc v (vcomp) = %0.1f", bot_dir, bot_spd,
			ucomp(bot_dir, bot_spd), vcomp(bot_dir, bot_spd));
		printf("\n500m DIR = %0.1f \ 500m spd = %0.1f \ 500m u (ucomp) = %0.1f \ 500m v (vcomp) = %0.1f", top_dir, top_spd,
			ucomp(top_dir, top_spd), vcomp(top_dir, top_spd));
		printf("\nsfc SR dir = %0.1f", sr_dir);
	        printf("\n0-500m shr dir = %0.1f", shr_dir);
		printf("\nCRITICAL ANGLE = %0.1f\n", ca);*/
	return ca;
}

float F1(float x) {
	if (x >= -10)
		return 1.0;
	else
		return (-.1*(x+70)+7);
}

float F2(float x) {
	if (x>=0)
		return 2.0;
	else if (x>=-10 && x<0)
		return (-.025*(x+10)+2.5);
	else
		return(-.125*(x+70)+10.0);
}

float F3(float x) {
	if (x>=0)
		return 3.0;
	else if (x>=-10 && x<0)
		return(-0.1*(x+10)+4.0);
	else
		return(-0.15*(x+50)+10.0);
}


int getCloudAmount(float temp,float dd) {
	if (dd<F1(temp))
		return 1;
	else if (dd<F2(temp))
		return 2;
	else if (dd<F3(temp))
		return 3;
	else
		return 4;
}
void draw_Clouds( struct  cloudInfoStr *cloudStr )
	/*****************************************************************/
	/* DRAW_CLOUDS                                                   */
	/* LARRY J. HINSON AWC/KCMO                                      */
	/* Chin: ported from "AWC nsharp" and  modified for CAVE         */
	/* Note: sndg parameter index is changed to be in line with BigNsharp
	 * implementation.
	 */
	/*****************************************************************/
	{
		int startflag,s1,s2,s3,i,spsub,epsub;
		float T1,T2,T3,dz,d2T,R1,R2,R3,d2R,startpres,endpres,t2,p2;
		float Tavg,DDavg,DD;
		float dd1,dd2,dd3,d2x;
		int cloudAmt,top,basefound;
		float d2xparam=0.0000;
		float Taccum=0.0;
		float DDaccum=0.0;
		float TCount=0.0;
		startflag=0;


		cloudStr->sizeFM=0;
		cloudStr->sizeCE=0;
		for (i=1;i<numlvl-2/*chin was -1*/;i++) {
			s1=i-1;
			s2=i;
			s3=i+1;
			if (sndg[s1][PTEMP]>-900.0 && sndg[s2][PTEMP] > -900.0 && sndg[s3][PTEMP]>-900) {
				T1=sndg[s1][PTEMP];
				T2=sndg[s2][PTEMP];
				T3=sndg[s3][PTEMP];
				dz=sndg[s3][PHGHT]-sndg[s1][PHGHT];
				if (dz==0.0) dz=1;
				d2T=(T3-2*T2+T1)/(dz*dz);
				R1=100*mixratio(sndg[s1][PPRESS],sndg[s1][PDEW])/mixratio(sndg[s1][PPRESS],sndg[s1][PTEMP]);
				R2=100*mixratio(sndg[s2][PPRESS],sndg[s2][PDEW])/mixratio(sndg[s2][PPRESS],sndg[s2][PTEMP]);
				R3=100*mixratio(sndg[s3][PPRESS],sndg[s3][PDEW])/mixratio(sndg[s3][PPRESS],sndg[s3][PTEMP]);
				d2R=(R3-2*R2+R1)/(dz*dz);
				if (d2T>=0 && d2R<=0 && !startflag) {
					startflag=1;
					startpres=sndg[s2][PPRESS];
					spsub=s2;

				}
				else if ( !(d2T>=0 && d2R<=0) && startflag) {
					startflag=0;
					endpres=sndg[s2][PPRESS];
					epsub=s2;
					Tavg=Taccum/TCount;
					DDavg=DDaccum/TCount;
					cloudAmt=getCloudAmount(Tavg,DDavg);
					Taccum=0.0;
					DDaccum=0.0;
					TCount=0;

					if (cloudAmt != FEW && cloudStr->sizeFM <MAX_CLOUD_LAYER) {
						cloudStr->preStartFM[cloudStr->sizeFM]= startpres;
						cloudStr->preEndFM[cloudStr->sizeFM]= endpres;
						cloudStr->cloudTypeFM[cloudStr->sizeFM] = cloudAmt;
						cloudStr->sizeFM++;
					}
				}

				if ((d2T>=0 && d2R<=0) && startflag) {
					Taccum+=sndg[s2][PTEMP];
					DD=sndg[s2][PTEMP]-sndg[s2][PDEW];
					DDaccum+=DD;
					TCount++;
				}
			}
		}
		top=0;
		for (i=numlvl-2/* was -1*/;i>0; i--) {
			basefound=0;
			s1=i-1;
			s2=i;
			s3=i+1;
			if (sndg[s1][PTEMP]>-900.0 && sndg[s2][PTEMP] > -900.0 && sndg[s3][PTEMP]>-900) {

				if (! top) {
					dd1=sndg[s1][PTEMP]-sndg[s1][PDEW];
					dd2=sndg[s2][PTEMP]-sndg[s2][PDEW];
					dd3=sndg[s3][PTEMP]-sndg[s3][PDEW];
					dz=(sndg[s3][PHGHT]-sndg[s1][PHGHT])/2.0;
					if (dz==0) dz=1;
					d2x=(dd3-2*dd2+dd1)/(dz*dz);
					if (d2x>0 && dd2<4.5) {
						top=-1;
						endpres=sndg[s2][PPRESS];
						epsub=s2;
						/* Now work downward till you get moistening with height */
						/* Do this until you reach lowest level of this condition */
						;
						while(i>1 && ! basefound) {
							i--;
							s1=i-1;
							s2=i;
							s3=i+1;
							if (sndg[s1][PTEMP]>-900.0 && sndg[s2][PTEMP] > -900.0 && sndg[s3][PTEMP]>-900) {
								dd1=sndg[s1][PTEMP]-sndg[s1][PDEW];
								dd2=sndg[s2][PTEMP]-sndg[s2][PDEW];
								dd3=sndg[s3][PTEMP]-sndg[s3][PDEW];
								dz=(sndg[s3][PHGHT]-sndg[s1][PHGHT])/2.0;
								if (dz==0) dz=1;
								d2x=(dd3-2*dd2+dd1)/(dz*dz);
								if (d2x < -d2xparam) {
									while ((d2x < -d2xparam  && dd2<4.5) && i>1) {
										i--;
										s1=i-1;
										s2=i;
										s3=i+1;
										if (sndg[s1][PTEMP]>-900.0 && sndg[s2][PTEMP] > -900.0 && sndg[s3][PTEMP]>-900) {
											dd1=sndg[s1][PTEMP]-sndg[s1][PDEW];
											dd2=sndg[s2][PTEMP]-sndg[s2][PDEW];
											dd3=sndg[s3][PTEMP]-sndg[s3][PDEW];
											dz=(sndg[s3][PHGHT]-sndg[s1][PHGHT])/2.0;
											if (dz==0) dz=1;
											d2x=(dd3-2*dd2+dd1)/(dz*dz);
										}
									}
									/* Lowest level of drying found...compute LCL from s1*/
									drylift(sndg[s1][PPRESS],sndg[s1][PTEMP],sndg[s1][PDEW],&p2,&t2);
									startpres=p2;
									if (startpres<endpres) {
										startpres=sndg[s1][PPRESS];
									}
									spsub=s1;
									top=0;
									basefound=-1;
									if (cloudStr->sizeCE <MAX_CLOUD_LAYER) {
										cloudStr->preStartCE[cloudStr->sizeCE]= startpres;
										cloudStr->preEndCE[cloudStr->sizeCE]= endpres;
										cloudStr->sizeCE++;
									}
									//setcolor(2);
									//XFillRectangle(XtDisplay(draw_reg), canvas, gc,50,pres_to_pix(endpres),
									//		90-50,pres_to_pix(startpres)-pres_to_pix(endpres));

								}
							}
						}
					}
				}
			}
		}
	}


void getWinterInfo( WinterInfoStr * winterInfo )
/*************************************************************
 * Chin's note:
 * This function is based on show_winter_new() of xwvid3.c
 * by John Hart  NSSFC KCMO
 * Chin: Rewrite code to get all computed parameters/string for
 * CAVE.
 * All original BigNsharp plotting are removed.
 *************************************************************/
{
	float ix1, ix2;
	float  pose, nege;
	float ptop, pbot, htop, hbot, mrh, mq, mo, pw, mopw;
	short  pIndex, zIndex, tIndex;
	short phase;
	char st[100];
	struct _ptype ptype1;
	//char  pt[80];

	tIndex = getParmIndex("TEMP");
	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");


	/* Do dendritic growth zone calcs */
	ptop = temp_lvl(-17, &ix1);
	pbot = temp_lvl(-12, &ix1);
	if (ptop < 0) ptop = sndg[sfc()][pIndex];
	if (pbot < 0) pbot = sndg[sfc()][pIndex];
	htop = i_hght(ptop, I_PRES);
	hbot = i_hght(pbot, I_PRES);
	mrh = mean_relhum(&ix1, pbot, ptop);
	mq = mean_mixratio(&ix1, pbot, ptop);
	mo = mean_omeg(&ix1, pbot, ptop) * 1000;
	pw = precip_water(&ix1, pbot, ptop);
	mopw = (mo * pw) * mrh / 100;

	//added for cave
	winterInfo->mopw = mopw;

	// setcliprgn(1,1,xwdth, xhght);

	/* ----- Draw box around parameter area ----- */
	//setlinestyle(1,1);
	/*txtlin = skv.tlx + 731;*/
	//if (display_mode_right == DISPLAY_WINTER_RIGHT)
	//	{txtlin = skv.tlx + 1066;}
	//    if (display_mode_left == DISPLAY_WINTER_LEFT)
	//           {txtlin = skv.tlx + 712;}
	//    if (display_mode_left == DISPLAY_WINTER_LEFT && display_mode_right == DISPLAY_WINTER_RIGHT)
	//             {
	//            setcolor(5);
	//             set_font(6);
	//            sprintf( st, "Please choose another inset");
	//            outgtext ( st, txtlin +394, txtrow + 115);
	//	}
	// txtrow = skv.bry + 20;
	//  setcolor(0);
	//  rectangle(1, txtlin, txtrow, txtlin + 350, txtrow + 250);
	//  setcolor(31);
	//  rectangle(0, txtlin, txtrow, txtlin + 350, txtrow + 250);

	/* ----- Dendritic Zone Data ----- */
	//    setcolor(5);
	// set_font(4);
	//  txtlin += 5;
	//  txtrow += 11;
	//  strcpy(st, " * * * DENDRITIC GROWTH ZONE (-12 to -17C) * * *");
	//ix1 = (350 - getgtextextent(st))/2;
	//     outgtext(st, txtlin + ix1 - 5, txtrow);

	//    txtrow += 15;
	//if (mopw < -.1) setcolor(13); else setcolor(31);
	//sprintf(st, "OPRH (Omega*PW*RH):  %s", qc2(mopw, "", 2));
	sprintf(winterInfo->oprh, "OPRH (Omega*PW*RH):  %s", qc2(mopw, "", 2));
	//ix1 = (350 - getgtextextent(st))/2;
	//    outgtext(st, txtlin + ix1 - 5, txtrow);

	//  set_font(4);
	//setcolor(31);
	//    txtrow += 20;

	//strcpy(st, "Layer Depth:");
	//  outgtext(st, txtlin, txtrow);
	//sprintf( st, "%.0f ft (%.0f - %.0f ft msl)", mtof(htop-hbot), mtof(hbot), mtof(htop));
	sprintf( winterInfo->layerDepth, "Layer Depth:     %.0f ft (%.0f - %.0f ft msl)", mtof(htop-hbot), mtof(hbot), mtof(htop));
	//    outgtext(st, txtlin+100, txtrow);

	//     txtrow += 14;
	//strcpy(st, "Mean Layer RH:");
	//    outgtext(st, txtlin, txtrow);
	//   strcpy( st, qc2(mrh, " %", 0));
	strcpy( winterInfo->meanLayerRh, "Mean Layer RH:     ");
	strcat( winterInfo->meanLayerRh, qc2(mrh, " %", 0));
	//     disp_param( st, txtlin + 150, txtrow);

	//strcpy(st, "Mean Layer MixRat:");
	//    outgtext(st, txtlin+165, txtrow);
	//   strcpy( st, qc2(mq, " g/kg", 1));
	strcpy( winterInfo->meanLayerMixRat,"Mean Layer MixRat:     ");
	strcat( winterInfo->meanLayerMixRat, qc2(mq, " g/kg", 1));
		//   disp_param( st, txtlin + 315, txtrow);

	///    txtrow += 14;
	//strcpy(st, "Mean Layer PW:");
	//   outgtext(st, txtlin, txtrow);
	//   strcpy( st, qc2(pw, " in.", 2));
	strcpy( winterInfo->meanLayerPw, "Mean Layer PW:     ");
	strcat( winterInfo->meanLayerPw, qc2(pw, " in.", 2));
		//   disp_param( st, txtlin + 150, txtrow);

	//strcpy(st, "Mean Layer Omega:");
	//    outgtext(st, txtlin+165, txtrow);
	//    strcpy( st, qc2(mo, " ub/s", 0));
	strcpy( winterInfo->meanLayerOmega, "Mean Layer Omega:     ");
	strcat( winterInfo->meanLayerOmega, qc2(mo, " ub/s", 0));
	//    disp_param( st, txtlin + 315, txtrow);

	// txtrow += 14;
	// moveto(txtlin, txtrow);
	//   lineto(txtlin+320, txtrow);
	//  moveto(txtlin+160, txtrow);
	//  lineto(txtlin+160, txtrow-28);

	/* ----- Initial Phase of Precip ----- */
	//    txtrow += 8;
	//    set_font(4);
	//  setcolor(31);
	strcpy(st, init_phase(&ix1, &phase));

	ptype1.init_phase = phase; //moved from best_guess_ptype() (below) to avoid calling init_phase twice
	ptype1.init_temp = i_temp(ix1, I_PRES);
	ptype1.init_lvl = ix1;


	if (ix1>100)
	        {
		sprintf( winterInfo->initPhase, "Initial Phase:    %s from: %.0fmb (%.0f ft msl ; %.1f C)", st, ix1, mtof(i_hght(ix1, I_PRES)), i_temp(ix1, I_PRES));

	//         sprintf( st1, "Initial Phase:    %s from: %.0fmb (%.0f ft msl ; %.1f C)", st, ix1, mtof(i_hght(ix1, I_PRES)), i_temp(ix1, I_PRES));
	//         outgtext( st1, txtlin, txtrow);
	//printf( "%s\n", st1);
	         }
	  else
	          {
		  sprintf( winterInfo->initPhase, "Initial Phase:  No Precipitation layers found.");
	//          sprintf( st, "Initial Phase:  No Precipitation layers found.");
	//          outgtext( st, txtlin, txtrow);
	          }

	//   txtrow += 15;
	//   moveto(txtlin, txtrow);
	//   lineto(txtlin+320, txtrow);

	/* ----- Temperature Pos/Neg Areas ----- */
	posneg_temperature(-1, &pose, &nege, &ptop, &pbot);
	ptype1.tpos = pose; //moved from best_guess_ptype() (below) to avoid calling posneg_temperature twice
	ptype1.tneg = nege;
	//txtrow += 8;
	// set_font(4);
	//setcolor(31);
	//strcpy(st, "TEMPERATURE PROFILE");
	//outgtext(st, txtlin, txtrow);
	//     txtrow += 14;

	if ((pose > 0) && (nege < 0)) {
		sprintf( winterInfo->tempProfile1, "Pos= %.0f J/kg    Neg=%.0f J/kg", pose, nege); //Added for Cave
		//sprintf( st, "Pos= %.0f J/kg    Neg=%.0f J/kg", pose, nege);
		//outgtext(st, txtlin, txtrow);
		//txtrow += 14;
		ix1 = mtof(i_hght(ptop, I_PRES)-i_hght(pbot, I_PRES));
		sprintf(winterInfo->tempProfile2, "Melt Lyr:  %3.0f - %3.0fmb (%.0f ft)", ptop, pbot, ix1); //Added for Cave
		//sprintf(st, "Melt Lyr:  %3.0f - %3.0fmb (%.0f ft)", ptop, pbot, ix1);
		//outgtext(st, txtlin, txtrow);
		// txtrow += 14;
		ix2 = sndg[sfc()][pIndex];
		ix1 = mtof(i_hght(pbot, I_PRES)-i_hght(ix2, I_PRES));
		sprintf(winterInfo->tempProfile3, "Frz Lyr: %3.0f - %4.0fmb (%.0f ft)", pbot, ix2, ix1); //Added for Cave
		//sprintf(st, "Frz Lyr: %3.0f - %4.0fmb (%.0f ft)", pbot, ix2, ix1);
		//outgtext(st, txtlin, txtrow);
	}
	else {
		//txtrow += 14;
		strcpy(winterInfo->tempProfile1, ""); //Added for Cave
		strcpy(winterInfo->tempProfile2, "Warm/Cold layers not found.\n"); //Added for Cave
		strcpy(winterInfo->tempProfile3, ""); //Added for Cave
		// outgtext("Warm/Cold layers not found.", txtlin, txtrow);
		// txtrow += 14;
	}

	/* ----- WetBulb Pos/Neg Areas ----- */
	posneg_wetbulb(-1, &pose, &nege, &ptop, &pbot);
	//txtrow -= 42;
	//txtlin += 165;
	//set_font(4);
	//setcolor(31);
	//strcpy(st, "WETBULB PROFILE");
	//outgtext(st, txtlin, txtrow);
	//txtrow += 14;

	if ((pose > 0) && (nege < 0)) {
		sprintf( winterInfo->wetbulbProfile1, "Pos= %.0f J/kg    Neg=%.0f J/kg", pose, nege);
		//sprintf( st, "Pos= %.0f J/kg    Neg=%.0f J/kg", pose, nege);
		//outgtext(st, txtlin, txtrow);
		//txtrow += 14;
		ix1 = mtof(i_hght(ptop, I_PRES)-i_hght(pbot, I_PRES));
		sprintf( winterInfo->wetbulbProfile2, "Melt Lyr:  %3.0f - %3.0fmb (%.0f ft)", ptop, pbot, ix1);
		//sprintf(st, "Melt Lyr:  %3.0f - %3.0fmb (%.0f ft)", ptop, pbot, ix1);
		//outgtext(st, txtlin, txtrow);
		//txtrow += 14;
		ix2 = sndg[sfc()][pIndex];
		ix1 = mtof(i_hght(pbot, I_PRES)-i_hght(ix2, I_PRES));
		sprintf( winterInfo->wetbulbProfile3, "Frz Lyr: %3.0f - %4.0fmb (%.0f ft)", pbot, ix2, ix1);
		//sprintf(st, "Frz Lyr: %3.0f - %4.0fmb (%.0f ft)", pbot, ix2, ix1);
		//outgtext(st, txtlin, txtrow);
	}
	else {
		//txtrow += 14;
		//outgtext("Warm/Cold layers not found.", txtlin, txtrow);
		strcpy(winterInfo->wetbulbProfile1, ""); //Added for Cave
		strcpy(winterInfo->wetbulbProfile2, "Warm/Cold layers not found."); //Added for Cave
		strcpy(winterInfo->wetbulbProfile3, ""); //Added for Cave
		//txtrow += 14;
	}

	// txtrow += 14;
	//txtlin -= 165;
	// moveto(txtlin, txtrow);
	// lineto(txtlin+320, txtrow);
	// moveto(txtlin+160, txtrow);
	// lineto(txtlin+160, txtrow-56);

	/* ----- Best Guess Precip Type ----- */
	// txtrow += 8;
	// set_font(4);
	// setcolor(31);
	// strcpy(st, "* * * BEST GUESS PRECIP TYPE * * *");
	// ix1 = (350 - getgtextextent(st))/2;
	//outgtext(st, txtlin + ix1 - 5, txtrow);
	// txtrow += 18;

	//best_guess_ptype(txtlin, txtrow);
	//Chin: the following are derived from best_guess_ptype() of xwvid3.c

	//strcpy(st, init_phase(&ix1, &phase));
	//Chin: moved these lines up
	//ptype1.init_phase = phase;
	//ptype1.init_temp = i_temp(ix1, I_PRES);
	//ptype1.init_lvl = ix1;

	//posneg_temperature(-1, &pose, &nege, &ptop, &pbot);
	//Chin: moved these lines up
	//ptype1.tpos = pose;
	//ptype1.tneg = nege;

	tIndex = getParmIndex("TEMP");

	if (!sndg || tIndex == -1)
		ptype1.tsfc = RMISSD;
	else
		ptype1.tsfc = sndg[sfc()][tIndex];
	strcpy(winterInfo->bestGuess1, best_guess(ptype1)); //Cave
	//strcpy(pt, best_guess(ptype1));
	//     set_font(6);
	//     setcolor(31);
	//	sprintf(st, "%s", pt);
	//     ix1 = (350 - getgtextextent(st))/2;
	//     outgtext(st, txtlin + ix1 - 5, txtrow);

	//txtrow += 18;
	//     set_font(4);
	sprintf(winterInfo->bestGuess2, "Based on sfc temperature of %.1f F.", ctof(ptype1.tsfc));
	//sprintf(st, "Based on sfc temperature of %.1f F.", ctof(ptype1.tsfc));
	//     ix1 = (350 - getgtextextent(st))/2;
	//    outgtext(st, txtlin + ix1 - 5, txtrow);

}
void getFireInfo(FireInfoStr * fireInfo)
/*************************************************************
 * Chin Note: this function is derived from show_fire() of xwvid3.c
 * of BigNsharp by Rich Thompson SPC OUN
 * Chin: Rewrite code to get all computed parameters/string for
 * CAVE.
 * All original BigNsharp plotting are removed.
 *************************************************************/
{
	float ix1, ix2, ix3, ix4, pres,  h2, p1, p2, sfctemp, sfcdwpt, sfcpres, sfcrh;
	short  oldlplchoice, pIndex, zIndex, tIndex;
	Parcel pcl;

	oldlplchoice = lplvals.flag;
	tIndex = getParmIndex("TEMP");
	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");


	define_parcel(1, 0);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	p1 = sndg[sfc()][pIndex];
	p2 = sndg[sfc()][pIndex];
	relh(-1, &ix3);
	sfcrh = ix3;


	if (ix3 > 35) fireInfo->sfcRhColor =8;
	else if (ix3 > 30) fireInfo->sfcRhColor =18;
	else  if (ix3 > 20) fireInfo->sfcRhColor =31;
	else if (ix3 > 15) fireInfo->sfcRhColor =19;
	else if (ix3 > 10) fireInfo->sfcRhColor =2;
	else if (ix3 >=  0) fireInfo->sfcRhColor =7;
	else  fireInfo->sfcRhColor =31;

	sprintf( fireInfo->sfcRh, "SFC RH  = %s", qc2(ix3, "%", 0));
	mean_wind( p1, p2, &ix1, &ix2, &ix3, &ix4);
	if (ix3 < 0)
		strcpy( fireInfo->sfc, "SFC = M");
	else
		sprintf( fireInfo->sfc, "SFC = %4.0f/%.0f", ix3, ix4);

	h2 = 1000;
	sprintf( fireInfo->zeroOneKmRh, "0-1 km RH  = %s", qc2( mean_relhum(&ix3, p1, i_pres(msl(h2))), "%", 0 ));
	mean_wind( p1, i_pres(msl(h2)), &ix1, &ix2, &ix3, &ix4);
	if (ix3 < 0)
		strcpy( fireInfo->zeroOneKmMean, "0-1 km mean = M");
	else
		sprintf( fireInfo->zeroOneKmMean, "0-1 km mean = %4.0f/%.0f", ix3, ix4);

	pbl_top(&p2);
	sprintf( fireInfo->blMeanRh, "BL mean RH  = %s", qc2( mean_relhum(&ix3, p1, p2), "%", 0 ));

	mean_wind( p1, p2, &ix1, &ix2, &ix3, &ix4);
	if (ix3 < 0)
		strcpy( fireInfo->blMean, "BL mean = M");
	else
		sprintf( fireInfo->blMean, "BL mean = %4.0f/%.0f", ix3, ix4);

	define_parcel(3, 500);
	parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	precip_water(&ix1, -1, -1);
	if ((ix1 < 0.5) && (pcl.bplus > 50) && (sfcrh < 35)){
		fireInfo->pwColor = 2;
	}
	sprintf( fireInfo->pw, "PW  = %s", qc2( ix1, "in", 2 ));
	max_wind(&ix1, &ix2, &ix3, -1, p2);
	if (ix3 < 0)
		strcpy( fireInfo->blMax, "BL max = M");
	else{
		if (ix3 > 50) fireInfo->blMaxColor =7;
		else if (ix3 > 40) fireInfo->blMaxColor =2;
		else if (ix3 > 30) fireInfo->blMaxColor =19;
		else if (ix3 > 20) fireInfo->blMaxColor =31;
		else if (ix3 > 10) fireInfo->blMaxColor =18;
		else fireInfo->blMaxColor =8;
		sprintf( fireInfo->blMax, "BL max = %4.0f/%.0f", ix2, ix3);
	}
	ix3 = fosberg(&ix4);
	if (ix3 < 0)
		strcpy( fireInfo->fosberg, "Fosberg FWI = M");
	else {

		if (ix3 >= 70) fireInfo->fosbergColor =7;
		else if (ix3 >= 60) fireInfo->fosbergColor =2;
		else if (ix3 >= 50) fireInfo->fosbergColor =19;
		else if (ix3 >= 40) fireInfo->fosbergColor =31;
		else if (ix3 >= 30) fireInfo->fosbergColor =18;
		else fireInfo->fosbergColor =8;
		sprintf( fireInfo->fosberg, "Fosberg FWI = %4.0f", ix3);
	}


	/* parcel setback */
	if (oldlplchoice == 1)
		pres = 0;
	else if (oldlplchoice == 2)
		pres = 0;
	else if (oldlplchoice == 3)
		pres = mu_layer;
	else if (oldlplchoice == 4)
		pres = mml_layer;
	else if (oldlplchoice == 5)
		pres = user_level;
	else if (oldlplchoice == 6)
		pres = mu_layer;
	define_parcel(oldlplchoice, pres);

}
void getSarsInfo(SarsInfoStr * sarsInfo)
/*************************************************************
 * Chin Note: this function is derived from show_sars() of xwvid3.c
 * of BigNsharp by John Hart  NSSFC KCMO
 * Chin: Rewrite code to get all computed parameters/string for
 * CAVE.
 * All original BigNsharp gui functions are removed.
 *************************************************************/
{
	float ix1, ix2, ix3, ix4, sfctemp, sfcdwpt, sfcpres, j1, j2;
	float nv_cape, nv_cinh, nv_cap, pres, ptop, pbot, mucape, mumixr, avsize, matches2;
	float lr75, shr3, shr6, shr9, fzlh, mucinh, ship, esi2, t500;
	float srh3, matches, p1, p2, haillist[15], suplist[15], oldlplpres;
	float mucp, mlcp, mllcl, srh1, shr3k, shr6k, shr9k;
	short txtlin, txtrow, oldlplchoice, pIndex, zIndex, tIndex, trow2, i;
	short tdIndex, nsndgs, trx, j, temp_mark, y, totalsndgs;
	char st[100], st1[20], st2[20], sndglist[15][15], tempStr[16];
	char tortags[3][10] = { "NONTOR", "WEAKTOR", "SIGTOR" };
	Parcel pcl;
	Parcel pcl2;

	//initialize SarsInfoStr
	sarsInfo->numHailstr =SARS_STRING_LINES;
	sarsInfo->numsupcellstr=SARS_STRING_LINES;
	//memset(sarsInfo->hailStr,'\0', 600);

	tIndex = getParmIndex("TEMP");
	pIndex = getParmIndex("PRES");
	zIndex = getParmIndex("HGHT");
	tdIndex = getParmIndex("DWPT");

	oldlplchoice = lplvals.flag;
	oldlplpres = lplvals.pres;


	define_parcel(4,100);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	mlcp = pcl.bplus;
	mllcl = agl(i_hght(pcl.lclpres, I_PRES));

	define_parcel(3, mu_layer);
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	/* ----- Calculate Parcel Data ----- */
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
	mucp = pcl.bplus;
	mumixr = mixratio(lplvals.pres, lplvals.dwpt);


	/* ----- Titles ----- */

	/* Compute Hail Sars Data */
	t500 =  i_temp(500, I_PRES);
	lr75 = lapse_rate(&ix1, 700, 500);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); shr6 = kt_to_mps(ix4);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4); shr9 = kt_to_mps(ix4);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4); shr3 = kt_to_mps(ix4);

	if (agl(i_hght(p_bot, I_PRES)) > 0){
		wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.25)), &ix1, &ix2, &ix3, &ix4);  shr3 = kt_to_mps(ix4);
		wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.5)), &ix1, &ix2, &ix3, &ix4);  shr6 = kt_to_mps(ix4);
		wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.75)), &ix1, &ix2, &ix3, &ix4);  shr9 = kt_to_mps(ix4);
	}
	fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
	ship = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcp);
	srh3 = helicity(0, 3000, st_dir, st_spd, &ix2, &ix3);
	srh1 = helicity(0, 1000, st_dir, st_spd, &ix2, &ix3);

	if (agl(i_hght(p_bot, I_PRES)) > 0){
		srh3 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
		srh1 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
	}

	cavesars(&mumixr, &mucp, &t500, &lr75, &shr6, &shr9, &shr3, &ship, &srh3, &nsndgs, &matches, &p1, &avsize, &matches2, sndglist, &haillist, &sars_filename,&totalsndgs);
	// hail match strings...on right side of graphs
	if (nsndgs>0)
	{
		if (nsndgs > 10) nsndgs = 10;

		for (i=0;i<10;i++)
		{
			if(i < nsndgs){
				j = haillist[i];
				if (j<1) {sarsInfo->hailStrColor[i]=8;}
				else if (j<2) {sarsInfo->hailStrColor[i]=(18);}
				else sarsInfo->hailStrColor[i]=(5);
				//Chin note: sndglist contains matched date and stn from nlist.txt's first column
				//           haillist contains matched  hail size from nlist.txt's 3rd column
				memset(tempStr,'\0',16);
				strncpy(tempStr, sndglist[i],15);
				//printf("Before:tempSTr::%s hailsize=%.2f hairSTr::%s\n",tempStr,haillist[i],sarsInfo->hailStr[i]);
				sprintf( sarsInfo->hailStr[i], "%s %.2f%c", tempStr, haillist[i], '\0');
				//printf("After:hairSTr::%s\n",sarsInfo->hailStr[i]);
			}
			else {
				memset(sarsInfo->hailStr[i], '\0', 1);
			}
		}
		//sarsInfo->numHailstr =  nsndgs;
	}
	else
	{
		for (i=0;i<10;i++)
		{
			memset(sarsInfo->hailStr[i], '\0', 1);
		}
		sarsInfo->hailStrColor[5]=31;
		sprintf( sarsInfo->hailStr[5], "No Quality HAIL Matches");
		//sarsInfo->numHailstr =  1;
	}

	//----- Plot Hail SARS Result -----
	p2 = 100.0 - p1;
	if (matches>0){
		sarsInfo->hailStrColor[10]=(31);
		sarsInfo->hailStrColor[11]=(31);
		strcpy(st1, "Non-sig Hail");
	}
	else if (p1>=50){
		sarsInfo->hailStrColor[10]= 12;
		sarsInfo->hailStrColor[11]=12;
		strcpy(st1, "**SIG HAIL!**");
	}
	else {
		strcpy(st1, "No Matches");
		sarsInfo->hailStrColor[10]=(31);
		sarsInfo->hailStrColor[11]=(31);
	}
	if ((p1>=50) || (p1<50 && matches>0))
	{
		sprintf( sarsInfo->hailStr[10], "(%.0f matches out of %d sndgs)", matches,totalsndgs);
	}
	else {
		sprintf( sarsInfo->hailStr[10],"");
	}

	sprintf( sarsInfo->hailStr[11], "SARS:  %s  (%.0f%s SIG)", st1, p1, "%");

	//Supercell match plot on left side of graph
	//fromhere....

	wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); shr6k = ix4;
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4); shr9k = ix4;
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4); shr3k = ix4;

	cavespnsharp(&mlcp, &mllcl, &t500, &lr75, &shr6k, &srh1, &nsndgs, &matches, &p1, sndglist, &suplist, &sup_filename, &shr3k, &shr9k, &srh3, &totalsndgs);

	/* Chin TBD::: bignsharpV2013Jun12
	 * for (i=0; i < 15; i++) sndglist[i][14] = '\0';
        printf( "%d High Quality SUPERCELL Matches were found.\n", nsndgs);
	// modified loop and printf statement to properly display supercell match type - RLT 4/24/12
        for (i=0;i<nsndgs;i++) {
		j = suplist[i];
		printf( "SUPERCELL match = %s %s\n", sndglist[i], tortags[j]);
		}
        printf( "%.0f Total matches were found.\n", matches);
        printf( "%.0f Percent were TOR.\n", p1);
	 */
	// ----- Supercell SARS matches -----
	if (nsndgs>0)
	{
		if (nsndgs > 10) nsndgs = 10;
		for (i=0;i<10;i++)
		{
			if(i < nsndgs){
				// R/J's comments : I am now passing values in suplist (0-3).  I assigned an array of strings called tortags to plot the right stuff on the screen.
				j = suplist[i];
				if (j<1) {sarsInfo->supcellStrColor[i]=(18);}
				else if (j<2) {sarsInfo->supcellStrColor[i]=(6);}
				else sarsInfo->supcellStrColor[i]=(2);
				memset(tempStr,'\0',16);
				strncpy(tempStr, sndglist[i],15);
				sprintf(sarsInfo->supcellStr[i], "%s  %s%c", tempStr,tortags[j],'\0');
			}
			else {
				memset(sarsInfo->supcellStr[i], '\0', 1);
			}
		}

	}
	else
	{
		for (i=0;i<10;i++)
		{
			memset(sarsInfo->supcellStr[i], '\0', 1);
		}
		sarsInfo->supcellStrColor[5]= 31;
		sprintf( sarsInfo->supcellStr[5], "No Quality SUPERCELL Matches");

	}



	// ----- Plot Supercell SARS Result -----
	p2 = 100.0 - p1;
	sarsInfo->supcellStrColor[10]=(31);
	sarsInfo->supcellStrColor[11]=(31);
	strcpy(st1, "No Matches");
	define_parcel(1,0);
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	if (pcl.bplus >= 100){
		if (matches>0){
			strcpy(st1, "NONTOR");
		}
		if (p1>50){
			sarsInfo->supcellStrColor[10]=12;
			sarsInfo->supcellStrColor[11]=12;
			strcpy(st1, "**TOR!**");
		}
		if ((p1>50) || (p1<=50 && matches>0)){
			sprintf( sarsInfo->supcellStr[10], "(%.0f matches out of %d sndgs)", matches,totalsndgs);
		}
		else {
			sprintf( sarsInfo->supcellStr[10],"");
		}

		sprintf( sarsInfo->supcellStr[11], "SARS:  %s  (%.0f%s TOR)", st1, p1, "%");
	}

	// ----- Set Parcel Back -----
	if (oldlplchoice == 1)
		pres = 0;
	else if (oldlplchoice == 2)
		pres = 0;
	else if (oldlplchoice == 3)
		pres = mu_layer;
	else if (oldlplchoice == 4)
		pres = mml_layer;
	else if (oldlplchoice == 5)
		pres = user_level;
	else if (oldlplchoice == 6)
		pres = mu_layer;
	define_parcel(oldlplchoice, pres);


}
/*************************************************************
 * Chin Note: this function is derived from show_skewtpage1()
 * plus show_hail_new() of xwvid3.c
 * of BigNsharp by John Hart  NSSFC KCMO
 * Chin: Rewrite code to get all computed parameters/string for
 * CAVE.
 * All original BigNsharp gui functions are removed.
 *************************************************************/
void getHailInfo(){
	float hvars[30], h2[100];
	float ix1, ix2, ix3, ix4,  mumixr,esicat;
	float T0, Td0, el, pbot, ptop, base, depth, effdep, ebs;
	short tIndex, tdIndex, pIndex,zIndex;
	float sfctemp, sfcdwpt, sfcpres, j1, j2;
	float nv_cape, nv_cinh, nv_cap, pres,  mucape, mlcape;
	float lr75, shr3, shr6, shr9, fzlh, mucinh, ship, esi2, t500;
	float srh3, matches, matches2, avsize, p1, haillist[15];
	short txtlin, txtrow, oldlplchoice, trow2, i, j;
	short nsndgs, trx, temp_mark, y;
	char st[100], st1[20], sndglist[15][15];
	Parcel pcl;
	Parcel pcl2;

	// Chin: use memset() instead
	memset(h2, 0, sizeof(h2));
	memset(hvars,0, sizeof(hvars));
	//was::for (i=0;i<100;i++) {h2[i] = 0.0;}
	//was::for (i=0;i<30;i++) {hvars[i] = 0.0;}

	/* Compute Effective Vertical Shear.  Default to 6km if not available */
	ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");
	tdIndex = getParmIndex("DWPT");
	zIndex = getParmIndex("HGHT");
	pbot = sndg[sfc()][pIndex];
	el = 12000.0;
	if (pcl.bplus >= 100) {
		el = agl(i_hght(pcl.elpres, I_PRES));
		/* 24 Mar 2008 */
		/*  effective_inflow_layer(100, -250, &pbot, &ptop);*/
	}
	base = agl(i_hght(p_bot, I_PRES));
	depth = (el - base);
	effdep = base + (depth * 0.5);
	wind_shear(p_bot, i_pres(msl(effdep)), &ix1, &ix2, &ix3, &ix4);
	ebs = kt_to_mps(ix4)/effdep;
	//printf("Shear = %.1f kt    %.1f mps\nEBS = %.6f\nDepth = %.1f m\n", ix4, kt_to_mps(ix4), ebs, effdep);
	T0 = sndg[sfc()][tIndex];
	Td0 = sndg[sfc()][tdIndex];
	hailcast1(&T0, &Td0, &ebs, &hvars, &mumixr, &esicat);

	h2[0]=1;
	h2[1]=0;
	for (i=0;i<=30;i++)
	{
		printf( "HVARS[%d] = %f\n", i, hvars[i]);
		h2[i+2] = hvars[i];
	}
	//CHin Note::: Above code are from show_skewtpage1(). It does some parameters setting before calling show_hail_new().
	//Chin note::: From here down are rewriting code of show_hail_new(&h2);

	oldlplchoice = lplvals.flag;

	define_parcel(4, 100);
	mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);

	define_parcel(3, mu_layer);
	sfctemp = lplvals.temp;
	sfcdwpt = lplvals.dwpt;
	sfcpres = lplvals.pres;

	//----- Calculate Parcel Data -----
	ix1 = parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);

	// ----- Hail Model Output -----
	// Chin:: resume work from here...
	set_font(4);
	setcolor(31);
	sprintf(st, "Hailcast1 --> (%.0f convecting)    T/Td= %.0fF/%.0fF    Storm Cat: %.0f of 4", h2[18], ctof(h2[2]), ctof(h2[3]),h2[25]);
	outgtext ( st, txtlin, txtrow );

	if (h2[24] >= 1.00 && h2[18] >= 1) setcolor(3);
	if (h2[24] >= 1.95) setcolor(2);

	txtrow += 15;
	sprintf(st, "Avg: %.1f in.     Max: %.1f in.     Min: %.1f in.     SIG =  %.0f     SVR =  %.0f      ", h2[19], h2[20],h2[21],h2[22], h2[23]);
	outgtext ( st, txtlin, txtrow );

	txtrow +=20;
	set_font(4);
	setcolor(31);
	if(h2[4] == 0) setcolor(31);
	sprintf(st, "Hailcast2 --> (%.0f convecting)    T/Td= %.0fF/%.0fF    Storm Cat: %.0f of 4", h2[4], ctof(h2[2]), ctof(h2[3]),h2[17]);
	outgtext ( st, txtlin, txtrow );

	if (h2[15] >= 1.00 && h2[4] >= 1) setcolor(3);
	if (h2[15] >= 1.95) setcolor(2);

	if(h2[4] == 0) h2[15] = 0;
	sprintf(st, "Avg: %.1f in.     Max: %.1f in.     Min: %.1f in.     SIG =  %.0f     SVR =  %.0f      ", h2[5], h2[6],h2[7],h2[8], h2[9]);
	txtrow += 14;
	outgtext ( st, txtlin, txtrow );



	txtrow += 15;
	setcolor(31);
	moveto(txtlin, txtrow);
	lineto(txtlin+340, txtrow);



	setcolor(31);
	set_font(6);
	if (h2[4] == 0 && h2[18] == 0) {
		sprintf(st, "No Convecting Members");
		txtrow += 6;
		ix1 = (350 - getgtextextent(st))/2;
		outgtext(st, txtlin + ix1 - 5, txtrow);
	}else{
		// If convecting members then...........
		txtrow +=4;
		if (h2[24] < 1.00) setcolor(31);
		if (h2[24] >= 1.00 && h2[18] >= 1)  setcolor(3);
		if (h2[24] >= 1.95)  setcolor(2);
		sprintf(st, "Hailcast1--->   %.1f", h2[24]);
		ix1 = (350 - getgtextextent(st))/2;
		outgtext(st, txtlin + ix1 - 85, txtrow);

		if (h2[15] < 1.00)  setcolor(31);
		if (h2[15] >= 1.00 && h2[4] >= 1)  setcolor(3);
		if (h2[15] >= 1.95)  setcolor(2);
		sprintf(st, "Hailcast2--->   %.1f",h2[15]);
		ix1 = (350 - getgtextextent(st))/2;
		outgtext(st, txtlin + ix1 + 70, txtrow);

	}

	txtrow += 18;
	setcolor(31);
	moveto(txtlin, txtrow);
	lineto(txtlin+340, txtrow);

	/* Compute SARS Data */
	define_parcel(4, 100);
	mlcape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	define_parcel(3, 400);
	mucape = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
	mumixr = mixratio(lplvals.pres, lplvals.dwpt);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(6000)), &ix1, &ix2, &ix3, &ix4); shr6 = kt_to_mps(ix4);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(9000)), &ix1, &ix2, &ix3, &ix4); shr9 = kt_to_mps(ix4);
	wind_shear(sndg[sfc()][pIndex], i_pres(msl(3000)), &ix1, &ix2, &ix3, &ix4); shr3 = kt_to_mps(ix4);

	if (agl(i_hght(p_bot, I_PRES)) > 0){
		wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.25)), &ix1, &ix2, &ix3, &ix4);  shr3 = kt_to_mps(ix4);
		wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.5)), &ix1, &ix2, &ix3, &ix4);  shr6 = kt_to_mps(ix4);
		wind_shear(p_bot, i_pres(msl(agl(i_hght(p_top, I_PRES))*0.75)), &ix1, &ix2, &ix3, &ix4);  shr9 = kt_to_mps(ix4);
	}

	t500 =  i_temp(500, I_PRES);
	lr75 = lapse_rate(&ix1, 700, 500);
	fzlh = mtof(agl(i_hght(temp_lvl(0, &ix1), I_PRES)));
	ship = sig_hail(pcl.bplus, mumixr, lr75, t500, kt_to_mps(shr6), fzlh, pcl.bminus, 0, 0, 25, mlcape);
	srh3 = helicity(0, 3000, st_dir, st_spd, &ix2, &ix3);
	if (agl(i_hght(p_bot, I_PRES)) > 0){
		srh3 = helicity(p_bot, p_top, st_dir, st_spd, &ix2, &ix3);
	}


	sars(&mumixr, &mucape, &t500, &lr75, &shr6, &shr9, &shr3, &ship, &srh3, &nsndgs, &matches, &p1, &avsize, &matches2, sndglist, &haillist, &sars_filename);

	for (i=0; i < 15; i++) sndglist[i][14] = '\0';
	printf( "%d High Quality HAIL Matches were found.\n", nsndgs);
	for (i=0;i<nsndgs;i++) { printf( "HAIL match = %s  %.2f\n", sndglist[i], haillist[i]); }
	printf( "%.0f Total matches were found.\n", matches);
	printf( "%.0f Percent were SIG HAIL.\n", p1);


	/* ----- SARS matches ----- */
	/*  SARS hail size */
	txtrow += 6;
	set_font(4);
	 setcolor(5);
	strcpy( st, "* * * SARS HAIL SIZE * * *" );
	ix1 = (350 - getgtextextent(st))/2;
	outgtext(st, txtlin + ix1 - 5, txtrow);

	txtrow += 15;
	set_font(6);
	if (matches2 == 0) {
		 setcolor(31);
		sprintf(st, "No Matches");
		ix1 = (350 - getgtextextent(st))/2;
		outgtext ( st, txtlin + ix1 - 5, txtrow );
	}
	if (matches2 == 1 || avsize <= 1.49)  setcolor(31);
	if (matches2 >= 2 && (avsize < 2.06 && avsize > 1.49))  setcolor(3);
	if (matches2 >= 2 && avsize >= 2.06)  setcolor(2);
	if (matches2 >= 1) {
		set_font(6);
		if (avsize <= 1.49) {
			sprintf(st, "Best guess from SARS = < 1 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if ((avsize > 1.49) && (avsize <= 1.68)) {
			sprintf(st, "Best guess from SARS = 1 - 1.5 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if ((avsize > 1.68) && (avsize <= 2.06)) {
			sprintf(st, "Best guess from SARS = 1.75 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if ((avsize > 2.06) && (avsize <= 2.39)) {
			sprintf(st, "Best guess from SARS = 2 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if ((avsize > 2.39) && (avsize <= 2.52)) {
			sprintf(st, "Best guess from SARS = 2.5 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if ((avsize > 2.52) && (avsize <= 2.56)) {
			sprintf(st, "Best guess from SARS = 2.75 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if ((avsize > 2.56) && (avsize <= 2.64)) {
			sprintf(st, "Best guess from SARS = 3 - 4 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		if (avsize > 2.64) {
			sprintf(st, "Best guess from SARS = > 4 inch");
			ix1 = (350 - getgtextextent(st))/2;
			outgtext ( st, txtlin + ix1 - 5, txtrow );
		}
		txtrow += 18;
		set_font(4);
		sprintf(st, "AVG size = %.2f (based on %.0f matches)", avsize, matches2);
		ix1 = (350 - getgtextextent(st))/2;
		outgtext(st, txtlin + ix1 - 5, txtrow);
	}

	txtrow += 18;
	 setcolor(31);
	moveto(txtlin, txtrow);
	lineto(txtlin+340, txtrow);
	if (matches2 > 0) {
		txtrow += 7;
		set_font(4);
		 setcolor(31);
		strcpy( st, "SARS output ranges for reported sizes (white)");
		ix1 = (350 - getgtextextent(st))/2;
		outgtext(st, txtlin + ix1 - 5, txtrow);

		txtrow += 18;
		/* SARS for reported < 1" hail */
		if (avsize <= 1.49) {
			 setcolor(31);
			set_font(6);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			set_font(4);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 56, txtrow - 5, txtlin + 91, txtrow + 60);
		}
		/* SARS for reported 1-1.5" hail */
		if ((avsize > 1.49) && (avsize <= 1.68)) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			set_font(6);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			set_font(4);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 91, txtrow - 5, txtlin + 126, txtrow + 60);
		}
		/* SARS for reported 1.75" hail */
		if ((avsize > 1.68) && (avsize <= 2.06)) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			set_font(6);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			set_font(4);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 126, txtrow - 5, txtlin + 161, txtrow + 60);
		}
		/* SARS for reported 2" hail */
		if ((avsize > 2.06) && (avsize <= 2.39)) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			set_font(6);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			set_font(4);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 161, txtrow - 5, txtlin + 196, txtrow + 60);
		}
		/* SARS for reported 2.5" hail */
		if ((avsize > 2.39) && (avsize <= 2.52)) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			set_font(6);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			set_font(4);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 196, txtrow - 5, txtlin + 231, txtrow + 60);
		}
		/* SARS for reported 2.75" hail */
		if ((avsize > 2.52) && (avsize <= 2.56)) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			set_font(6);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			set_font(4);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 231, txtrow - 5, txtlin + 266, txtrow + 60);
		}
		/* SARS for reported 3-4" hail */
		if ((avsize > 2.56) && (avsize <= 2.64)) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			set_font(6);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			set_font(4);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 266, txtrow - 5, txtlin + 301, txtrow + 60);
		}
		/* SARS for reported >4" hail */
		if (avsize > 2.64) {
			 setcolor(31);
			set_font(4);
			strcpy(st, "<1");
			outgtext(st, txtlin + 60, txtrow);
			strcpy(st, "1-1.5");
			outgtext(st, txtlin + 95, txtrow);
			strcpy(st, "1.75");
			outgtext(st, txtlin + 130, txtrow);
			strcpy(st, "2");
			outgtext(st, txtlin + 165, txtrow);
			strcpy(st, "2.5");
			outgtext(st, txtlin + 200, txtrow);
			strcpy(st, "2.75");
			outgtext(st, txtlin + 235, txtrow);
			strcpy(st, "3-4");
			outgtext(st, txtlin + 270, txtrow);
			set_font(6);
			strcpy(st, ">4");
			outgtext(st, txtlin + 305, txtrow);
			 setcolor(27);
			rectangle(0, txtlin + 301, txtrow - 5, txtlin + 336, txtrow + 60);
		}

		txtrow += 15;
		 setcolor(31);
		set_font(4);
		strcpy(st, "+1 STD");
		outgtext(st, txtlin, txtrow);
		 setcolor(27);
		strcpy(st, "1.9");
		outgtext(st, txtlin + 60, txtrow);
		strcpy(st, "2.0");
		outgtext(st, txtlin + 95, txtrow);
		strcpy(st, "2.3");
		outgtext(st, txtlin + 130, txtrow);
		strcpy(st, "2.8");
		outgtext(st, txtlin + 165, txtrow);
		strcpy(st, "2.9");
		outgtext(st, txtlin + 200, txtrow);
		strcpy(st, "3.0");
		outgtext(st, txtlin + 235, txtrow);
		strcpy(st, "3.0");
		outgtext(st, txtlin + 270, txtrow);
		strcpy(st, "3.0");
		outgtext(st, txtlin + 305, txtrow);

		txtrow += 15;
		 setcolor(31);
		strcpy(st, "AVG");
		outgtext(st, txtlin, txtrow);
		 setcolor(27);
		strcpy(st, "1.5");
		outgtext(st, txtlin + 60, txtrow);
		strcpy(st, "1.5");
		outgtext(st, txtlin + 95, txtrow);
		strcpy(st, "1.8");
		outgtext(st, txtlin + 130, txtrow);
		strcpy(st, "2.3");
		outgtext(st, txtlin + 165, txtrow);
		strcpy(st, "2.5");
		outgtext(st, txtlin + 200, txtrow);
		strcpy(st, "2.5");
		outgtext(st, txtlin + 235, txtrow);
		strcpy(st, "2.6");
		outgtext(st, txtlin + 270, txtrow);
		strcpy(st, "2.7");
		outgtext(st, txtlin + 305, txtrow);

		txtrow += 15;
		 setcolor(31);
		strcpy(st, "-1 STD");
		outgtext(st, txtlin, txtrow);
		 setcolor(27);
		strcpy(st, "1.1");
		outgtext(st, txtlin + 60, txtrow);
		strcpy(st, "1.1");
		outgtext(st, txtlin + 95, txtrow);
		strcpy(st, "1.3");
		outgtext(st, txtlin + 130, txtrow);
		strcpy(st, "1.7");
		outgtext(st, txtlin + 165, txtrow);
		strcpy(st, "2.1");
		outgtext(st, txtlin + 200, txtrow);
		strcpy(st, "2.1");
		outgtext(st, txtlin + 235, txtrow);
		strcpy(st, "2.2");
		outgtext(st, txtlin + 270, txtrow);
		strcpy(st, "2.4");
		outgtext(st, txtlin + 305, txtrow);
	}

	/* ----- Set Parcel Back ----- */

	if (oldlplchoice == 1)
		pres = 0;
	else if (oldlplchoice == 2)
		pres = 0;
	else if (oldlplchoice == 3)
		pres = mu_layer;
	else if (oldlplchoice == 4)
		pres = mml_layer;
	else if (oldlplchoice == 5)
		pres = user_level;
	else if (oldlplchoice == 6)
		pres = mu_layer;
	define_parcel(oldlplchoice, pres);

}

/*
void getStpStats(StpStatsStr * stpStr)
/*************************************************************
 * Chin Note: this function is derived from show_stp_stats() of xwvid3.c
 * of BigNsharp by Rich Thompson SPC OUN
 * Chin: Rewrite code to get all computed parameters/string for
 * CAVE.
 * All original BigNsharp gui are removed.
 *************************************************************
{
float s10th, s25th, s50th, s75th, s90th, w10th, w25th, w50th, w75th, w90th;
float n10th, n25th, n50th, n75th, n90th, stpc, maxval, ix1, pres;
short tlx, tly, oldlplchoice, pIndex, zIndex, tIndex, trow2, i, y, hash;
char st[100];
Parcel pcl;
// plot sounding value of STPC
// max plotted STPC value will be 11
ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
stpc = sigtorn_cin(st_dir, st_spd);
if (stpc > 11) stpc = 11;
y = (short)(240 - (stpc * 20));
// set line color to match color coding of composite parameter inset
if (stpc >= 5.95) stpStr->stpcColor =7;
else if (stpc >= 3.95) stpStr->stpcColor =2;
else if (stpc >= 1.95) stpStr->stpcColor =19;
else if (stpc >= .45) stpStr->stpcColor =31;
else stpStr->stpcColor =8;
stpStr->stpCin = stpc;
//
// * the following from prob_sigt_mlcape() of xwvid3.c
//*
oldlplchoice = lplvals.flag;

// lift ML parcel
define_parcel(4, 100);
ix1 = parcel( -1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
mlcape = pcl.bplus;

if (mlcape >= 4500){ psigt_mlcape=.31;
setcolor(19);
}
else
        if (mlcape >= 3500 && mlcape < 4500){ psigt_mlcape=.23;
	setcolor(19);
	}
else
        if (mlcape >= 2500 && mlcape < 3500){ psigt_mlcape=.25;
	setcolor(19);
	}
else
        if (mlcape >= 1500 && mlcape < 2500){ psigt_mlcape=.14;
	setcolor(31);
	}
else
        if (mlcape < 1500 && mlcape >= 50){ psigt_mlcape=.08;
	setcolor(18);
	}
else
if (mlcape < 50){ psigt_mlcape=0.0;
	setcolor(8);
	}

sprintf( stpStr->mlcape, "based on CAPE: %.2f", psigt_mlcape);
disp_param( st, tlx+335, tly+45);
setcolor(31);
set_font(4);
sprintf( st, "based on CAPE: ");
outgtext( st, tlx+208, tly+45);

// ----- Set Parcel Back -----

if (oldlplchoice == 1)
  pres = 0;
else if (oldlplchoice == 2)
  pres = 0;
else if (oldlplchoice == 3)
  pres = mu_layer;
else if (oldlplchoice == 4)
  pres = mml_layer;
else if (oldlplchoice == 5)
  pres = user_level;
else if (oldlplchoice == 6)
  pres = mu_layer;
define_parcel(oldlplchoice, pres);

}
*/


