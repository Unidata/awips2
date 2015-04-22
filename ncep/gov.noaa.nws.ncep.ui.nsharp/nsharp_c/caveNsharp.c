#include "gui.h"
#include "sharp95.h"

struct sndg_struct myStaticSndgStruct;

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
} SndgParms;

typedef struct stormSlinkyStr
{
	int size;
	float tottim;
	float angl;
	float tsuv[200][2];
} StormSlinkyStr;

SndgParms *  populateSndgData(SndgParms snDataArray[], int arraySize, float winDir, float winSpd) {
	int i;
	/*if (sndgp)
		free(sndgp);
	sndgs[0] = (struct sndg_struct *)malloc(sizeof(struct sndg_struct));
	sndgp = sndgs[0];*/

	sndgs[0] = &myStaticSndgStruct;
	sndgp = sndgs[0];
    sndgp->numlev = arraySize;
    for (i =0; i< sndgp->numlev; i++){
    	sndgp->sndg[i].drct = snDataArray[i].drct;
    	sndgp->sndg[i].sped = snDataArray[i].sped;
    	sndgp->sndg[i].pres = snDataArray[i].pres;
    	sndgp->sndg[i].hght = snDataArray[i].hght;
    	sndgp->sndg[i].dwpt = snDataArray[i].dwpt;
    	sndgp->sndg[i].temp  = snDataArray[i].temp;
    	sndgp->sndg[i].omega = snDataArray[i].omega;
    }
    //Chin, I dont need this....copy_sndg();

    sndgp->st_dir = winDir;
    sndgp->st_spd = winSpd;
    return sndgp;
}
void showSndgData(){
	int i;
	printf("Snd layer num = %d\n", sndgp->numlev);
	for (i =0; i< sndgp->numlev; i++){
		printf(" %d: Pre %.4f Hgt %.4f Temp %.4f Dew %.4f Wspd %.4f Wdir %.2f\n",
	    	i,sndgp->sndg[i].pres,
	    	sndgp->sndg[i].hght,
	    	 sndgp->sndg[i].temp,sndgp->sndg[i].dwpt,
	    	sndgp->sndg[i].sped, sndgp->sndg[i].drct);
	}
}

void populateSndgTestData(void) {
	int i;
	sndgs[0] = (struct sndg_struct *)malloc(sizeof(struct sndg_struct));

	sndgp = sndgs[0];
    	sndgp->numlev = 10;
    	for (i =0; i< sndgp->numlev; i++){
    		sndgp->sndg[i].drct = i + 10;
    		sndgp->sndg[i].sped = i + 100;
    		sndgp->sndg[i].pres = i * 100 + 100;
    		sndgp->sndg[i].hght = (10-i) * 1000 + 100;
    		sndgp->sndg[i].dwpt = -50 + (i * 6);
    		sndgp->sndg[i].temp  = -80 + (i * 6);
    		sndgp->sndg[i].omega = -999;
    	}
    	copy_sndg();
}
void get_lpvaluesData(struct _lplvalues *pParcel){
	pParcel->temp = sndgp->lplvals.temp;
	pParcel->dwpt = sndgp->lplvals.dwpt;
	pParcel->pres = sndgp->lplvals.pres;
	pParcel->flag = sndgp->lplvals.flag;
	strcpy(pParcel->desc, sndgp->lplvals.desc);
	//printf("desc %s\n", pParcel->desc);
}

/*
 * This function is used to get surface pressure and temp directly from sndgp. As some other api need
 * them as input parameters and legacy code API does not provide such.
 */
void get_surface(float* pressure, float * temp, float * dewpt){
	*pressure = sndgp->sndg[sfc()].pres;
	*temp = sndgp->sndg[sfc()].temp;
	*dewpt = sndgp->sndg[sfc()].dwpt;
}
void get_surfaceWind(float* windSp, float * windDir){
	*windSp = sndgp->sndg[sfc()].sped;
	*windDir = sndgp->sndg[sfc()].drct;

}

/*
 * This function is used to get top layer pressure and temp directly from sndgp. As some other api need
 * them as input parameters and legacy code API does not provide such.
 */
void get_top(float* pressure, float * temp, float * dewpt){
	*pressure = sndgp->sndg[sndgp->numlev-1].pres;
	*temp = sndgp->sndg[sndgp->numlev-1].temp;
	*dewpt = sndgp->sndg[sndgp->numlev-1].dwpt;
}
/*
 * This function is used to get storm speed and direction directly from sndgp. As some other api need
 * them as input parameters and legacy code API does not provide such.
 */
void get_storm(float* speed, float * direction){
	*direction =  sndgp->st_dir;
	*speed = sndgp->st_spd;
}
void set_storm(float speed, float  direction){
	sndgp->st_dir = direction;
	sndgp->st_spd = speed;
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
	short i, lptr, uptr;
	float te1, pe1, te2, pe2, h1, h2, lyre, tdef1, tdef2, totp, totn;
	float te3, pe3, h3, tp1, tp2, tp3, tdef3, lyrf;
	float tote, dh, restim, uvv, ix1, ix2, tottim;
	float u, v, du, dv, tsu, tsv, tdist, angl;
	int size=0;
/*	float tangle; */

	lyre = -1.0F;
	totp = 25.0F;
	totn = 0.0F;
        tote = 0.0F;

	/* ----- See if default layer is specified ----- */
	if( lower == -1.0F) { lower = sndgp->sndg[sfc()].pres; }
	if( upper == -1.0F) { upper = sndgp->sndg[sndgp->numlev-1].pres; }

	/* ----- Make sure this is a valid layer ----- */
	if( lower > pres ) { lower = pres; }
	if( !qc( i_vtmp( upper ))) { return; }
	if( !qc( i_vtmp( lower ))) { return; }

	/* ----- Begin with Mixing Layer (LPL-LCL) ----- */
	te1 = i_vtmp( pres );
	pe1 = lower;
	h1 =  i_hght( pe1 );
	tp1 = virtemp( pres, temp, dwpt);

	drylift(pres, temp, dwpt, &pe2, &tp2);
	h2 =  i_hght( pe2 );
	te2 = i_vtmp( pe2 );

	if( lower > pe2 ) { lower = pe2; }

	/* ----- Find lowest observation in layer ----- */
	i = 0;
	while( sndgp->sndg[i].pres > lower)  { i++; }
	while ( !qc(sndgp->sndg[i].dwpt) ) { i++; }
	lptr = i;
	if( sndgp->sndg[i].pres == lower ) { lptr++; }

	/* ----- Find highest observation in layer ----- */
	i=(short)(sndgp->numlev-1);
	while(sndgp->sndg[i].pres < upper) { i--; }
	uptr = i;
	if( sndgp->sndg[i].pres == upper ) { uptr--; }

	/* ----- Start with interpolated bottom layer ----- */
	pe1 = lower;
	h1 =  i_hght( pe1 );
	te1 = i_vtmp( pe1 );
	tp1 = wetlift(pe2, tp2, pe1);

	totp = 25.0F;
	totn = 0.0F;
	tsu = 0.0F;
	tsv = 0.0F;
	restim = 0.0F;
	tottim = 0.0F;
	//printf("number of layer %d\n", sndgp->numlev);
	for( i = lptr; i < sndgp->numlev; i++)
	   {
	   if( qc(sndgp->sndg[i].temp) )
	      {
		   //printf( " temp %f\n", sndgp->sndg[i].temp);
	      /* ----- Calculate every level that reports a temp ----- */
	      pe2 = sndgp->sndg[i].pres;
	      h2 =  sndgp->sndg[i].hght;
	      te2 = i_vtmp( pe2 );
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

	      sr_wind( pe1, pe2, sndgp->st_dir, sndgp->st_spd, &u, &v, &ix1, &ix2);
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
		 h2 = i_hght( pe2 );
		 te2 = i_vtmp( pe2 );
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

		 sr_wind( pe1, pe2, sndgp->st_dir, sndgp->st_spd, &u, &v, &ix1, &ix2);
		 du = kt_to_mps(u) * restim;
		 dv = kt_to_mps(v) * restim;
		 tsu -= du;
		 tsv += dv;
		 tdist = (float)sqrt((double)(tsu*tsu) + (double)(tsv*tsv));
/*		 tangle = angle(tsu, tsv);  NOT used */

		 //vis_xy( tsu, tsv);
		 stmSlinky->tsuv[size][0]= tsu;
		 stmSlinky->tsuv[size][1]= tsv;
		 //printf( " returning with size %d\n", size+1);

		 angl = 90.0F - angle( tdist, agl(h2));

		 //write_vis_data( tottim, angl );
		 stmSlinky->tottim = tottim;
		 stmSlinky->angl = angl;
		 stmSlinky->size = size+1;
		 return;
		 }

	      //vis_xy( tsu, tsv);
	      stmSlinky->tsuv[size][0]= tsu;
	      stmSlinky->tsuv[size][1]= tsv;
	      stmSlinky->size = size+1;
	      size++;

	      //printf( "tsu=%f tsv=%f \n", tsu, tsv);
	      if(size>= 200)
	      	    	  return;
	      if( sndgp->sndg[i].pres == 500.0F )
		 {
		 angl = 90.0F - angle( tdist, agl(sndgp->sndg[i].hght));
		 }
	      }
	   }
	}
float cave_bulk_rich ( float lplpres, float bplus,float *brnshear )
/*************************************************************/
/* ported from skparams.c but change its input parameters     */
/* BRN                                                      */
/*  John Hart  NSSFC KCMO                                    */
/*                                                           */
/*  Calculates the Bulk Richardson Number for given parcel.  */
/*  Value is returned both as (param) and as a RETURN.       */
/*************************************************************/
{
float ptop, pbot, x1, y1, x2, y2, z1, z2, dx, dy;

if(sndgp->lplvals.flag < 4)
   {
	   ptop = i_pres(msl(6000));
   pbot = sndgp->sndg[sfc()].pres;
   }
else
   {
   pbot = i_pres(i_hght(lplpres) - 500);
   if (!qc(pbot)) pbot = sndgp->sndg[sfc()].pres;
   ptop = i_pres(i_hght(pbot) + 6000);
   }

/* ----- First, calculate lowest 500m mean wind ----- */
mean_wind( pbot, i_pres(i_hght(pbot)+500), &x1, &y1, &z1, &z2);

/* ----- Next, calculate 6000m mean wind ----- */
mean_wind( pbot, ptop, &x2, &y2, &z1, &z2);

/* ----- Check to make sure CAPE and SHEAR are avbl ----- */
if (!qc(bplus)) {return -999;}
if (!qc(x1)) {return -999;}
if (!qc(x2)) {return -999;}

/* ----- Calculate shear between winds ----- */
dx = x2 - x1;
dy = y2 - y1;
*brnshear = (float)(sqrt((dx * dx) + (dy * dy))) * .51479;
*brnshear = *brnshear * *brnshear / 2;

/* ----- Calculate and return BRN ----- Chin: I dont know why return this value*/
/* BRN valus is returned in  *brnshear*/
return bplus / *brnshear;
}

void printSfcInfo() {
	float sfctemp, sfcdwpt, sfcpres, lower, upper;
		struct _parcel pcl;

	sfctemp = sndgp->lplvals.temp;
		   sfcdwpt = sndgp->lplvals.dwpt;
		   sfcpres = sndgp->lplvals.pres;
		   parcel( -1, -1, sfcpres, sfctemp, sfcdwpt, &pcl);
		   lower = pcl.lfcpres;
		   upper = pcl.elpres;
     printf("before call meanwind sfctemp=%f sfcdwpt=%f sfcpres=%f lower=%f upper=%f\n", sfctemp, sfcdwpt, sfcpres, lower, upper);

}

