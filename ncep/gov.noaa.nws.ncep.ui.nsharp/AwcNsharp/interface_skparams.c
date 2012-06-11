/**************************************************************************** */
/* This module allows access to N-Sharp's skparams thermo-dynamic routines   */
/* defined in skparams.c, to the calling program plot_indices.f which        */
/* renders a planview display of these parameters via gempak calls           */
/* (modelled after SNMAP).  A                                                */
/* data array  of sounding data is reformatted to an NSharp sounding array,  */
/* so that the  routines can be accessed.                                    */
/* A most unstable parcel trajectory is used in these computations           */
/* Input parameters:                                                         */
/* lvls - Number of levels                                                   */
/* data - sounding data array                                                */
/*                                                                           */
/* Output parameters:                                                        */
/* vals - Computed parameters                                                */
/* 0 - 500 mb Lifted Index                                                   */
/* 1 - K-Index                                                               */
/* 2 - Total Totals                                                          */
/* 3 - Precipitable Water (Inches)                                           */
/* 4 - Cape                                                                  */
/* 5 - Bulk Richardson Number                                                */
/* 6 - LCL (ft AGL)                                                          */
/* 7 - LFC (ft AGL)                                                          */
/* 8 - EL (ft AGL)                                                           */
/* 9 - CINH                                                                  */
/* 10 - Sweat Index                                                          */
/*                                                                           */
/* Author: L. Hinson/AWC  July 2004                                          */
/*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "sharp95.h"
#define calc_vals calc_vals_

void calc_vals(short *lvls, float *data, float *vals) {
  int i, newlevels;
  float param,presck,ct,vt,ix1,mn_dwpt;
  struct _parcel pcl;
  
  numlvl=*lvls;
  newlevels = 0;
  
  for (i=0;i<numlvl;i++) {
     /* Is temp && dwpt passable? */
     if (qc(data[i*6+1]) && qc(data[i*6+2])) {
        sndg[newlevels][0]=0.0;     
	sndg[newlevels][1]=data[i*6+0];
	sndg[newlevels][2]=data[i*6+5];
	sndg[newlevels][3]=data[i*6+1];
	sndg[newlevels][4]=data[i*6+2];
	sndg[newlevels][5]=data[i*6+3];
	sndg[newlevels][6]=data[i*6+4]/.51479;
        newlevels++;
     }
  }
  numlvl = newlevels;
  xtnd_sndg();
  /*Initialize results array*/
  for (i=0;i<15;i++) {
    vals[i]=-9999.0;
  }
  

   /* Most-unstable Parcel calculations */
      presck = 400.0;
      if(sndg[0][1]-presck < 500.0)
        {
         presck =  sndg[0][1] - 500.0;
        }
      define_parcel(4,presck);
	  
      ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
      vals[0]=pcl.li5;
      vals[1]=k_index(&param);
      vals[2]=t_totals(&param,&ct,&vt);
      vals[3]=precip_water(&param,-1,-1);
      vals[4]=pcl.bplus;
      if (! isinf(pcl.brn) && (pcl.brn >= 0.0 || pcl.brn < 5000) ) 
	vals[5]=pcl.brn;
      else
        vals[5]=-999.;
      vals[6]=mtof(agl(i_hght(pcl.lclpres)));
      vals[7]=mtof(agl(i_hght(pcl.lfcpres)));
      vals[8]=mtof(agl(i_hght(pcl.elpres)));
      vals[9]=pcl.bminus;
      vals[10]=sweat_index(&ix1);
    /* Mean Mixlyr Parcel calculations */
      define_parcel(3, presck);
      ix1 = parcel(-1, -1, lplvals.pres, lplvals.temp, lplvals.dwpt, &pcl);
      vals[11] = pcl.bplus;
      vals[12] = pcl.bminus;
      mean_dwpt(&mn_dwpt,-1,-1);
      vals[13] = ctof(mn_dwpt);
      
}
  
