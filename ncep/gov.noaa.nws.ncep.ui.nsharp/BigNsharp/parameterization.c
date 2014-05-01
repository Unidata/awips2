#define GLOBAL
#ifdef _WIN32
#include <stdio.h>
#endif
#include "sharp95.h"

void write_scheme_file(char st[80])
	{
	int i, pIndex, tIndex, dIndex, kIndex, oIndex, wsIndex, wdIndex;
	FILE *fp;
	float ix1, tkel;

	pIndex = getParmIndex("PRES");
	tIndex = getParmIndex("TEMP");
	dIndex = getParmIndex("DWPT");
	wdIndex = getParmIndex("DRCT");
	wsIndex = getParmIndex("SPED");
	kIndex = getParmIndex("TKEL");
	oIndex = getParmIndex("OMEG");

	fp = fopen("/tmp/cpdata.txt","wt");
	fprintf( fp, "%s\n", raobtitle);
	fprintf( fp, "%s\n", raob_type);
	fprintf( fp, "%s\n", st);
	fprintf( fp, "Levels = %d\n", numlvl);
	for(i=0;i<numlvl;i++)
		{
		tkel = sndg[i][kIndex];

		fprintf( fp, "%8.2f,   %8.2f,   %8.2f,   %8.2f,   %8.2f,   %8.3f,   %8.3f\n",
		sndg[i][pIndex], sndg[i][tIndex], mixratio(sndg[i][pIndex], sndg[i][dIndex]), i_wndu(sndg[i][pIndex], I_PRES), i_wndv(sndg[i][pIndex], I_PRES), sndg[i][oIndex], tkel);
		}
	fclose(fp);
	}





void write_hail_file(char st[80])
        {
        int i, pIndex, tIndex, dIndex, zIndex, oIndex, wsIndex, wdIndex;
	char finam[80], finam1[80];
        FILE *fp;
        float ix1, press;
	int found850, found700, found500, found400, found300, found250, found200, found100;

	found850=0;
	found700=0;
	found500=0;
	found300=0;
	found400=0;
	found250=0;
	found200=0;
	found100=0;

        zIndex = getParmIndex("HGHT");
        pIndex = getParmIndex("PRES");
        tIndex = getParmIndex("TEMP");
        dIndex = getParmIndex("DWPT");
        wdIndex = getParmIndex("DRCT");
        wsIndex = getParmIndex("SPED");

        sprintf(finam, "/tmp/02000000.dat", st);
        fp = fopen(finam, "wt");

	if (sndg[sfc()][pIndex] < 850) found850=1;

        for(i=0;i<numlvl;i++)
                {
/* JL */
/*printf("i = %d, numlvl = %d\n", i, numlvl); */
/* JL */
		if ((sndg[i][pIndex]<=500) && found500==0)
		   {
		   press=500;
                   if(press != sndg[i][pIndex])
                      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
		      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES), 
		      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
		      found500=1;
		      }
		   }

                if ((sndg[i][pIndex]<=850) && found850==0)
                   {
                   press=850;
		   if(press != sndg[i][pIndex])
		      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found850=1;
		      }
                   }

                if ((sndg[i][pIndex]<=700) && found700==0)
                   {
                   press=700;
		   if(press != sndg[i][pIndex])
		      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found700=1;
		      }
                   }


                if ((sndg[i][pIndex]<=400) && found400==0)
                   {
                   press=400;
                   if(press != sndg[i][pIndex])
                      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found400=1;
                      }
                   }

                if ((sndg[i][pIndex]<=300) && found300==0)
                   {
                   press=300;
		   if(press != sndg[i][pIndex])
		      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found300=1;
		      }
                   }

                if ((sndg[i][pIndex]<=250) && found250==0)
                   {
                   press=250;
                   if(press != sndg[i][pIndex])
                      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found250=1;
                      }
                   }

                if ((sndg[i][pIndex]<=200) && found200==0)
                   {
                   press=200;
                   if(press != sndg[i][pIndex])
                      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found200=1;
                      }
                   }

                if ((sndg[i][pIndex]<=100) && found100==0)
                   {
                   press=100;
                   if(press != sndg[i][pIndex])
                      {
                      fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
                      press, i_hght(press, I_PRES), i_temp(press, I_PRES), i_dwpt(press, I_PRES),
                      i_wdir(press, I_PRES), i_wspd(press, I_PRES) * .514);
                      found100=1;
                      }
                   }

		if (sndg[i][tIndex] > -999)
			{
                	fprintf( fp, "%8.2f   %8.2f   %8.2f   %8.2f   %8.2f   %8.2f\n",
			sndg[i][pIndex], sndg[i][zIndex], sndg[i][tIndex], sndg[i][dIndex], 
			sndg[i][wdIndex], sndg[i][wsIndex] * .514);
			}
                }
        fclose(fp);
/*	printf( "Successfully wrote output file:  %s\n", finam);*/

	sprintf(finam1, "/tmp/hailtest.tak");
        fp = fopen(finam1, "wt");
	fprintf( fp, "02000000\n");
	fprintf( fp, "%8.2f\n", sndg[sfc()][tIndex]);
	fprintf( fp, "%8.2f\n\n\n", sndg[sfc()][dIndex]);
/*	printf( "Successfully wrote hailtest.tak file\n");*/
	fclose(fp);

	/* Write Lat/Lon File */
	sprintf(finam1, "/tmp/sndgloc.txt");
        fp = fopen(finam1, "wt");
	fprintf( fp, "Lat= %6.2f   Lon= %6.2f\n", globalsndg->lat, globalsndg->lon);
	fprintf( fp, "%s (%s)\n", raobtitle, raob_type);
	fclose(fp);
        }


float sig_hail(float mucape, float mumixr, float lr75, float t500, float shr6, float fzlh, float mucin, float davc, float davcb, float ic, float mlcape)
        {
	        float sighail, shear6, pbot, ptop, base;

                shear6 = shr6;
                if (shear6 > 27 ) shear6 = 27;
                if (shear6 < 7 ) shear6 = 7;

                if (mumixr > 13.6 ) mumixr = 13.6;
                if (mumixr < 11 ) mumixr = 11;

		if (t500 > -5.5 ) t500 = -5.5;
	
		effective_inflow_layer(100, -250, &pbot, &ptop);
		base = agl(i_hght(pbot, I_PRES));
		if (base == 0) {
			if ((mlcape/mucape) < 0.3){
				 mucape = mlcape/0.3;
				}
			}

	        sighail = ((mucape * mumixr * lr75 * (t500 * -1) * shear6)/42000000);	         

	        if (mucin < -200 ) sighail = 0;
	        if (mucape < 750 ) sighail = 0;
	        if (lr75 < 5.3 ) sighail = 0;
		if (t500 < -23.0 ) sighail = 0;
		if (fzlh < 2100 ) sighail = 0;		

		//Chin: change from the following original line after confirmed by Rich T. 10/19/2012
		//if (mucape < 1300) sighail*(mucape / 1300);
	        if (mucape < 1300) sighail = sighail*(mucape / 1300);
	        if (lr75 < 5.8 ) sighail = sighail*(lr75 / 5.8);
		if (fzlh < 2400) sighail = sighail*(fzlh / 2400);
		
	        return sighail;
	}

