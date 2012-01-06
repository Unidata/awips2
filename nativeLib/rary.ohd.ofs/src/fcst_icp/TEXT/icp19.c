/*****************************************************************************
 * icpi19 - called from ex19, gives initial values for SNOW-17
      CALL ICPI19(OPNAME,IDT, ITPX, PXADJ, ELEV, LTAPM, TAELEV, CSF, MFMAX,
                  MFMIN, NMF, UADJ, SI, GM, MBASE, PXTEMP, PLWHC, TIPM, ALAT,
                  ADC, LMFV, SMFV, LAEC, NPTAE, AE)
 *****************************************************************************/

#ifdef MAIN_ICP_C
#undef MAIN_ICP_C
#endif

#include "c_call_f/mdyh1.h"
#include "icpcommon/icp_out.h"

static int init_19=0;
void icpi19(opname,idt, itpx, pxadj, elev, ltapm, taelev, scf, mfmax, mfmin,
            nmf, uadj, si, gm, mbase, pxtemp, plwhc, tipm, alat, adc, lmfv,
            smfv, laec, nptae, ae,mainum)
int *idt, *itpx, *ltapm, *lmfv, *laec, *nptae,*mainum;
float *pxadj, *elev, *taelev, *scf, *mfmax, *mfmin, *nmf, *uadj, *si, *gm, 
      *mbase, *pxtemp, *plwhc, *tipm, *alat, *adc, *smfv,
      ae[14][2] /* *ae[2][14] */;
char *opname;
{
 int i, j, k, k_wyp, opr_num, nxtp, lcl_mp, mnptae, notxt_work, notxt_icp;
 int txt_outflg=0;
 Joint jnflt;
 char *src_ptr=opname, lcl_opname[9];
 char ts_int_name[9], ts_type[5], ts_type_name[13], ts_symbol[5];
/* printf("text_out_sno17 = %s\n",text_out_sno17);*/
 notxt_work = IsTxtNIfp (text_out_sno17,mainum) ;
 /****IsTxtNIfp locates in icp1.c ****/
 
 if (!notxt_work)
 {  
   strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_sno17);
   
   txt_outflg = 1;
   if(!(txt_out=fopen(work_file, init_19?"a":"w")))
   {
        printf("in icpi19: cannot open %s for %s!!\n\n",
            work_file,init_19?"appending":"writing");        
        txt_outflg = 0;        
   }
 }
 init_19++;
 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ') && (++i < 8));
 lcl_opname[i] = '\0';
 i=0;
 while( strcmp(lcl_opname, sno17_opnames[i]) && i < num_sno17) i++;
 if (i== num_sno17)
  {
   printf(" in icpi19 for %s, cannot find in sno17_opnames table!\n",lcl_opname);
   return;
  }
 notxt_icp = IsTxtNIfp (file_name_sno17[i],mainum) ;

 if (!notxt_icp)
 {
  if(!(icp_in_sno17=fopen(file_name_sno17[i], "w")))
   {
    printf("in icpi19: cannot open %s!!\n\n",file_name_sno17[i]); return;
   }
  }

 for (j=0; j<SNO17_VARS; j++) sno17_data[i][j]=0.0;
 sno17_idt=*idt;
 sno17_itpx=*itpx;
/* ----------------
   for use of the developer, a method of distinguishing between mcp3 output
   pre- and post- correction of a bug that resulted in the entire ae array
   being output for the sno17 analysis.  As follows: if nptae is > 0, then
   the output is pre-correction: < 0, post.  However, if the RSL is absent
   both laec and nptae are 0.  But -0 == 0 , and neither are < 0.  So, such
   an event looks like a pre- file while actually being a post- file.  Since
   the ae array is dimensioned 14, a value of nptae of 111 is really unlikely.
   The icp code that reads the data, computes on the value of laec, so setting
   nptae=-111 should cause no computational problem, while being a functional
   input descriptor flag.    hs  23oct96
   ps. clearly, mnptae is read in icp_read_sno17_vals() as nptae.  hs
   ---------------- */
 if (*laec) mnptae = -(*nptae);
 else mnptae = -111;
/*
 printf(" in icpi19 for %s:\n", lcl_opname);
 printf("  idt=%d, itpx=%d, ltapm=%d, lmfv=%d, laec=%d, nptae=%d\n"
        "  pxadj=%.3f, elev=%.3f, taelev=%.3f, scf=%.3f, mfmax=%.3f\n"
        "  mfmin=%.3f, nmf=%.3f, uadj=%.3f, si=%.3f, gm=%.3f, mbase=%.3f\n"
       "  pxtemp=%.3f, plwhc=%.3f, tipm=%.3f, alat=%.3f, adc=%.3f, smfv=%.3f\n",
         *idt, *itpx, *ltapm, *lmfv, *laec, *nptae,
         *pxadj, *elev, *ltapm ? *taelev : -999.9, *scf, *mfmax, *mfmin,
         *nmf, *uadj, *si, *gm,
         *mbase, *pxtemp, *plwhc, *tipm, *alat, *adc, *smfv);
*/
   
if (!notxt_work && txt_outflg == 1)
{
 fprintf(txt_out, " in icpi19 for %s (init_19=%d):\n", lcl_opname, init_19);
 fprintf(txt_out, "  idt=%d, itpx=%d, ltapm=%d, lmfv=%d, laec=%d, nptae=%d\n"
        "  pxadj=%.3f, elev=%.3f, taelev=%.3f, scf=%.3f, mfmax=%.3f\n"
        "  mfmin=%.3f, nmf=%.3f, uadj=%.3f, si=%.3f, gm=%.3f, mbase=%.3f\n"
       "  pxtemp=%.3f, plwhc=%.3f, tipm=%.3f, alat=%.3f, adc=%.3f, smfv=%.3f\n",
         *idt, *itpx, *ltapm, *lmfv, *laec, *nptae,
         *pxadj, *elev, *ltapm ? *taelev : -999.9, *scf, *mfmax, *mfmin,
         *nmf, *uadj, *si, *gm,
         *mbase, *pxtemp, *plwhc, *tipm, *alat, *adc, *smfv);
/*
 fprintf(txt_out, "  the ae[2][14] array:\n"
        " %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n"
        " %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n",
         ae[ 0][0],  ae[ 0][1],  ae[ 1][0],  ae[ 1][1],  ae[ 2][0],  ae[ 2][1],
         ae[ 3][0],  ae[ 3][1],  ae[ 4][0],  ae[ 4][1],  ae[ 5][0],  ae[ 5][1],
         ae[ 6][0],  ae[ 6][1],  ae[ 7][0],  ae[ 7][1],  ae[ 8][0],  ae[ 8][1],
         ae[ 9][0],  ae[ 9][1],  ae[10][0],  ae[10][1],  ae[11][0],  ae[11][1],
         ae[12][0],  ae[12][1],  ae[13][0],  ae[13][1]);
*/
 fprintf(txt_out, " leaving icpi19:\n\n");
}
if (!notxt_icp)
{
 if(ascii_out)
  fprintf(icp_in_sno17, " %9.9s %f %d %d %f %f\n",
         lcl_opname, *plwhc, *laec, mnptae,  ae[0][0],  ae[(*nptae)-1][0]),
 fprintf(icp_in_sno17,
        " %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n"
        " %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n",
         ae[ 0][0],  ae[ 0][1],  ae[ 1][0],  ae[ 1][1],  ae[ 2][0],  ae[ 2][1],
         ae[ 3][0],  ae[ 3][1],  ae[ 4][0],  ae[ 4][1],  ae[ 5][0],  ae[ 5][1],
         ae[ 6][0],  ae[ 6][1],  ae[ 7][0],  ae[ 7][1],  ae[ 8][0],  ae[ 8][1],
         ae[ 9][0],  ae[ 9][1],  ae[10][0],  ae[10][1],  ae[11][0],  ae[11][1],
         ae[12][0],  ae[12][1],  ae[13][0],  ae[13][1]);
 else
  fwrite(opname,                      sizeof(char),           9, icp_in_sno17),
  fwrite((char *)plwhc,               sizeof(float),          1, icp_in_sno17),
  fwrite((char *)laec,                sizeof(int),            1, icp_in_sno17),
  fwrite((char *)&mnptae,             sizeof(int),            1, icp_in_sno17),
  fwrite((char *)&ae[0][0],           sizeof(float),          1, icp_in_sno17),
  fwrite((char *)&ae[(*nptae)-1][0],  sizeof(float),          1, icp_in_sno17),
  fwrite((char *)ae,                  sizeof(float),         28, icp_in_sno17);
}
/*  Debug 
 printf( "icp19: av-> %9.9s %f %d %d %f %f\n",
         lcl_opname, *plwhc, *laec, mnptae,  ae[0][0],  ae[(*nptae)-1][0]),
 printf("icp19: av->"
        " %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n"
        " %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n %g, %g\n",
         ae[ 0][0],  ae[ 0][1],  ae[ 1][0],  ae[ 1][1],  ae[ 2][0],  ae[ 2][1],
         ae[ 3][0],  ae[ 3][1],  ae[ 4][0],  ae[ 4][1],  ae[ 5][0],  ae[ 5][1],
         ae[ 6][0],  ae[ 6][1],  ae[ 7][0],  ae[ 7][1],  ae[ 8][0],  ae[ 8][1],
         ae[ 9][0],  ae[ 9][1],  ae[10][0],  ae[10][1],  ae[11][0],  ae[11][1],
         ae[12][0],  ae[12][1],  ae[13][0],  ae[13][1]);
 */        
 if(txt_outflg == 1) 
 {
      
     fclose(txt_out);
 }
 fclose(icp_in_sno17);
 return;
}
/*****************************************************************************
 * icp19 - called from pack19 gives periodic values for SNOW-17
      CALL ICP19(OPNAME, MONTH, KYEAR, KDAY, KHOUR, PRAIN, PSFALL, RSL, TA,
                 PQNET, PSNWRO, PROBG, NEGHS, LIQW, LIQWMX, TWE, COVER, OWE,
                 OSC, WE, TEX, STORGE, ACCMAX, SB, SBAESC, SBWS, ADADJ, TINDEX)
 *****************************************************************************/

void icp19(opname, month, kyear, kday, khour, prain, psfall, rsl, ta, pqnet,
                   psnwro, probg, neghs, liqw, liqwmx, twe, cover, owe, osc,
                   we, tex, storge, accmax, sb, sbaesc, sbws, adadj,
		   tindex, mainum,simSnowDepth, simSnowTemp, obsSnowDepth)
int *month, *kyear, *khour, *kday, *mainum;
float *prain, *psfall, *rsl, *ta, *pqnet, *psnwro, *probg, *liqwmx, *twe,
      *cover, *owe, *osc, *we, *tex, *storge, *accmax, *sb, *sbaesc, *sbws,
      *adadj, *tindex, *liqw, *neghs;
float *simSnowDepth, *simSnowTemp, *obsSnowDepth;
char *opname;
{
 int i, j, k, num_obs, lcl_year=*kyear, lcl_month=*month, sum_num;
 int lcl_day=*kday, lcl_hour=*khour, notxt;
 int k_sno17, first_sno17;
 int int_out_array[5];
 int yi, mi, di, hi, ihr=12, hstzcode, first_of_month;
 int kint = lcl_hour / sno17_idt;
 int cento=100, nil=0, twelve=12, sixteen=16;
 float sum_px, *out_array, hour_avger= 24.0/(float)sno17_idt;
 char *src_ptr=opname, lcl_opname[9];
 
 static int ko =0;
 Joint jnflt;
/*av debug
 printf("icp19 Routine:*simSnowDepth=%f\n",*simSnowDepth);
 printf("icp19 Routine:*simSnowTemp=%f\n",*simSnowTemp);
 printf("icp19 Routine:*obsSnowDepth=%f\n",*obsSnowDepth); 
*/
 MDYH1(kday, &ihr, &mi, &di, &yi, &hi, &cento, &nil, &hstzcode);
 lcl_month= mi, lcl_year=yi;
 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ') && (++i < 8));
 lcl_opname[i] ='\0';
 k_sno17=0;
/*  while( strcmp(lcl_opname, sno17_opnames[k_sno17]) && k_sno17 < num_sno17)  */
/*  jgg changed conditions to ensure strcmp was valid - 11/01  */
 while( k_sno17 < num_sno17 &&  strcmp(lcl_opname, sno17_opnames[k_sno17]) )
       k_sno17++;
 if (k_sno17 == num_sno17)
  {
   printf(" in icp19 for %s, cannot find in sno17_opnames table!\n",lcl_opname);
   return;
  }
 out_array = sno17_data[k_sno17];
 jnflt.flt = out_array[SNO17_VARS-1];
 first_sno17=jnflt.fxd;
 
 /*printf("-->lcl_hour = %d first_sno17=%d\n",lcl_hour,first_sno17);*/
 if((first_of_month = (di ==1 && kint == 1)) || first_sno17==0)
 { 
     notxt = IsTxtNIfp (file_name_sno17[k_sno17],mainum) ;
  
    /*avDebug printf(" icp19 :-> file_name_sno17[k_sno17]=%s k_sno17 = %d\n",file_name_sno17[k_sno17],k_sno17);*/
   
     if(!(icp_in_sno17=fopen(file_name_sno17[k_sno17], "a")))
        {printf("in icp19: cannot open %s!!\n\n",file_name_sno17[k_sno17]); return;}

     if((!(lcl_year%4) && (lcl_year%100)) || !(lcl_year%400))sno17_leap_yr=1;
     else sno17_leap_yr=0;
     num_obs = mons[sno17_leap_yr][lcl_month];
     /*AVdebug printf("AV debug: num_obs = %d\n",num_obs);*/
     if (!notxt)
     {
        if(ascii_out){
            fprintf(icp_in_sno17," %2d  %2d  %2d  %4d  %2d\n",
            *khour, *kday, *month, *kyear, num_obs);            
        } 
        else
            int_out_array[0] = lcl_month,
            int_out_array[1] = lcl_year,
            int_out_array[2] = num_obs,
            int_out_array[3] = di * 100 + lcl_hour,
            memcpy((void *)&int_out_array[4],"\n   ",4),
            fwrite((char *)&int_out_array, sizeof(int), 5, icp_in_sno17);
           /* printf("Got here..lcl_month=%d lcl_year=%d num_obs = %d dyhr=%d\n",lcl_month,lcl_year,num_obs,int_out_array[3]);*/
     }
     /*printf("icp ----->:  %2d  %2d  %2d  %4d %2d\n",
            int_out_array[0], int_out_array[1], int_out_array[2], int_out_array[3],int_out_array[4]);  */      
     if(first_sno17 < 6)
     {
        strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_sno17);
        notxt = IsTxtNIfp (text_out_sno17,mainum) ;
        if (!notxt)
        {
           
           if(!(txt_out=fopen(work_file, "a")))
               printf("in icp19: cannot open %s for appending!!\n\n", work_file);
           else
               fprintf(txt_out," in icp19: snow-17 data output (%s, first_sno17=%d)!\n",
           lcl_opname, first_sno17),
           fprintf(txt_out, " %d %d %d\n", lcl_month, lcl_year, num_obs);
           fclose(txt_out);
           /*printf("Got here..111..work_file = %s\n",work_file);*/
        }

/*
      printf(" in icp19 for snow-17 data output (%s, first_sno17=%d)!\n",
       lcl_opname, first_sno17),
      printf(" %d %d %d\n", lcl_month, lcl_year, num_obs),

      fclose(txt_out);
*/   }    
     for (i=0; i< SNO17_VARS; i++) out_array[i] = 0.0; 
     fclose(icp_in_sno17);
 }
 out_array[0] += *psfall;
 out_array[1] += *prain;
 out_array[2] += *rsl;
 out_array[3] += *ta;
 out_array[5] += *pqnet;
 out_array[6] += *probg;
 out_array[7] += *psnwro;
 out_array[15] = *simSnowDepth;
 out_array[16] = *simSnowTemp;
 out_array[17] = *obsSnowDepth;
 /* debug 
     printf("AV debug --->"
           " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f"
           "%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
           out_array[ 0], out_array[ 1], out_array[ 2],
           out_array[ 3], out_array[ 4], out_array[ 5],
           out_array[ 6], out_array[ 7], out_array[ 8],
           out_array[ 9], out_array[10], out_array[11],
           out_array[12], out_array[13], out_array[14],
           out_array[15], out_array[16], out_array[17]); 
 */
 if (lcl_hour == 24)
 {
     if(!(icp_in_sno17=fopen(file_name_sno17[k_sno17], "a")))
       {printf("in icp19: cannot open %s!!\n\n",file_name_sno17[k_sno17]); return;}

     out_array[ 2] /= hour_avger;
     out_array[ 3] /= hour_avger;
     out_array[ 4] = *tindex;
     out_array[ 8] = *liqw;
     out_array[ 9] = *liqwmx;
     out_array[10] = *neghs;
     out_array[11] = *osc;
     out_array[12] = *cover;
     out_array[13] = *owe;
     out_array[14] = *twe; 
     out_array[15] = *simSnowDepth;
     out_array[16] = *simSnowTemp;
     out_array[17] = *obsSnowDepth;            
           
     notxt = IsTxtNIfp (file_name_sno17[k_sno17],mainum) ; 
   
    if (!notxt)
    {         
        if (ascii_out){
           fprintf(icp_in_sno17,
           " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f"
/*           "%5.1f%5.1f%5.1d%5.1f%5.1f%5.1f%5.1f\n",*//*Should not have %5.1d---kwz*/
           "%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
           out_array[ 0], out_array[ 1], out_array[ 2],
           out_array[ 3], out_array[ 4], out_array[ 5],
           out_array[ 6], out_array[ 7], out_array[ 8],
           out_array[ 9], out_array[10], out_array[11],
           out_array[12], out_array[13], out_array[14],
           out_array[15], out_array[16], out_array[17]);

        }
        else{
           ko++;
           fwrite((char *)out_array, sizeof(float), 18, icp_in_sno17);
           /*
           printf("18 arrays=  %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f"*/
/*           "%5.1f%5.1f%5.1d%5.1f%5.1f%5.1f%5.1f\n",*//*Should not have %5.1d---kwz*/
/*           "%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
           out_array[ 0], out_array[ 1], out_array[ 2],
           out_array[ 3], out_array[ 4], out_array[ 5],
           out_array[ 6], out_array[ 7], out_array[ 8],
           out_array[ 9], out_array[10], out_array[11],
           out_array[12], out_array[13], out_array[14],
           out_array[15], out_array[16], out_array[17]); av debug*/
           
        }
    }
    fclose (icp_in_sno17) ;
    if(first_sno17<6)
    { 
    
       notxt = IsTxtNIfp (text_out_sno17,mainum) ;
       if (!notxt)
       {
      
           strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_sno17);
           if(!(txt_out=fopen(work_file, "a")))
           printf("in icp19: cannot open %s for appending!!\n\n", work_file);
           else
           {            
	       fprintf(txt_out,
               " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f"
/*           "%5.1f%5.1f%5.1d%5.1f%5.1f%5.1f%5.1f\n",*//*Should not have %5.1d---kwz*/
               "%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
               out_array[ 0], out_array[ 1], out_array[ 2],
               out_array[ 3], out_array[ 4], out_array[ 5],
               out_array[ 6], out_array[ 7], out_array[ 8],
               out_array[ 9], out_array[10], out_array[11],
               out_array[12], out_array[13], out_array[14],
               out_array[15], out_array[16], out_array[17]);
              if(txt_out != NULL) fclose(txt_out);  /* added check - jgg  */
           }
  /*
    printf(
           " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f"
           "%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
           out_array[ 0], out_array[ 1], out_array[ 2],
           out_array[ 3], out_array[ 4], out_array[ 5],
           out_array[ 6], out_array[ 7], out_array[ 8],
           out_array[ 9], out_array[10], out_array[11],
           out_array[12], out_array[13], out_array[14]); 
  */
  
       }
   }
   for (i=0; i<20; i++) out_array[i]=0.0;
 /*  fclose(icp_in_sno17);*/
 }
 if(first_of_month || first_sno17==0)first_sno17++;
 jnflt.fxd = first_sno17;
 out_array[SNO17_VARS-1] = jnflt.flt;
/* fclose(icp_in_sno17);*/

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_icp/RCS/icp19.c,v $";
 static char rcs_id2[] = "$Id: icp19.c,v 1.12 2006/04/20 14:36:45 xfan Exp $";}
/*  ===================================================  */
}
