/*****************************************************************************
 * icpi1 - called from ex1, gives initial values for SAC-SMA-PLOT
      CALL ICPI1(OPNAME,IDT, UZTWM, UZFWM, LZTWM, LZFSM, LZFPM,
                 UZK, LZSK, LZPK, ZPERC, REXP, PCTIM, ADIMP, RIVA, 
                 PFREE, SIDE, PXADJ, PEADJ, EFC, IET, PL(IPLET), IFRZE)
 *****************************************************************************/

#ifdef MAIN_ICP_C
#undef MAIN_ICP_C
#endif

#include "c_call_f/mdyh1.h"
#include "icpcommon/icp_out.h"

static int init_1=0;

void icpi1(opname, idt, uztwm, uzfwm, lztwm, lzfsm, lzfpm,
                   uzk, lzsk, lzpk, zperc, rexp, pctim, adimp, riva, 
                   pfree, side, pxadj, peadj, efc, iet, pl/*(iplet)*/, ifrze,
		   mainum)
char *opname;
int *idt, *iet, *ifrze,*mainum;

float *uztwm, *uzfwm, *lztwm, *lzfsm, *lzfpm,
      *uzk, *lzsk, *lzpk, *zperc, *rexp, *pctim, *adimp, *riva,
      *pfree, *side, *pxadj, *peadj, *efc, *pl;
{

 int i, j, k, opr_num, nxtp, lcl_mp, out_array[8], notxt_work, notxt_icp;
 int txt_outflg;
 char *src_ptr=opname, lcl_opname[9];
 Joint jnflt;
 
 notxt_work = IsTxtNIfp (text_out_sac,mainum) ;
 
 if (!notxt_work)
 { 
      strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_sac);
      txt_outflg = 1;
      if(!(txt_out=fopen(work_file, init_1?"a":"w")))
      {
          txt_outflg = 0;
          printf("in icpi1: cannot open %s for %s!!\n\n",
                   work_file, init_1?"appending":"writing");
      }
  
  
 }

 init_1++;

 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ')
         && (++i < 8));
 lcl_opname[i] ='\0';
 i=0;
 while( strcmp(lcl_opname, sac_opnames[i]) && i < num_sac) i++;
 if (i== num_sac)
  {
   printf(" in icpi1 for %s, cannot find in sac_opnames table!\n",lcl_opname);
   return;
  } 
 notxt_icp = IsTxtNIfp (file_name_sac[i],mainum) ;
 
 if (!notxt_icp)
 { if(!(icp_in_sac=fopen(file_name_sac[i], "w")))
  {
   printf("in icpi1: cannot open %s!!\n\n",file_name_sac[i]); return;
  }
 }
 
 for (j=0; j<SAC_VARS; j++) sac_data[i][j]=0.0;
/*
  printf(" in icpi1 for sac_sma init data output (opname: %s)!\n",lcl_opname);
*/
if (!notxt_work && txt_outflg == 1)
 fprintf(txt_out,
     " in icpi1 for sac_sma init data output (opname: %s)!\n",lcl_opname);
 sac_time_steps= 24/(sac_idt = *idt);
 sac_frze = *ifrze;
if (!notxt_icp)
{
 if (ascii_out)
  fprintf(icp_in_sac," %9.9s %.1f %.1f %.1f %.1f %.1f %d\n",
   lcl_opname, *uztwm, *uzfwm, *lztwm, *lzfsm, *lzfpm, *ifrze);
 else
  fwrite(   lcl_opname, sizeof(char ), 9, icp_in_sac),
  fwrite((char *)uztwm, sizeof(float), 1, icp_in_sac),
  fwrite((char *)uzfwm, sizeof(float), 1, icp_in_sac),
  fwrite((char *)lztwm, sizeof(float), 1, icp_in_sac),
  fwrite((char *)lzfsm, sizeof(float), 1, icp_in_sac),
  fwrite((char *)lzfpm, sizeof(float), 1, icp_in_sac),
  fwrite((char *)ifrze, sizeof(int  ), 1, icp_in_sac); 
}
/*
  printf( "  uztwm=%.3f, uzfwm=%.3f, lztwm=%.3f, lzfsm=%.3f, lzfpm=%.3f, "
         " ifrze=%d, sac_idt=%d, sac_time_steps=%d\n",
   *uztwm, *uzfwm, *lztwm, *lzfsm, *lzfpm, sac_frze, sac_idt, sac_time_steps);
*/
if (!notxt_work && txt_outflg == 1)
 {
  fprintf(txt_out, 
          "  uztwm=%.3f, uzfwm=%.3f, lztwm=%.3f, lzfsm=%.3f, lzfpm=%.3f, "
          " ifrze=%d, sac_idt=%d, sac_time_steps=%d\n",
     *uztwm, *uzfwm, *lztwm, *lzfsm, *lzfpm, sac_frze, sac_idt, 
     sac_time_steps);
  fclose(txt_out);
 }
fclose(icp_in_sac); 
 /*-- AV --if file is already open close it */
 /* if(txt_outflg == 1 && txt_out == 1)fclose(txt_out); */
return;
}
/*****************************************************************************
 * icp1 - called from fland1, gives periodic values for SAC-SMA-PLOT
      CALL ICP1(OPNAME, MONTH, IYEAR, KDA, KINT, UZTWD, UZFWC, LZTWD,
                LZFSC, LXFPC, UZTWC, LZTWC, ADIMC, FGCO(1), PXV, 
                TCI, ROIMP, SDRO, SSUR, SIF, BFS, BFP, SPERC, EDMND,
                TET, E1, E3, E4, E5)
 *****************************************************************************/

void icp1(opname, month, iyear, kda, kint, uztwd, uzfwc, lztwd,
          lzfsc, lzfpc, uztwc, lztwc, adimc, fgco/*(1)*/, pxv, 
          tci, roimp, sdro, ssur, sif, bfs, bfp, sperc, edmnd,
          tet, e1, e3, e4, e5, mainum)
char *opname;
int *month, *iyear, *kda, *kint, *mainum;
float *uztwd, *uzfwc, *lztwd, *lzfsc, *lzfpc, *uztwc, *lztwc,
      *adimc, *fgco, *pxv,
      *tci, *roimp, *sdro, *ssur, *sif, *bfs, *bfp,
      *sperc, *edmnd, *tet, *e1, *e3, *e4, *e5;
{
 int i, j, k, k_sac, num_obs, lcl_year, lcl_month, first_sac, notxt;
 int int_out_array[5];
 float zero=0.0, *out_array;
 int yi, mi, di, hi, ihr=12, hstzcode, first_of_month;
 int cento=100, nil=0, twelve=12, sixteen=16;
 char *src_ptr=opname, lcl_opname[9];
 Joint jnflt;

 MDYH1(kda, &ihr, &mi, &di, &yi, &hi, &cento, &nil, &hstzcode);
 lcl_month= mi, lcl_year=yi;
 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ') && (++i < 8));
 lcl_opname[i] ='\0';
 k_sac=0;
 while( strcmp(lcl_opname, sac_opnames[k_sac]) && k_sac < num_sac) k_sac++;
 if (k_sac == num_sac)
  {
   printf(" in icp1 for %s, cannot find in sac_opnames table!\n",lcl_opname);
   return;
  }
 out_array=sac_data[k_sac];
 jnflt.flt=out_array[SAC_VARS-1];
 first_sac=jnflt.fxd;

 if((first_of_month = (di ==1 && *kint == 1)) || first_sac==0)
  {notxt = IsTxtNIfp (file_name_sac[k_sac],mainum) ;
   
   if (!notxt)
   if(!(icp_in_sac=fopen(file_name_sac[k_sac], "a")))
    { printf("in icp1: cannot open %s!!\n\n",file_name_sac[k_sac]); return; }

   if((!(lcl_year%4) && (lcl_year%100)) || !(lcl_year%400))sac_leap_yr=1;
    else sac_leap_yr=0;
   num_obs = mons[sac_leap_yr][lcl_month];
   if (!notxt)
   {if (ascii_out)
     fprintf(icp_in_sac, " %d %d %d %d\n", lcl_month,lcl_year,num_obs,sac_frze);
    else

/*     fwrite((char *)&lcl_month, sizeof(int), 1, icp_in_sac),
     fwrite((char *)&lcl_year, sizeof(int), 1, icp_in_sac),
     fwrite((char *)&num_obs, sizeof(int), 1, icp_in_sac),
     fwrite((char *)&sac_frze, sizeof(int), 1, icp_in_sac),
     fwrite("\n   ", 1, 4, icp_in_sac);
 */   
    int_out_array[0] = lcl_month,
    int_out_array[1] = lcl_year,
    int_out_array[2] = num_obs,
    int_out_array[3] = sac_frze,
    memcpy((void *)&int_out_array[4],"\n   ",4);
    
    }

    if (!notxt)
      fwrite((char *)&int_out_array, sizeof(int), 5, icp_in_sac);
   if(first_sac<6)
    {
     notxt = IsTxtNIfp (text_out_sac,mainum) ;
         
     if (!notxt)
     {
     strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_sac);
     if(!(txt_out=fopen(work_file, "a")))
      printf("in icpi1: cannot open %s for appending!!\n\n", work_file);
     else
     { 
      fprintf(txt_out," in icp1 for sac_sma data output (%s, first_sac=%d)!\n",
                lcl_opname, first_sac),
      fprintf(txt_out," %d %d %d %d\n", lcl_month, lcl_year, num_obs,sac_frze); 
      fclose(txt_out);
      }}
/*
     printf(" in icp1 for sac_sma data output (%s, first_sac=%d)!\n",
                          lcl_opname, first_sac),
     printf(" %d %d %d %d\n", lcl_month, lcl_year, num_obs, sac_frze),
*/
     for(i=5;i <13; i++)out_array[i]=0.0;
    }
   fclose(icp_in_sac);
  }
 out_array[ 5] += *roimp; /* = sac_rocl[ROIMP] */
 out_array[ 6] += *sdro ; /* = sac_rocl[SDRO ] */
 out_array[ 7] += *ssur ; /* = sac_rocl[SSUR ] */
 out_array[ 8] += *sif  ; /* = sac_rocl[SIF  ] */
 out_array[ 9] += *bfs  ; /* = sac_rocl[BFS  ] */
 out_array[10] += *bfp  ; /* = sac_rocl[BFP  ] */
 out_array[11] += *pxv  ; /* = sac_pxv */
 out_array[12] += *tci  ; /* = sac_tci */

 if (*kint == sac_time_steps)
  {
   if(!(icp_in_sac=fopen(file_name_sac[k_sac], "a")))
    { printf("in icp1: cannot open %s!!\n\n",file_name_sac[k_sac]); return; }

   notxt = IsTxtNIfp (file_name_sac[k_sac],mainum) ;
   
   out_array[ 0]=*uztwd,
   out_array[ 1]=*uzfwc,
   out_array[ 2]=*lztwd,
   out_array[ 3]=*lzfsc,
   out_array[ 4]=*lzfpc;
   if(sac_frze)
   out_array[13]=*fgco;
   else
   out_array[13]=zero;
if (!notxt)
{
   if (ascii_out)
    fprintf(icp_in_sac,
          " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f" 
          "%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
           out_array[ 0], out_array[ 1], out_array[ 2],
           out_array[ 3], out_array[ 4], out_array[ 5],
           out_array[ 6], out_array[ 7], out_array[ 8],
           out_array[ 9], out_array[10], out_array[11],
           out_array[12], out_array[13]);
   else
    fwrite((char *)out_array, sizeof(float), 14, icp_in_sac);
}
   if(first_sac<6)
    {
      notxt = IsTxtNIfp (text_out_sac,mainum) ;
      
      if (!notxt)
      {
     strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_sac);
     if(!(txt_out=fopen(work_file, "a")))
         printf("in icpi1: cannot open %s for appending!!\n\n", work_file);
/*
     printf(" %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f",
            out_array[ 0], out_array[ 1], out_array[ 2],
            out_array[ 3], out_array[ 4], out_array[11],
            out_array[13], out_array[12]);
     printf(" %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
            out_array[ 5], out_array[ 6], out_array[ 7],
            out_array[ 8], out_array[ 9], out_array[10]);
*/

     fprintf(txt_out,
           " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f%5.1f",
            out_array[ 0], out_array[ 1], out_array[ 2],
            out_array[ 3], out_array[ 4], out_array[11],
            out_array[13], out_array[12]);
     fprintf(txt_out, " %5.1f%5.1f%5.1f%5.1f%5.1f%5.1f\n",
            out_array[ 5], out_array[ 6], out_array[ 7],
            out_array[ 8], out_array[ 9], out_array[10]);

     fclose(txt_out);}
    }
   for(i=5; i<13; i++)out_array[i]=0.0;
   fclose(icp_in_sac);
  }
 if(first_of_month || first_sac==0)first_sac++;
 jnflt.fxd = first_sac;
 out_array[SAC_VARS-1] = jnflt.flt;
 return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_icp/RCS/icp1.c,v $";
 static char rcs_id2[] = "$Id: icp1.c,v 1.8 2006/04/20 14:35:06 xfan Exp $";}
/*  ===================================================  */

}

/*************************************/
int IsTxtNIfp (char *InStr, int *mainum)
/****This subroutine returns true if the last 4 char of InStr == .txt and
     mainum == 1 ***** else false *****/
{ int length, notxt;
  char *tmpstr ;

  length = strlen (InStr);

  /*
  tmpstr = (char *)malloc (length * sizeof (char)) ;
  strcpy (tmpstr,InStr);
  tmpstr=tmpstr+length-4 ;  /* get the last four characters 
  notxt = !strcmp(tmpstr,".txt") && (*mainum == 1) ;
  tmpstr=tmpstr-length+4;
  free ((void *)tmpstr);
  */

  tmpstr = &InStr[length - 4];
  notxt=0;
  if( !strcmp(tmpstr,".txt") && (*mainum == 1)) 
    notxt = 1;

  return notxt ;
}
