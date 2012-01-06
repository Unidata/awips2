/*****************************************************************************
 * icpi18 - called from ex18, gives initial values for PLOTTS
      CALL ICPI18(OPNAME, PO)
 *****************************************************************************/

#ifdef MAIN_ICP_C
#undef MAIN_ICP_C
#endif

#include "c_call_f/mdyh1.h"
#include "icpcommon/icp_out.h"

#define SCREEN_OUT  0

//cfan
union 
{
  char chr_utzc[5] ;
  int int_utzc ;
} utzc;
//cfan

typedef struct _TimeSeries
 {
  char tsid[9], type[5], tstl[13], symbol[5], valnm[9], filename[256];
  int idt, nvpdt, locv, nlocd, nmlen, num_obs, count;
 } TimeSeries;
typedef struct _Plot
 {
  char type[5], unitsm[5], unitse[5];
  int npts, nts;
  float pmin, pmax, cfact, cconst;
  TimeSeries *times;
 } Plot;
typedef struct _Plotts
 {
  int ntts, nplots, nopt, ver, nper, idtp;
  char opname[9], title[21];
  Plot *plot;
 } Plotts;

static int init_18=0, which_one, lcl_ldarun;
Plotts *plotts;
Plotts * plotts_ptr;
Plot * plot_ptr;
TimeSeries * times_ptr;

void icpi18(opname, po, idarun, ihrrun, ldarun, lhrrun)
char *opname, *po;
int *idarun, *ihrrun, *ldarun, *lhrrun;
{
 int i, j, k, l, m, n, totalts, max_filenm_len=0;
 char fname_bld[300];
 FILE *tst_file;
 /*  plotts variables */
 float fl_nopt, fl_nplots, fl_ntts, fl_nper, fl_idtp, fl_ver;
 int nopt, nplots, ntts, nper, idtp, ver;
 /*  plot variables */
 float fl_npts, pmin, pmax, fl_nts, cfact, cconst;
 int npts, nts;
 /*  time series variables */
 float fl_idt, fl_nvpdt, fl_locv;
 /* end special variables */
 Joint jnflt;
 char *src_ptr=opname, lcl_opname[9],dummy_str[4];

 lcl_ldarun = *ldarun;
 strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_plotts);
 if(!(txt_out=fopen(work_file, init_18?"a":"w")))
    printf("in icpi18: cannot open %s for %s!!\n\n",
             work_file,init_18?"appending":"writing");
 if (!init_18)
   plotts= (Plotts*)calloc( num_plotts, sizeof (Plotts));
 init_18=1;

 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ')
         && (++i < 8));
 lcl_opname[i] = '\0';
 i=0;
 while( strcmp(lcl_opname, plotts_opnames[i]) && i < num_plotts) i++;
 if (i== num_plotts)
  {
   printf(" in icpi18 for %s, not found in plotts_opnames table!\n",lcl_opname);
   return;
  }
 if(!(icp_in_plotts=fopen(file_name_plotts[i], "w")))
  {
   printf("in icpi18: cannot open %s!!\n\n",file_name_plotts[i]); return;
  }
 which_one = i;
 strcpy(plotts[which_one].opname,lcl_opname);
 strcat( strcat( strcat( strcpy(work_file, intrfc_dir), "/"),
                 shed_name), ".plotts.");
 if(SCREEN_OUT)
  printf("in icpi18: opened file %s\n",file_name_plotts[i]);
 fprintf(txt_out, "in icpi18: opened file %s\n",file_name_plotts[i]);
 for (j=0; j<PLOTTS_VARS; j++) plotts_data[i][j]=0.0;
 pin = po;
 FLIT(fl_ver)
 for (k=0; k<20; k++)plotts[which_one].title[k]=*pin++; 
                     plotts[which_one].title[k]='\0';
 FLIT(fl_nopt)
 FLIT(fl_nplots)
 FLIT(fl_ntts)
 FLIT(fl_nper)
 FLIT(fl_idtp)
 plotts[which_one].ver   =ver   =fl_ver;
 plotts[which_one].nopt  =nopt  =fl_nopt;
 plotts[which_one].nplots=nplots=fl_nplots;
 plotts[which_one].ntts  =ntts  =fl_ntts;
 plotts[which_one].nper  =nper  =fl_nper;
 plotts[which_one].idtp  =idtp  =fl_idtp;
 plotts[which_one].plot      =(Plot*)calloc(nplots, sizeof(Plot));

/**/
 i = which_one;
 if(SCREEN_OUT)
  {
   printf(" in icpi18 for %s,  mcp3 version is %d:\n", 
            plotts[which_one].opname, plotts[i].ver);
   printf(" title: %s\n", plotts[i].title);
   printf(" opname=%s, nopt=%d, nplots=%d, ntts=%d, nper=%d, idtp=%d\n",
            plotts[i].opname, nopt, nplots, ntts, nper, idtp);
   printf(" sys_opname=%s, nopt=%d, nplots=%d, ntts=%d, nper=%d, idtp=%d\n",
            plotts[i].opname, plotts[i].nopt, plotts[i].nplots,
            plotts[i].ntts,   plotts[i].nper, plotts[i].idtp);
  }
/**/
 fprintf(txt_out," in icpi18 for %s,  mcp3 version is %d:\n", 
                   plotts[which_one].opname, ver);
 fprintf(txt_out," title: %s\n", plotts[i].title);
 fprintf(txt_out,
        " opname=%s, nopt=%d, nplots=%d, ntts=%d, nper=%d, idtp=%d\n",
          plotts[i].opname, nopt, nplots, ntts, nper, idtp);
 fprintf(txt_out,
        "sys_ opname=%s, nopt=%d, nplots=%d, ntts=%d, nper=%d, idtp=%d\n",
          plotts[i].opname, plotts[i].nopt, plotts[i].nplots,
          plotts[i].ntts,   plotts[i].nper, plotts[i].idtp);
 if (ascii_out)
  fprintf(icp_in_plotts, " %9.9s %d %d %d %d %d\n",
     plotts[i].opname, plotts[i].nopt, plotts[i].nplots, 
     plotts[i].idtp,   plotts[i].ntts, plotts[i].nper);
 else
  fwrite(plotts[i].opname,         sizeof(char),           9, icp_in_plotts),
  fwrite(&plotts[i].nopt,          sizeof(int),            1, icp_in_plotts),
  fwrite(&plotts[i].nplots,        sizeof(int),            1, icp_in_plotts),
  fwrite(&plotts[i].ntts,          sizeof(int),            1, icp_in_plotts),
  fwrite(&plotts[i].nper,          sizeof(int),            1, icp_in_plotts),
  fwrite(&plotts[i].idtp,          sizeof(int),            1, icp_in_plotts);
 if(SCREEN_OUT)
  {
   printf(" -----  The Plots -----\n\n");
   printf(" type  pts     min       max    nts  metr   eng   cfact    cconst\n");
  }
 fprintf(txt_out, " -----  The Plots -----\n\n");
 fprintf(txt_out,
         " type  pts     min       max    nts  metr   eng   cfact    cconst\n");
 for(j=0; j<nplots; j++)
  {
   plot_ptr = &plotts[which_one].plot[j];
   for (k=0; k<4; k++)plot_ptr->type[k]=*pin++; 
                      plot_ptr->type[k]='\0';
   FLIT(fl_npts)  npts=fl_npts;
   FLIT(pmin)
   FLIT(pmax)
   FLIT(fl_nts)   nts =fl_nts;
   plot_ptr->times=(TimeSeries*)calloc(nts,sizeof(TimeSeries));
   for (k=0;k<4;k++)plot_ptr->unitsm[k]=*pin++; 
   i=0, src_ptr = plot_ptr->unitsm;
   while ( (*src_ptr++ != ' ') && (++i < 4));
                    plot_ptr->unitsm[i]='\0';
   for (k=0;k<4;k++)plot_ptr->unitse[k]=*pin++; 
   i=0, src_ptr = plot_ptr->unitse;
   while ( (*src_ptr++ != ' ') && (++i < 4));
                    plot_ptr->unitse[i]='\0';
   FLIT(cfact)
   FLIT(cconst)
   i = which_one;
   plot_ptr->npts   = npts = fl_npts;
   plot_ptr->nts    = nts  = fl_nts ;
   plot_ptr->pmin   = pmin;
   plot_ptr->pmax   = pmax;
   plot_ptr->cfact  = cfact;
   plot_ptr->cconst = cconst;
   if(SCREEN_OUT)
    printf(" %s, %3d, %8.4f, %8.4f, %2d,  %-4s, %-4s, %7.3f, %7.3f\n",
       plot_ptr->type,   plot_ptr->npts, plot_ptr->pmin,   plot_ptr->pmax,
       plot_ptr->nts,    plot_ptr->unitsm, plot_ptr->unitse, plot_ptr->cfact,
       plot_ptr->cconst);
   fprintf(txt_out, " %s, %3d, %8.4f, %8.4f, %2d,  %-4s, %-4s, %7.3f, %7.3f\n",
      plot_ptr->type,   plot_ptr->npts, plot_ptr->pmin,   plot_ptr->pmax,
      plot_ptr->nts,    plot_ptr->unitsm, plot_ptr->unitse, plot_ptr->cfact,
      plot_ptr->cconst);
   if (ascii_out)
    fprintf(icp_in_plotts,
          " %s, %3d, %8.4f, %8.4f, %2d,  %-4s, %-4s, %7.3f, %7.3f\n",
      plot_ptr->type,   plot_ptr->npts, plot_ptr->pmin,   plot_ptr->pmax,
      plot_ptr->nts,    plot_ptr->unitsm, plot_ptr->unitse, plot_ptr->cfact,
      plot_ptr->cconst);
   else
    fwrite( plot_ptr->type,    sizeof(char),           5, icp_in_plotts),
    fwrite(&plot_ptr->npts,    sizeof(int),            1, icp_in_plotts),
    fwrite(&plot_ptr->pmin,    sizeof(float),          1, icp_in_plotts),
    fwrite(&plot_ptr->pmax,    sizeof(float),          1, icp_in_plotts),
    fwrite(&plot_ptr->nts,     sizeof(int),            1, icp_in_plotts),
    fwrite( plot_ptr->unitsm,  sizeof(char),           5, icp_in_plotts),
    fwrite( plot_ptr->unitse,  sizeof(char),           5, icp_in_plotts),
    fwrite(&plot_ptr->cfact,   sizeof(float),          1, icp_in_plotts),
    fwrite(&plot_ptr->cconst,  sizeof(float),          1, icp_in_plotts);
  }
 if(SCREEN_OUT)
  {
   printf("\n\n -----  The Time Series -----\n\n");
   printf
     ("   tsid    type  idt    title      symb  valnm   nvpdt  locv  num_obs\n");
  }
 fprintf(txt_out, "\n\n -----  The Time Series -----\n\n");
 fprintf(txt_out, 
      "   tsid    type  idt    title      symb  valnm   nvpdt  locv  num_obs\n");
 totalts = plotts[i].plot[l=0].nts;
 plot_ptr = &plotts[which_one].plot[l];
 for(m=j=0; j<ntts; m++,j++)
  {
   strcat( strcat( strcpy(fname_bld, work_file), plotts[i].opname), ".");
   if (j==totalts) m=0,totalts += plotts[i].plot[++l].nts,
                       plot_ptr = &plotts[which_one].plot[l];
   times_ptr = &plot_ptr->times[m];
   for (k=0; k<8; k++)times_ptr->tsid[k]=*pin++; 
   i=0, src_ptr = times_ptr->tsid;
   while ( (*src_ptr++ != ' ') && (++i < 8));
                      times_ptr->tsid[i]='\0';
   for (k=0; k<4; k++)times_ptr->type[k]=*pin++; 
   i=0, src_ptr = times_ptr->type;
   while ( (*src_ptr++ != ' ') && (++i < 4));
                      times_ptr->type[i]='\0';
   FLIT(fl_idt)
   for (k=0; k<12; k++)times_ptr->tstl[k]=*pin++; 
   i=0, src_ptr = times_ptr->tstl;
   while ( (*src_ptr++ != ' ') && (++i < 12));
                       times_ptr->tstl[i]='\0';
   for (k=0; k<4;  k++)times_ptr->symbol[k]=*pin++; 
                       times_ptr->symbol[1]='\0';
   for (k=0; k<8;  k++)times_ptr->valnm[k]=*pin++; 
   i=0, src_ptr = times_ptr->valnm;
   while ( (*src_ptr++ != ' ') && (++i < 8));
                       times_ptr->valnm[i]='\0';
   strcat(fname_bld,times_ptr->tsid);
/*   strcat( strcat( strcat(fname_bld, strlen(times_ptr->valnm) ? 
                                            times_ptr->valnm  :
                                            times_ptr->type),
                                            ".bin."), file_suffix);
*/
   strcat(fname_bld, strlen(times_ptr->valnm)? times_ptr->valnm :times_ptr->type);
   sprintf (dummy_str,"%d",j) ;
   strcat(fname_bld,dummy_str) ;/*add j to make filename unique*/
   strcat(fname_bld,".bin.") ;
   strcat(fname_bld,file_suffix) ;

   if ((n=strlen(fname_bld))>255)
    {
     printf(" NEED TO MAKE FILENAME DIMENSION IN TIMESERIES STRUCTURE IN ICP18"
            " LARGER\n At Least %d!!!\n", strlen(fname_bld)+1);
     exit(0);
    }
   else
    {
     if (n>max_filenm_len) max_filenm_len = n;
     strcpy(times_ptr->filename, fname_bld);
    }
   if(!(tst_file=fopen(times_ptr->filename, "w")))
    {
     printf("in icpi18: cannot open %s!!\n\n",
                times_ptr->filename); return;
    }
   fclose(tst_file);
   FLIT(fl_nvpdt)
   FLIT(fl_locv)
   i = which_one;
   times_ptr->nmlen   = n;
   times_ptr->idt     = fl_idt;
   times_ptr->nvpdt   = fl_nvpdt;
   times_ptr->locv    = fl_locv;
   times_ptr->num_obs = ((*ldarun-*idarun)*24+(*lhrrun-*ihrrun))/times_ptr->idt;
   times_ptr->count   = 0;
   if(SCREEN_OUT)
    printf(" %-8s, %-4s, %3d, %-12s, %s, %-8s, %4d, %4d, %7d\n",
             times_ptr->tsid,   times_ptr->type,
             times_ptr->idt,    times_ptr->tstl,
             times_ptr->symbol, times_ptr->valnm,
             times_ptr->nvpdt,  times_ptr->locv, times_ptr->num_obs);
   fprintf(txt_out, " %-8s, %-4s, %3d, %-12s, %s, %-8s, %4d, %4d, %7d\n"
                    "   idarun = %d, ldarun = %d, ihrrun = %d, lhrrun = %d\n",
            times_ptr->tsid,   times_ptr->type,
            times_ptr->idt,    times_ptr->tstl,
            times_ptr->symbol, times_ptr->valnm,
            times_ptr->nvpdt,  times_ptr->locv, times_ptr->num_obs,
            *idarun, *ldarun, *ihrrun, *lhrrun );
   if (ascii_out)
    fprintf(icp_in_plotts, " %-8s, %-4s, %3d, %-12s, %s, %-8s, %4d, %4d, %7d\n"
                           "%s\n",
            times_ptr->tsid,   times_ptr->type,
            times_ptr->idt,    times_ptr->tstl,
            times_ptr->symbol, times_ptr->valnm,
            times_ptr->nvpdt,  times_ptr->locv, times_ptr->num_obs,
            times_ptr->filename);
   else
    fwrite( times_ptr->tsid,    sizeof(char),  9, icp_in_plotts),
    fwrite( times_ptr->type,    sizeof(char),  5, icp_in_plotts),
    fwrite(&times_ptr->idt,     sizeof(int),   1, icp_in_plotts),
    fwrite( times_ptr->tstl,    sizeof(char), 13, icp_in_plotts),
    fwrite( times_ptr->symbol,  sizeof(char),  2, icp_in_plotts),
    fwrite( times_ptr->valnm,   sizeof(char),  9, icp_in_plotts),
    fwrite(&times_ptr->nvpdt,   sizeof(int),   1, icp_in_plotts),
    fwrite(&times_ptr->locv,    sizeof(int),   1, icp_in_plotts),
    fwrite(&times_ptr->nmlen,   sizeof(int),   1, icp_in_plotts),
    fwrite(&times_ptr->num_obs, sizeof(int),   1, icp_in_plotts),
    fwrite( times_ptr->filename,sizeof(char),  n, icp_in_plotts);
  }
 fclose(icp_in_plotts);

/**/
 if(SCREEN_OUT)
  {
   printf("\n----- The Filenames -----\n\n");
   for (i=0; i< num_plotts;            i++)
   for (j=0; j< plotts[i].nplots;      j++)
   for (k=0; k< plotts[i].plot[j].nts; k++)
   printf("%s\n", plotts[i].plot[j].times[k].filename);
   printf("\n leaving icpi18:\n\n");
  }
/**/
 fprintf(txt_out, "\n----- The Filenames -----\n\n");
 for (i=0; i< num_plotts;            i++)
 for (j=0; j< plotts[i].nplots;      j++)
 for (k=0; k< plotts[i].plot[j].nts; k++)
 fprintf(txt_out, "%-*s : %7d obs.\n", max_filenm_len,
                                          plotts[i].plot[j].times[k].filename,
                                          plotts[i].plot[j].times[k].num_obs);
 fprintf(txt_out, "\n leaving icpi18!\n\n");
 fclose(txt_out);
 return;
}
/*****************************************************************************
 * icp18 - called from ex18, gives periodic values for PLOTTS 
      CALL ICP18(OPNAME, PO)
 *****************************************************************************/

void icp18(opname, po, d, ilocd, ida/*j1*/, idadat, ihr, noutz, noutds) 
char *opname, *po;
float  *d;
int *ilocd, *ida, *idadat, *ihr, *noutz, *noutds;
{
 int i, j, k, l, sum_num, nobs, nhrs;
 int lcl_year, lcl_month, lcl_day, lcl_hour;
 int prt_day, prt_hr, lcl_nlocd;
 int k_plotts, n_plots, n_times, ntts_off, lcl_idt;
 int int_out_array[6], first_plotts;
 float sum_px, *out_array;
 char *src_ptr=opname, lcl_opname[9], tzone[5];
 int TimeZone;                                           //cfan
 Joint jnflt;

 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ') && (++i < 8));
 lcl_opname[i] ='\0';
 k_plotts=0;
 while( strcmp(lcl_opname, plotts_opnames[k_plotts]) &&
        k_plotts < num_plotts) k_plotts++;
 if (k_plotts == num_plotts)
  {
   printf("in icpi18 for %s, cannot find in plotts_opnames table!\n",lcl_opname);
   return;
  }
 strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_plotts);
 out_array = plotts_data[k_plotts];
 jnflt.flt = out_array[PLOTTS_VARS-1];
 first_plotts=jnflt.fxd;
 if(!(txt_out=fopen(work_file, "a")))
    printf("in icpi18: cannot open %s for appending!!\n\n", work_file);

//cfan  MDYH1(idadat, ihr, &lcl_month, &lcl_day, &lcl_year, &lcl_hour,
//cfan       noutz, noutds, tzone);

  MDYH1(idadat, ihr, &lcl_month, &lcl_day, &lcl_year, &lcl_hour,
       noutz, noutds, &TimeZone);        
  utzc.int_utzc = TimeZone;
  strncpy(tzone,utzc.chr_utzc,4) ;
  tzone[4] = '\0';

//cfan

 if(!(lcl_year%4) && ((lcl_year%100) || (lcl_year%400)))plotts_leap_yr=1;
   else plotts_leap_yr=0;
 nhrs = mons[plotts_leap_yr][lcl_month]*24;
 if(SCREEN_OUT)
  {
   printf(" idadat, ihr,  lcl_mo, day, year, hr,  tz, tds,   zn\n"
          " %5d,  %2d,     %2d,    %2d, %d, %2d,  %3d, %d,   %4.4s\n",
         *idadat, *ihr, lcl_month, lcl_day, lcl_year, lcl_hour,
         *noutz, *noutds, tzone);
   printf(" icp18: %s found --", lcl_opname);
   printf(" ida= %d, idadat= %d, ihr= %d\n", *ida, *idadat, *ihr);
   printf(          "  nlocd  idt nvpdt  locv  ilocd  off  nobs   type\n");
  }
 fprintf(txt_out, 
        " idadat, ihr,  lcl_mo, day, year, hr,  tz, tds,   zn\n"
        " %5d,  %2d,     %2d,    %2d, %d, %2d,  %3d, %d,   %4.4s\n",
       *idadat, *ihr, lcl_month, lcl_day, lcl_year, lcl_hour,
       *noutz, *noutds, tzone);
 fprintf(txt_out, " icp18: %s found --", lcl_opname);
 fprintf(txt_out, " ida= %d, idadat= %d, ihr= %d\n", 
                   *ida, *idadat, *ihr);
 fprintf(txt_out, "  nlocd  idt nvpdt  locv  ilocd  off  nobs   type\n");
 n_plots=plotts[k_plotts].nplots;
 ntts_off = 0;
 for(i=0; i< n_plots; i++)
  {
   n_times = plotts[k_plotts].plot[i].nts;
   for ( j=0; j < n_times; j++)
    {
     times_ptr = &plotts[k_plotts].plot[i].times[j];
     lcl_idt = times_ptr->idt;
     times_ptr->nlocd =
     ( (((*ida-*idadat)*24)/lcl_idt)*times_ptr->nvpdt +
        ((*ihr-1)          /lcl_idt)*times_ptr->nvpdt + 
        times_ptr->locv +
        ilocd[ntts_off++] -2);
     nobs = nhrs/lcl_idt;
     times_ptr->count += nobs;
     if(SCREEN_OUT)
      printf(" %5d,  %3d, %2d,     %2d, %5d, %2d,  %3d,   %4.4s\n",
             times_ptr->nlocd, lcl_idt, times_ptr->nvpdt, times_ptr->locv,
             ilocd[ntts_off-1], ntts_off-1, nobs, times_ptr->type);
     fprintf(txt_out, " %5d,  %3d, %2d,     %2d, %5d, %2d,  %3d,   %4.4s\n",
            times_ptr->nlocd, lcl_idt, times_ptr->nvpdt, times_ptr->locv,
            ilocd[ntts_off-1], ntts_off-1, nobs, times_ptr->type);
     if(!(icp_in_plotts=fopen(times_ptr->filename, "a")))
      {
       printf("in icpi18: cannot open %s!!\n\n", times_ptr->filename);
       return;
      }
     prt_day=lcl_day, prt_hr=lcl_idt, lcl_nlocd=times_ptr->nlocd;
     if(ascii_out)
      {
       for(l=0; l<nobs;l++)
        {
         fprintf(icp_in_plotts," %4d  %2d  %2d  %2d  %10.4f\n", 
                            lcl_year, lcl_month, prt_day, prt_hr, d[lcl_nlocd]);
         if(first_plotts < 6)
          {
           if(SCREEN_OUT)
            printf(" %4d  %2d  %2d  %2d  %10.4f\n", 
                             lcl_year, lcl_month, prt_day, prt_hr, d[lcl_nlocd]);
           fprintf(txt_out," %4d  %2d  %2d  %2d  %10.4f\n", 
                            lcl_year, lcl_month, prt_day, prt_hr, d[lcl_nlocd]);
          }
         lcl_nlocd+=times_ptr->nvpdt;
         if((prt_hr += lcl_idt) > 24) prt_hr=lcl_idt, prt_day++;
        }
      }
     else
      {
       for(l=0; l<nobs;l++)
        {
/*
         fwrite((char *)&lcl_year,     sizeof(int),   1, icp_in_plotts),
         fwrite((char *)&lcl_month,    sizeof(int),   1, icp_in_plotts),
         fwrite((char *)&prt_day,      sizeof(int),   1, icp_in_plotts),
         fwrite((char *)&prt_hr,       sizeof(int),   1, icp_in_plotts),
         fwrite((char *)&d[lcl_nlocd], sizeof(float), 1, icp_in_plotts),
         fwrite("\n   ",               1,             4, icp_in_plotts);
*/
         int_out_array[0] = lcl_year,
         int_out_array[1] = lcl_month,
         int_out_array[2] = prt_day,
         int_out_array[3] = prt_hr,
         jnflt.flt        = d[lcl_nlocd],
         int_out_array[4] = jnflt.fxd,
         memcpy((void *)&int_out_array[5],"\n   ",4),
         fwrite((char *)&int_out_array, sizeof(int), 6, icp_in_plotts);
         if(first_plotts < 6)
          {
           if(SCREEN_OUT)
            printf(" %4d  %2d  %2d  %2d  %10.4f\n", 
                             lcl_year, lcl_month, prt_day, prt_hr, d[lcl_nlocd]);
           fprintf(txt_out," %4d  %2d  %2d  %2d  %10.4f\n", 
                            lcl_year, lcl_month, prt_day, prt_hr, d[lcl_nlocd]);
          }
         lcl_nlocd+=times_ptr->nvpdt;
         if((prt_hr += lcl_idt) > 24) prt_hr=lcl_idt, prt_day++;
        }
      }
     if ( (*idadat + mons[plotts_leap_yr][lcl_month]) >= lcl_ldarun)
      fprintf(txt_out," %64s: %6d\n", times_ptr->filename, times_ptr->count);
     fclose(icp_in_plotts); 
    }
  }
 jnflt.fxd = ++first_plotts;
 out_array[PLOTTS_VARS-1] = jnflt.flt;
 fclose(txt_out);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_plotts/RCS/icp18.c,v $";
 static char rcs_id2[] = "$Id: icp18.c,v 1.4 2006/04/20 14:36:19 xfan Exp $";}
/*  ===================================================  */

}
