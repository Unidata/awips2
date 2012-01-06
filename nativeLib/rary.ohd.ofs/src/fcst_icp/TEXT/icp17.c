/*****************************************************************************
 * icpi17 - called from ex17, gives initial values for WY-PLOT
      CALL ICPI17(OPNAME, NPLOT, LPLOTQ, LPX, IDT, AREA, PMAX, PO(15))
 *****************************************************************************/

#ifdef MAIN_ICP_C
#undef MAIN_ICP_C
#endif
#include <icpcommon/icp_out.h>

static int init_17=0;

void icpi17(opname, nplot, lplotq, lpx, idt, area, pmax, po)
int *nplot, *lplotq, *lpx, *idt;
float *area, *pmax;
char *opname, *po;
{
 int i, j, k, k_wyp, opr_num, nxtp, lcl_mp;
 Joint jnflt;
 char *src_ptr=opname, lcl_opname[9];
 char ts_int_name[9], ts_type[5], ts_type_name[13], ts_symbol[5];
 char *flow_pt_name=po-56, flow_point[21];

 for (i=0; i<20; i++) /*v->flow_point[i]=*/flow_point[i]=*flow_pt_name++;
 flow_point[20]='\0';
 strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_wyp);
 if(!(txt_out=fopen(work_file, init_17?"a":"w")))
    printf("in icpi17: cannot open %s for %s!!\n\n",
             work_file,init_17?"appending":"writing");

 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ')
         && (++i < 8));
 lcl_opname[i] = '\0';
 i=0;
 while( strcmp(lcl_opname, wyp_opnames[i]) && i < num_wyp) i++;
 if (i== num_wyp)
  {
   printf(" in icpi17 for %s, cannot find in wyp_opnames table!\n",lcl_opname);
   return;
  }
 if(!(icp_in_wyp=fopen(file_name_wyp[i], "w")))
  {
   printf("in icpi17: cannot open %s!!\n\n",file_name_wyp[i]); return;
  }
 for (j=0; j<WYP_VARS; j++) wyp_data[i][j]=0.0;
/*
 printf(" in icpi17 for %s:\n", lcl_opname);
 printf(" flow_point: %s\n", flow_point);
 printf(" lcl_opname=%s, nplot=%d, lpx=%d, area=%.1f, pmax=%.1f, idt=%d\n",
          lcl_opname, *nplot, *lpx, *area, *pmax, *idt);
*/
 fprintf(txt_out, " in icpi17 for %s:\n", lcl_opname);
 fprintf(txt_out, " flow_point: %s\n", flow_point);
 fprintf(txt_out,
        " lcl_opname=%s, nplot=%d, lpx=%d, area=%.1f, pmax=%.1f, idt=%d\n",
          lcl_opname, *nplot, *lpx, *area, *pmax, *idt);
 if (ascii_out)
  fprintf(icp_in_wyp, " %s\n %9.9s %d %d %d %d %.1f %.3f\n",
     flow_point, lcl_opname, *nplot, *lplotq, *lpx, *idt, *area, *pmax);
 else
  fwrite(flow_point,     sizeof(char),          21, icp_in_wyp),
  fwrite(opname,         sizeof(char),           9, icp_in_wyp),
  fwrite((char *)nplot,  sizeof(int),            1, icp_in_wyp),
  fwrite((char *)lplotq, sizeof(int),       *nplot, icp_in_wyp),
  fwrite((char *)lpx,    sizeof(int),            1, icp_in_wyp),
  fwrite((char *)idt,    sizeof(int),            1, icp_in_wyp),
  fwrite((char *)area,   sizeof(float),          1, icp_in_wyp),
  fwrite((char *)pmax,   sizeof(float),          1, icp_in_wyp),
  fwrite(        po,     sizeof(float), (*nplot)*7, icp_in_wyp);
 for(j=0, pin=po; j<*nplot; j++)
  {
   for (k=0; k<8; k++)ts_int_name[k]=*pin++; ts_int_name[k]='\0';
   for (k=0; k<4; k++)ts_type[k]=*pin++; ts_type[k]='\0';
   for (k=0; k<12; k++)ts_type_name[k]=*pin++; ts_type_name[k]='\0';
   for (k=0; k<4; k++)ts_symbol[k]=*pin++; ts_symbol[k]='\0';
/*
   printf("    %d  %s  %s  %s    %s\n",
    lplotq[j],ts_int_name, ts_type, ts_type_name, ts_symbol);
*/
   fprintf(txt_out, "    %d  %s  %s  %s    %s\n",
    lplotq[j],ts_int_name, ts_type, ts_type_name, ts_symbol);
   if (ascii_out)
    fprintf(icp_in_wyp, "    %d  %s  %s  %s    %s\n",
     lplotq[j],ts_int_name, ts_type, ts_type_name, ts_symbol);
  }
 fclose(icp_in_wyp);

/*
 printf(" leaving icpi17:\n\n");
*/
 fprintf(txt_out, " leaving icpi17:\n\n");
 fclose(txt_out);
 return;
}
/*****************************************************************************
 * icp17 - called from ex17, gives periodic values for WY-PLOT
      CALL ICPI17(OPNAME, NPLOT, LPLOTQ, LPX, IDT, AREA, PMAX, PO(15))
 *****************************************************************************/

void icp17(opname, month, kyear, nplot, lplotq, d, lpx, px, idt)
int *month, *kyear, *nplot, *lplotq, *idt, *lpx;
char *opname;
float *px, *d;
{
 int i, j, k, num_obs, lcl_year=*kyear, lcl_month=*month, sum_num;
 int k_wyp;
 int int_out_array[5];
 float sum_px, *out_array;
 char *src_ptr=opname, lcl_opname[9];
 Joint jnflt;

 i=0;
 while ( ((lcl_opname[i] = tolower(*src_ptr++)) != ' ') && (++i < 8));
 lcl_opname[i] ='\0';
 k_wyp=0;
 while( strcmp(lcl_opname, wyp_opnames[k_wyp]) && k_wyp < num_wyp) k_wyp++;
 if (k_wyp == num_wyp)
  {
   printf(" in icp17 for %s, cannot find in wyp_opnames table!\n",lcl_opname);
   return;
  }
 out_array = wyp_data[k_wyp];
 jnflt.flt = out_array[WYP_VARS-1];
 first_wyp=jnflt.fxd;

 if(first_wyp==0 || lcl_month==1)
  if((!(lcl_year%4) && (lcl_year%100)) || !(lcl_year%400))wyp_leap_yr=1;
  else wyp_leap_yr=0;
 num_obs = mons[wyp_leap_yr][lcl_month];
/* ======Maint Req    - 27FEB97
   ===== account for absent Rain+Melt ts if(*idt==0), don't divide by it === */
 if (*idt && (*idt != 24)) sum_num = 24/(*idt);else sum_num =0;
 strcat( strcat( strcpy(work_file,intrfc_dir), "/"), text_out_wyp);
 if(!(txt_out=fopen(work_file, "a")))
    printf("in icpi17: cannot open %s for appending!!\n\n", work_file);

 if(!(icp_in_wyp=fopen(file_name_wyp[k_wyp], "a")))
  {
   printf("in icp17: cannot open %s!!\n\n",file_name_wyp[k_wyp]); return;
  }
 if(ascii_out)
  {
   fprintf(icp_in_wyp," %2d  %4d  %2d  %2d\n", *month, *kyear, num_obs, *lpx);
   for(j=0; j<num_obs;j++)
    {
/* ======Maint Req # 211  - 27FEB97
   ===== account for absent Rain+Melt ts *lpx==0, don't write it ========= */
     if(*lpx)
      {
       if(sum_num)
        {
         sum_px=0.0;
         for( i=j*sum_num; i<(j+1)*sum_num; i++) sum_px +=px[i];
         fprintf(icp_in_wyp, seven_3, sum_px);
        }
       else fprintf(icp_in_wyp, seven_3, px[j]);
      } /* MR # 211 ========= */
     for (i=0; k=lplotq[i], i< *nplot; i++)
          fprintf(icp_in_wyp,seven_2,d[k+j-1]);
     fprintf(icp_in_wyp, "\n");
    }
  }
 else
  {
/*
   fwrite((char *)month, sizeof(int), 1, icp_in_wyp),
   fwrite((char *)kyear, sizeof(int), 1, icp_in_wyp),
   fwrite((char *)&num_obs, sizeof(int), 1, icp_in_wyp),
   fwrite((char *)lpx, sizeof(int), 1, icp_in_wyp),
   fwrite("\n   ", 1, 4, icp_in_wyp);
*/
   int_out_array[0] = lcl_month,
   int_out_array[1] = lcl_year,
   int_out_array[2] = num_obs,
   int_out_array[3] = *lpx,
   memcpy((void *)&int_out_array[4],"\n   ",4),
   fwrite((char *)&int_out_array, sizeof(int), 5, icp_in_wyp);
   for(j=0; j<num_obs;j++)
    {
/* ======Maint Req # 211   - 27FEB97
   ===== account for absent Rain+Melt ts if(*lpx==0), don't write it ======= */
     if(*lpx)
      {
       if(sum_num)
        {
         sum_px=0.0;
         for( i=j*sum_num; i<(j+1)*sum_num; i++) sum_px +=px[i];
         fwrite((char *)&sum_px, sizeof(float), 1, icp_in_wyp);
        }
       else fwrite((char *)&px[j  ], sizeof(float), 1, icp_in_wyp);
      } /* MR # 211 ========= */
     for (i=0; k=lplotq[i], i< *nplot; i++)
        fwrite((char *)&d[k+j-1], sizeof(float), 1, icp_in_wyp);
     fwrite("\n   ", 1, 4, icp_in_wyp);
    }
  }
 if(first_wyp < 6)
  {
   fprintf(txt_out, " in icp17(first_wyp=%d)\n", first_wyp);
   fprintf(txt_out," %2d  %4d  %2d  %2d  %d  %d\n", 
					  *month, *kyear, num_obs, *lpx, *idt, sum_num);
/*
   printf(" in icp17(first_wyp=%d)\n", first_wyp);
   print(" %2d  %4d  %2d  %2d  %d  %d\n", 
					  *month, *kyear, num_obs, *lpx, *idt, sum_num);
*/
   for(j=0; j<num_obs;j++)
    {
/* ======Maint Req # 211 - 27FEB97
   ===== account for absent Rain+Melt ts *lpx==0, don't write it ========= */
     if(*lpx)
      {
       if(sum_num)
        {
         sum_px=0.0;
         for( i=j*sum_num; i<(j+1)*sum_num; i++) sum_px +=px[i];
         fprintf(txt_out, seven_3, sum_px);
/*       printf(seven_3, sum_px); */
        }
      else
       {
        fprintf(txt_out, seven_3, px[j]);
/*      printf(seven_3, px[j]); */
       }
      } /* MR # 211 ========= */
     for (i=0; k=lplotq[i], i< *nplot; i++)
          fprintf(txt_out,seven_2,d[k+j-1]) /* ,
          printf(seven_2,d[k+j-1]) */ ;
     fprintf(txt_out, "\n") /* , printf("\n") */ ;
    }
  }
 jnflt.fxd = ++first_wyp;
 out_array[WYP_VARS-1] = jnflt.flt;
 fclose(icp_in_wyp); fclose(txt_out);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_icp/RCS/icp17.c,v $";
 static char rcs_id2[] = "$Id: icp17.c,v 1.3 1997/04/08 20:28:16 page Exp $";}
/*  ===================================================  */

}
