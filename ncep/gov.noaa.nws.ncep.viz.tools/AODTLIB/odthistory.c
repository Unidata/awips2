/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

#define maxline 450
#define histlen 181
#define histnum 38

int aodtv64_readhistoryfile(void);
int aodtv64_inserthistoryrec( int *, int * );
int aodtv64_writehistoryfile(void);
int aodtv64_commenthistoryrec( char * ); 

int aodtv64_readhistoryfile(void)
/* Read history file from ASCII file into structure odthistoryfirst_v64
    Inputs  : none
    Outputs : global structure odthistoryfirst_v64
*/
{
  FILE *fp;
  int  count=0,iok,bytes;
  int  time,escene,cscene,esceneo,csceneo,r9,r8,rlb,land;
  int  rcb,rcbv,efft,cfft,cwring,apos,tief,csat;
  char line[maxline]; 
  char *cdate;
  char *comm;
  float rawt,rawto,finalt,f3t,ci,eye,cloud,cloud2,cw,lat,lon,esize,estdv,csym;
  float ciadjp,tier,tiea,sst,rmw;
  double juldate;
  struct odtdata *odthistory;

  /* get file information */
  fp=fopen(hfile_v64,"r");
  if(fp==0) {
    odthistoryfirst_v64=0;
    /* return -1; */
    return 0; 
  }

  comm=(char *)calloc((size_t)50,sizeof(char)); 
  cdate=(char *)calloc((size_t)12,sizeof(char)); 
  odthistoryfirst_v64=(struct odtdata *)malloc(sizeof(struct odtdata));
  odthistory=odthistoryfirst_v64;

  /* read history file, quit at EOF */
  while(fgets(line,maxline,fp) != NULL) {
    if(count>0) {
      odthistory->nextrec=(struct odtdata *)malloc(sizeof(struct odtdata));
      odthistory=odthistory->nextrec;
    }
    iok=sscanf(line,"%s %d %lf %f %f %f %f %f %f %f %f %f %f %f %f %f %f %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %f %d %f %f %f %f %50c%n",
                 cdate,&time,&juldate,
                 &rawto,&rawt,&finalt,&f3t,&ci,&eye,&cloud,&cloud2,&cw,&lat,&lon,&esize,&estdv,&csym,
                 &csat,&escene,&cscene,&esceneo,&csceneo,&r9,&r8,&rlb,&land,&efft,&cfft,
                 &rcb,&rcbv,&cwring,&apos,&ciadjp,&tief,&tier,&tiea,&sst,&rmw,comm,&bytes);
    if(iok<histnum) {
//printf("NATIVE...line=%s\n", line);
//printf("NATIVE...aodtv64_readhistoryfile...iok=%d, finalt = %f, f3t=%f, sst=%f, rmw=%f, comm=%s,bytes=%d\n", iok, finalt, f3t, sst, rmw, comm, bytes); 
      count=-1;
      break;
    } else if (iok==histnum) {
      comm[0]='\0';
    } else {
      comm[bytes-histlen-1]='\0';
    }
    odthistory->IR.date=aodtv64_cmonth2julian(cdate);
    odthistory->IR.time=time;
    odthistory->IR.TrawO=rawto;
    odthistory->IR.Traw=rawt;
    odthistory->IR.Tfinal=finalt;
    odthistory->IR.Tfinal3=f3t;
    odthistory->IR.CI=ci;
    odthistory->IR.eyet=eye;
    odthistory->IR.cloudt=cloud;
    odthistory->IR.cloudt2=cloud2;
    odthistory->IR.cwcloudt=cw;
    odthistory->IR.latitude=lat;
    odthistory->IR.longitude=lon;
    odthistory->IR.eyesize=esize;
    odthistory->IR.eyestdv=estdv;
    odthistory->IR.cloudsymave=csym;
    odthistory->IR.sattype=csat;
    odthistory->IR.eyescene=escene;
    odthistory->IR.cloudscene=cscene;
    odthistory->IR.eyesceneold=esceneo;
    odthistory->IR.cloudsceneold=csceneo;
    odthistory->IR.rule9=r9;
    odthistory->IR.rule8=r8;
    odthistory->IR.LBflag=rlb;
    odthistory->IR.land=land;
    odthistory->IR.eyefft=efft;
    odthistory->IR.cloudfft=cfft;
    odthistory->IR.ringcb=rcb;
    odthistory->IR.ringcbval=rcbv;
    odthistory->IR.autopos=apos;
    odthistory->IR.cwring=cwring;
    odthistory->IR.CIadjp=ciadjp;
    odthistory->IR.TIEflag=tief;
    odthistory->IR.TIEraw=tier;
    odthistory->IR.TIEavg=tiea;
    odthistory->IR.sst=sst;
    odthistory->IR.rmw=rmw;
    strcpy(odthistory->IR.comment,comm);
    odthistory->IR.comment[strlen(comm)]='\0';  
    odthistory->nextrec=0; /* make pointer for last record equal to 0 */
    count++;
  }

  if(count==0) {
    /* this guards against an empty history file */
    odthistoryfirst_v64=0;
  }
  fclose(fp);
  free(comm); comm=NULL;
  free(cdate); cdate=NULL;
  return count;
}

int aodtv64_inserthistoryrec(int *modified,int *ioflag)
/* Insert or overwrite a record in a history file.  Global structure
   odthistory will be modified and the ASCII history file will be
   rewritten in another routine.
    Inputs  : none
    Outputs : modified   - number of records modified
              ioflag     - overwrote (1) or insert (2) flag value 
    Return  : record number overwritten will be return value
*/
{
  int modcount=0,count=0;
  double curtime,xtime;
  logical found=FALSE;
  struct odtdata *prevrec,*odttemp,*odthistory,*tempcurrent;
  struct odtdata *newcurrent;

  newcurrent=(struct odtdata *)malloc(sizeof(struct odtdata));

  odthistory=odthistoryfirst_v64;
  tempcurrent=odtcurrent_v64;
  memcpy(newcurrent,odtcurrent_v64,sizeof(struct odtdata));

  curtime=aodtv64_calctime(odtcurrent_v64->IR.date,odtcurrent_v64->IR.time);

  *ioflag=0;
  prevrec=odthistory;                  /* save previous record pointer */
  while(odthistory!=NULL) {
    xtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if((xtime==curtime)&&(!found)) {
      /* OVERWRITE RECORD */
      odttemp=odthistory;
      if(odthistory==odthistoryfirst_v64) {
        /* overwrite record at beginning of record list */
        odthistoryfirst_v64=newcurrent;
      } else {
        prevrec->nextrec=newcurrent;
      }
      odthistory=newcurrent;
      odthistory->nextrec=odttemp->nextrec;
      prevrec=odthistory;              /* save previous record pointer */
      odthistory=odthistory->nextrec;  /* switch pointer to next record */
      found=TRUE;
      *ioflag=1;
    } else if((xtime>curtime)&&(!found)) {
      /* INSERT RECORD */
      if(odthistory==odthistoryfirst_v64) {
        /* insert at beginning of record list */
        odttemp=odthistory;
        odthistoryfirst_v64=newcurrent;   /* reset first history file pointer to newcurrent */
        odthistoryfirst_v64->nextrec=odttemp;  /* this is original first record */
      } else {
        /* insert between two records */
        prevrec->nextrec=newcurrent;    /* reset previous next record pointer to inserted record */
        newcurrent->nextrec=odthistory; /* set next record pointer for inserted record */
      }
      found=TRUE;
      *ioflag=2;
    } else {
      if(found) {
        /* previously found records to insert, so all records following
           the inserted record must be recalculated */
        ostartstr_v64=FALSE;
        odtcurrent_v64=odthistory;
        if((odtcurrent_v64->IR.land==1)&&(odtcurrent_v64->IR.Traw<1.0)) {
          aodtv64_initcurrent(1);
        } else {
          aodtv64_calcintensity();      /* recompute intensity */
        }
        odthistory=odtcurrent_v64;
        modcount++;
      }
      /* nothing yet... keep searching */
      prevrec=odthistory;              /* save previous record pointer */
      odthistory=odthistory->nextrec;  /* switch pointer to next record */
      count++;
    }
  }

  if(!found) {
    newcurrent->nextrec=0;       /* set next record pointer to 0 for end */
    if(odthistoryfirst_v64==odthistory) {
      /* record will be placed at start of new history structure */
      odthistoryfirst_v64=newcurrent;
      *ioflag=3;
    } else {
      /* record will be placed at end of history structure */
      prevrec->nextrec=newcurrent; /* reset previous next record pointer to inserted record */
      *ioflag=4;
    }
  } else {
    odtcurrent_v64=tempcurrent;
  }

  *modified=modcount;

  /* free(newcurrent);    will be freed as part of free(odthistoryfirst_v64) */
  return count;

}

/*=====================================================================*/

int aodtv64_writehistoryfile(void)
/* Write odthistory structure to ASCII history file.
   Inputs  : none
   Outputs : number of records written is return value
*/
{
  FILE *fp;
  int  count=0,iok;
  char *cdate; 
  double juldate,j1;
  struct odtdata *odthistory;

  odthistory=odthistoryfirst_v64;

  /* get file information */

  fp=fopen(hfile_v64,"w+");
  if(fp==0) {
    return -2;
  }
  cdate=(char *)calloc((size_t)12,sizeof(char));
  /* print history file to output ASCII history file */
  while (odthistory!=0) {

    (void)aodtv64_julian2cmonth(odthistory->IR.date,cdate);

    j1=(double)aodtv64_calctime( odthistory->IR.date, odthistory->IR.time );
    juldate=j1-(double)((int)j1/1000)*1000.0;

    iok=fprintf(fp,"%9s %6.6d %8.4lf %3.1f %3.1f %3.1f %3.1f %3.1f %6.2f %6.2f %6.2f %6.2f %6.2f %7.2f %5.1f %4.1f %4.1f %2d %1d %1d %2d %2d %1d %2.2d %1d %1d %2d %2d %1d %2d %3d %2d %7.2f %1d %6.1f %6.1f %5.1f %5.1f %-s\n",
                 cdate,odthistory->IR.time,juldate,
                 odthistory->IR.TrawO,odthistory->IR.Traw,odthistory->IR.Tfinal,odthistory->IR.Tfinal3,odthistory->IR.CI,
                 odthistory->IR.eyet,odthistory->IR.cloudt,odthistory->IR.cloudt2,odthistory->IR.cwcloudt,
                 odthistory->IR.latitude,odthistory->IR.longitude,
                 odthistory->IR.eyesize,odthistory->IR.eyestdv,odthistory->IR.cloudsymave,
                 odthistory->IR.sattype,odthistory->IR.eyescene,odthistory->IR.cloudscene,
                 odthistory->IR.eyesceneold,odthistory->IR.cloudsceneold,
                 odthistory->IR.rule9,odthistory->IR.rule8,odthistory->IR.LBflag,odthistory->IR.land,
                 odthistory->IR.eyefft,odthistory->IR.cloudfft,
                 odthistory->IR.ringcb,odthistory->IR.ringcbval,
                 odthistory->IR.cwring,odthistory->IR.autopos,
                 odthistory->IR.CIadjp,
		 odthistory->IR.TIEflag,odthistory->IR.TIEraw,odthistory->IR.TIEavg,odthistory->IR.sst,
                 odthistory->IR.rmw,
                 odthistory->IR.comment);
    if(iok<histlen) {
      count=-3;
      break;
    }
    odthistory=odthistory->nextrec;
    count++;
  }

  fclose(fp);
  free(cdate); cdate=NULL;
  return count;
}

int aodtv64_deletehistoryrec(int *modified)
/* Delete record(s) in a history file.  Routine will modify
   structure odthistory, which will then be rewritten to 
   ASCII history file in another subroutine.
    Inputs  : none : needs global variables for 
                     starttime_v64 - first date/time of record(s) delete
                     endtime_v64   - last date/time of record(s) delete
    Outputs : modified  - number of records modified
              global structure odthistory will be modified
              number of records deleted will be return value
*/
{
  int  modcount=0,count=0;
  double curtime;
  logical found=FALSE;
  struct odtdata *prevrec,*odthistory,*tempcurrent;

  odthistory=odthistoryfirst_v64;
  tempcurrent=odtcurrent_v64;

  prevrec=odthistory;                  /* save previous record pointer */
  while(odthistory!=0) {
    curtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    if((curtime>=starttime_v64)&&(curtime<=endtime_v64)) {
      /* record falls within time boundaries to delete */
      if(odthistory==prevrec) {
        /* deleting first record, need to reset first record pointer */
        odthistoryfirst_v64=prevrec->nextrec;
        prevrec=odthistory->nextrec;
      } else {
        /* reset previous next record pointer to next record */
        prevrec->nextrec=odthistory->nextrec;
      }
      count++;
      found=TRUE;
    } else {
      /* record not within time boundaries */
      if(found) {
        /* previously found records to delete, so all records following
           the last deleted record must be recalculated */
        ostartstr_v64=FALSE;
        odtcurrent_v64=odthistory;
        if((odtcurrent_v64->IR.land==1)&&(odtcurrent_v64->IR.Traw<1.0)) {
          aodtv64_initcurrent(1);
        } else {
          aodtv64_calcintensity();      /* recompute intensity */
        }
        odthistory=odtcurrent_v64;
        modcount++;
      }
      /* reset prevrec pointer to current record */
      prevrec=odthistory;
    }
    /* reset record pointer ot next record */
    odthistory=odthistory->nextrec;
  }

  if(found) odtcurrent_v64=tempcurrent;
  *modified=modcount;
  return count;
}

/*=====================================================================*/

int aodtv64_listhistory(struct odtdata *historyrec,int itype,char *srcID,char *string)
/* List the ASCII history file between given date/times.
    Inputs  : historyrec - pointer to next record in history structure
              string     - character string containint comment for history file record
    Outputs : none
*/
{
  int  date,time,cscene,escene;
  int  r9,r8,land,listland=1;
  int  fixtype,commlen;
  int  rawflag,finalflag,stormID,iday,imon,iyear,ibasin,isat;
  int  cix,vmax,presx,tnox,trawx,teye,tcloud,tnoxtp;
  int  posconf,vconf,presconf,ciconf,rule89;
  char *cdate,*scene,*clatlon,*crmw,*commnt,*user;
  char *ctime,*cdt,*clat,*clon,*cbasin,*ctnox,*csite,*cursat;
  char *cns,*cew;
  char r89[4][3]={ "  ","R8","R9","89" };
  char cyn[4][4]={ "OFF","WKN"," ON","N/A" };
  char esc[6][7]={ "EYE/C ", "EYE/P ", "EYE/L ", "EYE/LR", "EYE/R ", "EYE/OB" };
  char csc[6][7]={ "UNIFRM", "EMBC  ", "IRRCDO", "CRVBND", "SHEAR ", "      " };
  char cscA[6][5]={ " CDO", "EMBC", "ICDO", "CBND", "SHER", "    " };
  char fix[8][6]={ " MAN ","FCST ", "LAPL ", "WARM ", "SPRL ", "COMBO", "EXTRP", " N/A " };
/*
  char cr8[30][10]={ "NO LIMIT ","0.5T/6hr ","1.0T/6hr ","1.5T/12hr","2.0T/12hr","2.5T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour",
                     "NO LIMIT ","0.5T/6hr ","1.5T/6hr ","2.0T/12hr","2.5T/12hr","3.0T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour",
                     "NO LIMIT ","0.5T/6hr ","0.5T/6hr ","1.0T/12hr","1.5T/12hr","2.0T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour" };
*/
  char cr8[30][10]={ "NO LIMIT ","0.5T/6hr ","1.2T/6hr ","1.7T/12hr","2.2T/12hr","2.7T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour",
                     "NO LIMIT ","0.5T/6hr ","1.7T/6hr ","2.2T/12hr","2.7T/12hr","3.2T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour",
                     "NO LIMIT ","0.5T/6hr ","0.7T/6hr ","1.2T/12hr","1.7T/12hr","2.2T/24hr",
                     "         ","         ","0.1T/hour","0.5T/hour" };
  float rawt,rawto,finalt,f3t,ci,eye,cloud,lat,lon,ciw,cip;
  float ciadjp,rmw;
  logical firstrec=TRUE;

  char *iout2; 

  iout2=(char *)calloc((size_t)5000,sizeof(char));
  cdate=(char *)calloc((size_t)12,sizeof(char));
  commnt=(char *)calloc((size_t)50,sizeof(char));
  clatlon=(char *)calloc((size_t)18,sizeof(char));
  crmw=(char *)calloc((size_t)7,sizeof(char));
  scene=(char *)calloc((size_t)7,sizeof(char));
  user=(char *)calloc((size_t)5,sizeof(char));
  ctime=(char *)calloc((size_t)5,sizeof(char));
  cdt=(char *)calloc((size_t)13,sizeof(char));
  clat=(char *)calloc((size_t)6,sizeof(char));
  clon=(char *)calloc((size_t)7,sizeof(char));
  cbasin=(char *)calloc((size_t)3,sizeof(char));
  ctnox=(char *)calloc((size_t)2,sizeof(char));
  csite=(char *)calloc((size_t)6,sizeof(char));
  cursat=(char *)calloc((size_t)7,sizeof(char));

  if(historyrec==0) {
    if(itype==-1) {
      /* original format history file listing */
      (void)sprintf(iout2,"                  --------Intensity-------  ---Tno Values--  -Tno/CI Rules-  -Temperature-                 \n");strcat(string,iout2);
      (void)sprintf(iout2,"           Time        Final/MSLPLat/Vmax   6hr 3hr Adj Ini   Cnstrnt  Wkng   Eye    Mean   Scene  EstRMW   Storm Location  Fix\n");strcat(string,iout2);
      (void)sprintf(iout2,"   Date    (UTC)   CI  MSLP /BiasAdj/(kts)  Ave Ave Raw Raw    Limit   Flag  Region  Cloud  Type    (km)     Lat     Lon    Mthd   Comments\n");strcat(string,iout2);
    } else {
      /* ATCF format listing */
      (void)sprintf(iout2,"\n");strcat(string,iout2);
    }
  } else {
    date=historyrec->IR.date;
    time=historyrec->IR.time;
    rawt=historyrec->IR.Traw;
    rawto=historyrec->IR.TrawO;
    finalt=historyrec->IR.Tfinal;
    f3t=historyrec->IR.Tfinal3;
    ci=historyrec->IR.CI;
    eye=historyrec->IR.eyet;
    cloud=historyrec->IR.cloudt;
    lat=historyrec->IR.latitude;
    lon=historyrec->IR.longitude;
    isat=historyrec->IR.sattype;
    cscene=historyrec->IR.cloudscene;
    escene=historyrec->IR.eyescene;
    land=historyrec->IR.land;
    r8=historyrec->IR.rule8;
    r9=historyrec->IR.rule9;
    ciadjp=historyrec->IR.CIadjp;
    rmw=historyrec->IR.rmw;
    fixtype=historyrec->IR.autopos;
    
    commlen=strlen(historyrec->IR.comment);
    strcpy(commnt,historyrec->IR.comment); 
    commnt[commlen]='\0';

    if((land==1)&&(ci<1.0)) listland=0;
  
    if(listland==0) {
      ciadjp=0.0;
      cip=0.0;
      ciw=0.0;
      strcpy(scene,"LAND  ");
      r8=6;
      r9=3;
      fixtype=7;
      strcpy(crmw,"  N/A "); 
    } else {
      if(ixdomain_v64==0&&firstrec) {
        idomain_v64=0;
        if(lon<0.0) idomain_v64=1;
        firstrec=FALSE;
      }
      cip=aodtv64_getpwval(0,ci);
      ciw=aodtv64_getpwval(1,ci);
      if((cscene==3)||(cscene==4)) {
        strcpy(scene,csc[cscene]);
      } else if(escene<6) {
        strcpy(scene,esc[escene]);
      } else {
        strcpy(scene,csc[cscene]);
      }
      if(r9==2) r9=0;
      r9=r9>2 ? 2 : r9;
      if((cscene<=5)&&(escene<=2)) {
        sprintf(crmw,"%3d IR",(int)rmw); 
      } else {
        strcpy(crmw,"  N/A "); 
      }
    }
    scene[strlen(scene)]='\0';
    crmw[strlen(crmw)]='\0';
  
    if(itype==-1) {
      /* original format history file listing */
      (void)aodtv64_julian2cmonth(date,cdate);
      sprintf(clatlon,"%6.2f %7.2f",lat,lon);
      clatlon[strlen(clatlon)]='\0'; 
      (void)sprintf(iout2,"%9s %6.6d  %3.1f %6.1f/ %+5.1f /%5.1f  %3.1f %3.1f %3.1f %3.1f  %8s  %3s  %6.2f %6.2f  %6s %6s %15s   %5s  %-s\n",
             cdate,time,ci,cip+ciadjp,ciadjp,ciw,finalt,f3t,rawt,rawto,cr8[r8],cyn[r9],eye,cloud,scene,crmw,clatlon,fix[fixtype],commnt);strcat(string,iout2);
    } else {
      /* ATCF format listing */
      rawflag=itype/1000;
      finalflag=(itype-(rawflag*1000))/100;
      stormID=itype-(rawflag*1000)-(finalflag*100);
      (void)aodtv64_yddmy(date,&iday,&imon,&iyear);
      sprintf(ctime,"%4.4i",time/100);
      ctime[strlen(ctime)]='\0'; 
      sprintf(cdt,"%4i%2.2d%2.2d%4s",iyear,imon,iday,ctime);
      cdt[strlen(cdt)]='\0'; 
      cns=lat>0.0 ? "N" : "S";
      cew=lon>0.0 ? "W" : "E";
      sprintf(clat,"%4i%1s",(int)(lat*100),cns);
      sprintf(clon,"%5i%1s",(int)(lon*100),cew);
      clat[strlen(clat)]='\0'; 
      clon[strlen(clon)]='\0'; 
      ibasin=aodtv64_oceanbasin(lat,lon);
      if(ibasin==0) {  /* atlantic */
        strcpy(cbasin,"AL");
        if(lat<0.0) strcpy(cbasin,"SH");
      }
      if(ibasin==1) {  /* west pacific */
        strcpy(cbasin,"WP");
        if(lat<0.0) {
          strcpy(cbasin,"SH");
        } 
      }
      if(ibasin==2) {  /* east pacific */
        strcpy(cbasin,"EP");
        if(lon>=140.0) strcpy(cbasin,"CP");
        if(lat<0.0) {
          strcpy(cbasin,"SH");
        }
      }
      if(ibasin==3) {  /* indian */
        strcpy(cbasin,"IO");
        if(lat<0.0) strcpy(cbasin,"SH");
      }
      cbasin[strlen(cbasin)]='\0';
      vmax=(int)(ciw);
      presx=(int)(cip+ciadjp);
      cix=(int)((ci+0.01)*10);
      if(rawflag==0) {
        trawx=(int)((rawt+0.01)*10);   /* adjusted Raw T# - default */
      } else {
        trawx=(int)((rawto+0.01)*10);  /* unadjusted Raw T# */
      }
      if(finalflag==0) {
        tnox=(int)((finalt+0.01)*10); strcpy(ctnox,"T"); tnoxtp=6;   /* Final T# - default */
      } else {
        tnox=(int)((f3t+0.01)*10); strcpy(ctnox,"L"); tnoxtp=3;      /* 3hr Final T# */
      }
      ctnox[strlen(ctnox)]='\0';
      teye=(int)(eye*10);
      tcloud=(int)(cloud*10);
      if(listland==0) {
        strcpy(scene,"LAND");
        ciconf=3;
        vconf=3;
        presconf=3;
        posconf=2;
        rule89=0;
      } else {
        rule89=0;
        if((r8%10)>0) rule89=1;
        if(r9==2) rule89=2;
        if((r8%10)>0&&r9==2) rule89=3;
        if((cscene==3)||(cscene==4)) {
          strcpy(scene,cscA[cscene]);
          ciconf=2;
          vconf=2;
          presconf=2;
          posconf=3;
        } else if(escene<6) {
          /* strcpy(scene,escA[cscene]); */
          strcpy(scene," EYE");
          ciconf=1;
          vconf=1;
          presconf=1;
          posconf=1;
        } else {
          strcpy(scene,cscA[cscene]);
          ciconf=2;
          vconf=2;
          presconf=2;
          posconf=2;
        }
      }
      (void)aodtv64_sattypes(isat,cursat);
      
      strcpy(csite,srcID); 
      csite[strlen(srcID)]='\0';

      if(fixtype==0) {
        strcpy(user,"MAN");
      } else {
        strcpy(user,"AUT");
      }
      user[strlen(user)]='\0';

      (void)sprintf(iout2,"%2s, %2.2d, %12s, %3d, %4s, %10s, %1s, %5s, %6s, %5s, %1d, %3d, %1d, %4d, %1d, %4s, %3s, %4s, %4s, %4s, %4s, %4s, %1s, %1s, %1s, %1s, %1s, %3s, %3s, %1s, %5s, %3s, %4s, %2d, %1d, %2d, %3d, %1s, %2d, %4d, %4d, %4s, %2s, %6s, %1s, %-s\n",
                          cbasin,stormID,cdt,20,"DVTO","CI"," ",clat,clon," ",posconf,vmax,vconf,presx,presconf,"DVRK",
                          " "," "," "," "," "," "," "," "," "," "," "," "," "," ",
                          csite,user,
                          "   I",cix,ciconf,tnox,tnoxtp,ctnox,trawx,teye,tcloud,scene,r89[rule89],cursat,"T",commnt);strcat(string,iout2);
      string[strlen(string)]='\0';
    }
  }

  free(iout2); iout2=NULL;
  free(cdate); cdate=NULL;
  free(commnt); commnt=NULL;
  free(clatlon); clatlon=NULL;
  free(crmw); crmw=NULL;
  free(scene); scene=NULL;
  free(user); user=NULL;
  free(ctime); ctime=NULL;
  free(cdt); cdt=NULL;
  free(clat); clat=NULL;
  free(clon); clon=NULL;
  free(cbasin); cbasin=NULL;
  free(ctnox); ctnox=NULL;
  free(csite); csite=NULL;
  free(cursat); cursat=NULL;

  return 0;

}

int aodtv64_commenthistoryrec(char *string) 
/* Insert comment in history file at specifified date/time.
    Inputs  : string - character string field to be interted
    Outputs : none
    Return  : -1 : error finding record at date/time
             >=0 : record number modified
*/
{ 
  int    iret,count=0;
  double curtime;
  struct odtdata *odthistory;
  logical found=FALSE;

  odthistory=odthistoryfirst_v64;

  while((!found)&&(odthistory!=0)) {
    curtime=aodtv64_calctime(odthistory->IR.date,odthistory->IR.time);
    count++;
    if(curtime==starttime_v64) {
      strcpy(odthistory->IR.comment,string); 
      odthistory->IR.comment[strlen(string)]='\0';
      found=TRUE;
    }
    /* reset record pointer ot next record */
    odthistory=odthistory->nextrec;
  }
  iret=count;
  if(!found) iret=-1;
  
  return iret;
}

int aodtv64_datetime(int ot1,int ot2,char *od1,char *od2,logical odel) 
/* Obtain start and end time to list/graph/delete using DATE=
   command line entries for start and end dates.  Output values
   will be in ddd.ddd format.
    Inputs  : variables containing start and end date/time information
              odel flag indicating delete option being used
    Outputs : global variables containing floating point starttime_v64 
              and endtime_v64 values 
    Return  : -1 : error with date/time and delete option
*/
{
  int iyyddd1=0,iyyddd2=0;

  /* convert character date format to Julian date format */
  iyyddd1=aodtv64_cmonth2julian(od1);
  iyyddd2=aodtv64_cmonth2julian(od2);

  /* if deleting or commenting records, must specify beginning record */
  if(odel) {
    if(iyyddd1==0) {
      /* must specify first record for delete option */
      return -1;
    }
    starttime_v64=aodtv64_calctime(iyyddd1,ot1);
    if(iyyddd2==0) {
      endtime_v64=starttime_v64;
    } else {
      endtime_v64=aodtv64_calctime(iyyddd2,ot2);
    }

  /* if graphing or listing, no input specifying date limits will
     result in list/graph of entire file */
  } else {
    if(odthistoryfirst_v64==0)  return -1;
    if(iyyddd1==0) {
      starttime_v64=aodtv64_calctime(odthistoryfirst_v64->IR.date,odthistoryfirst_v64->IR.time);
    } else {
      starttime_v64=aodtv64_calctime(iyyddd1,ot1);
    }
    if(iyyddd2==0) {
      endtime_v64=aodtv64_calctime(9999999,235959);
    } else {
      endtime_v64=aodtv64_calctime(iyyddd2,ot2);
    }
  }
  if((starttime_v64<0.01)||(endtime_v64<0.01)) return -1;
  
  return 0;
}
