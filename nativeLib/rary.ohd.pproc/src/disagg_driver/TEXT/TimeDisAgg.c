#include <stdio.h>
#include <sys/utsname.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <sqlca.h>

#include "DbmsAccess.h"
#include "DbmsDefs.h"
#include "CurPP.h"
#include "startdb.h"
#include "closedb.h"
#include "get_last_run_time.h"
#include "time_convert.h"
#include "time_defs.h"
#include "List.h"
#include "current_GMT_dt.h"
#include "QualityCode.h"
#include "Location.h"
#include "HourlyPP.h"
#include "gage_pp_init.h"
#include "gage_pp_write_rec.h"
#include "wr2log.h"
#include "PerfLog.h"

#include "TimeDisAgg.h"

#include "messenger_inc/msgwrappers.h"

/*******************************************************************************/
/*   TIME DISAGGREGATION PROGRAM                                               */
/*                                                                             */
/*                                                                             */
/*  disagg: this program processes data from xmrg files and from CurPP         */
/*          table to perform time distributed aggregation.  It updates/inserts */
/*          new data to the HourlyPP table and writes the last disagg run      */
/*          time, number of records inserting into the  HourlyPP and elapse    */
/*          time to the perflog table.                                         */
/*                                                                             */
/*   this program calls WHFS routines                                          */
/*    - get_last_run_time()                                                    */
/*    - current_GMT_dt()                                                       */
/*    - yearsec_dt_to_ansi()                                                   */
/*    - GetCurPrecip()  - Not available in ob6                                 */
/*    - GetCurPP()   - replaced GetCurPrecip                                   */
/*    - ListFirst()                                                            */
/*    - ListCount()                                                            */
/*    - wr2log()                                                               */
/*   Called API routines: initmsg() , writemsg() to create a file for logging  */
/*                       messages and debbuged data.                           */
/*  New  Routines:                                                             */
/*       OpenXMRGFiles(): routine to check if there are enough files for       */
/*                        processing time distribution.                        */
/*       GetXMRGVal(): routine to calculate radar values base on radar         */
/*                      radius and values from xmgr files.                     */
/*       int durinHrs(): routine to pass in shef duration encoder and return   */
/*                       integer numeric value for duration                    */
/*       getPosbyLid(): routine to pass in location id and return lat          */
/*                     and longitude for that lid                              */
/*                       5-18-2004 added code to read user set current date    */
/*                             two new tokens: disagg_set_date - year-mon-day  */
/*                                             disagg_set_hour - current hour  */
/*                             (accomodate cbrfc request)- add this feature for*/
/*                             running disagg on historical data               */
/*******************************************************************************/

extern int 	OpenXMRGFiles (time_t StartTime, time_t EndTime, int Radius, char XMRGDir[]);
extern float  	GetXMRGVal (time_t StartTime, time_t CurrTime,
                                         struct PosStruct Pos, int Radius);
extern int GetRadiusVal();
FILE	*fptr;
LIDPOS 	*LidPosPtr;
int 	gNlids;



int disagg_main()
{

        CurPP           *FCprecipHead,FCprecip_ptr, *FCprecipPtr =&FCprecip_ptr ;
        time_t          t, startTime, endTime, curtm, pStartTime;
        time_t          begDisaggTime, endDisaggTime, sTime, eTime;
        time_t          intEtime, intStime, usrtimet,tmpEndtm;
        dtime_t         current_time;

        struct          tm   *logTime;
        struct          PosStruct  pos;
        struct          utsname   uts_struct;

        long            irc;
        float           elapseTime, radarVal[30], msummation, gageVal, nval,tcpu;

        int             n, iretDur, inpDurSet = 0, indur;
        int             inpDur, lCount, ct, nrecprocess, initDur,inpLB;
        int             foundXmrg = 0;
        int             lendir, len, lentime, lenlookbck,lendebug_level;
        int             i, j, m;
        int             setcurDate,user_setDate;
        /***** -- API initialization ---         ****/
        int             reqdlev[] = {1, 1, TIER_3, TIER_3, 1, 1, 1, 1, 1, 1, 1};
        int             defevt = -1;

        int             debugLevels;
        int             bad_xmgr_data = 0, insert_failCount;
        int		radius, DirLength, newHour;




        char            timestr[30],timestr1[30], curtmstr[22],logstrTm[30],pTime[30];
        char            strtime[5],lookBackT[5],debug_level[5];
        char            process_name[10];
        char            last_run_time[22], strcurr[22];
	char            outdir[100];

        char     	where[300];
        char    	orderClause[40];
        char            CurPrecip_querystmt[240];
        char            dbname[30];
        char            filename[100];
        char            versionNumber[10];
        char            *versNumb = "OB6";
        char            XMRGDir[100];
        char            user_setDatestr[22],usrtstr[22];  /* added 5/22/04 fou user to set start date
                                              which is also = last_run_time*/

        char            user_setHrstr[22],tmpstr[10];

        HourlyPP        hourlyPP ;
        int             hour_slot, status, l;
        const char      zero_offset_code = '0';
        char            msgstr [512] ;
        char            obsdate [11] ;
        short int       revision [NUM_HOURLY_SLOTS], timeDisagg_value ;
        short int       revision_6hour[NUM_6HOURLY_SLOTS];
        WriteInfo       write_info ;
        GagePPoptions   options ;
        const char      timeDisagg_qc='T';
        char            abtime[30],aetime[30],actime[30];

/*---------------------------------------------*/
/*   start elapsed time timer                  */
/*---------------------------------------------*/

      time(&begDisaggTime);
      logTime=gmtime(&begDisaggTime);
      sprintf(logstrTm,"%02d%02d%02d%02d%02d",logTime->tm_mon+1,
              logTime->tm_mday, logTime->tm_year%100, logTime->tm_hour,logTime->tm_min);


/*---------------------------------------------*/
/*   set up log file                           */
/*   if unable to open log file, then continue */
/*---------------------------------------------*/

      lendir = strlen("disagg_log_dir");
      get_apps_defaults("disagg_log_dir",&lendir,outdir,&lendir);

      if (strcmp(outdir," ")==(char)NULL){
          /*Use Home directory as a default */

          strcpy(outdir, getenv("HOME"));
      }

      sprintf(filename,"%s/disagg.%s",outdir,logstrTm);

/*---------------------------------------------*/
/* set up debug level                          */
/*---------------------------------------------*/
      lendebug_level = strlen("disagg_msglog_level");
      get_apps_defaults("disagg_msglog_level",&lendebug_level,debug_level,&lendebug_level);
      debugLevels = atoi(debug_level);
      if (debugLevels <=0 || debugLevels >90){
         debugLevels=1;
      }
/*---------------------------------------------*/
/*   Check to see if user set the last runtime */
/*   date                                      */
/*---------------------------------------------*/
      setcurDate = strlen("disagg_set_date");
      get_apps_defaults("disagg_set_date",&setcurDate,
                                       user_setDatestr,&setcurDate);
      user_setDate = 1;
      if (strcmp(user_setDatestr,"")==(char)NULL ||
           strcmp(user_setDatestr,"0") == 0){
          user_setDate = 0;
      }


/*---------------------------------------------*/
/*   start initializing API msg                */
/*---------------------------------------------*/

      for(i=0;i<=10;i++){
         reqdlev[i] =  debugLevels;
      }


      initmsg(&defevt, NULL, filename, reqdlev);

      writemsg(EVT_START_RTN+SEV_DEBUG+TIER_4,"Enter subroutine Disagg.\n");

     /*---------------------------------------------*/
     /*   read version number and print to log file */
     /*   print operating system (HP or LINUX)      */
     /*---------------------------------------------*/
      uname(&uts_struct);
      writemsg(EVT_LOGGING + SEV_INFO+ ALWAYS_PRINT, "Operating System: %s\n",uts_struct.sysname);

      strcpy(versionNumber,versNumb);
      writemsg(EVT_LOGGING + SEV_INFO+ ALWAYS_PRINT, "Version: %s\n",versionNumber);

      /*current_time.dt_qual = TU_DTENCODE(TU_YEAR,TU_SECOND); PostGreSQL conversion */
      dtcurrent(&current_time);
      lenlookbck = strlen("disagg_look_back");
      get_apps_defaults("disagg_look_back",&lenlookbck,lookBackT,&lenlookbck);
      inpLB = atoi(lookBackT);

     /*---------------------------------------------------------------------------*/
     /* set look back in time (default = 0)                             ----------*/
     /*  Use lookback time to reset last_run_time from the current date time------*/
     /*---------------------------------------------------------------------------*/

      if(inpLB < 0 || inpLB > 100)
         inpLB = 0;


      lentime = strlen("disagg_dur");
      get_apps_defaults("disagg_dur",&lentime,strtime,&lentime);

      inpDur = atoi(strtime);
      if(inpDur <= 1)inpDur = 1;
      if(inpDur > 100)inpDur = 100;
      /*---------------------------*/
      /* Set initial Duration value*/
      /*---------------------------*/
      initDur  = 2001;

      len = strlen("db_name");
      get_apps_defaults("db_name",&len,dbname,&len);
      if (strcmp(dbname,"")==(char)NULL){
         writemsg(EVT_DATA+DQ_BAD+PS_CORRECT+SEV_ERROR,
          " Error: There is no database Token set!! Program stop.\n");
         return(3);

      }
      writemsg(EVT_LOGGING + SEV_INFO+ TIER_1,
                                     "database name:  %s\n", dbname);
      /*--------------------------*/
      /*  open database           */
      /*--------------------------*/
      startdb(&irc);

      if(irc != 0)
      {
         writemsg(EVT_SOURCE_IO+DQ_BAD+PS_CORRECT+SEV_ERROR + ALWAYS_PRINT,
                                "ERROR: error in open database: %s\n",dbname);
         writemsg(EVT_SOURCE_IO+DQ_BAD+PS_CORRECT+SEV_ERROR + ALWAYS_PRINT,
               "ERROR: PostgreSQL error %ld attempting to open database\n", irc);
	 return(1);
      }


    /*--------------------------------*/
    /*  initialization disagg process */
    /*--------------------------------*/
      strcpy(process_name,"disagg");

    /*---------------------------------------------*/
    /*   find last_run_time and print to log file  */
    /*---------------------------------------------*/
    /* check disagg_user_setdate token  */
    if(user_setDate == 0){
      get_last_run_time(process_name, last_run_time, &irc);
      if(irc != 0)
      {
         writemsg(EVT_SOURCE_IO+DQ_BAD+PS_CORRECT+SEV_ERROR+ALWAYS_PRINT,
                 "ERROR: PostgreSQL error %ld attempting to find last run time\n", irc);
         return(1);
      }
      /* ----------------------------------------------------------------------------*/
      /* if last run time is blank then use the current date with time = 00:00:00 "--*/
      /* ----------------------------------------------------------------------------*/

      current_GMT_dt(&current_time);
      yearsec_dt_to_ansi(current_time,curtmstr);
      yearsec_ansi_to_timet(curtmstr,&curtm);
      memset(strcurr,'\0',22);
      if (strcmp(last_run_time,"")==(char)NULL ){
           yearsec_dt_to_ansi(current_time, strcurr);
           memset(last_run_time,'\0',22);

          /* -----------------------------------------------------------------*/
          /* use the first 11 characters of curdate info e.g."2002-06-10 "    */
          /* -----------------------------------------------------------------*/
           strncpy(last_run_time,strcurr,11);
           strcat(last_run_time,"00:00:00");
       }


     /* -----------------------------------------------------------------------*/
     /*   inpLB = disagg_lookback token                                        */
     /*   if inpLB equal zero then the default last run time =current date with*/
     /*   time = 00:00:00                                                      */
     /*   else last run time= current time - inpLB                             */
     /* -----------------------------------------------------------------------*/
      if (inpLB != 0){

          timet_to_yearsec_ansi((curtm-inpLB*3600), strcurr);
          memset(last_run_time,'\0',22);
          strcpy(last_run_time,strcurr);
      }


   }/* user did not overwrite current date user_set_Date = 0*/
   else  {
       /*  if user_setdate token is set then read disagg_set_hour token */
       setcurDate = strlen("disagg_set_hour");
       get_apps_defaults("disagg_set_hour",&setcurDate,
                                       user_setHrstr,&setcurDate);
       if (strcmp(user_setHrstr,"")!=(char)NULL){
            newHour = atoi(user_setHrstr);
            sprintf(tmpstr," %02d:00:00",newHour);
            strcat(user_setDatestr,tmpstr);
       }
       else{
             strcat(user_setDatestr," 00:00:00");
       }
       yearsec_ansi_to_timet(user_setDatestr,&usrtimet);
       memset(usrtstr,'\0',22);
       timet_to_yearsec_ansi(usrtimet,usrtstr);
         /* print to log file if user overide last runtime date 5/20/04*/
       writemsg(EVT_LOGGING + SEV_INFO + TIER_1,
                    "User Set Current Time = %s Z\n",user_setDatestr);

       strcpy(last_run_time,user_setDatestr);

       /* look back token founds.  Calculate last run time*/
       if (inpLB > 0){
           memset(usrtstr,'\0',22);
           timet_to_yearsec_ansi((usrtimet-inpLB*3600), usrtstr);
           strcpy(last_run_time,usrtstr);
           /*store end time */
           yearsec_ansi_to_timet(usrtstr,&tmpEndtm);
       }

   }
   writemsg(EVT_LOGGING + SEV_INFO + TIER_1,
                    "last disagg run time = %s Z\n",last_run_time);

   /*----------------------------------------------------------------------*/
   /*  User over ride the start time. Start time = end time - duration     */
   /*  If inpDur = 1 use duration from curPP table from each indvidual lid */
   /*  This dur is used for determine start time and end time              */
   /*----------------------------------------------------------------------*/
     if(inpDur > 1 && inpDur <= 24){
         inpDurSet = 1;
         if(inpDur == 24) indur = 2001;
         else indur = inpDur + 1000;
     }
     else{
         indur = initDur;
     }

     radius = GetRadiusVal();
     getLocTbl();

     /*----------------------------------------------------------------------*/
     /*   create where clause for select in pe tables based on posting time  */
     /*   create ORDER BY clauses                                            */
     /*----------------------------------------------------------------------*/
      if(user_setDate==1){
       sprintf(where,
              " WHERE obstime > '%s' "
              " AND obstime <= '%s' "
              " AND dur >'1001' "
              " AND dur <='%d'"
              " AND value >= 0.0"
               ,last_run_time,user_setDatestr,indur);
      }
      else{
          sprintf(where,
              " WHERE obstime > '%s' "
              " AND dur >'1001' "
              " AND dur <='%d'"
              " AND value >= 0.0"
               ,last_run_time,indur);

      }
      sprintf(orderClause,"ORDER BY lid");

      sprintf(orderClause," ");
      sprintf(CurPrecip_querystmt," %s  %s",where,orderClause);

      writemsg(EVT_LOGGING + SEV_DEBUG+ TIER_1,
                    "select * from CurPP %s \n",where);


      DirLength = strlen("ofs_griddb_dir");
      get_apps_defaults("ofs_griddb_dir", &DirLength, XMRGDir, &DirLength);

      writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
        "ofs_griddb_dir Token = %s\n",XMRGDir);
      writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
        "disagg_radius Token = %d \n",radius);
       writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
        "disagg_dur Token = %d \n",indur);
      writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
        "disagg_look_back Token = %d \n",atoi(lookBackT));
       writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
        "disagg_msglog_level Token = %d \n",atoi(debug_level));
    /*--------------------------------------------------------*/
    /*  get records in linked list form from CurPP table      */
    /*  records are ordered by lid                            */
    /*--------------------------------------------------------*/
      /*debug printf("CurPrecip_querystmt=%s\n",CurPrecip_querystmt);*/
      FCprecipHead = GetCurPP(CurPrecip_querystmt);

      if(SQLCODE !=0 ){
         writemsg(EVT_SOURCE_IO+SEV_WARNING+ALWAYS_PRINT,
         "PostgreSQL error %ld attempting select of precip data\n", SQLCODE);
      }

      if(FCprecipHead){

         FCprecipPtr = (CurPP*) ListFirst(&FCprecipHead->list);

         lCount = ListCount(&FCprecipHead->list);

         writemsg(EVT_LOGGING + SEV_INFO + TIER_3,
                    "Number of records found = %d\n",lCount);
         yearsec_dt_to_timet(FCprecipPtr->obstime, &t);

	 intStime   = (t + 30*60)/3600;
         t          = intStime * 3600;
         startTime = endTime = sTime = t;


         /* find start and end date */
         while(FCprecipPtr)
         {
             iretDur = 2;
             if(inpDurSet !=2){
                if(durinHrs(FCprecipPtr->dur) != 0)
                  iretDur = durinHrs(FCprecipPtr->dur);
                else{
                  writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,
                               "bad duration value, set duration = 2 \n");
                }
             }
             else{
                 if(durinHrs(indur) != 0)
                   iretDur = durinHrs(indur);
                 else
                   writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,
                               "bad input duration token. set duration = 2 \n");
             }
 	     yearsec_dt_to_timet(FCprecipPtr->obstime, &t);

             /* if starttime is 10:30 then push back to top of the hour=10:00 */
             /* anytime from 10:5-10:30 will push it back to the */
             /* the top of the hour.  Any time from 10:35 - 10:55*/
             /* will push it forward an hour = 11:00 */

             intEtime   = (t + 30*60)/3600;
             t = intEtime * 3600;
             sTime = (t - (iretDur-1)*3600);
             if( sTime < startTime ){
                startTime = sTime;

             }
             if( t > endTime )  {endTime = t;}
             FCprecipPtr = (CurPP*) ListNext(&FCprecipPtr->node);

 	 }/* end while loop */

         if(user_setDate == 1){
            intStime   = (usrtimet + 30*60)/3600;
            t          = intStime * 3600;
            endTime = sTime = t;
            startTime = tmpEndtm;
         }
         /*-----------------------------------------------------------------*/
         /* Check to see if there are xmrg files from start time to endtime */
         /*-----------------------------------------------------------------*/

         timet_to_yearsec_ansi(startTime, timestr);

         timet_to_yearsec_ansi(endTime , timestr1);
         writemsg(EVT_LOGGING + SEV_DEBUG+ ALWAYS_PRINT,
                        "Read xmgr files from = %s to %s\n",timestr,timestr1);
         foundXmrg = OpenXMRGFiles(startTime,endTime, radius, XMRGDir);
       }/*(FprecipHead)*/
       else{
          writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
                    "There are no new records found since last run time= %s\n",
                    last_run_time);
          return(2);
       }


      /*-----------------------------------------------------*/
      /*  if not having enough xmgr files for disaggreration */
      /*  Stop !!                                            */
      /*  Close all xmgr files,close database                */
      /*-----------------------------------------------------*/


      if( !foundXmrg) {
          if(FCprecipPtr)
              FreeCurPP(FCprecipPtr);
          writemsg(EVT_LOGGING + SEV_ERROR + ALWAYS_PRINT,
                "There are not enough xmrg files to Disagg.  Program stop! \n");
          return(4);
      }
      /* Move Pointer to first record */
      FCprecipPtr = (CurPP*) ListFirst(&FCprecipHead->list);

      nrecprocess = 0;

      /*for every record found,calculating the new value using calc-xmgr values*/
      for (n = 0; n < lCount ; n++){

          if(check_qccode(QC_QUESTIONABLE,FCprecipPtr->quality_code)){
               writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,
                              "     value failed qc test -- record ignored");
          }
          else{
               pos = getPosbyLid(FCprecipPtr->lid);
               yearsec_dt_to_timet(FCprecipPtr->obstime, &t);
               /* Store endtime */
               eTime = t;
               /* round off to the top of the hour */
               intEtime   = (endTime + 30*60)/3600;
               endTime   = intEtime *3600;
               /* convert gageVal unit = inches  */
               gageVal = FCprecipPtr->value;
               iretDur = 2;
               if(inpDurSet !=2){
                  if(durinHrs(FCprecipPtr->dur) != 0)
                     iretDur = durinHrs(FCprecipPtr->dur);
               }
               else{
                  if(durinHrs(indur) != 0)
                     iretDur = durinHrs(indur);
                  else
                     writemsg(EVT_LOGGING + SEV_WARNING + TIER_2,
                               "bad input duration token. Set default duration = 2 \n");
               }
               sTime = eTime - (iretDur-1)*3600;
               msummation = 0.0;
               m = 0;
               bad_xmgr_data = 0;
               insert_failCount = 0;
               /* ..........................................................   */
               /* Calculate new value base on xmrg data and value from curpp.  */
               /* Determine sTime and eTime using the obstime found in curpp   */
               /* and duration                                                 */
               /* ..........................................................   */
               /*loop to calculate summation of xmrg values for the entire duration */
               for ( ct = sTime; ct <= eTime; ct +=3600){

                    if( gageVal != 0){
                       /* radar value comes in as mm -->need to convert to inches */
                       radarVal[m] = GetXMRGVal(sTime,ct,pos,radius) * 1/25.4;

                       if(radarVal[m] < 0.0 ) {
                           writemsg(EVT_LOGGING+SEV_INFO + TIER_3,
                           "Bad values found in xmrg files for station id=%s\n",
                           FCprecipPtr->lid);
                           bad_xmgr_data = 1;
                       }
                       else {
                           msummation = msummation + radarVal[m];
                       }

                    } /*gageVal != 0 */
                    else{
                       radarVal[m] = 0.0;
                    }

                    /* When disagg_msglog_level set to 30 will not see the below writemsg */
                    /*writemsg(EVT_LOGGING + SEV_DEBUG+ TIER_3,
                        "Calc - msumation=%f radarVal[%d]=%f \n",msummation,m,radarVal[m]); */


                    m++;

                    if(bad_xmgr_data)break;
               }/* for loop ct*/
               l=0;
               /*valid xmgr data found, calculate new hourly value */
               if(!bad_xmgr_data){
                  yearsec_dt_to_timet(FCprecipPtr->obstime, &pStartTime);
                  timet_to_yearsec_ansi(pStartTime,pTime);

                  for ( ct = sTime; ct <= eTime; ct +=3600){

                       /*----------------------------------------------------------------------*/
                       /*----------Time disagg ------------------------------------------------*/
                       /* gageVal is value get from CurPP table     ---------------------------*/
                       /* if gageVal is zero then time disagg for all hours Val = 0   ---------*/
                       /* if gaveVal > zero and the sumation of the all radar hours------------*/
                       /* values from HRAP grid files > zero, then time disagg value for each--*/
                       /* hour = radar value * (gage value / sumation). If sumation is zero  --*/
                       /* then time disagg = value from CurPP table divide by num     ---------*/
                       /* of duration            ----------------------------------------------*/
                       /*----------------------------------------------------------------------*/

                       timet_to_yearsec_ansi(ct,actime);
                       nval = 0.0;
                       if(gageVal >= 0.0){
                         if ( msummation == 0.0 ){
                            nval = gageVal/iretDur;

                         }
                         else{

                            nval = radarVal[l] * ( gageVal / msummation);

                         }
                         writemsg(EVT_LOGGING + SEV_DEBUG+ TIER_3,
                         "msummation=%f, radarval[%d]=%f gval=%f lid= %s dur=%d obstime= %s newval=%f\n",msummation,l,radarVal[l],gageVal,FCprecipPtr->lid,
                                  FCprecipPtr->dur,actime,nval);
                         l++;
                       }

                       /*..................................*/
                       /* value in inch * 100  */
                       /*timeDisagg_value = (short int)((nval/25.4) * 100);*/

                       timeDisagg_value = (short int)((nval + .005) * 100);
                       timet_to_yearsec_ansi(sTime,abtime);
                       timet_to_yearsec_ansi(eTime,aetime);

                       hour_slot = gage_pp_init ( & hourlyPP,
                                          actime,
                                          FCprecipPtr->lid,
                                          FCprecipPtr->ts,
                                          timeDisagg_value,
                                          obsdate,
                                          zero_offset_code,
                                          timeDisagg_qc);


                       for ( j = 0; j < NUM_HOURLY_SLOTS; ++j )
                       {
                          revision [ j ] = 0;
                       }

                       for ( j = 0; j < NUM_6HOURLY_SLOTS; ++j )
                       {
                          revision_6hour [ j ] = 0;
                       }

                       revision [ hour_slot - 1 ] = 1 ;
                       status = gage_pp_write_rec ( & hourlyPP ,
                                                    & write_info ,
                                                    msgstr ,
                                                    "PP" ,
                                                    obsdate ,
                                                    & options ,
                                                    revision ,
                                                    revision_6hour,
                                                    1 ) ;

                       /*----------------------------------------------------------------*/
                       /* count number of records that will insert into hourlyPP table */
                       /*----------------------------------------------------------------*/
                       nrecprocess++;
                       if( status != 0 ){
                           writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                               "error in inserting/updating data to hourlyPP table\n");
                           writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                               "for the reason %s\n",msgstr);
                           insert_failCount++;
                       }
                       writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                               "lid=%s,value=%d, hour_slot=%d %s posted to HourlyPP table\n",
                                hourlyPP.lid,timeDisagg_value,hour_slot,actime);
                       /*else{


                        nrecprocess = nrecprocess + 1;
                        writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                               "lid=%s,value=%d, hour_slot=%d %s posted to HourlyPP table\n",
                                hourlyPP.lid,timeDisagg_value,hour_slot,actime);

                        if(insert_failCount > 0)
                           writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                               " %d records failed to update/insert to HourlyPP table\n", insert_failCount);
                      }  */

                  } /*end for loop ct2*/

                  writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                               " %d records posted to HourlyPP table\n", nrecprocess);

                  if( insert_failCount )  {
                      writemsg(EVT_LOGGING + SEV_DEBUG + TIER_3,
                         " %d records failed to update/insert to HourlyPP table\n", insert_failCount);
                  }
               }/*bad_xmgr_data*/

          }/*end else */

          FCprecipPtr = (CurPP*) ListNext(&FCprecipPtr->node);
      }/*end for = n*/

      /*-----------------------------------------------*/
      /*   WRITE PERFORMANCE RESULTS TO perflog TABLE  */
      /*-----------------------------------------------*/

      tcpu = 0.0;
      time( &endDisaggTime);

      elapseTime = ( float )(endDisaggTime - begDisaggTime);
      if(nrecprocess > 0){
          wr2log(process_name,&current_time,&nrecprocess,&elapseTime,&tcpu,fptr);
          yearsec_dt_to_ansi(current_time, strcurr);
          writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
           "Write elapseTime = %f currenttime = %s to perflog\n",
                           elapseTime,strcurr);
          writemsg(EVT_LOGGING + SEV_INFO + ALWAYS_PRINT,
           "%d records were posted to HourlyPP Table\n",
                           nrecprocess);
      }
      else{
          writemsg(EVT_LOGGING + SEV_INFO + TIER_1,
                     "Timedisagg did not insert any new records to HourlyPP Table\n");
      }
      closedb(&irc);

      if(irc != 0)
      {
          writemsg(EVT_SOURCE_IO+DQ_BAD+PS_CORRECT+SEV_ERROR,
         "IPostgreSQL error %ld attempting to close database\n", irc);
         return(1);
      }
  if(FCprecipPtr)
     FreeCurPP(FCprecipPtr);

  writemsg(EVT_FINISH_RTN+SEV_DEBUG+TIER_4, "Leaving routine TimeDisAgg\n");
  return (0);
}
/*--------------------------------------------------------------------*/
/* Routine to convert shef duration encoder to numeic hours        ---*/
/* Input/Output   descriptions:                                    ---*/
/* dur           Input    duration read in from CurPP table        ---*/
/* durinHrs      Output   Duration in numeric hours                ---*/
/* -------------------------------------------------------------------*/

int durinHrs(int dur)

{

  int  tdur = 0;
  writemsg(EVT_START_RTN+SEV_DEBUG+TIER_4,
                               "Entering subroutine durinHrs.\n");
  if(dur > 1001 && dur <2000){
     tdur = dur%1000;
  }
  if(dur == 2001) tdur = 24;
  writemsg(EVT_FINISH_RTN+SEV_DEBUG+TIER_4,
                               "Leaving routine durinHrs.\n");
  return(tdur);
}



/* ---------------------------------------------------------*/
/* Routine to store lid, latitute and longitude          ---*/
/* from location table                                   ---*/
/* Input/Output   descriptions:                          ---*/
/* ---------------------------------------------------------*/

int getLocTbl(){

   int n, lidCount;

   Location *lHead, *lPtr;

   writemsg(EVT_START_RTN+SEV_DEBUG+TIER_4,
                               "Entering subroutine  getLocTbl.\n");
   lHead = GetLocation("");

   if ( ! lHead ) {
      writemsg(EVT_SOURCE_IO+DQ_BAD+PS_NON_CORRECT+SEV_ERROR,
            "error attempting select in location table .");
      return (0);
   }
   lidCount = ListCount(&lHead->list);

   LidPosPtr = (LIDPOS *)malloc(sizeof(LIDPOS) * (lidCount +1 ));
   lPtr = (Location *) ListFirst(&lHead->list);
   for ( n = 0; n < lidCount; n++) {
      strcpy((LidPosPtr + n)->lid, lPtr->lid);
      (LidPosPtr + n)->pos.lat = lPtr->lat;
      (LidPosPtr + n)->pos.lon = lPtr->lon;
      lPtr = (Location *) ListNext(&lPtr->node);

   }
   gNlids = lidCount;

   if ( lHead )
      FreeLocation(lHead);
   writemsg(EVT_FINISH_RTN+SEV_DEBUG+TIER_4,
                               "Leaving routine getLocTbl.\n");
   return (1);

}

/* ---------------------------------------------------------*/
/* Routine to return latitute and longitude structure    ---*/
/* when passing in location id                           ---*/
/* Input/Output   descriptions:                          ---*/
/* dbLevel    Input    Levels of API debug               ---*/
/* lid            Input    Location ID                   ---*/
/* PosStruct      Output   Structure contains Lat and Lon---*/
/* ---------------------------------------------------------*/
struct PosStruct getPosbyLid( char *lid)
{
   int n;
   struct PosStruct pos;
   pos.lat=pos.lon=0.0;
   writemsg(EVT_START_RTN+SEV_DEBUG+TIER_4,
                               "Entering subroutine  getPosbyLid.\n");
   for ( n = 0; n < gNlids; n++) {
      if(strcmp(lid,(LidPosPtr + n)->lid)==0){
         pos.lat = (LidPosPtr + n)->pos.lat;
         pos.lon = (LidPosPtr + n)->pos.lon;

      }
   }
 /* printf("lid = %s pos.lat=%f pos.lon=%f\n",lid, pos.lat,pos.lon);
 */
  writemsg(EVT_FINISH_RTN+SEV_DEBUG+TIER_4,
                               "Leaving routine getPosbyLid.\n");
  return  pos;
}

