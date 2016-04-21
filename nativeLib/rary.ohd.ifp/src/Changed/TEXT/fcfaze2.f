C$PRAGMA C (set_end_date_atom)
C$PRAGMA C (set_start_date_atom)
C$PRAGMA C (ifp_icp)
C$PRAGMA C (del_all_files)
C MEMBER FAZE2
C  (from old member FCFAZE2)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/11/95.09:01:21 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  DESC: EXECUTE ONE SEGMENT AT A TIME
C
      SUBROUTINE FAZE2 (MC,C,MD,D,MP,P,MT,T,MTS,TS)
C
C
C     ROUTINE ORIGINALLY WRITTEN BY
C                 GEORGE F SMITH - HRL - 25 MARCH 1980
C     UPDATED BY GEORGE F SMITH - NOV 1984 FOR NEW RUN-TIME MODS
C     UPDATED BY GEORGE F SMITH - MAY 1985 TO CHECK FOR API-CIN OPER.
C     UPDATED BY GEORGE F SMITH - JUL 1985 TO ADD CALLS FOR IFP
C     UPDATED BY GEORGE F SMITH - JUNE 1986 TO CLARIFY MESSAGE WHEN
C                                          START OR END DATES CHANGE
C........................
C
      REAL*8 APINAM
c
c  The following common added 7/19/91 to pass the return code from
c   the event loop in cex25.
c  Option 7 (fatal_error_no_save) added by gfs - hrl - 7 Sept 1994
c    Valid return codes are:    rerun                1
c                               next                 2
c                               continue             3
c                               go_upstream          4
c                               quit                 5
c                               quit_no_save         6
c                               fatal_error_no_save  7
c
      Integer event_loop_exit_status
      Common /cex_exit_status/ event_loop_exit_status      
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/where'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/errdat'
      INCLUDE 'common/flarys'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/fprog'
      INCLUDE 'common/ffgctl'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/killcd'
      INCLUDE 'common/modscb'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/opfil55'
      COMMON/FCFUTP/IFPR
      COMMON /FCKTMP/ NEEDTX
      COMMON/OUTCTL/IOUTYP
C
      INTEGER T
      LOGICAL NOTCPU,LBUG,DUMPCB,DCBA,DCBB,DCBC,DCBD,DCBE,DCBF
C
      DIMENSION C(MC),D(MD),P(MP),T(MT),TS(MTS)
      CHARACTER*8 RTNNAM/'FAZE2'/,OPNOLD
C
cew  added calls to dump out sac model states.
cew   added fengmt
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Changed/RCS/fcfaze2.f,v $
     . $',                                                             '
     .$Id: fcfaze2.f,v 1.6 2004/09/24 18:57:01 jgofus Exp $
     . $' /
C  =====================================================================
C
C
      DATA ILT/4HLT  /
C
C
      IOLDOP=IOPNUM
      IOPNUM=0
      CALL UMEMOV (OPNAME,OPNOLD,2)
      CALL UMEMOV (RTNNAM,OPNAME,2)
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,20)
20    FORMAT (' *** ENTER FAZE2')
C
      NOTCPU=.TRUE.
      IF (IFBUG('CPU ').EQ.1) NOTCPU=.FALSE.
      LBUG=.FALSE.
      IF (IFBUG('PHZ2').EQ.1) LBUG=.TRUE.
      IF (.NOT.NOTCPU) LBUG=.TRUE.
      DUMPCB=.FALSE.
      IF (IALL.EQ.1) GO TO 30
      DCBA=.FALSE.
      DCBB=.FALSE.
      DCBC=.FALSE.
      DCBD=.FALSE.
      DCBE=.FALSE.
      DCBF=.FALSE.
      IF (IFBUG('DCBA').EQ.1) DCBA=.TRUE.
      IF (IFBUG('DCBB').EQ.1) DCBB=.TRUE.
      IF (IFBUG('DCBC').EQ.1) DCBC=.TRUE.
      IF (IFBUG('DCBD').EQ.1) DCBD=.TRUE.
      IF (IFBUG('DCBE').EQ.1) DCBE=.TRUE.
      IF (IFBUG('DCBF').EQ.1) DCBF=.TRUE.
      IF (DCBA.OR.DCBB.OR.DCBC) DUMPCB=.TRUE.
      IF (DCBD.OR.DCBE.OR.DCBF) DUMPCB=.TRUE.
C
c  Change IOPT to read get segment by name, not record number
c
c30    IOPT=1
30    IOPT=0
      NOPARM=0
      NF2ERR=0
C
      NCARDS=0
c
      event_loop_exit_status = 0      
C
      IF (LBUG) WRITE (IODBUG,40)
40    FORMAT (' IN FAZE2 - ABOUT TO ENTER DO LOOP')
      IF (DUMPCB) CALL FDMPCB (DCBA,DCBB,DCBC,DCBD,DCBE,DCBF)
C
C  COMPUTE LENGTH OF FORECAST PERIOD
      IENDRN=((LDARUN-1)*24)+LHRRUN
      IOBSRV=((LDACPD-1)*24)+LHRCPD
      IDIFF=IENDRN-IOBSRV
      IF (IDIFF.GE.IFPR) GO TO 60
      IF (IDIFF.LT.0.AND.IFPR.EQ.0) GO TO 60
      IF (IDIFF.LT.0.AND.IFPR.EQ.-1) THEN
         WRITE (IPR,45) IFPR
         CALL WARN
         GO TO 60
         ENDIF
45    FORMAT ('0**WARNING** NONE OF THE FORECAST RUN IS ',
     * 'INTO THE FUTURE. FUTPRECP TECHNIQUE (',I3,') WILL NOT BE ',
     * 'CHANGED.')
C
      ITEMP=IFPR
      IFPR=IDIFF
      WRITE (IPR,50) ITEMP,IDIFF,IDIFF
      CALL WARN
50    FORMAT ('0**WARNING** VALUE IN FUTPRECP TECHNIQUE (',I3,
     * ') EXCEEDS THE LENGTH OF FORECAST PERIOD (',I3,' HOURS). ',
     * 'FUTPRECP TECHNIQUE WILL BE SET TO ',I3,'.')
C
C  PROCESS EACH SEGMENT
C
60    DO 620 ISEGEX=1,NSEGEX
C
      IF (LBUG) WRITE (IODBUG,70) ISEGEX
70    FORMAT (' IN FAZE2 - ISEGEX=',I5)
C
      IF (LBUG) WRITE (IODBUG,80)
80    FORMAT (' IN FAZE2 - ABOUT TO CALL FGETSG')
C
C  READ INFORMATION INTO THE P, T AND TS ARRAYS FOR A SEGMENT
      CALL FGETSG (idsegn,IRSGEX(ISEGEX),
     *             MP,P,MT,T,MTS,TS,IOPT,NOPARM,IER)
C
      IF (LBUG) WRITE (IODBUG,90) IDSEGN
90    FORMAT (' IN FAZE2 - BACK FROM FGETSG - IDSEGN=',2A4)
      IF (.NOT.NOTCPU) CALL FPRCPU
C
      IF (IER.EQ.0) GO TO 130
      IF (NF2ERR.LE.10) THEN
         WRITE (IPR,100) ISEGEX
         CALL ERROR
         ENDIF
100   FORMAT ('0**ERROR** ERROR ENCOUNTERED IN FGETSG FOR ',
     *   'SEGMENT NUMBER ',I6,'.')
C
      IF (IER.EQ.1.AND.NF2ERR.LE.10) THEN
         WRITE (IPR,110) IOPT,IRSGEX(ISEGEX)
         CALL ERROR
         ENDIF
110   FORMAT ('0**ERROR** INVALID ARGUMENTS PASSED TO FGETSG : ',
     *   'IOPT=',I5,' IREC =',I11)
C
      IF (IER.EQ.2.AND.NF2ERR.LE.10) THEN
         WRITE (IPR,120) MP,MT,MTS
         CALL ERROR
         ENDIF
120   FORMAT ('0**ERROR** NOT ENOUGH SPACE TO DEFINE P, T, OR TS ',
     *   'ARRAYS : MP=',I5,' MT=',I5,' MTS=',I5)
      GO TO 590
C
C  CHECK FOR A FLASH-FLOOD-GUIDANCE-ONLY RUN
C   IF IT IS, CHECK TO SEE IF THERE IS AN FFG OPERATION (OP # 32)
C   IN THE SEGMENT.  IF THERE IS NOT, SKIP PROCESSING THIS SEGMENT
C
130   IF (IFFG.NE.1) GO TO 135
      LOCP=1
      CALL FSERCH (32,RTNNAM,LOCP,P,MP)
      IF (LBUG) WRITE (IODBUG,125) LOCP
125   FORMAT (' ** IN FAZE2 **  FLASH-FLOOD-GUIDANCE-ONLY RUN - ',
     * 'FLASH-FLOOD-GUIDANCE OPERATION AT P ARRAY LOC=',I4)
      IF (LOCP.EQ.0) GO TO 610
C
C  FILL COMMON BLOCK FLARYS FROM COMMON BLOCK FCSEGN
C
135   LTS=NTS
      LP=NP
      LC=NC
      LT=NT
      LD=ND
C
C  SET ISEG IN COMMON BLOCK WHERE FROM IDSEGN IN COMMON BLOCK FCSEGN
C
      ISEG(1)=IDSEGN(1)
      ISEG(2)=IDSEGN(2)
C
      IF (LBUG) WRITE (IODBUG,140)
140   FORMAT (' IN FAZE2 - ABOUT TO CALL FCEXCT')
C
C  COMPUTE THE MINIMUM DELTA T FOR A SEGMENT BY LOOKING AT
C  THE DELTA TS FOR EACH OPERATION IN THE SEGMENT
      CALL FCEXCT (MINDT,IER)
C
      IF (LBUG) WRITE (IODBUG,150)
150   FORMAT (' IN FAZE2 - BACK FROM FCEXCT')
      IF (.NOT.NOTCPU) CALL FPRCPU
C
C     IER = 0, ALL OK
C         = 1, CANNOT FIND VALID START/END TIMES
C         = 2, CHANGE START/END TIMES, REDUCE SIMULATION PERIOD
      IF (IER.EQ.0) GO TO 155
      IF (IER.EQ.1) GO TO 590
C
      IF (LHRRUN.GT.0) GO TO 35
         LHRRUN=24
         LDARUN=LDARUN-1
   35 LDA=LDARUN
      LHR=LHRRUN
C
      CALL MDYH2 (LDARUN,LHRRUN,NENDM,NENDD,NENDY,NENDH,NZXX,NDXX,
     *   INPTZC)
c
c  post Xwindow atom for new end date/time
c
      Call set_end_date_atom(NENDM,NENDD,NENDY,NENDH,INPTZC)
c
      CALL MDYH2 (IDARUN,IHRRUN,NBEGM,NBEGD,NBEGY,NBEGH,NZXX,NDXX,
     *   INPTZC)
C
      WRITE (IPR,440) NBEGM,NBEGD,NBEGY,NBEGH,INPTZC,
     *              NENDM,NENDD,NENDY,NENDH,INPTZC
C
C
155   IF (LBUG) WRITE (IODBUG,160)
160   FORMAT (' IN FAZE2 - ABOUT TO CALL FCARGS')
C
C  SET ANY NON-UNIVERSAL TECHNIQUE AND ARGUMENT VALUES AND LOADS
C  ANY RUN-TIME MODS FOR THE CURRENT SEGMENT INTO ARRAY MODCRD
      CALL FCARGS (MODCRD,NCARDS,MAXCRD,IER)
C
      IF (LBUG) WRITE (IODBUG,170)
170   FORMAT (' IN FAZE2 - BACK FROM FCARGS')

      IF (.NOT.NOTCPU) CALL FPRCPU
C
      IF (IER.EQ.0) GO TO 190
C
      IF (NF2ERR.LE.10) THEN
         WRITE (IPR,180) IDSEGN
         CALL WARN
         ENDIF
180   FORMAT ('0**WARNING** PROBLEM IN SEGMENT ',2A4,
     *   ' GETTING TECHNIQUES, ARGUMENTS OR MODS.')
      GO TO 590
C
190   IF (NOPROT.EQ.1) GO TO 210
C
      IF (LBUG) WRITE (IODBUG,200) IDSEGN
200   FORMAT (' IDSEGN=',2A4)
C
C  CHANGE MOD CARDS IF IN INTERACTIVE FORECAST PROGRAM
C  (FMDINP WAS ORIGINALLY SUPPOSED TO MODIFY MOD CARDS FOR USE
C   BY THE INTERACTIVE PROGRAM. THERE IS NO NEED TO CHANGE THEM,
C   SO FMDINP IS CURRENTLY STUBBED OFF.)
210   IF (MAINUM.EQ.0) CALL FMDINP (MAXCRD,NCARDS,MODCRD,IDSEGN)
C
      IF (ITYPRN.EQ.1) GO TO 240
C
      IF (LBUG) WRITE (IODBUG,220)
220   FORMAT (' IN FAZE2 - ABOUT TO CALL FCDATE')
C
C  FILL COMMON BLOCK FCSEGC WITH THE CARRYOVER DATES AND TIMES
      CALL FCDATE (IDSEGN,0)
C
      IF (LBUG) WRITE (IODBUG,230)
230   FORMAT (' IN FAZE2 - BACK FROM FCDATE')
      IF (.NOT.NOTCPU) CALL FPRCPU
C
240   IF (NCOPS.LT.1) GO TO 340
      IF (IHRRUN.GT.0) GO TO 250
      IHRRUN=24
      IDARUN=IDARUN-1
C
250   IF (LBUG) WRITE (IODBUG,260)
260   FORMAT (' IN FAZE2 - ABOUT TO CALL FGETCO')
C
C  READ CARRYOVER INFORMATION INTO THE C ARRAY
      CALL FGETCO (IDSEGN,IDARUN,IHRRUN,C,MC,'ERROR',IER)
C
      IF (LBUG) WRITE (IODBUG,270)
270   FORMAT (' IN FAZE2 - BACK FROM FGETCO')
      IF (.NOT.NOTCPU) CALL FPRCPU
C
      IF (IHRRUN.LT.24) GO TO 280
      IHRRUN=0
      IDARUN=IDARUN+1
C
280   IF (IER.EQ.0) GO TO 340
C
      IF (NF2ERR.LE.10) THEN
         WRITE (IPR,290) IDSEGN
         CALL ERROR
         ENDIF
290   FORMAT ('0**ERROR** ERROR ENCOUNTERED IN FGETCO FOR SEGMENT ',
     *   2A4,'.')
C
      IF (NCSTOR.GT.0) WRITE (IPR,300)
300   FORMAT ('0**NOTE** CARRYOVER SAVE TERMINATED.')
      NCSTOR=0
C
      IDA=IDARUN
      IHR=IHRRUN
      LDA=LDARUN
      LHR=LHRRUN
      IDADAT=IDARUN
C
      IF (IER.NE.1) GO TO 320
C
      CALL MDYH2 (IDARUN,IHRRUN,NBEGM,NBEGD,NBEGY,NBEGH,NZXX,NDXX,
     *   INPTZC)
      CALL MDYH2 (LDARUN,LHRRUN,NENDM,NENDD,NENDY,NENDH,NZXX,NDXX,
     *   INPTZC)
C
c  post Xwindow atoms for new start and end dates/times
c
      Call set_start_date_atom(IDARUN,IHRRUN)
c
      Call set_end_date_atom(NENDM,NENDD,NENDY,NENDH,INPTZC)
c
      IF (NF2ERR.LE.10) THEN
         WRITE (IPR,310) IDSEGN,NBEGM,NBEGD,NBEGY,NBEGH,
     *      INPTZC,NENDM,NENDD,NENDY,NENDH,INPTZC
         CALL WARN
         ENDIF
310   FORMAT ('0**WARNING** START AND END DATES HAVE BEEN ',
     *  'CHANGED IN SEGMENT ',2A4,
     *  '. START=',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,
     *  ' END=',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4)
      GO TO 340
C
320   IF (NF2ERR.LE.10) THEN
         WRITE (IPR,330) IDSEGN
         CALL ERROR
         ENDIF
330   FORMAT ('0**ERROR** UNABLE TO READ CARRYOVER FOR SEGMENT ',
     *   2A4,'.')
      GO TO 590
C
C  SET VALUES IN FCTIME - GET DATA FOR ENTIRE PERIOD
C
340   IDA=IDARUN
      IHR=IHRRUN
      LDA=LDARUN
      LHR=LHRRUN
      IDADAT=IDARUN
C
C  SEARCH P ARRAY TO SEE IF API-CIN OPERATION IN THIS SEGMENT.
C  IF SO, SET VARIABLE NEEDTX SO THAT TEMPERATURE T.S. ARE
C  CHECKED FOR MISSING DATA IN ROUTINE FCTSRD.
C
      NEEDTX=0
      LOCCIN=1
      CALL FSERCH (33,APINAM,LOCCIN,P,MP)
      IF (LOCCIN.GT.0) NEEDTX=1
C
C    CHECK IF API-CONT OPERATION USING ATI IS IN THIS SEGMENT.
C    IF SO, SET NEEDTX=1 FOR SAME REASON AS ABOVE.
C
      LOCCIN=1
350   CALL FSERCH (24,APINAM,LOCCIN,P,MP)
      IF (LOCCIN.EQ.0) GO TO 360
      IVOPT=P(LOCCIN+13)
      IF (IVOPT.NE.2) GO TO 350
      NEEDTX=1
C
360   IF (LBUG) WRITE (IODBUG,370) NEEDTX
370   FORMAT (' IN FAZE2 - ABOUT TO CALL FCTSRD, NEEDTX=',I2)
C
C  READ ALL INPUT AND UPDATE TIME SERIES INTO THE D ARRAY
C
c  Set time series portion of the D array to missing.
c  This change is needed because the ifp PLOT-TUL operation (cex25)
c   displays RAIM and INFW time series without checking (in PIN25)
c   to see if they have been filled.  When a segment is defined with
c   the PLOT-TUL operation near the top of the operations table
c   (specifically, before any rainfall/runoff or snow operations)
c   the RAIM or INFW time series may not be filled.  This fix will
c   assure that all unfilled time series values in the D array
c   are set to missing.  Only fill up to location IWKLOC (from common
c   block FCSEGN) because this is the first location of available
c   work space in the D array (i.e., no time series data are stored
c   beyond this location).
c  Changed by gfs - hrl - 23 Aug 1994
c
      Do 364 idloc = 1, IWKLOC
 364  D(idloc) = -999.0
c
      CALL FCTSRD (TS,MTS,D,MD,IHRRUN,IER,MISSJD,MISSHR,IWKLOC)
C
      IF (LBUG) WRITE (IODBUG,380)
380   FORMAT (' IN FAZE2 - BACK FROM FCTSRD')
      IF (.NOT.NOTCPU) CALL FPRCPU
C
C     RESET WHERE COMMON BLOCK
C
      IOPNUM=0
      ISEG(1)=IDSEGN(1)
      ISEG(2)=IDSEGN(2)
      CALL UMEMOV (RTNNAM,OPNAME,2)
C
C     IER=0, ALL OK
C        =1, CANNOT READ TIME SERIES
C        =2, MISSING DATA ENCOUNTERED, REDUCE SIMULATION PERIOD
C
      IF (IER.EQ.0) GO TO 500
      IF (IER.EQ.1) GO TO 460
C
      CALL MDYH2 (MISSJD,MISSHR,MMONTH,MDAY,MYEAR,MHOUR,NZXX,NDXX,
     *   INPTZC)
C
      LHRRUN=((MISSHR-1)/MINDT)*MINDT
      LDARUN=MISSJD
      IF (LHRRUN.GT.0) GO TO 390
      LHRRUN=24
      LDARUN=LDARUN-1
390   LDA=LDARUN
      LHR=LHRRUN
C
      CALL MDYH2 (LDARUN,LHRRUN,NENDM,NENDD,NENDY,NENDH,NZXX,NDXX,
     *   INPTZC)
c
c  post Xwindow atom for new end date/time
c
      Call set_end_date_atom(NENDM,NENDD,NENDY,NENDH,INPTZC)
c
      WRITE (IPR,400) NENDM,NENDD,NENDY,NENDH,INPTZC,
     *  MMONTH,MDAY,MYEAR,MHOUR,INPTZC
      CALL WARN
400   FORMAT ('0**WARNING** THE ENDING DATE AND TIME HAVE BEEN ',
     *  'CHANGED TO ',I2,'/',I2,'/',I4,'-',I2,' ',A4 /
     *  23X,'BECAUSE MISSING DATA WERE ENCOUNTERED AT ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,' ',A4,'.')
C
      CALL MDYH2 (IDARUN,IHRRUN,NBEGM,NBEGD,NBEGY,NBEGH,
     *   NZXX,NDXX,INPTZC)
      WRITE (IPR,440) NBEGM,NBEGD,NBEGY,NBEGH,INPTZC,
     *              NENDM,NENDD,NENDY,NENDH,INPTZC
440   FORMAT ('0**NOTE** THE RUN PERIOD IS NOW FROM ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,' ',A4,' TO ',
     *  I2.2,'/',I2.2,'/',I4,'-',I2.2,' ',A4,'.')
C
C  CHECK WHETHER THE START OF THE RUN (IDARUN,IHRRUN) IS BEFORE
C  THE END OF THE RUN (LDARUN,LHRRUN).  STATUS RETURNED IN IILT.
      CALL FDATCK (IDARUN,IHRRUN,LDARUN,LHRRUN,ILT,IILT)
C
      IF (IILT.EQ.1) GO TO 480
C
      WRITE (IPR,450)
      CALL ERROR
450   FORMAT ('0**ERROR** THE ENDING DATE AND TIME IS BEFORE ',
     *  'OR AT THE BEGINNING DATE AND TIME.')
c
c  Set event_loop_exit_status to 7 (fatal_error_no_save) because fatal errors
c   have been encountered.  This will stop the main_fortran loop from
c   starting to process another segment, and avoid "out of control"
c   processing through all segments in a forecast group.
c  Added by gfs - hrl - 7 Sept 1994.
c
      event_loop_exit_status = 7
c      
      GO TO 630
C
460   IF (NF2ERR.LE.10) THEN
         WRITE (IPR,470) IDSEGN
         CALL ERROR
         ENDIF
470   FORMAT ('0**ERROR** UNABLE TO READ TIME SERIES ',
     *   'FOR SEGMENT ',2A4,'.')
      GO TO 590
C
c  Change end date atom if we encounter missing data so that
c   subsequent segments will not simulate for longer than
c   we are producing output time series.
c  gfs - 7/30/91
c
 87   Continue
      Call set_end_date_atom(NENDM,NENDD,NENDY,NENDH,INPTZC)
c
480   IF (NCSTOR.EQ.0) GO TO 500
C
C  CHECK IF DATES FOR WHICH CARRYOVER WILL BE SAVED ARE BEFORE OR AT
C  THE END OF THE RUN
      CALL FDATCK (LDARUN,LHRRUN,ICDAY(NCSTOR),ICHOUR(NCSTOR),
     *   ILT,IILT)
C
      IF (IILT.NE.1) GO TO 500
      WRITE (IPR,490)
      CALL ERROR
490   FORMAT ('0**ERROR** THE ENDING DATE AND TIME ARE ',
     *  'PRIOR TO THE LAST CARRYOVER SAVE DATE.')
      NCSTOR=0
C
C   ENDRUN MUST BE AT LEAST 24 HRS PAST LSTCMPDY FOR AN FFG RUN
C    (TURN OFF FFG ACTIVITY IF CRITERIA NOT MET)
C
500   IF (IFFG.EQ.0) GO TO 505
      LSTCMP=(LDACPD-1)*24+LHRCPD
      LSTRUN=(LDARUN-1)*24+LHRRUN
      IF (LSTRUN.GE.(LSTCMP+24)) GO TO 505
      CALL MDYH1 (LDACPD,LHRCPD,JCMO,JCDA,JCYR,JCHR,NOUTZ,
     *   NOUTDS,TZC)
      WRITE (IPR,405) NENDM,NENDD,NENDY,NENDH,INPTZC,
     *   JCMO,JCDA,JCYR,JCHR,TZC
      CALL WARN
405   FORMAT ('0**WARNING** THE FFG TECHNIQUE HAS BEEN TURNED ',
     *  'OFF AS THE RESET END OF THE RUN',
     *  ' (',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,')' /
     *  11X,'IS NOW LESS THAN 24 HOURS BEYOND THE END OF THE ',
     *  'COMPUTATIONAL PERIOD',
     *  ' (',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,').')
C
C  IF THE RUN WAS FLASH-FLOOD GUIDANCE ONLY, JUMP OUT OF SEGMENT LOOP
C   EFFECTIVELY KILLING THIS FCEXEC FUNCTION EXECUTION.  OTHERWISE,
C   JUST SHUT OFF FFG PROCESSING
C
      IF (IFFG.EQ.1) GO TO 630
      IFFG=0
C
C  TURN OFF ALL PRINTOUT IS THIS IS A FLASH FLOOD GUIDANCE ONLY RUN,
C
505   NE=NERRS
      IF (IFFG.EQ.1) NOPROT=1
c
      event_loop_exit_status = 0      
C
cew   clean out previous sac model and snow model state files and
cew   open new ones 
cew ADD AN IF ON IOUTYP
      IF (IOUTYP.EQ.1)THEN 
        CALL ifp_icp(IDA, IHR, LDA, LHR, IDARUN, LDARUN, 
     +  METRIC, P, MP, IDSEGN, IFILERR)
        IF (IFILERR.EQ.1) THEN
          IOUTYP=0
          WRITE (IPR,507) 
          CALL WARN
507       FORMAT ('0**WARNING** TROUBLE OPENING FILES IN ~/.ifp_files/',
     +   'sac_snow). IOUTYP IS SET TO ZERO. SAC SNOW FILES WILL NOT BE ',
     +   'CREATED OR DISPLAYED.')
        ENDIF
      ELSE
C *** del_all_files is locate in ifp_icp.c ****
         CALL del_all_files ()
      ENDIF

      IF (LBUG) WRITE (IODBUG,510)
510   FORMAT (' IN FAZE2 - ABOUT TO CALL FDRIVE')
      IF (DUMPCB) CALL FDMPCB (DCBA,DCBB,DCBC,DCBD,DCBE,DCBF)
C
C    SET FLDGRAF FILE
      IOPFIL55 = 0

C jgg following change added for MR 1954 
C    Set flag to identify IFP run to allow flood mapping to be controlled
      IFP55 = 1
C end of jgg changes      
C
C  CALL ROUTINES THAT EXECUTE THE OPERATIONS FOR THIS SEGMENT
      CALL FDRIVE (P,MP,C,MC,T,MT,TS,MTS,D,MD,IHRRUN)
C
C
      IF (LBUG) WRITE (IODBUG,520)
520   FORMAT (' IN FAZE2 - BACK FROM FDRIVE')
      IF (.NOT.NOTCPU) CALL FPRCPU
      IF (DUMPCB) CALL FDMPCB (DCBA,DCBB,DCBC,DCBD,DCBE,DCBF)
C
C     RESET COMMON BLOCK /WHERE/ AFTER CALL TO FDRIVE
C
      ISEG(1)=IDSEGN(1)
      ISEG(2)=IDSEGN(2)
      CALL UMEMOV (RTNNAM,OPNAME,2)
C
      IF (NE.EQ.NERRS) GO TO 550
C
      IF (NF2ERR.LE.10) THEN
         WRITE (IPR,530) IDSEGN
         CALL ERROR
         WRITE (IPR,540)
         ENDIF
530   FORMAT ('0**ERROR** ERROR OCCURRED PROCESSING OPERATIONS ',
     *  'TABLE FOR SEGMENT ',2A4,'.')
540   FORMAT ('0**NOTE** SEGMENT ENDED - NO TIME SERIES WRITTEN ',
     *   'TO FILE.')
      GO TO 590
C
c
c  If 'Rerun' (1), 'Go Upstream' (4), or 'Quit_no_save' was selected in
c   Run_NWSRFS 'Control' menu do not finish operations table - just
c   leave and restart the Forecast_Program for the current
c   segment or the selected upstream segment can be (re)simulated -
c   no need to write output time series -- gfs, 7/23/91
c
550   if((event_loop_exit_status .eq. 1) .or.
     1   (event_loop_exit_status .eq. 4) .or.
     2   (event_loop_exit_status .eq. 6)) go to 630
c
c  If 'Next', 'Continue', or 'Quit' was selected, we want
c   to finish processing the segment, including applying
c   any final Mods and writing output time series.
c
      IF (LBUG) WRITE (IODBUG,560) IFFG
560   FORMAT (' IN FAZE2 - ABOUT TO CALL FCTSWT. IFFG=',I2)
C
C  WRITE OUTPUT AND UPDATE TIME SERIES FROM THE D ARRAY TO FILE
C  IF THIS IS NOT A FFG-ONLY RUN
      IF (IFFG.NE.1) THEN
         CALL FCTSWT (TS,MTS,D,MD,IHRRUN,IWKLOC,IER)
         ENDIF
C
C  RESET WHERE COMMON BLOCK
      IOPNUM=0
      ISEG(1)=IDSEGN(1)
      ISEG(2)=IDSEGN(2)
      CALL UMEMOV (RTNNAM,OPNAME,2)
C
      IF (LBUG) WRITE (IODBUG,570)
570   FORMAT (' IN FAZE2 - BACK FROM FCTSWT')
      IF (.NOT.NOTCPU) CALL FPRCPU
C
      IF (IER.EQ.0) GO TO 610
C
      IF (NF2ERR.LE.10) THEN
         WRITE (IPR,580) IDSEGN
         CALL ERROR
         ENDIF
580   FORMAT ('0**ERROR** ERROR OCCURRED WRITING TIME SERIES ',
     *  'TO FILE FOR SEGMENT ',2A4,'.')
C
590   IF (NCSTOR.EQ.0) GO TO 600
         WRITE (IPR,300)
         NCSTOR=0
C
600   NF2ERR=NF2ERR+1
c
c  Set event_loop_exit_status to 7 (fatal_error_no_save) because fatal errors
c   have been encountered.  This will stop the main_fortran loop from
c   starting to process another segment, and avoid "out of control"
c   processing through all segments in a forecast group.
c  Added by gfs - hrl - 7 Sept 1994.
c
      event_loop_exit_status = 7
C
610   IF (KLCODE.GT.8) GO TO 630
C
620   CONTINUE
C
630   IF (NF2ERR.GT.0) WRITE (IPR,640) NF2ERR
640   FORMAT ('0**NOTE** TOTAL NUMBER OF ERRORS ENCOUNTERED IN ',
     *  'ROUTINE FAZE2 IS ',I4,'.')
C
      IOPNUM=IOLDOP
      CALL UMEMOV (OPNOLD,OPNAME,2)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,650)
650   FORMAT ('0** EXIT FAZE2 **')
C
      RETURN
C
      END
