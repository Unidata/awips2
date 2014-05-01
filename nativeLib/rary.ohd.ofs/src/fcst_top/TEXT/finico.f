C MODULE FINICO
C  (used to be FNITCO)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/27/95.11:40:22 BY $WC20SV
C
C @PROCESS LVL(77)
C
C
C DESC: INITIALIZES COMMON BLOCKS AND VARIABLES FOR CARRYOVER RUN
C
C  INPUT -- CGID1 , THE CARRYOVER GROUP IDENTIFIER (2A4)
C
C.......................................................................
C
      SUBROUTINE FINICO (CGID1)
C.......................................................................
C
C  ROUTINE ORIGINALLY WRITTEN BY
C          JOE OSTROWSKI -- HRL -- 791022
C
C......................................................................
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcio'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccvp'
      INCLUDE 'common/fctime'
C
      DIMENSION CGID1(2)
      DIMENSION OPNOLD(2)
      DIMENSION JSAVE(10),ITEMP(10),ISUM(10),IPTMP(10)
      EQUIVALENCE (ZAP,IZAP)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/finico.f,v $
     . $',                                                             '
     .$Id: finico.f,v 1.3 1996/01/14 17:44:10 page Exp $
     . $' /
C    ===================================================================
C
      DATA ZAP/4H    /
C
C
      IOPNUM=0
      CALL UMEMOV (OPNAME,OPNOLD,2)
      CALL UMEMOV ('FINICO   ',OPNAME,2)
C
      IBUG=0
      IF (IFBUG('COTR').EQ.1) IBUG=1
      IF (IFBUG('COBG').EQ.1) IBUG=2
C
C
      IF (IBUG.GE.1) WRITE (IODBUG,170)
C
C.......................................................................
C
C  INITIALIZE SEGMENT AND CARRYOVER GROUP ID'S IN /FCIO/ AND /FCSEGC/
C
C.......................................................................
C
      DO 10 I=1,2
         IDSEG(I)=IZAP
         IDC(I)=IZAP
10       CONTINUE
C
C......................................................................
C
C  AN 'UPDATE' RUN IF NCSTOR= -1.
C  NOT A CARRYOVER SAVE RUN IF NCSTOR (FROM /FCARY/) IS ZERO.
C
C.......................................................................
C
      IF (IBUG.GE.2) WRITE (IODBUG,180) NCSTOR,NSLOTS,CGID1
C
C
      IF (NCSTOR.EQ.-1) GO TO 20
      IF (NCSTOR.LT.1) GO TO 150
C
C.......................................................................
C
C  NO CARRYOVER WILL BE SAVED IF ONLY ONE SLOT HAS BEEN ALLOCATED
C
C.......................................................................
C
20    IF (NSLOTS.GT.1) GO TO 30
      WRITE (IPR,190)
      CALL ERROR
      GO TO 150
C
30    CONTINUE
C
      IF (IBUG.GE.2) WRITE (IODBUG,200) IDSEG,IDC,CGIDC
C
C.......................................................................
C
C  A MAXIMUM OF TEN SAVE DATES IS ALLOWED.
C
C.......................................................................
C
      MCSTOR=10
      IF (NCSTOR.GT.MCSTOR) THEN
         WRITE (IPR,210) NCSTOR,MCSTOR
         CALL ERROR
         GO TO 150
         ENDIF
C
C.......................................................................
C
C  THE NUMBER OF CARRYOVER SLOTS MUST EXCEED THE NUMBER OF SAVE DATES.
C
      IF (NCSTOR.GT.NSLOTS) THEN
         WRITE (IPR,220) NCSTOR,NSLOTS,NSLOTS
         CALL WARN
         NCSTOR=NSLOTS
         ENDIF
C
C.......................................................................
C
C  NCVALS AND NVALSW FROM /FCIO/ INITIALIZED TO ONE AND ZERO, RESP..
C  KFCTMP(I) ARE TEMPORARY CARRYOVER FILES.
C
C.......................................................................
C
      NCVALS=1
C
      DO 40 I=1,10
         NVALSW(I)=0
40       CONTINUE
C
      DO 50 I=1,10
         ICFNUM(I)=KFCTMP(I)
50       CONTINUE
C
C
C....................................................................
C
C  THE INPUT CO GROUP ID (CGID1) MUST BE PREVIOUSLY DEFINED.
C   THE ID HELD IN /FCCGD1/ MUST MATCH THAT ID INPUT TO THE ROUTINE.
C   COMMON /FCCGD1/ MUST HAVE BEEN FILLED PRIOR TO ENTERING 'FCFINICO'.
C
C.......................................................................
C
      IF(CGID1(1).NE.CGIDC(1).OR.CGID1(2).NE.CGIDC(2)) GO TO 60
C
      GO TO 70
C
60    WRITE (IPR,230) CGID1,CGIDC
       NCSTOR=0
      CALL ERROR
      GO TO 150
C
C............................................................
C
C  ROUTINE FASINC PERFORMS SEVERAL FUNCTIONS:
C   1) REMOVES ANY USER ASSIGNED CARRYOVER DATES THAT MATCH OR PRECEDE
C      THE INITIAL RUN DATE OR EXCEED THE END DATE.
C   2) CHECKS FOR AND REMOVES ANY DUPLICATE USER ASSIGNED CARRYOVER
C      DATES.
C   3) ASSIGNS CARRYOVER SLOTS TO SAVE CARRYOVER:
C       A) USES ANY SLOT WHOSE DATE MATCHES ANY USER ASSIGNED DATE
C       B) USES ANY SLOT THAT FALLS WITHIN THE RUN PERIOD
C       C) ASSIGNS THE REMAINDER OF THE SLOTS BY GIVING UP THE
C          INCOMPLETE SLOTS( EITHER VOLATILE OR PROTECTED) FIRST,
C          AND THEN USING THE COMPLETE VOLATILE SLOTS.
C           ** COMPLETE PROTECTED SLOTS ARE SACRED AND UNTOUCHED
C              WHEN ASSIGNING SLOTS AT THIS POINT **
C
C............................................................
C
70    CALL FASINC (JSAVE)
C
C....................................................................
C
C  REORDER ALL CARRYOVER SAVE DATES - IN ASCENDING ORDER.
C
C  REORDER THE ASSIGNED SLOTS WITH THE DATES.
C  ALSO REORDER THE VOL/PROTECT SWITCHES WITH THE DATES.
C
C....................................................................
C
      DO 80 KJ=1,NCSTOR
         ISUM(KJ)=ICDAY(KJ)*24+ICHOUR(KJ)
         IPTMP(KJ)=IPROT(KJ)
         ITEMP(KJ)=JSAVE(KJ)
80       CONTINUE
C
      CALL FCOBBL (ICDAY,ICHOUR,NCSTOR)
      IF (NCSTOR.EQ.0) GO TO 150
C
      DO 100 KJ=1,NCSTOR
         IRP=ICDAY(KJ)*24+ICHOUR(KJ)
         DO 90 JK=1,NCSTOR
            IF (IRP.NE.ISUM(JK)) GO TO 90
            JSAVE(KJ)=ITEMP(JK)
            IPROT(KJ)=IPTMP(JK)
            GO TO 100
90          CONTINUE
100      CONTINUE
C
      IF (IBUG.LT.2) GO TO 120
      WRITE (IODBUG,240)
      DO 110 IL=1,NCSTOR
         WRITE (IODBUG,250) IL,ICDAY(IL),ICHOUR(IL)
110      CONTINUE
C
C.....................................................................
C
C  FOR ALL THE SAVE DATES (NCSTOR), WRITE THE FIRST RECORD OF THE
C   TEMPORARY FILES.
C
C   ISLOT IS THE SLOT NO. ON THE PERMANENT FILES.
C   CGID1 IS THE CARRYOVER GROUP IDENT.
C   IPROT IS THE PROTECTION STATUS FOR THE SAVE DATE
C   ICDAY AND ICHOUR ARE THE SAVE DAY AND TIME TO BE STORED.
C
C  UNFORMATTED WRITE IS USED FOR THESE SEQUENTIAL FILES.
C
C.....................................................................
C
120   DO 140 I=1,NCSTOR
         ISLOT=JSAVE(I)
         IU=ICFNUM(I)
         REWIND IU
C     CHANGE ALL SAVE DATES WITH HOUR OF ZERO.
C     REDUCE JULIAN DAY BY 1 AND SET HOUR TO 24
         IF (ICHOUR(I).NE.0) GO TO 130
         ICDAY(I)=ICDAY(I)-1
         ICHOUR(I)=24
130      IF (IBUG.GE.2) WRITE (IODBUG,260) IU,CGID1,ISLOT,IPROT(I),
     *      ICDAY(I),ICHOUR(I)
         WRITE (IU) ISLOT,CGID1,IPROT(I),ICDAY(I),ICHOUR(I)
140      CONTINUE
C
150   CALL UMEMOV (OPNOLD,OPNAME,2)
C
      IF (IBUG.GE.1) WRITE (IPR,160)
160   FORMAT (' *** EXIT FINICO')
C
      RETURN
C
C- - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT (' *** ENTER FINICO')
180   FORMAT(28H  NO. OF SAVE DATES(NCSTOR)=,I3,22H NO. OF SLOTS(NSLOTS)
     *=, I4,25H INPUT COGROUP ID(CGID1)=,2A4)
190   FORMAT ('0**ERROR** NO CARRYOVER WILL BE SAVED. ONLY ONE ',
     *   'SLOT HAS BEEN ALLOCATED AND WILL NOT BE OVERWRITTEN.')
200   FORMAT(08H  IDSEG=,2A4,06H  IDC=,2A4,08H  CGIDC=,2A4)
210   FORMAT ('0**ERROR** NUMBER OF CARRYOVER DATES REQUESTED (',I2,
     *  ') EXCEEDS MAXIMUM ALLOWED (',I2,').')
220   FORMAT ('0**ERROR** THE NUMBER OF SAVE DATES REQUESTED (',I2,
     *  ') EXCEEDS THE NUMBER OF CARRYOVER SLOTS ALLOCATED (',I2,
     *    '). THE NUMBER OF DATES HAS BEEN RESET TO ',I3,'.')
230   FORMAT ('0**ERROR** CARRYOVER GROUP ID PASSED TO ROUTINE ',
     *  'FINICO (',2A4,') DOES NOT MATCH THE ID HELD IN COMMON ',
     *  'FCCGD1. NO CARRYOVER WILL BE SAVED.')
240   FORMAT(1H0,9X,42HNO.   DAY      HOUR  --   CARRY SAVE DATES,
     * 27H SORTED IN ASCENDING ORDER.)
250   FORMAT (10X,I2,I8,I8)
260   FORMAT(17H  TEMP. UNIT NO.=,I3,13H COGROUP ID= ,2A4,06H SLOT=,I3
     * /  18H     PROT. STATUS=,I3,10H SAVE DAY=,I6,11H SAVE TIME=,I10)
C
      END
