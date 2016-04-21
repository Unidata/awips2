C MODULE SETUP2
C-----------------------------------------------------------------------
C
      SUBROUTINE SETUP2 (P,MP,C,MC,T,MT,TS,MTS,D,MD,NWORK,LWORK,IDT,
     1  IPASS,NXP2,LEFTP,IUSEP,NXC2,LEFTC,IUSEC,NXT,LEFT,IUSET,ISKIP,
     2  NUMOP,IERR)
C
C  SETUP2 CALLS INPUT AND OPERATIONS TABLE ENTRY ROUTINES
C     FOR OPERATIONS 20 - 40
C
C  ROUTINE INITIALLY WRITTEN BY - ERIC ANDERSON - HRL   APRIL 1981
C
      DIMENSION P(MP),C(MC),TS(MTS),D(MD)
C
      INTEGER T(MT)
C
      INCLUDE 'common/fclfls'
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      EQUIVALENCE (ICVAR1,IAVAIL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/setup2.f,v $
     . $',                                                             '
     .$Id: setup2.f,v 1.4 2002/02/11 12:46:43 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE(IODBUG,*) 'ENTER SETUP2'
C
C  GO TO THE SECTION FOR THE CURRENT OPERATION AND CALL INPUT ROUTINE
C  IF IPASS IS 0 OR CALL OPERATIONS TABLE ENTRY ROUTINE IF IPASS IS 1
      NUM=NUMOP-19
      GO TO (20,60,80,100,120,140,160,180,200,220,
     1       240,255,260,290,310,330,350,370,390,
     3       430,450),NUM
      GO TO 480
C
C.......................................................................
C
C  CHANGE TIME INTERVAL OPERATION
C
20    IF(IPASS.EQ.1) GO TO 50
      CALL PIN20(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF(IUSEP.GT.0) GO TO 40
30    ISKIP=1
      GO TO 490
40    NEEDC=P(NXP2+9)
      IF(NEEDC.EQ.0) GO TO 490
      IF(IUSEC.GT.0) GO TO 490
      GO TO 30
50    CALL TAB20(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,NWORK,
     1NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  DWOPER OPERATION
C
60    IF(IPASS.EQ.1) GO TO 70
      CALL PIN21(P(NXP2),P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC,D,MD)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
70    CALL TAB21(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,NWORK,
     1NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  SS-SAC OPERATION
C
80    IF(IPASS.EQ.1) GO TO 90
      CALL PIN22(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
90    CALL TAB22(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS)
      GO TO 490
C
C.......................................................................
C
C  STAGE-DISCHARGE CONVERSION
C
100   IF(IPASS.EQ.1) GO TO 110
      CALL PIN23(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
110   CALL TAB23(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,NWORK,
     1NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C   API-CONT OPERATION
C
120   IF (IPASS.EQ.1) GO TO 130
      CALL PIN24(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF ((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
130   CALL TAB24(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,
     1   LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  PLOT-TUL OPERATION
C
140   IF(IPASS.EQ.1) GO TO 150
      CALL PIN25(P(NXP2),LEFTP,IUSEP)
      IF (IUSEP.GT.0) GO TO 490
      ISKIP=1
      GO TO 490
150   CALL TAB25(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),TS,MTS,NWORK,
     1  NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  SINGLE RESERVOIR SIMULATION OPERATION
C
160   IF(IPASS.EQ.1)GO TO 170
      CALL PIN26(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC,D,MD)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
170   CALL TAB26(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),
     1NXC2,TS,MTS,NWORK,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  FORT WORTH TABULAR DISPLAY
C
180   IF(IPASS.EQ.1)GO TO 190
      CALL PIN27(P(NXP2),LEFTP,IUSEP)
      IF(IUSEP.GT.0)GO TO 490
      ISKIP=1
      GO TO 490
190   CALL TAB27(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),
     1   TS,MTS,NWORK,NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  CHANNEL LEAK OPERATION
C
200   IF(IPASS.EQ.1) GO TO 210
      CALL PIN28(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
210   CALL TAB28(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,
     1   TS,MTS,NWORK,NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  KANSAS CITY API OPERATION
C
220   IF(IPASS.EQ.1) GO TO 230
      CALL PIN29(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
230   CALL TAB29(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,
     1  TS,MTS,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  MERGE TIME SERIES OPERATION
C
240   IF(IPASS.EQ.1) GO TO 250
      CALL PIN30(P(NXP2),LEFTP,IUSEP)
      IF (IUSEP.GT.0) GO TO 490
      ISKIP=1
      GO TO 490
250   CALL TAB30(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),
     1  TS,MTS,LWORK,IDT)
      GO TO 490
C.......................................
C     SNOW-43 OPERATION-ALL APPLICATIONS--WORKING SPACE
  255 IF(IPASS.EQ.1) GO TO 256
      CALL PIN31(p,mp,P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
  256 CALL TAB31(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,NWORK,
     1  LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  FLASH FLOOD GUIDANCE OPERATION
C
260   IF (IPASS.EQ.1) GO TO 280
      IAVAIL=1
      IF (IAVAIL.EQ.0) THEN
         WRITE (IPR,500) 'FLASH FLOOD QUIDANCE'
         CALL WARN
         IUSEP=0
         GO TO 270
         ENDIF
      IF (MAINUM.NE.1) GO TO 270
      CALL PIN32(P(NXP2),LEFTP,IUSEP,P,MP,D,MD)
      IF (IWRPPP.GT.0) THEN
         CALL WPPPCO (IERR)
         IF (IERR.GT.0) THEN
            WRITE (IPR,510)
            CALL ERROR
            ELSE
               WRITE (IPR,520)
            ENDIF
         CALL UCLOSL
         ENDIF
      IF (IUSEP.GT.0) GO TO 490
270   ISKIP=1
      GO TO 490
280   CALL TAB32(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),TS,MTS,P,MP,
     1  NWORK,LWORK)
      GO TO 490
C
C.......................................................................
C
C  CINCINNATI API OPERATION
C
290   IF (IPASS .EQ. 1) GO TO 300
      CALL PIN33(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF ((IUSEP .GT. 0) .AND. (IUSEC .GT. 0)) GO TO 490
      ISKIP = 1
      GO TO 490
300   CALL TAB33(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,
     1  LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  SALT LAKE CITY API OPERATION
C
310   IF (IPASS .EQ. 1) GO TO 320
      CALL PIN34(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF ((IUSEP .GT. 0) .AND. (IUSEC .GT. 0)) GO TO 490
      ISKIP = 1
      GO TO 490
320   CALL TAB34(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,
     1  LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  HARRISBURG RFC API OPERATION
C
330   IF (IPASS .EQ. 1) GO TO 340
      CALL PIN35(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF ((IUSEP .GT. 0) .AND. (IUSEC .GT. 0)) GO TO 490
      ISKIP = 1
      GO TO 490
340   CALL TAB35(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,
     1  LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C     XINANJIANG MODEL
C
350   IF(IPASS.EQ.1) GO TO 360
      CALL PIN36(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF ((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
360   CALL TAB36(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,NWORK,
     1   LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  MINNEAPOLIS RUNOFF TABULATION
C
370   IF (IPASS .EQ. 1) GO TO 380
      CALL PIN37(P(NXP2),LEFTP,IUSEP,P,MP)
      IF (IUSEP.GT.0) GO TO 490
      ISKIP = 1
      GO TO 490
380   CALL TAB37(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),TS,MTS,
     1  NWORK,NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  BASEFLOW OPERATION
C
390   IF(IPASS.EQ.1)GO TO 420
      CALL PIN38(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC)
      IF(IUSEP.GT.0)GO TO 410
400   ISKIP=1
      GO TO 490
410   NEEDC=P(NXP2+11)
      IF(NEEDC.EQ.0)GO TO 490
      IF(IUSEC.GT.0)GO TO 490
      GO TO 400
420   CALL TAB38(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),
     1           NXC2,TS,MTS,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  TABLE LOOKUP -- FT. WORTH
C
430   IF(IPASS.EQ.1)GO TO 440
      CALL PIN39(P(NXP2),LEFTP,IUSEP)
      IF(IUSEP.GT.0)GO TO 490
      ISKIP=1
      GO TO 490
440   CALL TAB39(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),
     1 TS,MTS,NWORK,NDD,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  WATER BALANCE OPERATION -- CALIBRATION ONLY
C
450   IF(IPASS.EQ.1) GO TO 470
      IF (MAINUM.GT.2) GO TO 460
      ISKIP=1
      GO TO 490
460   CALL DEFWY
      CALL PIN40(P(NXP2),LEFTP,IUSEP,C(NXC2),LEFTC,IUSEC,P,MP,D,MD)
      IF((IUSEP.GT.0).AND.(IUSEC.GT.0)) GO TO 490
      ISKIP=1
      GO TO 490
470   CALL TAB40(T(NXT),LEFT,IUSET,NXT,NXP2,P(NXP2),NXC2,TS,MTS,P,MP,
     1  NWORK,LWORK,IDT)
      GO TO 490
C
C.......................................................................
C
C  OPERATION NOT FOUND
480   WRITE (IPR,530) NUMOP
      CALL ERROR
      IERR=1
C
490   IF (ITRACE.GE.1) WRITE(IODBUG,*) 'EXIT SETUP2'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
500   FORMAT ('0**WARNING** THE ',A,' OPERATION IS NOT CURRENTLY ',
     *   'AVAILABLE AND WILL BE IGNORED.')
510   FORMAT ('0**ERROR** WRITING PREPROCESSOR PARAMETRIC ',
     *   'DATA BASE CONTROL RECORDS.')
520   FORMAT ('0**NOTE** PREPROCESOR PARAMETRIC ',
     *   'DATA BASE CONTROL RECORDS ',
     *   'SUCCESSFULLY WRITTEN.')
530   FORMAT ('0**ERROR** OPERATION NUMBER ',I3,' NOT FOUND.')
C
      END
