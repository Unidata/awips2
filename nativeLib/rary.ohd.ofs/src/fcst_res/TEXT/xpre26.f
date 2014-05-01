C MEMBER XPRE26
C  (from old member FCXPRE26)
C
C DESC PERFORM PRE-TIME-INTERVAL-LOOP TASKS (IE - EXECUTE UTILITIES)
C-----------------------------------------------------------------
      SUBROUTINE XPRE26(PO,CO,W,D,IDPT,LOCWS,LOCOWS,LOBSA)
C------------------------------------------------------------------
C  SUBROUTINE TO PERFORM ANY REQUESTED PRE-COMPUTATION LOOP TASKS,
C  AND TO SET SOME SWITCHES FOR LOOP TASKS.
C-------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - JULY 1983
C------------------------------------------------------------------
C
      INCLUDE 'common/exg26'
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/xre26'
C
      DIMENSION PO(1),CO(1),W(1),D(1),IDPT(1),LOCOWS(1),LOCWS(1)
C
      LOGICAL DORULE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xpre26.f,v $
     . $',                                                             '
     .$Id: xpre26.f,v 1.6 2005/02/22 13:52:35 hsu Exp $
     . $' /
C    ===================================================================
C
C
C  PRINT TRACE INFO IF REQUESTED
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XPRE26 ***')
C
C  INITIALIZE SWITCHES TO THE OFF POSITION
C
      DORULE = .FALSE.
      DOBACK = .FALSE.
      DOSUMF = .FALSE.
      DORAIN = .FALSE.
      ADJUST = .FALSE.
      BFSAVQ = .FALSE.
      WRADLQ = .FALSE.
C
C  SCAN THRU THE S/U POINTER ARRAY FOR REQUESTS FOR VARIOUS UTILITIES
C  TO BE EXECUTED
C
c      IF (ISUPT(1,14).GT.0) DORULE = .TRUE.
      ICSU = 0
      DO 550 I=1,NSUDEF
      LOCSUN = LOCPTR + (I-1)*4 + 1
      ISUNUM = PO(LOCSUN)
      IBASE=ISUNUM/10
      IF (IBASE.EQ.151) DORULE = .TRUE.
C
C      IF (ISUPT(1,18).GT.0) ICSU = ICSU + 1
C      IF (ISUPT(1,15).GT.0) ICSU = ICSU + 2
C      IF (ISUPT(1,16).GT.0) ICSU = ICSU + 4
C      IF (ISUPT(1,17).GT.0) ICSU = ICSU + 8
      IF (IBASE.EQ.155) ICSU = ICSU + 1
      IF (IBASE.EQ.152) ICSU = ICSU + 2
      IF (IBASE.EQ.153) ICSU = ICSU + 4
      IF (IBASE.EQ.154) ICSU = ICSU + 8
 550  CONTINUE
C
      IF (MOD(ICSU,2).EQ.1)  DOBACK = .TRUE.
      IF (MOD(ICSU,4).GE.2)  DOSUMF = .TRUE.
      IF (MOD(ICSU,8).GE.4)  DORAIN = .TRUE.
      IF (MOD(ICSU,16).GE.8) ADJUST = .TRUE.

      IF (MAINUM .EQ. 200) THEN
        DOBACK = .FALSE.
        ADJUST = .FALSE.
        BFSAVQ = .FALSE.
      ENDIF
      
C
C  COMPUTE AREA CURVE IF RAINEVAP IS REQUESTED.
C
      I5 = 5
      LPTAEL = LOCOWS(I5) + NDD*NTIM24
      LPTARA = LPTAEL + NSE
      IF (DORAIN) CALL XCMA26(PO(LESELV),NSE,W(LPTAEL),W(LPTARA))
      IF (DORAIN) CALL XREV26(PO,W,IDPT,LOCOWS)
C
C  NOW EXECUTE REQUESTED UTILITIES
C
      IF (DOBACK) CALL XU1826(PO,W,D,CO,LOCWS,LOCOWS,IDPT,LOBSA)
      IF (DORULE) CALL XU1426(PO,CO,W,D,IDPT,LOCOWS)
C
C
C  DEBUG OUTPUT IF REQUESTED.
C
      IF (IBUG.GE.2) WRITE(IODBUG,1690) DORULE,DOBACK,DOSUMF,DORAIN,
     .               ADJUST
 1690 FORMAT(1H0,' ** DORULE =',L4/'   ** DOBACK =',L4/'    ** DOSUMF ='
     .       ,L4/'     ** DORAIN =',L4/'      ** ADJUST =',L4)
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XPRE26 ***')
      RETURN
      END
