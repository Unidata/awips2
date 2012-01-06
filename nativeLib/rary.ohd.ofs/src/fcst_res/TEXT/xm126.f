C MEMBER XM126
C  (from old member FCXM126)
C
C DESC EXECUTE 'SETQMEAN' MOD FOR TIME PERIOD BY CALLING 'SETQ'
C DESC  COMPUTATIONAL ROUTINE.
C--------------------------------------------------------------------
      SUBROUTINE XM126(VALUE,PO,W,LOCWS)
C----------------------------------------------------------------
C  SUBROUTINE TO CALL THE 'SETQ' ROUTINE AFTER SUPPLYING THE
C  PROPER PARAMETERS TO USING THE ROUTINE. XM126 IS NEEDED FOR
C  THE SETQMEAN RUN-TIME MOD.
C------------------------------------------------------------------
C  WRITTEN BY - JTOSTROWSKI - FEBRUARY 1985 - HRL
C--------------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/exg26'
      INCLUDE 'common/pres26'
      INCLUDE 'common/resv26'
C
      DIMENSION PO(1),W(1),LOCWS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xm126.f,v $
     . $',                                                             '
     .$Id: xm126.f,v 1.1 1995/09/17 19:06:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG .GE. 1) WRITE(IODBUG,1601)
 1601 FORMAT('   *** ENTER XM126 ***')
C
      IF (IBUG .GE. 2) WRITE(IODBUG,1602) VALUE
 1602 FORMAT('  SETQMEAN MOD IN EFFECT - MEANQ = ',F12.3)
C
C  SET 'SETQ' MISSING VALUE OPTION TO PASS INFLOW
C
      IOPT = 0
C
C  SET 'SETQ' DATA TYPE TO PERIOD MEAN
C
      IQO = 0
C
C  SET MEAN PERIOD DISCHARGE
C
      QOM = VALUE * NTIM24
C
C  CALL 'SETQ' MODEL
C
      CALL PREQ26(QOM,PO(LESSTO),PO(LESELV))
C
C  STORE 'SETQ' OUTPUTS IN TRACE LOCATION
C
      NSCHEX = 1
      IOFF = LOCWS(3)
      W(IOFF+2) = QOM
      W(IOFF+3) = QO2
      W(IOFF+4) = ELEV1
      W(IOFF+5) = ELEV2
      W(IOFF+6) = S2
C
C  THAT'S IT. DO A LITTLE TRACE OUTPUT IF NEEDED.
C
      IF (IBUG .GE. 1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XM126 ***')
      RETURN
      END
