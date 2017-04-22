C     MEMBER OPECRV
C
      SUBROUTINE OPECRV(ETHIGH,ETLOW,E,ENEW,OPNEW)
C
C..........................................
C     THIS SUBROUTINE COMPUTES A NEW EVAPORATION CURVE BASED ON
C     THE VALUES OF ETHIGH AND ETLOW.
C..........................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   JUNE 1981   VERSION 1
C..........................................
C
      DIMENSION E(12),OPNEW(2),ENEW(12),EPOR(12)
      DIMENSION OLDOPN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/opecrv.f,v $
     . $',                                                             '
     .$Id: opecrv.f,v 1.2 1996/07/11 20:48:57 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA IBUG/4HOPT /
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1000)
 1000 FORMAT(1H0,17H** OPECRV ENTERED)
C
      CALL FSTWHR('OPECRV  ',0,OLDOPN,IOLDOP)
C
C     INITIALIZE VARIABLES.
      EMAX=0.0
      EMIN=0.0
C     FIND MAXIMUM AND MINIMUM EVAP VALUES.
C
      DO 50 J=1,12
      IF(E(J).GT.EMAX) EMAX=E(J)
      IF(J.EQ.1) EMIN=EMAX
      IF(E(J).LT.EMIN) EMIN=E(J)
C
   50 CONTINUE
C
C     COMPUTE RELATIVE POSITION OF EVAP VALUES.
C
      DIFF=EMAX-EMIN
      IF(DIFF.NE.0) GO TO 60
      IF(IOTHER.GT.1) GO TO 51
      WRITE(IPR,900) OPNEW
  900 FORMAT(11X,57H**WARNING** THE ET-DEMAND OR PE-ADJUSTMENT CURVE FOR
     * THE ,2A4,54H SAC-SMA OPERATION HAS THE SAME VALUE FOR EVERY MONTH
     *.)
      CALL WARN
C
   51 DO 52 I=1,12
   52 EPOR(I)=0.0
      GO TO 100
C
   60 CONTINUE
C
      DO 70 I=1,12
      EPOR(I)=(E(I)-EMIN)/DIFF
   70 CONTINUE
C
C     COMPUTE NEW EVAP VALUES.
  100 CONTINUE
C
      BASE=ETLOW*EMIN
      DO 110 J=1,12
      ENEW(J)=EPOR(J)*(ETHIGH*EMAX-BASE)+BASE
  110 CONTINUE
C
  200 CONTINUE
      CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1002)
 1002 FORMAT(1H0,14H** EXIT OPECRV)
C
      RETURN
      END
