C MEMBER GMDMP1
C  (from old member PPGMDMP1)
C
      SUBROUTINE GMDMP1(NUM, W, WN, WX, A, BI, E1, E2, G1, G2, CP,
     * POW, IRFRO, B, C, Y)
C
C.....THIS SUBROUTINE DUMPS OUT A RAINFALL-RUNOFF RELATION PARAMETER
C.....RECORD.
C
C.....WRITTEN BY:  JERRY M. NUNN   WGRFC FT. WORTH   SEPTEMBER 1, 1987
C.....REVISED BY:  JERRY M. NUNN   WGRFC FT. WORTH   JULY 2, 1991
C
C.....ARGUMENT LIST CHANGED AND ROUTINE MODIFIED TO DUMP OUT THE
C.....DATE-DEPENDENT PARAMETERS Y AND C.
C
      DIMENSION SNAME(2), WN(1), WX(1), A(1), BI(1), E1(1), E2(1), G1(1)
      DIMENSION G2(1), CP(1), POW(1), IRFRO(1), B(1), C(1), Y(1)
C
      INCLUDE 'common/pudbug'
      INCLUDE 'common/where'
      INCLUDE 'gcommon/gdate'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gmdmp1.f,v $
     . $',                                                             '
     .$Id: gmdmp1.f,v 1.1 1995/09/17 19:01:59 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4hGMDM, 4hP1  /
C
  900 FORMAT(1H0, '*** ENTER GMDMP1 ***')
  901 FORMAT(1H0, '*** EXIT GMDMP1 ***')
  902 FORMAT(1H0, 'RAINFALL-RUNOFF RELATION CONSTANTS AND DATE-DEPENDENT
     * PARAMETERS...', A3, 1X, A3, 1X, I2, 1X, I4)
  903 FORMAT(1H0, 'RELN    A     WN    WX     BI    E1    E2     G1
     * G2     CP     POW     B      W     C      Y')
  904 FORMAT(1H0, 1X, I2, 3X, F5.2, 2X, F4.1, 2X, F4.1, 2X, F5.2, 2X,
     * F5.2, 2X, F5.2, 2X, F5.2, 2X, F5.2, 2X, F5.2, 2X, F5.2, 2X,
     * F5.2, 2X, F4.1, 2X, F5.2, 2X, F5.2)
      INCLUDE 'gcommon/setwhere'
C
      IF(IPTRCE .GE. 6) WRITE(IOPDBG,900)
C
      WRITE(IOPDBG,902) NWKDAY, MONTH, NDATE, NYEAR
      WRITE(IOPDBG,903)
C
      DO 100 NP = 1, NUM
      WRITE(IOPDBG,904) IRFRO(NP), A(NP), WN(NP), WX(NP), BI(NP),
     * E1(NP), E2(NP), G1(NP), G2(NP), CP(NP), POW(NP), B(NP), W,
     * C(NP), Y(NP)
  100 CONTINUE
C
      IF(IPTRCE .GE. 6) WRITE(IOPDBG,901)
      RETURN
      END
