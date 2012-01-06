C MEMBER GSGRDA
C  (from old member PPGSGRDA)
C
      SUBROUTINE GSGRDA(NGRIDA, MDRBOX, NUM)
C
C.....THIS SUBROUTINE SORTS IN NUMERICALLY ASCENDING ORDER BY GRID POINT
C.....THE GRID ADDRESS AND MDR BOX NUMBERS FOR LATER USE BY THE MDR
C.....ROUTINE.
C
C.....THIS IS THE ARGUMENT LIST:
C
C.....NGRIDA - THE ARRAY OF GRID POINT ADDRESSES.
C.....MDRBOX - THE ARRAY OF MDR BOX NUMBERS.
C.....NUM    - THE NUMBER OF GRID POINTS IN THE GRID BOX NETWORK.
C
      INTEGER*2 NGRIDA(1), MDRBOX(1)
      DIMENSION SNAME(2)
C
      INCLUDE 'common/where'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gsgrda.f,v $
     . $',                                                             '
     .$Id: gsgrda.f,v 1.1 1995/09/17 19:02:35 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4hGSGR, 4hDA  /
C
  900 FORMAT(1H0, '*** GSGRDA ENTERED ***')
  901 FORMAT(1H0, '*** EXIT GSGRDA ***')
C
C.....SET WHERE COMMON BLOCK.
C
      IOPNUM = -1
      OPNAME(1) = SNAME(1)
      OPNAME(2) = SNAME(2)
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
      LP = NUM - 1
      DO 200 KP = 1, LP
C
      ISMALL = NGRIDA(KP)
C
      JP = KP + 1
C
      DO 100 NP = JP, NUM
      IF(ISMALL .LE. NGRIDA(NP)) GOTO 100
C
      JX = MDRBOX(KP)
      KX = NGRIDA(KP)
C
      ISMALL = NGRIDA(NP)
C
      NGRIDA(KP) = NGRIDA(NP)
      MDRBOX(KP) = MDRBOX(NP)
      NGRIDA(NP) = KX
      MDRBOX(NP) = JX
  100 CONTINUE
C
  200 CONTINUE
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,901)
C
      RETURN
      END
