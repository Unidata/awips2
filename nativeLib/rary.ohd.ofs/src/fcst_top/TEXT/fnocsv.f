C MEMBER FNOCSV
C  (from old member FCFNOCSV)
C
C     DESC - SET IDSEG IN CB CIO AND IDC IN CB FCSEGC TO BLANK
C     DESC - FOR NON SAVE CARRYOVER RUN.
C
C.....................
      SUBROUTINE FNOCSV
C.....................
C
C      SUBROUTINE ORIGINALLY WRITTEN BY
C         GEORGE F SMITH - HRL - 3/21/80
C.................................................................
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcio'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/where'
C
      DIMENSION SBNAME(2),OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fnocsv.f,v $
     . $',                                                             '
     .$Id: fnocsv.f,v 1.1 1995/09/17 19:08:17 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HNOCS,4HSV  /
      DATA IBLANK/4H    /
C
C        WHERE
C.............
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
C        TRACE LEVEL = 1
C.............
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,18H ** FNOCSV ENTERED)
C
      DO 20 I=1,2
      IDSEG(I)=IBLANK
   20 IDC(I)=IBLANK
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
