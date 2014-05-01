C MEMBER XCHAR
C  (from old member PPXCHAR)
C
      SUBROUTINE XCHAR(CHAR,N)
C.......................................
C     THIS SUBROUTINE PRINTS CHARACTERISTICS AND OTHER I*2 VARIABLES
C       FOR DEBUG PURPOSES.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON,HRL -- APRIL 1983
C.......................................
      INTEGER*2 CHAR(1)
C
C     COMMON BLOCK
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xchar.f,v $
     . $',                                                             '
     .$Id: xchar.f,v 1.1 1995/09/17 18:59:24 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     PRINT CHARACTERISTICS
      IF (N.EQ.0) RETURN
      WRITE(IOPDBG,900) (CHAR(I),I=1,N)
  900 FORMAT(1H ,20I6)
C.......................................
      RETURN
      END
