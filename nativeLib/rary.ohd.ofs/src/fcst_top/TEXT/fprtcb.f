C MEMBER FPRTCB
C  (from old member FCDMPCBS)
C
      SUBROUTINE FPRTCB(NAME,NUM,ICB,XCB)
C
      INCLUDE 'common/fdbug'
C
      DIMENSION NAME(2),ICB(NUM),XCB(NUM)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprtcb.f,v $
     . $',                                                             '
     .$Id: fprtcb.f,v 1.1 1995/09/17 19:08:22 dws Exp $
     . $' /
C    ===================================================================
C
C
      WRITE(IODBUG,600)NAME,NUM
  600 FORMAT(1H0,10X,'**COMMON BLOCK ',2A4,2H (,I5,' VALUES)')
C
      WRITE(IODBUG,601)ICB
  601 FORMAT(10(1X,I11))
      WRITE(IODBUG,602)ICB
  602 FORMAT(25(1X,A4))
      WRITE(IODBUG,603)XCB
  603 FORMAT(8(1X,G15.7))
C
      RETURN
      END
