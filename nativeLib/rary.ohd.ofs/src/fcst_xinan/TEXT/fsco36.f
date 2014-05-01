C MEMBER FSCO36
C  (from old member FCEX36)
C
      SUBROUTINE FSCO36(C,NUMBER)
C.......................................
C     THIS SUBROUTINE TRANSFERS SOIL MOISTURE CARRYOVER INTO THE
C          CARRYOVER ARRAY USED IN THE XINANJING OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ....
C            QINGPING ZHU -YRCC CHINA   JULY 1988
C.......................................
      DIMENSION C(1)
C
C     COMMON BLOCKS
      COMMON/FCO36/WUC,WLC,WDC,SC,FRC,QIC,QGC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_xinan/RCS/fsco36.f,v $
     . $',                                                             '
     .$Id: fsco36.f,v 1.1 1995/09/17 18:58:14 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     SOIL -MOISTURE CARROVER.
      J=7*(NUMBER-1)
      C(J+1)=WUC
      C(J+2)=WLC
      C(J+3)=WDC
      C(J+4)=SC
      C(J+5)=QIC
      C(J+6)=QGC
      C(J+7)=FRC
C.......................................
      RETURN
      END
