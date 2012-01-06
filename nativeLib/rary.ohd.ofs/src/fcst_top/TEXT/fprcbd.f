C MEMBER FPRCBD
C  (from old member FCDMPCBS)
C
      SUBROUTINE FPRCBD ()
C
      INCLUDE 'common/fd'
C
      DIMENSION NAMES(3,1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprcbd.f,v $
     . $',                                                             '
     .$Id: fprcbd.f,v 1.1 1995/09/17 19:08:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAMES/4HFD  ,4H    ,4001/
C
C     ***** COMMON BLOCK FD
      CALL FPRTCB(NAMES(1,1),NAMES(3,1),MD,MD)
C
      RETURN
      END
