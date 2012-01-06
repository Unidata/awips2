C MEMBER FPRCBF
C  (from old member FCDMPCBS)
C
      SUBROUTINE FPRCBF
C
      INCLUDE 'common/fratng'
      INCLUDE 'common/frcfil'
C
      DIMENSION NAMES(3,2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprcbf.f,v $
     . $',                                                             '
     .$Id: fprcbf.f,v 1.1 1995/09/17 19:08:22 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAMES/4HFRAT,4HNG  ,300,4HFRCF,4HIL  ,19/
C
C     ***** COMMON BLOCK FRATNG
      CALL FPRTCB(NAMES(1,1),NAMES(3,1),RTCVID,RTCVID)
C     ***** COMMON BLOCK FRCFIL
      CALL FPRTCB(NAMES(1,2),NAMES(3,2),NDEF,NDEF)
C
      RETURN
      END
