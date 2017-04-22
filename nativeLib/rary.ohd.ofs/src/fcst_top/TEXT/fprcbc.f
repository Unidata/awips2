C MEMBER FPRCBC
C  (from old member FCDMPCBS)
C
      SUBROUTINE FPRCBC ()
C
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fsglst'
      INCLUDE 'common/frcptr'
C
      DIMENSION NAMES(3,3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprcbc.f,v $
     . $',                                                             '
     .$Id: fprcbc.f,v 1.1 1995/09/17 19:08:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAMES/4HFCRU,4HNC  ,506,4HFSGL,4HST  ,1502,
     1 4HFRPC,4HTR  ,1503/
C
C     ***** COMMON BLOCK FCRUNC
      CALL FPRTCB(NAMES(1,1),NAMES(3,1),ITYPRN,ITYPRN)
C     ***** COMMON BLOCK FSGLST
      CALL FPRTCB(NAMES(1,2),NAMES(3,2),MLIST,MLIST)
C     ***** COMMON BLOCK FRCPTR
      CALL FPRTCB(NAMES(1,3),NAMES(3,3),NRC,NRC)
C
      RETURN
      END
