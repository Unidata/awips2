C MEMBER FPRCBA
C  (from old member FCDMPCBS)
C
      SUBROUTINE FPRCBA ()
C
      INCLUDE 'common/fc'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
C
      DIMENSION NAMES(3,4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprcba.f,v $
     . $',                                                             '
     .$Id: fprcba.f,v 1.1 1995/09/17 19:08:18 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAMES/4HFC  ,4H    ,42,4HFP  ,4H    ,1201,
     1           4HFT  ,4H    ,251,4HFTS ,4H    ,501/
C
C     ***** COMMON BLOCK FC
      CALL FPRTCB(NAMES(1,1),NAMES(3,1),MC,MC)
C     ***** COMMON BLOCK FP
      CALL FPRTCB(NAMES(1,2),NAMES(3,2),MP,MP)
C     ***** COMMON BLOCK FT
      CALL FPRTCB(NAMES(1,3),NAMES(3,3),MT,MT)
C     ***** COMMON BLOCK FTS
      CALL FPRTCB(NAMES(1,4),NAMES(3,4),MTS,MTS)
C
      RETURN
      END
