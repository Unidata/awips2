C MODULE FPRCBE
C-----------------------------------------------------------------------
C
      SUBROUTINE FPRCBE
C
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcio'
      INCLUDE 'common/fciobf'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/fcsegn'
C
      DIMENSION NAMES(3,6)
      DATA NAMES/
     *   4hFCCG,4hD   ,114,
     *   4hFCCG,4hD1  ,114,
     *   4hFCIO,4h    ,43,
     *   4hFCIO,4hBF  ,101,
     *   4hFCSE,4hGC  ,42,
     *   4hFCSE,4hGN  ,66
     *   /
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fprcbe.f,v $
     . $',                                                             '
     .$Id: fprcbe.f,v 1.2 2001/06/13 12:14:21 mgm Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK FCCGD
      CALL FPRTCB (NAMES(1,1),NAMES(3,1),NSLOTS,NSLOTS)
C
C  COMMON BLOCK FCCGD1
      CALL FPRTCB (NAMES(1,2),NAMES(3,2),CGIDC,CGIDC)
C
C  COMMON BLOCK FCIO
      CALL FPRTCB (NAMES(1,3),NAMES(3,3),IDSEG,IDSEG)
C
C  COMMON BLOCK FCIOBF
      CALL FPRTCB (NAMES(1,4),NAMES(3,4),MZZBUF,MZZBUF)
C
C  COMMON BLOCK FCSEGC
      CALL FPRTCB (NAMES(1,5),NAMES(3,5),IDC,IDC)
C
C  COMMON BLOCK FCSEGN
      CALL FPRTCB (NAMES(1,6),NAMES(3,6),IRSEG,IRSEG)
C
      RETURN
C
      END
