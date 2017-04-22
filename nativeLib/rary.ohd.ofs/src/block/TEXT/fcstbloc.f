C MODULE FCSTBLOC
C-----------------------------------------------------------------------
C
      BLOCK DATA FCSBLK
C
C  INITIALIZE COMMON BLOCKS FOR PROGRAM FCST.
C
      INCLUDE 'common/x'
      COMMON /OUTCTL/ IOUTYP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSFCSTBLOC      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/fcstbloc.f,v $
     . $',                                                             '
     .$Id: fcstbloc.f,v 1.3 2001/06/13 11:21:01 mgm Exp $
     . $' /
C    ===================================================================
C
C
C  COMMON BLOCK  /X/
      DATA MX/150000/,X/150000*0.0/
C
C  COMMON BLOCK  /OUTCTL/
      DATA IOUTYP/0/
C
      END
