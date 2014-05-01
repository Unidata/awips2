C MODULE FCSTM2
C-----------------------------------------------------------------------
C
C
      SUBROUTINE FCSTM2
C
C  THIS ROUTINE IS NEEDED BECAUSE VARIABLE NFIELD IS IN COMMON
C  BLOCKS UFREEI AND UFREEX
C
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fcstm2.f,v $
     . $',                                                             '
     .$Id: fcstm2.f,v 1.2 1998/07/02 20:33:26 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET VARIABLES IN COMMON UFREEX
      IOPREP=1
      ICDSPC=0
      ICDQTE=0
C
      RETURN
C
      END
