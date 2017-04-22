C MODULE XFCKIN
C-----------------------------------------------------------------------
C
      SUBROUTINE XFCKIN (MAX,IUSE,ISTAT)
C
C  THIS ROUTINE CHECKS IF ARRAY INFMAP IS FULL.
C
C    MAX  = SPACE AVAILABLE
C    IUSE = SPACE ABOUT TO BE USED
C
C  WRITTEN BY ERIC ANDERSON, HRL -- MAY 1986
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fmap/RCS/xfckin.f,v $
     . $',                                                             '
     .$Id: xfckin.f,v 1.2 2000/03/14 12:15:07 page Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      IF (IUSE.GT.MAX) THEN
         WRITE (IPR,10)
10    FORMAT ('0**ERROR** THE SPACE REQUIRED TO STORE DECODED ',
     1 'VALUES HAS BEEN EXCEEDED. THIS AND SUBSEQUENT CARDS ',
     2 'FOR THIS TYPE OF MOD WILL BE IGNORED.')
         ISTAT=1
         ENDIF
C
      RETURN
C
      END
