C MEMBER DETM51
C----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE DETM51(D,LDX,NUMX,CDUM)
C
C DESC CONVERTS ALL DATA IN A TIME SERIES FROM METRIC TO INPUT UNITS
C
C  WRITTEN BY - KUANG HSU - HRL - OCTOBER 1994
C
      DIMENSION D(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/detm51.f,v $
     . $',                                                             '
     .$Id: detm51.f,v 1.1 1996/03/21 13:33:58 page Exp $
     . $' /
C    ===================================================================
C
C
      DO 100 I=1,NUMX
      IF(IFMSNG(D(LDX+I-1)).EQ.0) D(LDX+I-1)=D(LDX+I-1)/CDUM
 100  CONTINUE
      RETURN
      END
