C MEMBER CSAV19
C  (from old member FCCSAV19)
C.......................................
      SUBROUTINE CSAV19(CS,IVER)
C.......................................
C     THIS SUBROUTINE STORES CARRYOVER VALUES IN THE SNCO19
C        COMMON BLOCK IN ARRAY CS.
C.......................................
C     WRITTEN BY ERIC ANDERSON - HRL   MAY 1980
C
CVK   MODIFIED 4/20/00 BY V. KOREN TO INCLUDE TWO MORE STATES
CEA   MODIFIED 11/05 BY E. ANDERSON TO INCLUDE TAPREV
C.......................................
      REAL NEGHS,LIQW
      DIMENSION CS(*)
C
C     COMMON BLOCK
CVK  ADDED SNOW DEPTH AND TEMPERATURE STATES ----
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,
CVK     1   AEADJ,NEXLAG,EXLAG(7)
CEA  ADDED TAPREV STATE
CEA     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow/RCS/csav19.f,v $
     . $',                                                             '
     .$Id: csav19.f,v 1.3 2006/10/03 19:33:05 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
      CS(1)=WE
      CS(2)=NEGHS
      CS(3)=LIQW
      CS(4)=TINDEX
      CS(5)=ACCMAX
      CS(6)=SB
      CS(7)=SBAESC
      CS(8)=SBWS
      CS(9)=STORGE
      CS(10)=AEADJ
      DO 100 I=1,NEXLAG
  100 CS(10+I)=EXLAG(I)
C
CVK  TWO NEW STATES 
CEA  SNOW DEPTH ONLY INCLUDED AFTER VERSION 1
      IF (IVER.GT.1) THEN 
        CS(11+NEXLAG)=SNDPT
        CS(12+NEXLAG)=SNTMP
      ENDIF
CEA  TAPREV ONLY INCLUDED AFTER VERSION 3
      IF (IVER.GT.3) CS(13+NEXLAG)=TAPREV
C.......................................
      RETURN
      END