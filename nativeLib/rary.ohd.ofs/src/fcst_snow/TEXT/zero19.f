C MEMBER ZERO19
C  (from old member FCPACK19)
C
      SUBROUTINE ZERO19
C.......................................
C     THIS SUBROUTINE SETS ALL CARRYOVER VALUES TO NO SNOW CONDITIONS
C        FOR THE 'SNOW-17 ' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980
C
C    MODIFIED 4/20/00 BY V. KOREN TO ADD TWO MORE STATES: SNDPT & SNTMP
C
C    MODIFIED 11/05 BY E. ANDERSON TO ADD TAPREV TO COMMON SNCO19
C.......................................
      REAL LIQW,NEGHS
C
C     COMMON BLOCK
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,
CVK     1   AEADJ,NEXLAG,EXLAG(7)
CEA     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow/RCS/zero19.f,v $
     . $',                                                             '
     .$Id: zero19.f,v 1.3 2006/10/03 19:32:56 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
      WE=0.0
      NEGHS=0.0
      LIQW=0.0
      TINDEX=0.0
      ACCMAX=0.0
      SB=0.0
      SBAESC=0.0
      SBWS=0.0
      STORGE=0.0
      AEADJ=0.0
C
CVK  ADDED TWO MORE STATES
      SNDPT=0.0
      SNTMP=0.0
C            
      DO 100 N=1,NEXLAG
  100 EXLAG(N)=0.0
C
CEA  NOTE: TAPREV IS LEFT AS IS.
CEA    VERSION 1 TAPREV IS NOT USED - SET=-99.
CEA    VERSIONS 2-4 WITH NOSNOW=0
CEA      ACTUAL VALUE MAINTAINED SO THAT AVAILABLE WHEN SNOW NEXT
CEA        EXISTS OR TO SAVE WITH VERSION 4 CARRYOVER
CEA    VERSIONS 2-4 WITH NOSNOW=1
CEA      SET TO -99. SINCE TEMPERATURE DATA ASSUMED NOT TO BE AVAILABLE
CEA        THE VALUE OF TAPREV CANNOT BE DETERMINED.
C.......................................
      RETURN
      END