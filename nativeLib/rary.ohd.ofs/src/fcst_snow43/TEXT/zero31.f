C     Module ZERO31
C
      SUBROUTINE ZERO31
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE SETS ALL CARRYOVER VALUES TO NO SNOW CONDITIONS
C        FOR THE 'SNOW-43 ' OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for SNOW-17
C     MODIFIED BY...
C        Jay Day - RTi, July 1988 FOR USE IN THE SNOW-43 OPERATION
C.......................................
C     No Arguments:
C
C.......................................
C
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- L O C A L ---
      integer i, j
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snco31'
      include 'snow43/cupdt31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/zero31.f,v $
     . $',                                                             '
     .$Id: zero31.f,v 1.1 1996/05/07 11:10:14 page Exp $
     . $' /
C    ===================================================================
C
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
      DO 100 i=1,NEXLAG
  100    EXLAG(i)=0.0
C
      AESC=0.0
      DO 120 I=1,nnfltr
          DO 110 J=1,nnfltr
              P(I,J)=0.0
  110     CONTINUE
  120 CONTINUE
C.......................................
C
  999 RETURN
      END
C
