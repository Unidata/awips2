C     Module ENDO31
C
      SUBROUTINE ENDO31(X)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     Sets AESC endogenous states which are
C     not included in the state space equations  
C     - SB, SBWS, SBAESC.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        JAY DAY - RTi
C        Nalneesh Gaur - RTi, July 1995
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      X             I          Array containing updated states
C
C.......................................
C
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real x
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snco31'
C
C----- D I M E N S I O N --------------------------------
      DIMENSION X(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/endo31.f,v $
     . $',                                                             '
     .$Id: endo31.f,v 1.1 1996/05/07 11:00:53 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C
      IF(WELIQW.GT.ACCMAX) ACCMAX=WELIQW
      IF(WELIQW.LT.AI) GO TO 100
C
C   CASE 1
C
      SB=WELIQW
      SBWS=WELIQW
      GO TO 999
C
  100 IF(WELIQW.GT.SB.AND.SBWS.GT.SB) GO TO 999
C
C   CASE 2
C
      SB=WELIQW
      SBWS=WELIQW
      SBAESC=X(5)
C
  999 RETURN
      END
