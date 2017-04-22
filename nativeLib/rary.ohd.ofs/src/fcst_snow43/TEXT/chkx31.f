C     Module CHKX31
C
      SUBROUTINE CHKX31(X)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE CHECKS THE VALUES OF THE FIVE MODEL STATES
C     IN THE X ARRAY IN THE 'SNOW-43 ' OPERATION AND CORRECTS
C     ANY THAT ARE OUTSIDE OF PHYSICAL LIMITS.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        JAY DAY - RTi
C        Nalneesh Gaur - RTi, July 1995
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C       X           I/O         States Array
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real x
C     --- L O C A L ---
      real liqwmx
C
C----- D I M E N S I O N --------------------------------
      DIMENSION X(5)
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snpm31'
      include 'snow43/snco31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/chkx31.f,v $
     . $',                                                             '
     .$Id: chkx31.f,v 1.1 1996/05/07 11:00:35 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................
C
      IF(X(1).GT.0.0) GO TO 100
      X(1)=0.0
      X(2)=0.0
      X(3)=0.0
      X(4)=0.0
      X(5)=0.0
      GO TO 999
  100 IF(X(4).GT.0.0.OR.X(2).LT.0.0001) X(4)=0.0
      IF(X(3).LT.0.0) X(3)=0.0
      LIQWMX=PLWHC*X(1)
      IF(X(3).GT.LIQWMX) X(3)=LIQWMX
      IF(X(2).LT.0.0) X(2)=0.0
      IF(X(2).GT.0.33*X(1)) X(2)=.33*X(1)
      IF(X(5).GT.1.0) X(5)=1.0
      IF(X(5).LT.0.05) X(5)=0.05
C
  999 RETURN
      END
