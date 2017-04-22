C     Module SWST31
C
      SUBROUTINE SWST31(X,IS)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     This routine switches the states to and from the
C     carryover common block and the X array based on IS. 
C     If IS = 1, then states are moved from the common block
C     to the X array. If the IS flag is 0, then states are 
C     moved from the X array to the common block.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        Jay Day - RTi., July 1988 for use in the SNOW-43 operation
C.......................................
C     Arguments:
C
C     Argument       I/O         Description
C       X            I/O         Array containing states
C       IS            I          Flag, (1=place states in x/
C                                       0=move states from x)
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      integer is
      real    x
C
C----- D I M E N S I O N --------------------------------
      DIMENSION X(5)
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snco31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/swst31.f,v $
     . $',                                                             '
     .$Id: swst31.f,v 1.1 1996/05/07 11:09:58 page Exp $
     . $' /
C    ===================================================================
C
C....................................
C
C
      IF(IS.GE.1) GO TO 100
C
      WE=X(1)
      NEGHS=X(2)
      LIQW=X(3)
      TINDEX=X(4)
      AESC=X(5)
      GO TO 999
C
  100 X(1)=WE
      X(2)=NEGHS
      X(3)=LIQW
      X(4)=TINDEX
      X(5)=AESC
C
  999 RETURN
      END
