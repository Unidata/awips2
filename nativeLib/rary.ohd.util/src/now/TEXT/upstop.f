C  =====================================================================
C  pgm: UPSTOP .. Call the FORTRAN STOP command after closing files
C
C  use:     CALL UPSTOP(NUM)
C
C   in: NUM .... integer number to output after stop - INT
C   in:          (must be 0, 1, 2, 4, 8, or 16; else no number is used)
C  =====================================================================
      SUBROUTINE UPSTOP(NUM)

      EXTERNAL       UPCLOA

      INTEGER        NUM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upstop.f,v $
     . $',                                                             '
     .$Id: upstop.f,v 1.1 1997/04/06 13:23:50 page Exp $
     . $' /
C    ===================================================================
C

        CALL UPCLOA()

      IF (NUM .EQ.  0) STOP 0
      IF (NUM .EQ.  1) STOP 1
      IF (NUM .EQ.  2) STOP 2
      IF (NUM .EQ.  4) STOP 4
      IF (NUM .EQ.  8) STOP 8
      IF (NUM .EQ. 16) STOP 16
      IF (NUM .NE.  0) STOP

      RETURN
      END
