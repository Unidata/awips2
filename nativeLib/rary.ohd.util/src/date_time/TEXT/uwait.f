C  MODULE UWAIT
C  ====================================================================
C  pgm: UWAIT .. Sleep routine
C
C  use:      CALL UWAIT(NOFTU)
C
C   in: NOFTU ..... Number of time units (seconds) to wait - INT
C
C  rqd: system routines - system, sleep
C  ====================================================================
      SUBROUTINE UWAIT(NOFTU)

      INTRINSIC     CHAR
      EXTERNAL      system

      INTEGER       NOFTU
      CHARACTER*8   CHSEC
      CHARACTER*1   CHAR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/uwait.f,v $
     . $',                                                             '
     .$Id: uwait.f,v 1.1 1997/08/29 18:31:06 dws Exp $
     . $' /
C    ===================================================================
C

        WRITE(CHSEC,'(I8)') NOFTU
        CALL system( 'sleep ' // CHSEC  // CHAR(0) )

      RETURN
      END
