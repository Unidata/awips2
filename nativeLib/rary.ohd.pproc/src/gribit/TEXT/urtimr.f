C$PRAGMA C (GETCPU)
C  =====================================================================
C  pgm: URTIMR .. Get lapse and total CPU time
C
C  use:     CALL URTIMR(ITLAPS,ITTOT)
C
C  out: ITLAPS ........ lapse CPU time in 0.01 seconds since last call
C  out:                 to this routine - INT
C  out: ITTOT ......... total CPU time in 0.01 seconds since the first
C  out:                 call to this routine - INT
C  =====================================================================
      SUBROUTINE URTIMR(ITLAPS,ITTOT)

CC    EXTERNAL   GETCPU
      INTEGER    ITLAPS,ITTOT,ITBEG,ITLAS,ITXX

      SAVE       ITBEG,ITLAS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/urtimr.f,v $
     . $',                                                             '
     .$Id: urtimr.f,v 1.1 2006/05/03 13:44:00 gsood Exp $
     . $' /
C    ===================================================================
C

      DATA       ITBEG / -1 /

        CALL GETCPU(ITXX)
        ITXX = ITXX/10

        IF (ITBEG .LT. 0) THEN
          ITBEG  = ITXX
          ITLAS  = ITXX
        ENDIF

        ITLAPS = ITXX-ITLAS
        ITTOT  = ITXX-ITBEG
        ITLAS  = ITXX

      RETURN
      END
