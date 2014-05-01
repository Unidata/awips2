C  =====================================================================
C  pgm: SHCTIM .. Check time (hr-min-sec) for correctness
C
C  use:     CALL SHCTIM(LHOUR,LMIN,LSEC,KSTAT)
C
C   in: LHOUR ...... hour number (0-24) (yes, even 24!) - INT
C   in: LMIN ....... minute number (0-59) - INT
C   in: LSEC ....... second number (0-59) - INT
C  out: KSTAT ...... status of given time - INT
C  out:                0 = no error
C  out:                1 = hour number error
C  out:                2 = minute number error
C  out:                3 = second number error
C  out:                4 = time greater that 24:00:00
C  =====================================================================
      SUBROUTINE SHCTIM(LHOUR,LMIN,LSEC,KSTAT)

      INTEGER        LHOUR,LMIN,LSEC,KSTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shctim.f,v $
     . $',                                                             '
     .$Id: shctim.f,v 1.1 1995/11/29 22:21:16 dws Exp $
     . $' /
C    ===================================================================
C

        IF (LHOUR.LT.0 .OR. LHOUR.GT.24) THEN
            KSTAT = 1
        ELSEIF (LMIN.LT.0 .OR. LMIN.GT.59) THEN
            KSTAT = 2
        ELSEIF (LSEC.LT.0 .OR. LSEC.GT.59) THEN
            KSTAT = 3
        ELSEIF (LHOUR.EQ.24 .AND. (LMIN.GT.0 .OR. LSEC.GT.0)) THEN
            KSTAT = 4
        ELSE
            KSTAT = 0
        ENDIF

      RETURN
      END
