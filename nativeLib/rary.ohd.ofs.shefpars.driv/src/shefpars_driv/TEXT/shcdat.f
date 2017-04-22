C  =====================================================================
C  pgm: SHCDAT .. Check date (yr-mon-day) for correctness (2-digit year)
C
C  use:     CALL SHCDAT(LYEAR,LMON,LDAY,KSTAT)
C
C   in: LYEAR ...... year number (1753-2199) - INT
C   in: LMON ....... month number (1-12) - INT
C   in: LDAY ....... day number (1-31) - INT
C  out: KSTAT ...... status of given date - INT
C  out:                0 = no error
C  out:                1 = year number error
C  out:                2 = month number error
C  out:                3 = day number error
C  =====================================================================
      SUBROUTINE SHCDAT(LYEAR,LMON,LDAY,KSTAT)

      INTEGER        LYEAR,LMON,LDAY,KSTAT,NODIM,NOD(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shcdat.f,v $
     . $',                                                             '
     .$Id: shcdat.f,v 1.3 1998/04/07 19:11:49 page Exp $
     . $' /
C    ===================================================================
C

      DATA       NOD / 31,29,31,30,31,30,31,31,30,31,30,31 /

C                   Check for correct year, then month, then day

        IF (LYEAR.LT.1753 .OR. LYEAR.GT.2199) THEN
            KSTAT = 1
        ELSEIF (LMON.LT.1 .OR. LMON.GT.12) THEN
            KSTAT = 2
        ELSE
          NODIM = NOD(LMON)
          IF (LMON .EQ. 2) THEN
            NODIM = NODIM+LYEAR/4-(LYEAR+3)/4
            IF (LYEAR .EQ. 1900) NODIM = 28
            IF (LYEAR .EQ. 1800) NODIM = 28
            IF (LYEAR .EQ. 2100) NODIM = 28
          ENDIF
          IF (LDAY.LT.1 .OR. LDAY.GT.NODIM) THEN
            KSTAT = 3
          ELSE
            KSTAT = 0
          ENDIF
        ENDIF

      RETURN
      END
