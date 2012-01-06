C  =====================================================================
C  pgm: SHGJUL .. Get frm cal-dt, jul-dt (get julda)
C
C  use:     CALL SHGJUL(JUL,LYR,LMO,LDA)
C
C  out: JUL ...... day of year (001-366) - INT
C   in: LYR ...... year number (1753-2199) - INT
C   in: LMO ...... month number (01-12) - INT
C   in: LDA ...... day number (01-31) - INT
C  =====================================================================
      SUBROUTINE SHGJUL(JUL,LYR,LMO,LDA)

      INTEGER       JUL,LYR,LMO,LDA,JULT,LYRT,LMOT,LDAT,NOD(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shgjul.f,v $
     . $',                                                             '
     .$Id: shgjul.f,v 1.3 1998/04/07 19:13:07 page Exp $
     . $' /
C    ===================================================================
C
      DATA     NOD / 0,31,59,90,120,151,181,212,243,273,304,334 /

        LYRT = LYR
        LMOT = LMO
        LDAT = LDA

        JULT = NOD(LMOT)+LDAT
        IF (LMOT .GT. 2) THEN
          IF (LYRT .EQ. (LYRT/4)*4) THEN
            JULT = JULT+1
            IF (LYRT .EQ. 1900) JULT = JULT-1
            IF (LYRT .EQ. 1800) JULT = JULT-1
            IF (LYRT .EQ. 2100) JULT = JULT-1
          ENDIF
        ENDIF

        JUL = JULT

      RETURN
      END
