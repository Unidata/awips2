C  =====================================================================
C  pgm: SHIEOM .. Increment the end-of-month by given number of months
C
C  use:     CALL SHIEOM(JYR,JMO,JDA,INC,ISTAT)
C
C  i/o: JYR ...... year number (1753-2199) - INT
C  i/o: JMO ...... month number (01-12) - INT
C  i/o: JDA ...... day number (01-31) - INT
C   in: INC ...... number of months (pos or neg) for increment - INT
C  out: ISTAT .... if 0 then no error, if 1 then not end-of-month, - INT
C  out:            if 2 then bad year number resulted
C  =====================================================================
      SUBROUTINE SHIEOM(JYR,JMO,JDA,INC,ISTAT)

      INTEGER       JYR,JMO,JDA,INC,ISTAT,JYRT,JMOT,JDAT,MXD(12),MDAY
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shieom.f,v $
     . $',                                                             '
     .$Id: shieom.f,v 1.3 1998/04/07 19:13:19 page Exp $
     . $' /
C    ===================================================================
C
      DATA     MXD / 31,29,31,30,31,30,31,31,30,31,30,31 /

        JYRT = JYR
        JMOT = JMO
        JDAT = JDA

        MDAY = MXD(JMOT)
        IF (JMOT .EQ. 2) THEN
            MDAY = MDAY-(JYRT+3)/4+JYRT/4
            IF (JYRT .EQ. 1900) MDAY = 28
            IF (JYRT .EQ. 1800) MDAY = 28
            IF (JYRT .EQ. 2100) MDAY = 28
        ENDIF

        IF (MDAY .NE. JDA) THEN
            ISTAT = 1
          ELSE
            ISTAT = 0

            JMOT = JMOT+INC
  100       IF (JMOT.GE.1 .AND. JMOT.LE.12) GOTO 110

              IF (JMOT .GT. 12) THEN
                  JMOT = JMOT-12
                  JYRT = JYRT+1
                  IF (JYRT .EQ. 100) JYRT = 0
                ELSE
                  JMOT = JMOT+12
                  JYRT = JYRT-1
                  IF (JYRT .EQ. -1) JYRT = 99
              ENDIF

              GOTO 100
  110       CONTINUE

            JDAT = MXD(JMOT)
            IF (JMOT .EQ. 2) THEN
                JDAT = JDAT-(JYRT+3)/4+JYRT/4
                IF (JYRT .EQ. 1900) MDAY = 28
                IF (JYRT .EQ. 1800) MDAY = 28
                IF (JYRT .EQ. 2100) MDAY = 28
            ENDIF

            IF (JYRT .GE. 100) THEN
                IF (JYRT.LT.1753 .OR. JYRT.GT.2199) ISTAT = 2
            ENDIF
        ENDIF

        JYR = JYRT
        JMO = JMOT
        JDA = JDAT

      RETURN
      END
