C  =====================================================================
C  pgm: SHIMO .. Increment the calendar date by the given num of months
C
C  use:     CALL SHIMO(JYR,JMO,INC)
C
C  i/o: JYR ...... year number (1753-2199) - INT
C  i/o: JMO ...... month number (01-12) - INT
C   in: INC ...... number of months (pos or neg) for increment - INT
C  =====================================================================
      SUBROUTINE SHIMO(JYR,JMO,INC)

      INTEGER        JYR,JMO,INC,JYRT,JMOT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shimo.f,v $
     . $',                                                             '
     .$Id: shimo.f,v 1.3 1998/04/07 19:14:03 page Exp $
     . $' /
C    ===================================================================
C

        JYRT = JYR
        JMOT = JMO+INC

  100   IF (JMOT.GE.1 .AND. JMOT.LE.12) GOTO 110

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
  110   CONTINUE

        JYR = JYRT
        JMO = JMOT

      RETURN
      END
