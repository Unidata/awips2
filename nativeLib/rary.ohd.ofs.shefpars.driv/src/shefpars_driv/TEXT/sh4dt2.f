C  =====================================================================
C  pgm: SH4DT2 .. Complete date-of-yr and yr using 90/10 yr, 6-mo rules
C
C  use:     CALL SH4DT2(JCN,JYR,JMO,JDA,LC,LY,LDOY)
C
C   in: JCN ....... century number (17-21) for update - INT
C   in: JYR ....... 2-digit year number (0-99) for update - INT
C   in: JMO ....... month number (1-12) for update - INT
C   in: JDA ....... day number (1-31) for update - INT
C   in: LC ........ current value of century number (-1, 17-21) - INT
C  i/o: LY ........ current value of year number (-1, 1753-2199) - INT
C  i/o:             input as 2-digit, output as 4-digit
C  i/o: LDOY ...... current value of day-of-yr (1-366) - INT
C
C  rqd: SHGJUL
C  =====================================================================
      SUBROUTINE SH4DT2(JCN,JYR,JMO,JDA,LC,LY,LDOY)

      EXTERNAL       SHGJUL

      INTEGER        JCN,JYR,JMO,JDA,LC,LY,LDOY,TMYR,TMCN,DFDOY,DFYR
      INTEGER        JDOY,JCNYR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh4dt2.f,v $
     . $',                                                             '
     .$Id: sh4dt2.f,v 1.2 1998/04/07 19:11:35 page Exp $
     . $' /
C    ===================================================================
C

        IF ((LY.LT.1753 .OR. LY.GT.2199) .AND.
     $      (LDOY.GE.1 .AND. LDOY.LE.366)) THEN

C                   Set data year number to current year number

          TMYR = LY
          IF (LY .LT. 0) THEN
             TMYR = JYR

C                   If cur day-of-yr is 183 days off (about half a
C                    year), adjust data year num

             JCNYR = (JCN*100) + JYR
             CALL SHGJUL(JDOY,JCNYR,JMO,JDA)
             DFDOY = LDOY-JDOY
            IF (DFDOY .GT. 183) TMYR = TMYR-1
            IF (DFDOY .LE.-183) TMYR = TMYR+1

C                   Adjust again for two digit year number

            IF (TMYR.LT.  0) TMYR = 99
            IF (TMYR.EQ.100) TMYR = 0
          ENDIF

C                   Set century number if missing

          TMCN = LC
          IF (LC .LT. 0) THEN
             TMCN = JCN

C                   -90, +10 year rule (adjust century number if year
C                   number is 1-9 years higher than current year)

             DFYR = TMYR-JYR
            IF (DFYR.GT. 10                   ) TMCN = TMCN-1
            IF (DFYR.LT.-90                   ) TMCN = TMCN+1
            IF (DFYR.EQ.-90 .AND. LDOY.LE.JDOY) TMCN = TMCN+1
            IF (DFYR.EQ. 10 .AND. LDOY.GT.JDOY) TMCN = TMCN-1

          ENDIF

          LY = 100*TMCN + TMYR

        ENDIF

      RETURN
      END
