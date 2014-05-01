C  =====================================================================
C  pgm: SH4DT0 .. Complete 4-digit year with 90/10 century, 6-mo rules
C
C  use:     CALL SH4DT0(JCN,JYR,JMO,JDA,LC,LY,LM,LD)
C
C   in: JCN ....... century number (17-21) for update - INT
C   in: JYR ....... 2-digit year number (0-99) for update - INT
C   in: JMO ....... month number (1-12) for update - INT
C   in: JDA ....... day number (1-31) for update - INT
C   in: LC ........ current value of century number (-1, 17-21) - INT
C  i/o: LY ........ current value of year number (-1, 1753-2199) - INT
C  i/o:             input as 2-digit, output as 4-digit
C   in: LM ........ current value of month number (1-12) - INT
C   in: LD ........ current value of day number (1-31) - INT
C  =====================================================================
      SUBROUTINE SH4DT0(JCN,JYR,JMO,JDA,LC,LY,LM,LD)

      INTEGER        JCN,JYR,JMO,JDA,LC,LY,LM,LD,TMYR,TMCN,DFMO,DFYR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh4dt0.f,v $
     . $',                                                             '
     .$Id: sh4dt0.f,v 1.2 1998/04/07 19:11:20 page Exp $
     . $' /
C    ===================================================================
C

        IF ((LY.LT.1753 .OR. LY.GT.2199) .AND.
     $      (LM.GE.1 .AND. LM.LE.12 .AND. LD.GE.1 .AND. LD.LE.31)) THEN

C                   Set data year number to current year number

          TMYR = LY
          IF (LY .LT. 0) THEN
             TMYR = JYR

C                   If cur month is 6 months off, adjust data year num

             DFMO = LM-JMO
            IF (DFMO.GT. 6                ) TMYR = TMYR-1
            IF (DFMO.LT.-6                ) TMYR = TMYR+1
            IF (DFMO.EQ.-6 .AND. LD.LT.JDA) TMYR = TMYR+1
            IF (DFMO.EQ. 6 .AND. LD.GT.JDA) TMYR = TMYR-1

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
            IF (DFYR.GT. 10                ) TMCN = TMCN-1
            IF (DFYR.LT.-90                ) TMCN = TMCN+1
            IF (DFYR.EQ.-90 .AND. LM.LT.JMO) TMCN = TMCN+1
            IF (DFYR.EQ. 10 .AND. LM.GT.JMO) TMCN = TMCN-1
            IF (DFYR.EQ.-90 .AND. LM.EQ.JMO
     $                      .AND. LD.LT.JDA) TMCN = TMCN+1
            IF (DFYR.EQ. 10 .AND. LM.EQ.JMO
     $                      .AND. LD.GT.JDA) TMCN = TMCN-1

          ENDIF

          LY = 100*TMCN + TMYR

        ENDIF

      RETURN
      END
