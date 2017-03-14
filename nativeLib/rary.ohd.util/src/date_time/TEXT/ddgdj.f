C  =====================================================================
C  pgm: DDGDJ .. Get frm da-sum, jul-dt (frm yr-0001)
C
C  use:     CALL DDGDJ(DS1,J1,Y1)
C
C   in: DS1 ..... day-sum since Jan 1, 0001 - INT
C  out: J1 ...... day of year (001-366) - INT
C  out: Y1 ...... 4-digit year number - INT
C
C  lvl: DD1
C
C  cmt:  639907 is the day number for 31 Dec 1752.
C  cmt:  657438 is the day number for 31 Dec 1800.
C  cmt:  730487 is the day number for 31 Dec 2000.
C  cmt:    1461 is the number of days in a 4 year period.
C  cmt:   36524 is the number of days in a 100 year period.
C  cmt:  146097 is the number of days in a 400 year period.
C  =====================================================================
      SUBROUTINE DDGDJ(DS1,J1,Y1)

      INTEGER    DS1,J1,Y1,DS1T,J1T,Y1T,PER,DLF,PYR,PER100,PER400
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgdj.f,v $
     . $',                                                             '
     .$Id: ddgdj.f,v 1.1 1998/10/14 15:35:10 page Exp $
     . $' /
C    ===================================================================
C

        DS1T = DS1

        IF ( DS1T .LE. 657438 ) THEN

          IF ( DS1T .GT. 639907 ) DS1T = DS1T + 11
          PER = (DS1T-1)/1461
          DLF = DS1T - 1461*PER
          PYR = (4*DLF - 1)/1461
          J1T = DLF - 365*PYR
          Y1T = 4*PER + PYR + 1

        ELSEIF ( DS1T .LT. 730487 ) THEN

          DS1T = DS1T - 657438
          PER100 = (DS1T-1)/36524
          DS1T = DS1T - 36524*PER100

          PER = (DS1T-1)/1461
          DLF = DS1T - 1461*PER
          PYR = (4*DLF - 1)/1461
          J1T = DLF - 365*PYR
          Y1T = 1800 + 100*PER100 + 4*PER + PYR + 1

        ELSEIF ( DS1T .EQ. 730487 ) THEN

          J1T = 366
          Y1T = 2000

        ELSE

          DS1T = DS1T - 730487
          PER400 = (DS1T-1)/146097
          DS1T = DS1T - 146097*PER400

          IF ( DS1T .LT. 146097 ) THEN
            PER100 = (DS1T-1)/36524
            DS1T = DS1T - 36524*PER100

            PER = (DS1T-1)/1461
            DLF = DS1T - 1461*PER
            PYR = (4*DLF - 1)/1461
            J1T = DLF - 365*PYR
            Y1T = 2000 + 400*PER400 + 100*PER100 + 4*PER + PYR + 1
          ELSE
            J1T = 366
            Y1T = 2000 + 400*(PER400+1)
          ENDIF

        ENDIF

        J1 = J1T
        Y1 = Y1T

      RETURN
      END
