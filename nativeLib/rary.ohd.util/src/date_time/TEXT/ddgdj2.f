C  =====================================================================
C  pgm: DDGDJ2 .. Get frm da-sum, jul-dt (frm yr-1900)
C
C  use:     CALL DDGDJ2(DU1,J1,Y1)
C
C   in: DU1 ..... day-sum since Jan 1, 1900 - INT
C  out: J1 ...... day of year (001-366) - INT
C  out: Y1 ...... 4-digit year number - INT
C
C  lvl: DD1
C
C  cmt:   36890 is the day number for 31 Dec 2000.
C  cmt:    1461 is the number of days in a 4 year period.
C  cmt:   36524 is the number of days in a 100 year period.
C  cmt:  146097 is the number of days in a 400 year period.
C  =====================================================================
      SUBROUTINE DDGDJ2(DU1,J1,Y1)

      INTEGER    DU1,J1,Y1,DU1T,J1T,Y1T,PER,DLF,PYR,PER100,PER400
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgdj2.f,v $
     . $',                                                             '
     .$Id: ddgdj2.f,v 1.1 1998/07/02 19:59:56 page Exp $
     . $' /
C    ===================================================================
C

        DU1T = DU1

        IF ( DU1T .LE. 365 ) THEN

          J1T = DU1T
          Y1T = 1900

        ELSEIF ( DU1T .LE. 36890 ) THEN

          DU1T = DU1T - 365
          PER = (DU1T-1)/1461
          DLF = DU1T - 1461*PER
          PYR = (4*DLF - 1)/1461
          J1T = DLF - 365*PYR
          Y1T = 1900 + 4*PER + PYR + 1

        ELSE

          DU1T = DU1T - 36890
          PER400 = (DU1T-1)/146097
          DU1T = DU1T - 146097*PER400

          IF ( DU1T .LT. 146097 ) THEN
            PER100 = (DU1T-1)/36524
            DU1T = DU1T - 36524*PER100

            PER = (DU1T-1)/1461
            DLF = DU1T - 1461*PER
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
