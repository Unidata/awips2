C  =====================================================================
C  pgm: DDGCJ .. Get frm cal-dt, jul-dt (get julda)
C
C  use:     CALL DDGCJ(J1,Y1,M1,D1)
C
C  out: J1 ...... day of year (001-366) - INT
C   in: Y1 ...... 4-digit year number - INT
C   in: M1 ...... month number (01-12) - INT
C   in: D1 ...... day number (01-31) - INT
C
C  lvl: DD1
C  =====================================================================
      SUBROUTINE DDGCJ(J1,Y1,M1,D1)

      INTEGER    J1,Y1,M1,D1,J1T,Y1T,M1T,D1T,NODIYT(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgcj.f,v $
     . $',                                                             '
     .$Id: ddgcj.f,v 1.2 1998/07/02 16:24:45 page Exp $
     . $' /
C    ===================================================================
C
      DATA       NODIYT / 0,31,59,90,120,151,181,212,243,273,304,334 /

        Y1T = Y1
        M1T = M1
        D1T = D1

        IF ( M1T.LT.1 .OR. M1T.GT.12 ) M1T = 1
        J1T = NODIYT(M1T) + D1T
        IF (         M1T.GT.2
     $       .AND.   Y1T.EQ.(Y1T/4)*4
     $       .AND.   Y1T.NE.1800
     $       .AND.   Y1T.NE.1900
     $       .AND. ( Y1T.LT.2100 .OR. Y1T.EQ.(Y1T/400)*400
     $                           .OR. Y1T.NE.(Y1T/100)*100 )
     $     ) J1T=J1T + 1
        IF ( Y1T.EQ.1752 .AND. J1T.GT.246 ) J1T = J1T - 11

        J1 = J1T

      RETURN
      END
