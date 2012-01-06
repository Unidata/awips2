C  =====================================================================
C  pgm: DDGJC .. Get frm jul-dt, cal-dt (get mo, da)
C
C  use:     CALL DDGJC(J1,Y1,M1,D1)
C
C   in: J1 ...... day of year (001-366) - INT
C   in: Y1 ...... 4-digit year number - INT
C  out: M1 ...... month number (01-12) - INT
C  out: D1 ...... day number (01-31) - INT
C
C  lvl: DD1
C
C  cmt: Alternate algorithm:      M1T = (J1T*9 + 269)/275
C  cmt:                           D1T = J1T - (M1T*275)/9 + 30
C  =====================================================================
      SUBROUTINE DDGJC(J1,Y1,M1,D1)

      INTEGER    J1,Y1,M1,D1,II,J1T,Y1T,M1T,D1T
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgjc.f,v $
     . $',                                                             '
     .$Id: ddgjc.f,v 1.1 1998/07/02 20:01:20 page Exp $
     . $' /
C    ===================================================================
C

        J1T = J1
        Y1T = Y1

        II = 2
        IF (         Y1T.EQ.(Y1T/4)*4
     $       .AND.   Y1T.NE.1800
     $       .AND.   Y1T.NE.1900
     $       .AND. ( Y1T.LT.2100 .OR. Y1T.EQ.(Y1T/400)*400
     $                           .OR. Y1T.NE.(Y1T/100)*100 )
     $     ) II = 1

        IF ( Y1T.EQ.1752 .AND. J1T.GT.246 ) J1T = J1T + 11

        IF ( J1T .GT. (61-II) ) J1T = J1T + II
        M1T = (J1T*67 + 2012)/2048
        D1T = J1T - (M1T*489)/16 + 30

        M1 = M1T
        D1 = D1T

      RETURN
      END
