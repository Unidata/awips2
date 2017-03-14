C  =====================================================================
C  pgm: DDGJD .. Get frm jul-dt, da-sum (frm yr-0001)
C
C  use:     CALL DDGJD(DS1,J1,Y1)
C
C  out: DS1 ..... day-sum since Jan 1, 0001 - INT
C   in: J1 ...... day of year (001-366) - INT
C   in: Y1 ...... 4-digit year number - INT
C
C  lvl: DD1
C
C  cmt: Day 639798 is 2 Sep 1752; 11 days are skipped to 14 Sep 1752.
C  =====================================================================
      SUBROUTINE DDGJD(DS1,J1,Y1)

      INTEGER    DS1,J1,Y1,DS1T,J1T,Y1T,YM1,JJJ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgjd.f,v $
     . $',                                                             '
     .$Id: ddgjd.f,v 1.1 1998/10/14 14:12:29 page Exp $
     . $' /
C    ===================================================================
C

        J1T = J1
        Y1T = Y1

        YM1 = Y1T - 1
        DS1T = 365*YM1 + YM1/4

        IF ( YM1 .GE. 1800) THEN
          JJJ = (YM1-1700)/100
          JJJ = JJJ - (JJJ+1)/4
          DS1T = DS1T - JJJ
        ENDIF

        IF ( DS1T .GT. 639798 ) DS1T = DS1T - 11

        DS1 = DS1T + J1T

      RETURN
      END
