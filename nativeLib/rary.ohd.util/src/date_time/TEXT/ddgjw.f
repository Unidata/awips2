C  =====================================================================
C  pgm: DDGJW .. Get frm jul-dt, day-of-week
C
C  use:     CALL DDGJW(J1,Y1,W1,DOW)
C
C   in: J1 ...... day of year number (1-366) - INT
C   in: Y1 ...... 4-digit year number - INT
C  out: W1 ...... day in week number (1=Sunday, 2=Monday) - INT
C  out: DOW ..... day in week as character string - CHAR*(*)
C
C  lvl: DD1
C
C  cmt: Day 639798 is 2 Sep 1752; 11 days are skipped to 14 Sep 1752.
C  =====================================================================
      SUBROUTINE DDGJW(J1,Y1,W1,DOW)

      INTEGER        J1,Y1,W1,J1T,Y1T,DS1T,II,JJ,KK,YM1,JJJ
      INTEGER        LS1(7),LS2(7),LS3(7)
      CHARACTER*(*)  DOW
      CHARACTER*50   NAM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgjw.f,v $
     . $',                                                             '
     .$Id: ddgjw.f,v 1.1 2001/08/01 17:32:43 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  LS1 / 6,7,1,2,3,4,5 /
      DATA  LS2 / 37,43,1,7,13,20,29 /
      DATA  LS3 / 42,50,6,12,19,28,36 /
      DATA  NAM / 'SundayMondayTuesdayWednesdayThursdayFridaySaturday' /

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

        DS1T = DS1T + J1T

        KK = DS1T - (DS1T/7)*7 + 1

        II  = LS2(KK)
        JJ  = LS3(KK)

        W1  = LS1(KK)
        DOW = NAM(II:JJ)

      RETURN
      END
