C  =====================================================================
C  pgm: DDGHC2 .. Get frm hr-sum, cal-dt (frm yr-1900)
C
C  use:     CALL DDGHC2(HU1,Y1,M1,D1,H1)
C
C   in: HU1 ..... hour-sum since Jan 1, 1900 - INT
C  out: Y1 ...... 4-digit year number - INT
C  out: M1 ...... month number (01-12) - INT
C  out: D1 ...... day number (01-31) - INT
C  out: H1 ...... hour number (0-23) - INT
C
C  rqd: DDGDJ2,DDGJC
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDGHC2(HU1,Y1,M1,D1,H1)

      EXTERNAL   DDGDJ2,DDGJC

      INTEGER    HU1,Y1,M1,D1,H1,HU1T,DS1T,J1T
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddghc2.f,v $
     . $',                                                             '
     .$Id: ddghc2.f,v 1.1 1998/07/02 20:01:02 page Exp $
     . $' /
C    ===================================================================
C

        HU1T = HU1
        DS1T = HU1T/24
        HU1T = HU1T - 24*DS1T
        DS1T = DS1T + 1

        CALL DDGDJ2(DS1T,J1T,Y1)
        CALL DDGJC(J1T,Y1,M1,D1)

        H1 = HU1T

      RETURN
      END
