C  =====================================================================
C  pgm: DDGCH2 .. Get frm cal-dt, hr-sum (frm yr-1900)
C
C  use:     CALL DDGCH2(HU1,Y1,M1,D1,H1)
C
C  out: HU1 ..... hour-sum since Jan 1, 1900 (hr 1 is 1AM 1900) - INT
C   in: Y1 ...... 4-digit year number (1900 plus) - INT
C   in: M1 ...... month number (01-12) - INT
C   in: D1 ...... day number (01-31) - INT
C   in: H1 ...... hour number (0-23) - INT
C
C  rqd: DDGJD2,DDGCJ
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDGCH2(HU1,Y1,M1,D1,H1)

      EXTERNAL   DDGJD2,DDGCJ

      INTEGER    HU1,Y1,M1,D1,H1,J1T,DS1T
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgch2.f,v $
     . $',                                                             '
     .$Id: ddgch2.f,v 1.1 1998/07/02 16:23:53 page Exp $
     . $' /
C    ===================================================================
C

        CALL DDGCJ(J1T,Y1,M1,D1)
        CALL DDGJD2(DS1T,J1T,Y1)

        HU1 = (DS1T-1)*24 + H1

      RETURN
      END
