C  =====================================================================
C  pgm: DDGCD2 .. Get frm cal-dt, da-sum (frm yr-1900)
C
C  use:     CALL DDGCD2(DU1,Y1,M1,D1)
C
C  out: DU1 ..... day-sum since Jan 1, 1900 - INT
C   in: Y1 ...... 4-digit year number (1900 plus) - INT
C   in: M1 ...... month number (01-12) - INT
C   in: D1 ...... day number (01-31) - INT
C
C  rqd: DDGJD2,DDGCJ
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDGCD2(DU1,Y1,M1,D1)

      EXTERNAL   DDGJD2,DDGCJ

      INTEGER    DU1,Y1,M1,D1,J1T
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgcd2.f,v $
     . $',                                                             '
     .$Id: ddgcd2.f,v 1.1 1998/07/02 19:03:57 page Exp $
     . $' /
C    ===================================================================
C

        CALL DDGCJ(J1T,Y1,M1,D1)
        CALL DDGJD2(DU1,J1T,Y1)

      RETURN
      END
