C  =====================================================================
C  pgm: DDGCCD .. Get frm cal-dt to cal-dt, da-inc
C
C  use:     CALL DDGCCD(Y1,M1,D1,Y2,M2,D2,DAI)
C
C   in: Y1 ...... starting 4-digit year number - INT
C   in: M1 ...... starting month number (1-12) - INT
C   in: D1 ...... starting day number (1-31) - INT
C   in: Y2 ...... ending 4-digit year number - INT
C   in: M2 ...... ending month number (1-12) - INT
C   in: D2 ...... ending day number (1-31) - INT
C  out: DAI ..... number of days between dates - INT
C
C  rqd: DDGJD,DDGCJ
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDGCCD(Y1,M1,D1,Y2,M2,D2,DAI)

      EXTERNAL   DDGJD,DDGCJ

      INTEGER    Y1,M1,D1,Y2,M2,D2,DAI,J1T,J2T,NOD1,NOD2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgccd.f,v $
     . $',                                                             '
     .$Id: ddgccd.f,v 1.1 1998/10/14 14:12:11 page Exp $
     . $' /
C    ===================================================================
C

        CALL DDGCJ(J1T,Y1,M1,D1)
        CALL DDGJD(NOD1,J1T,Y1)

        CALL DDGCJ(J2T,Y2,M2,D2)
        CALL DDGJD(NOD2,J2T,Y2)

        DAI = NOD2 - NOD1

      RETURN
      END
