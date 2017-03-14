C  =====================================================================
C  pgm: DDVCCD .. Convert cal-dt to cal-dt by da-inc
C
C  use:     CALL DDVCCD(Y1,M1,D1,Y2,M2,D2,DAI)
C
C   in: Y1 ...... starting 4-digit year number - INT
C   in: M1 ...... starting month number (1-12) - INT
C   in: D1 ...... starting day number (1-31) - INT
C  out: Y2 ...... ending 4-digit year number - INT
C  out: M2 ...... ending month number (1-12) - INT
C  out: D2 ...... ending day number (1-31) - INT
C   in: DAI ..... num of days to add or subtract from startg date - INT
C
C  rqd: DDGCJ,DDGJC,DDGJD,DDGDJ
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDVCCD(Y1,M1,D1,Y2,M2,D2,DAI)

      EXTERNAL   DDGCJ,DDGJC,DDGJD,DDGDJ

      INTEGER    Y1,M1,D1,Y2,M2,D2,DAI,J1T,J2T,DS1T
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddvccd.f,v $
     . $',                                                             '
     .$Id: ddvccd.f,v 1.2 1998/10/14 15:36:16 page Exp $
     . $' /
C    ===================================================================
C

        CALL DDGCJ(J1T,Y1,M1,D1)
        CALL DDGJD(DS1T,J1T,Y1)
        CALL DDGDJ(DS1T+DAI,J2T,Y2)
        CALL DDGJC(J2T,Y2,M2,D2)

      RETURN
      END
