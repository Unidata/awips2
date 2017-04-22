C  =====================================================================
C  pgm: DDSC .. Status of cal-dt
C
C  use:     CALL DDSC(Y1,M1,D1,COND)
C
C   in: Y1 ...... 4-digit year number - INT
C   in:           (2-digit gives only a warning or neg status)
C   in: M1 ...... month number (01-12) - INT
C   in: D1 ...... day number (01-31) - INT
C  out: COND .... status of date:  0 ... good numbers
C  out:                           -1 ... good numbers but 2-digit yr
C  out:                            1 ... bad year
C  out:                            2 ... bad month
C  out:                            3 ... bad day
C
C  rqd: DDGCDM,DDYCDL
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDSC(Y1,M1,D1,COND)

      INTEGER    Y1,M1,D1,COND,Y1T,M1T,D1T,DIM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddsc.f,v $
     . $',                                                             '
     .$Id: ddsc.f,v 1.1 2001/06/13 08:57:45 mgm Exp $
     . $' /
C    ===================================================================
C

        Y1T = Y1
        M1T = M1
        D1T = D1

        COND = 0

        IF (Y1T.LT.1 .OR. Y1T.GE.2100) THEN
            COND = 3
        ELSEIF (Y1T .LT. 136) THEN
            COND = -1
        ELSEIF (M1T.LT.1 .OR. M1T.GT.12) THEN
            COND = 2
        ELSEIF (D1T.LT.1 .OR. D1T.GT.31) THEN
            COND = 1
        ELSE
          CALL DDYCDL(Y1T,M1T,D1T)
          CALL DDGCDM(Y1T,M1T,DIM)
          IF (D1T .GT. DIM) THEN
            COND = 1
          ENDIF
        ENDIF

      RETURN
      END
