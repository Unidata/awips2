C  =====================================================================
C  pgm: DDEC .. Edit if exists cal-dt
C
C  use:     CALL DDEC(Y1,M1,D1,COND)
C
C   in: Y1 ...... 4-digit year number - INT
C   in: M1 ...... month number (01-12) - INT
C   in: D1 ...... day number (01-31) - INT
C  out: COND .... edit status: 0=date exists, 1=bad date - INT
C
C  rqd: DDGCDM
C
C  lvl: 2
C  =====================================================================
      SUBROUTINE DDEC(Y1,M1,D1,COND)

      EXTERNAL   DDGCDM

      INTEGER    Y1,M1,D1,COND,NODIM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddec.f,v $
     . $',                                                             '
     .$Id: ddec.f,v 1.1 1998/10/14 14:11:16 page Exp $
     . $' /
C    ===================================================================
C

        COND = 1
        IF (Y1.GT.0 .AND. M1.GT.0 .AND. M1.LT.13 .AND. D1.GT.0) THEN
          IF (Y1.EQ.1752 .AND. M1.EQ.9) THEN
            IF (D1.LT.3 .OR. (D1.GT.13 .AND. D1.LT.31)) COND = 0
          ELSE
            CALL DDGCDM(Y1,M1,NODIM)
            IF (D1 .LE. NODIM) COND = 0
          ENDIF
        ENDIF

      RETURN
      END
