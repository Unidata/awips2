C  =====================================================================
C  pgm: DDICH1 .. Increment cal-dt by 1 hour
C
C  use:     CALL DDICH1(Y1,M1,D1,H1)
C
C  i/o: Y1 ...... 4-digit year number - INT
C  i/o: M1 ...... month number (01-12) - INT
C  i/o: D1 ...... day number (01-31) - INT
C  i/o: H1 ...... hour number (0-23) - INT
C
C  rqd: DDGCDM
C
C  lvl: DD2
C  =====================================================================
      SUBROUTINE DDICH1(Y1,M1,D1,H1)

      EXTERNAL   DDGCDM

      INTEGER    Y1,M1,D1,H1,Y1T,M1T,D1T,H1T,NOD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddich1.f,v $
     . $',                                                             '
     .$Id: ddich1.f,v 1.1 1998/07/06 13:26:39 page Exp $
     . $' /
C    ===================================================================
C

        Y1T = Y1
        M1T = M1
        D1T = D1
        H1T = H1

        H1T = H1T + 1

        IF ( H1T .GE. 24 ) THEN
          H1T = 0
          D1T = D1T + 1
          CALL DDGCDM(Y1T,M1T,NOD)
          IF ( D1T .GT. NOD ) THEN
            D1T = 1
            M1T = M1T + 1
            IF ( M1T .GT. 12 ) THEN
              M1T = 1
              Y1T = Y1T + 1
            ENDIF
          ELSEIF ( Y1T.EQ.1752 .AND. M1T.EQ.9 .AND. D1T.EQ.3 ) THEN
            D1T = 14
          ENDIF
        ENDIF

        Y1 = Y1T
        M1 = M1T
        D1 = D1T
        H1 = H1T

      RETURN
      END
