C  =====================================================================
C  pgm: DDGJD2 .. Get frm jul-dt, da-sum (frm yr-1900)
C
C  use:     CALL DDGJD2(DU1,J1,Y1)
C
C  out: DU1 ..... day-sum since Jan 1, 1900 - INT
C   in: J1 ...... day of year (001-366) - INT
C   in: Y1 ...... 4-digit year number (1900 plus) - INT
C
C  lvl: DD1
C  =====================================================================
      SUBROUTINE DDGJD2(DU1,J1,Y1)

      INTEGER    DU1,J1,Y1,DU1T,J1T,Y1T,YII,YJJ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgjd2.f,v $
     . $',                                                             '
     .$Id: ddgjd2.f,v 1.1 1998/07/02 16:25:08 page Exp $
     . $' /
C    ===================================================================
C

        J1T = J1
        Y1T = Y1

        DU1T = 0
        IF ( Y1T .GE. 1900) THEN

          YII = Y1T - 1900
          IF ( YII .GT. 0 ) THEN
            DU1T = 365*YII + (YII-1)/4
            IF ( YII .GT. 200 ) THEN
              YJJ = YII - 101
              DU1T = DU1T - YJJ/100 + YJJ/400
            ENDIF
          ENDIF

          DU1T = DU1T + J1T
        ENDIF

        DU1 = DU1T

      RETURN
      END
