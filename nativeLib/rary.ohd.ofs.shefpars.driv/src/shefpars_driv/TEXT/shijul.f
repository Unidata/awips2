C  =====================================================================
C  pgm: SHIJUL .. Increment jul-dt by given number of pos or neg days
C
C  use:     CALL SHIJUL(JUL,JYR,INC)
C
C  i/o: JUL ...... day of year (001-366) - INT
C  i/o: JYR ...... year number (1753-2199) - INT
C   in: INC ...... number of days (pos or neg) to increment date - INT
C  =====================================================================
      SUBROUTINE SHIJUL(JUL,JYR,INC)

      INTEGER        JUL,JYR,INC,JULT,JYRT,MAXD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shijul.f,v $
     . $',                                                             '
     .$Id: shijul.f,v 1.3 1998/04/07 19:13:48 page Exp $
     . $' /
C    ===================================================================
C

        JULT = JUL+INC
        JYRT = JYR

        MAXD = 366+JYRT/4-(JYRT+3)/4
         IF (JYRT .EQ. 1900) MAXD = MAXD-1
         IF (JYRT .EQ. 1800) MAXD = MAXD-1
         IF (JYRT .EQ. 2100) MAXD = MAXD-1
  100   IF (JULT.GE.1 .AND. JULT.LE.MAXD) GOTO 110

            IF (JULT .GT. MAXD) THEN
                JULT = JULT-MAXD
                JYRT = JYRT+1
                IF (JYRT .EQ. 100) JYRT = 0
                MAXD = 366+JYRT/4-(JYRT+3)/4
                 IF (JYRT .EQ. 1900) MAXD = MAXD-1
                 IF (JYRT .EQ. 1800) MAXD = MAXD-1
                 IF (JYRT .EQ. 2100) MAXD = MAXD-1
              ELSE
                JYRT = JYRT-1
                IF (JYRT .EQ. -1) JYRT = 99
                MAXD = 366+JYRT/4-(JYRT+3)/4
                 IF (JYRT .EQ. 1900) MAXD = MAXD-1
                 IF (JYRT .EQ. 1800) MAXD = MAXD-1
                 IF (JYRT .EQ. 2100) MAXD = MAXD-1
                JULT = JULT+MAXD
            ENDIF

            GOTO 100
  110   CONTINUE

        JUL = JULT
        JYR = JYRT

      RETURN
      END
