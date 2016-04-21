C  =====================================================================
C  pgm: DDGCDM ..  Get frm cal-dt, num-of-days in month
C
C  use:     CALL DDGCDM(Y1,M1,NODIM)
C
C   in: Y1 ...... 4-digit year number - INT
C   in: M1 ...... month number (01-12) - INT
C  out: NODIM ... number of days in month (19,28-31) - INT
C
C  lvl: DD1
C  =====================================================================
      SUBROUTINE DDGCDM(Y1,M1,NODIM)

      INTEGER    Y1,M1,NODIM,NODIMT(13),Y1T,M1T,NN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddgcdm.f,v $
     . $',                                                             '
     .$Id: ddgcdm.f,v 1.1 1998/07/06 13:26:15 page Exp $
     . $' /
C    ===================================================================
C
      DATA       NODIMT / 31,28,31,30,31,30,31,31,30,31,30,31,0 /

        Y1T = Y1
        M1T = M1

        IF ( M1T.LT.1 .OR. M1T.GT.12 ) M1T = 13
        NN = NODIMT(M1T)
        IF (         M1T.EQ.2
     $       .AND.   Y1T.EQ.(Y1T/4)*4
     $       .AND.   Y1T.NE.1800
     $       .AND.   Y1T.NE.1900
     $       .AND. ( Y1T.LT.2100 .OR. Y1T.EQ.(Y1T/400)*400
     $                           .OR. Y1T.NE.(Y1T/100)*100 )
     $     ) NN = 29
        IF ( Y1T.EQ.1752 .AND. M1T.EQ.9 ) NN = 19

        NODIM = NN

      RETURN
      END
