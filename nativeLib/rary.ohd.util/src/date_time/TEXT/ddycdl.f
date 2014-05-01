C$PRAGMA C (DDRMCL)
C  =====================================================================
C  pgm: DDYCDL .. Updt yr for cal-dt-da by lcl 90/10
C
C  use:     CALL DDYCDL(Y1,M1,D1)
C
C  i/o: Y1 ...... 4-digit year number (may updt by lcl 90/10 rule) - INT
C  i/o:             (if input is 2-digits, it is converted to 4-digits)
C   in: M1 ...... month number (01-12) - INT
C   in: D1 ...... day number (01-31) - INT
C
C  rqd: DDRMCL
C
C  lvl: DD1
C
C  cmt: The century is determined by 90 yrs in past, 10 yrs in future.
C  =====================================================================
      SUBROUTINE DDYCDL(Y1,M1,D1)

cfan $pgf90 port 7/3/01      EXTERNAL   DDRMCL

      INTEGER    Y1,M1,D1,Y1T,M1T,D1T,YC,MC,DC,HC,NC,SC,DF,CC,AD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddycdl.f,v $
     . $',                                                             '
     .$Id: ddycdl.f,v 1.2 2002/02/11 16:32:34 dws Exp $
     . $' /
C    ===================================================================
C

        Y1T = Y1
        M1T = M1
        D1T = D1

        IF ( Y1T.GE.0 .AND. Y1T.LE.136 ) THEN
          CALL DDRMCL(YC,MC,DC,HC,NC,SC)

          AD = 0
          CC = (YC/100)*100
          YC = YC - CC
          DF = Y1T - YC
          IF ( Y1T.GE.100 .AND. CC.GT.1900                     ) THEN
              AD = -100
          ELSEIF ( Y1T .LT. 100) THEN
            IF     ( DF.GT. 10                                 ) THEN
              AD = -100
            ELSEIF ( DF.EQ. 10 .AND. MC.LT.M1T                 ) THEN
              AD = -100
            ELSEIF ( DF.EQ. 10 .AND. MC.EQ.M1T .AND. DC.LT.D1T ) THEN
              AD = -100
            ELSEIF ( DF.LT.-90                                 ) THEN
              AD =  100
            ELSEIF ( DF.EQ.-90 .AND. MC.GT.M1T                 ) THEN
              AD =  100
            ELSEIF ( DF.EQ.-90 .AND. MC.EQ.M1T .AND. DC.GT.D1T ) THEN
              AD =  100
            ENDIF
          ENDIF

          Y1 = Y1T + CC + AD
        ENDIF

      RETURN
      END
