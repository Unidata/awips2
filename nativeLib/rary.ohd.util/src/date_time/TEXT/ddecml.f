C$PRAGMA C (DDRMCL)
C  =====================================================================
C  pgm: DDECML .. Edit gvn m/d, get nearest lcl cal-dt
C
C  use:     CALL DDECML(Y1,M1,D1,COND)
C
C  i/o: Y1 ...... 4-digit year number - INT
C  i/o: M1 ...... month number (01-12), set to cur mon if bad - INT
C  i/o: D1 ...... day number (01-31), set to cur day if bad - INT
C  out: COND .... edit status: 0=date exists, 1=bad date - INT
C
C  rqd: DDEC,DDRMCL,DDGCCD
C
C  lvl: 3
C
C  cmt: If a previous or next computed date is the same number of days
C  cmt:  from the given day, use the earlier date.
C  cmt: If the given day number is bad, output the current date.
C  cmt: Note, if month/day is 2/29, the nearest leap year is returned.
C
C  cmt: Changed to make Y1 i/o. If it is 0, use system clock, otherwise
C  cmt:  use the year passed in for the calculations (to allow @SETTOD
C  cmt:  overrides to work properly) - JTO - MARFC - 10/2005
C  cmt: Also added comments on logic flow - JTO - MARFC - 10/2005
C  =====================================================================
      SUBROUTINE DDECML(Y1,M1,D1,COND)

cc AV pgf90 port 7/3/01      EXTERNAL   DDEC,DDRMCL,DDGCCD
      EXTERNAL   DDEC,DDGCCD

      INTEGER    Y1,M1,D1,COND,Y1T,M1T,D1T,YP,MP,DP,YN,MN,DN,NODP,NODN
      INTEGER    CY1,CM1,CD1,CH1,CN1,CS1,JCOND
      INTEGER    CYT,CMT,CDT,CHT,CNT,CST
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/ddecml.f,v $
     . $',                                                             '
     .$Id: ddecml.f,v 1.3 2005/10/31 18:11:15 xfan Exp $
     . $' /
C    ===================================================================
C
C    Set some temporary holders for current system time values
C     to use in case Y1 is set non-zero but the month
C     or day passed in is invalid

        CALL DDRMCL(CYT,CMT,CDT,CHT,CNT,CST)

C    Set temp month and day based on args passed in
        M1T = M1
        D1T = D1

C    If Y1 non-zero, override is in place,otherwise get current time
        IF (Y1 .NE. 0) THEN
          CY1 = Y1
          CM1 = M1
          CD1 = D1
          Y1T = Y1
        ELSE
C         Get current time values for year, month and day
          CALL DDRMCL(CY1,CM1,CD1,CH1,CN1,CS1)
          Y1T = CY1
        ENDIF

C    Start check for invalid month/day combination
        COND = 0
        IF (D1T.LT.1 .OR. D1T.GT.31 .OR. M1T.LT.1 .OR. M1T.GT.12) THEN
          COND = 1
        ELSEIF (D1T .EQ. 31) THEN
          IF (M1T.EQ.2 .OR. M1T.EQ.4 .OR. M1T.EQ.6 .OR.
     $        M1T.EQ.9 .OR. M1T.EQ.11 ) COND = 1
        ELSEIF (M1T.EQ.2 .AND. D1T.EQ.30) THEN
          COND = 1
        ENDIF

C    Cond .ne. 0, then bad month/day combo; reset to current clock
        IF (COND .NE. 0) THEN
          M1T = CMT
          D1T = CDT

C    See if Y1T/M1T/D1T is legit date
        ELSE
          MP = 0
          MN = 0
          CALL DDEC(Y1T,M1T,D1T,JCOND)
          IF (JCOND .EQ. 0) THEN
C    Good date, start setting previous and next year and month
C    (DDGCCD computes # days between date1 and date2)  
            CALL DDGCCD(Y1T,M1T,D1T,CY1,CM1,CD1,NODP)
            IF (NODP .GE. 0) THEN
              YP = Y1T
              MP = M1T
            ELSE
              NODN = -NODP
              YN = Y1T
              MN = M1T
            ENDIF
          ENDIF

C    Reach here if date check above failed.
          IF (MP .EQ. 0) THEN
            YP = Y1T-1
            MP = M1T
            DP = D1T
C     See if reset date is legit
            CALL DDEC(YP,MP,DP,JCOND)

   40       IF (JCOND.EQ.0 .OR. COND.EQ.1) GOTO 50
              IF (YP .LT. Y1T-8) THEN
                COND = 1
                M1T = CMT
                D1T = CDT
              ELSE
                YP = YP-1
                CALL DDEC(YP,MP,DP,JCOND)
              ENDIF
              GOTO 40
   50       CONTINUE

            IF (COND .EQ. 0) CALL DDGCCD(YP,MP,DP,CY1,CM1,CD1,NODP)
          ENDIF

          IF (COND.EQ.0 .AND. MN.EQ.0) THEN
            YN = Y1T+1
            MN = M1T
            DN = D1T
            CALL DDEC(YN,MN,DN,JCOND)

   60       IF (JCOND.EQ.0 .OR. COND.EQ.1) GOTO 70
              IF (YN .GT. Y1T+8) THEN
                COND = 1
                M1T = CMT
                D1T = CDT
              ELSE
                YN = YN+1
                CALL DDEC(YN,MN,DN,JCOND)
              ENDIF
              GOTO 60
   70       CONTINUE

            IF (COND .EQ. 0) CALL DDGCCD(CY1,CM1,CD1,YN,MN,DN,NODN)
          ENDIF

          IF (COND .EQ. 0) THEN
            IF (NODP .LE. NODN) THEN
              Y1T = YP
            ELSE
              Y1T = YN
            ENDIF
          ENDIF
        ENDIF

        Y1 = Y1T
        M1 = M1T
        D1 = D1T

      RETURN
      END
