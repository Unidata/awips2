C$PRAGMA C (UTIM)
C  =====================================================================
C  pgm: UTIMF .. Get or write out real, user, system, or cpu times
C
C  use:     CALL UTIMF(CMD,STMT,IUN,NTIM)
C
C   in: CMD ...... routine command as follows: - CHAR*(*)
C   in:              SetBeg ......... initialize times
C   in:              Set ............ reset lapse times
C   in:              SetGetLapReal .. set times, get lapse real time
C   in:              AndGetLapReal .. just get lapse real time
C   in:              SetWriLapReal .. set times, write lapes real time
C   in:              AndWriLapReal .. just write lapes real time
C   in:              SetObtLapReal .. set times, obtain lap real in STMT
C   in:              AndObtLapReal .. just obtain lapse real tm in STMT
C   in:              SetGetTotReal .. set times, get total real time
C   in:              AndGetTotReal .. just get total real time
C   in:              SetWriTotReal .. set times, write total real time
C   in:              AndWriTotReal .. just write total real time
C   in:              SetObtTotReal .. set times, obtain tot real in STMT
C   in:              AndObtTotReal .. just obtain total real tm in STMT
C   in:            repeat last 12 commands except replace "Real" with
C   in:                       Cpu ... for cpu time (user + system time)
C   in:                       User .. for user time (user space in cpu)
C   in:                       Sys ... for system time
C  i/o: STMT ..... for commands with "Wri", this is an input text string
C  i/o:              to be printed before the time,
C  i/o:            for commands with "Obt", this is an output text str
C  i/o:              of 10 chars to contain the time as "XXXXm XX.YYs"
C   in: IUN ...... unit number for write commands (if -1, no write)
C  out: NTIM ..... time desired in 1/1000s sec for Get, Wri, Obt cmds
C
C  rqd: UTIM
C  =====================================================================
      SUBROUTINE UTIMF(CMD,STMT,IUN,NTIM)

cfan  UTIM double defined  07/12
cfan  EXTERNAL       UTIM

      CHARACTER*(*)  CMD,STMT
      CHARACTER*12   CMDX,STMTX
      INTEGER        IUN,NTIM,ITR,ITU,ITS,JJ1,JJ2,JJ3,JE,INITZ
      INTEGER        BEGR,BEGU,BEGS,PRVR,PRVU,PRVS,TMPR,TMPU,TMPS

      SAVE           BEGR,BEGU,BEGS,PRVR,PRVU,PRVS,TMPR,TMPU,TMPS,INITZ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/utimf.f,v $
     . $',                                                             '
     .$Id: utimf.f,v 1.2 2002/02/11 21:15:47 dws Exp $
     . $' /
C    ===================================================================
C

      DATA           INITZ / 0 /

        NTIM = 0
        CMDX = CMD

        IF (INITZ .EQ. 0) THEN
          CMDX(1:6) = 'SetBeg'
          INITZ = 1
        ENDIF

        IF (CMDX(1:1) .EQ. 'S') THEN
          CALL UTIM(ITR,ITU,ITS)
          IF (CMDX(4:4) .EQ. 'B') THEN
              BEGR = ITR
              BEGU = ITU
              BEGS = ITS
              PRVR = ITR
              PRVU = ITU
              PRVS = ITS
              TMPR = PRVR
              TMPU = PRVU
              TMPS = PRVS
          ELSE
              TMPR = PRVR
              TMPU = PRVU
              TMPS = PRVS
              PRVR = ITR
              PRVU = ITU
              PRVS = ITS
          ENDIF
        ENDIF

        IF (CMDX(4:4).EQ.'G' .OR. CMDX(4:4).EQ.'W'
     $                       .OR. CMDX(4:4).EQ.'O') THEN
          IF (CMDX(7:7) .EQ. 'L') THEN
              IF (CMDX(10:10) .EQ. 'R') THEN
               NTIM = PRVR - TMPR
              ELSEIF (CMDX(10:10) .EQ. 'U') THEN
               NTIM = PRVU - TMPU
              ELSEIF (CMDX(10:10) .EQ. 'S') THEN
               NTIM = PRVS - TMPS
              ELSEIF (CMDX(10:10) .EQ. 'C') THEN
               NTIM = PRVU - TMPU + PRVS - TMPS
              ENDIF
          ELSEIF (CMDX(7:7) .EQ. 'T') THEN
              IF (CMDX(10:10) .EQ. 'R') THEN
               NTIM = PRVR - BEGR
              ELSEIF (CMDX(10:10) .EQ. 'U') THEN
               NTIM = PRVU - BEGU
              ELSEIF (CMDX(10:10) .EQ. 'S') THEN
               NTIM = PRVS - BEGS
              ELSEIF (CMDX(10:10) .EQ. 'C') THEN
               NTIM = PRVU - BEGU + PRVS - BEGS
              ENDIF
          ENDIF
        ENDIF

        IF (CMDX(4:4).EQ.'W' .OR. CMDX(4:4).EQ.'O') THEN
              JJ2 = NTIM/1000
              JJ3 = NTIM - (1000*JJ2)
              JJ3 = JJ3/10
              JJ1 = JJ2/60
              JJ2 = JJ2 - (60*JJ1)
              IF (JJ1.GT.0 .AND. JJ1.LT.10000) THEN
               WRITE(STMTX,'(I4,''m'',I3,''.'',I2.2,''s'')') JJ1,JJ2,JJ3
              ELSE
               WRITE(STMTX,'(5X,I3,''.'',I2.2,''s'')') JJ2,JJ3
              ENDIF
        ENDIF

        IF (CMDX(4:4).EQ.'W' .AND. IUN.GE.0) THEN
              WRITE(IUN,'(A,A)',IOSTAT=JE) STMT,STMTX
        ELSEIF (CMDX(4:4) .EQ. 'O') THEN
              STMT = STMTX
        ENDIF

      RETURN
      END
