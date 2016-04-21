C  =====================================================================
C  pgm: SHLINE .. All operations (i/o and retrieve) for a message line
C
C  use:      CALL SHLINE(CMD,KHAR,KHPOS)
C
C   in: CMD ....... command or message to control operations - CHAR*16
C   in:               'INITIALIZE' ..... set or reset parameters, units
C   in:                                  (initialize KHAR and KHPOS)
C   in:               'GET_NEW_LINE' ... read in a new shef message line
C   in:               'GET_NEXT_CHAR' .. get next char from current line
C   in:               'WRITE_COPY' ..... write out a copy of cur line
C   in:               'WRITE_WARNING' .. write out a warning message
C   in:               'WRITE_ERROR' .... write out an error message
C   in:               'WRITE_NUM_LINES'  write out total number of lines
C  out: KHAR ...... next char entered and/or returned - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: (subrtn) .. enter logical unit numbers outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFIN',<number>)
C   in:               CALL SHSAVU('P_SHEFERROR',<number>)
C   in:               CALL SHSAVU('P_SHEFCOPY',<number>)
C
C  rqd: SHSAVU,SHEDLN
C  =====================================================================
      SUBROUTINE SHLINE(CMD,KHAR,KHPOS)

      EXTERNAL       SHSAVU,SHEDLN
      INTRINSIC      LEN

      CHARACTER*1004 LINE
      CHARACTER*10   FMAT
      CHARACTER*1    KHAR
      CHARACTER*16   CMD
      INTEGER        KHPOS,LBEG,LEND,LNUM,LSTAT,INITZ,MAXEND,JJ,LEN
      INTEGER        LUNC,LUNE,LUNI,LLC,LLE,LLEX

      SAVE           INITZ,MAXEND,LBEG,LEND,LNUM,LINE
      SAVE           LUNI,LUNE,LUNC,LLC,LLE,LLEX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shline.f,v $
     . $',                                                             '
     .$Id: shline.f,v 1.5 1998/07/22 12:33:51 page Exp $
     . $' /
C    ===================================================================
C

      DATA           INITZ,FMAT / 0, '(001X,''?'')' /

      IF (CMD.EQ.'INITIALIZE      ' .OR. INITZ.EQ.0) THEN
          INITZ  = 1
          MAXEND = LEN(LINE)
          LINE   = ':: '
          LINE(MAXEND:MAXEND) = ':'
          LBEG   = 4
          LEND   = 3
          LNUM   = 0
           CALL SHSAVU('G_SHEFIN    ',LUNI)
           CALL SHSAVU('G_SHEFERROR ',LUNE)
           CALL SHSAVU('G_SHEFCOPY  ',LUNC)
           LLC  = -1
           LLE  = -1
           LLEX = -1
           IF (LUNE.GE.0 .AND. LUNE.NE.LUNC) LLEX = 1
            KHAR  = ':'
            KHPOS = 0
            IF (LUNI .GE. 0) KHPOS = 2
      ENDIF

      IF (CMD.EQ.'GET_NEW_LINE    ') THEN
          LBEG  = 4
          LEND  = 3
          LSTAT = 0
          KHPOS = 0
          KHAR  = LINE(1:1)

  100     IF (LBEG.LE.LEND .OR. LSTAT.NE.0 .OR. LUNI.LT.0) GOTO 200
            READ(LUNI,'(A)',IOSTAT=LSTAT) LINE(LBEG:MAXEND-1)

            IF (LSTAT .GT. 0) THEN
                KHPOS = -LSTAT
                KHAR  = ':'
              ELSEIF (LSTAT .EQ. 0) THEN
                LNUM = LNUM+1
                LLC  = LUNC
                LLE  = LLEX
                 CALL SHEDLN(LBEG,LEND,LINE)
                KHPOS = 3
                KHAR  = LINE(KHPOS:KHPOS)
              ELSE
                KHPOS = 0
                KHAR  = ':'
            ENDIF

            GOTO 100
  200     CONTINUE

      ELSEIF (CMD.EQ.'GET_NEXT_CHAR   ') THEN
          IF (KHPOS .EQ. LEND) THEN
              KHPOS = 2
              KHAR  = LINE(KHPOS:KHPOS)
            ELSEIF (KHPOS .GT. 2) THEN
              KHPOS = KHPOS+1
              KHAR  = LINE(KHPOS:KHPOS)
          ENDIF

      ELSEIF (CMD.EQ.'WRITE_COPY      ') THEN
          IF (LLC.GE.0 .AND. LBEG.LE.LEND) THEN
              WRITE(LUNC,'(1X,A)',IOSTAT=LSTAT) LINE(LBEG:LEND)
              LLC = -1
          ENDIF

      ELSEIF (CMD.EQ.'WRITE_WARNING   ' .OR.
     $        CMD.EQ.'WRITE_ERROR     '      ) THEN
          IF (LLE.EQ.1 .AND. LBEG.LE.LEND) THEN
              WRITE(LUNE,'(1X,A,''   (line'',I8,'')'')',IOSTAT=LSTAT)
     $         LINE(LBEG:LEND),LNUM
              LLE = -1
          ENDIF
          IF (KHPOS.GT.0 .AND. LUNE.GE.0) THEN
              JJ = KHPOS-3
              IF (JJ .LE. 0) JJ = LEND-3
              WRITE(FMAT(2:4),'(I3)',IOSTAT=LSTAT) JJ
              WRITE(LUNE,FMAT,IOSTAT=LSTAT)
          ENDIF

      ELSEIF (CMD.EQ.'WRITE_NUM_LINES ') THEN
          IF (LUNE .GE. 0) THEN
           WRITE(LUNE,'(''    TOTAL NUMBER OF LINES ..'',I8)',
     $      IOSTAT=LSTAT) LNUM
          ENDIF

      ENDIF

      RETURN
      END
