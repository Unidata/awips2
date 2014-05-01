C  =====================================================================
C  pgm: SHSEND .. Get send code and 7-am flag (read "shefparm" file)
C
C  use:     CALL SHSEND(CMD,KHAR,KHPOS,PARCOD,ISEND,NFLAG)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ..... force read shefparm file
C   in:               'GET_VALUE' ...... get shefparm value(s)
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: PARCOD .... up to 8-char parameter code - CHAR*8
C  out: ISEND ..... send code indicating alias parm code, else -9 - INT
C  out: NFLAG ..... data at 7-am if flag is on (ie. 1) - INT
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHERR,SHSAVU
C  =====================================================================
      SUBROUTINE SHSEND(CMD,KHAR,KHPOS,PARCOD,ISEND,NFLAG)

      EXTERNAL       SHPABG,SHERR,SHSAVU

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*12   CMD
      CHARACTER*1    KHAR,KH1,KH2
      CHARACTER*2    KHFIND
      CHARACTER*8    PARCOD,SCOD,KPRMCD(50)
      INTEGER        KHPOS,INITZ,ICHRA,II,NU1,NU2,IERR,NUM,ICOUNT,LUNP
      INTEGER        ISEND,NFLAG,LIM,ISEN(676),NGFLAG(50)

      SAVE       INITZ,ICHRA,ISEN,NGFLAG,KPRMCD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsend.f,v $
     . $',                                                             '
     .$Id: shsend.f,v 1.5 1999/04/26 12:02:58 page Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,ISEN,KHFIND / 0, 676*-9, '*6' /
      DATA       LIM / 50 /

C                   If first pass, get ISEN,NGFLAG,KPRMCD arrays

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          ICHRA = ICHAR('A')
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)
 
          IF (IERR .EQ. 0) THEN
            INITZ  = 1
            ICOUNT = 0
            READ(LUNP,'(A1,A1,1X,A8,I2)',IOSTAT=IERR) KH1,KH2,SCOD,NUM
  130       IF (IERR.NE.0 .OR. KH1.EQ.'*' .OR. ICOUNT.GE.LIM) GOTO 140
              NU1 = ICHAR(KH1) - ICHRA
              IF (NU1.GE.0 .AND. NU1.LE.25) THEN
                NU2 = ICHAR(KH2) - ICHRA
                IF (NU2.GE.0 .AND. NU2.LE.25) THEN
                  II = 26*NU1 + NU2 + 1
                  ICOUNT = ICOUNT + 1
                  ISEN(II) = ICOUNT
                  KPRMCD(ICOUNT) = SCOD
                  NGFLAG(ICOUNT) = NUM
                ENDIF
              ENDIF
              READ(LUNP,'(A1,A1,1X,A8,I2)',IOSTAT=IERR) KH1,KH2,SCOD,NUM
              GOTO 130
  140       CONTINUE
          ELSEIF (IERR .LT. 0) THEN
            CALL SHERR('W',72,KHPOS,KHAR)
            INITZ = 1
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

C                   Get send code and change PARCOD, get NFLAG

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          NFLAG = 0
          ISEND = -9
          IF (KHPOS .GT. 1) THEN
            KH1 = PARCOD(1:1)
            KH2 = PARCOD(2:2)
            II  = 26*(ICHAR(KH1)-ICHRA) + (ICHAR(KH2)-ICHRA) + 1
            IF (II.GE.1 .AND. II.LE.676) THEN
              ISEND = ISEN(II)
            ELSE
              CALL SHERR('E',29,KHPOS,KHAR)
            ENDIF
          ENDIF

          IF (ISEND .GT. 0) THEN
            IF (PARCOD(3:3) .EQ. ' ') THEN
              PARCOD = KPRMCD(ISEND)
              NFLAG  = NGFLAG(ISEND)
            ELSEIF (KPRMCD(ISEND)(4:4) .NE. ' ') THEN
              CALL SHERR('E',30,KHPOS,KHAR)
            ENDIF
          ENDIF

C                   Expand PARCOD to full value including defaults

          IF (KHPOS .GT. 1) THEN
            IF (PARCOD(3:3) .EQ. ' ') PARCOD(3:3) = 'I'
            IF (PARCOD(4:4) .EQ. ' ') PARCOD(4:4) = 'R'
            IF (PARCOD(5:5) .EQ. ' ') PARCOD(5:5) = 'Z'
            IF (PARCOD(6:6) .EQ. ' ') PARCOD(6:6) = 'Z'
            IF (PARCOD(4:4).EQ.'Z' .AND. PARCOD(5:5).NE.'Z') THEN
              CALL SHERR('E',29,KHPOS,KHAR)
            ENDIF
            IF (PARCOD(4:4) .EQ. 'Z') PARCOD(4:4) = 'R'
          ENDIF
        ENDIF

      RETURN
      END
