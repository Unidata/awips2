C  =====================================================================
C  pgm: SHPROB .. Get probability code (read "shefparm" file first)
C
C  use:     CALL SHPROB(CMD,KHAR,KHPOS,PARCOD,CODP)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ..... force read shefparm file
C   in:               'GET_VALUE' ...... get shefparm value(s)
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: PARCOD .... up to 8-char parameter code - CHAR*8
C  out: CODP ...... probability code - REAL
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHERR,SHSAVU
C  =====================================================================
      SUBROUTINE SHPROB(CMD,KHAR,KHPOS,PARCOD,CODP)

      EXTERNAL       SHPABG,SHERR,SHSAVU

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*12   CMD
      CHARACTER*1    KHAR,KH1,KH2
      CHARACTER*2    KHFIND
      CHARACTER*8    PARCOD
      REAL           CODP,RNUM,PROB(35)
      INTEGER        KHPOS,INITZ,ICHRA,ICHR1,II,IERR,LUNP

      SAVE       INITZ,ICHRA,ICHR1,PROB
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shprob.f,v $
     . $',                                                             '
     .$Id: shprob.f,v 1.4 1998/07/22 12:35:09 page Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,PROB,KHFIND / 0, 35*-8.1E9, '*5' /

C                   If first pass, get PROB array from "shefparm" file

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          ICHRA = ICHAR('A')
          ICHR1 = ICHAR('1')
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(A1,A1,G20.0)',IOSTAT=IERR) KH1,KH2,RNUM
            IF (IERR.NE.0 .OR. KH1.EQ.'*') PROB(26) = -1.0
  130       IF (IERR.NE.0 .OR. KH1.EQ.'*') GOTO 140
              IF (KH1.GE.'A' .AND. KH1.LE.'Z') THEN
                II = ICHAR(KH1) - ICHRA + 1
                PROB(II) = RNUM
              ELSEIF (KH1.GE.'1' .AND. KH1.LE.'9') THEN
                II = ICHAR(KH1) - ICHR1 + 27
                PROB(II) = RNUM
              ENDIF
              READ(LUNP,'(A1,A1,G20.0)',IOSTAT=IERR) KH1,KH2,RNUM
              GOTO 130
  140       CONTINUE
          ELSEIF (IERR .LT. 0) THEN
            CALL SHERR('W',76,KHPOS,KHAR)
            PROB(26) = -1.0
            INITZ = 1
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

C                   Get probability value, CODP, from PROB array

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          CODP = -1.0
          IF (KHPOS .GT. 1) THEN
            KH1 = PARCOD(7:7)
            IF (KH1 .NE. ' ') THEN
              IF (KH1.GE.'A' .AND. KH1.LE.'Z') THEN
                II = ICHAR(KH1) - ICHRA + 1
                RNUM = PROB(II)
              ELSEIF (KH1.GE.'1' .AND. KH1.LE.'9') THEN
                II = ICHAR(KH1) - ICHR1 + 27
                RNUM = PROB(II)
              ELSE
                RNUM = -8.1E9
              ENDIF

              IF (RNUM .LE. -8.0E9) THEN
                CALL SHERR('E',63,KHPOS,KHAR)
              ELSE
                CODP = RNUM
              ENDIF
            ENDIF
          ENDIF
        ENDIF

      RETURN
      END
