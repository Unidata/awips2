C  =====================================================================
C  pgm: SHQUAL .. Get or check data qualifier codes (read "shefparm")
C
C  use:     CALL SHQUAL(CMD,KHAR,KHPOS,KWAL)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ...... read shefparm file in sequence
C   in:               'GET_VALUE' ...... get shefparm value(s)
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: KWAL ...... set to qualifier code if found, else null - CHAR*1
C  out:             (set to '-' if warning of bad qualifier)
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHERR,SHSAVU,SH2NXD
C  =====================================================================
      SUBROUTINE SHQUAL(CMD,KHAR,KHPOS,KWAL)

      EXTERNAL       SHPABG,SHERR,SHSAVU,SH2NXD

      CHARACTER*12   CMD
      CHARACTER*1    KHAR,KH1,KWAL
      CHARACTER*26   KWALLS,KWALZZ
      CHARACTER*2    KHFIND
      INTEGER        KHPOS,INITZ,II,IERR,LUNP

      SAVE       INITZ,KWALLS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shqual.f,v $
     . $',                                                             '
     .$Id: shqual.f,v 1.6 2000/03/14 14:22:08 page Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,KHFIND / 0, '*7' /
      DATA       KWALZZ / 'ZZZZZZZZZZZZZZZZZZZZZZZZZZ' /

C                   If first pass, get KWASLS string from "shefparm"

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ  = 0
          KWALLS = KWALZZ
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(A1)',IOSTAT=IERR) KH1
  100       IF (IERR.NE.0 .OR. KH1.EQ.'*') GOTO 130
              IF (KH1.GE.'A' .AND. KH1.LE.'Z' .AND. KH1.NE.'Z') THEN

                II = 1
  110           IF (KWALLS(II:II).EQ.KH1 .OR. II.EQ.26) GOTO 120
                  IF (KWALLS(II:II) .EQ. 'Z') THEN
                    KWALLS(II:II) = KH1
                   ELSE
                    II = II+1
                  ENDIF
                  GOTO 110
  120           CONTINUE

              ENDIF
              READ(LUNP,'(A1)',IOSTAT=IERR) KH1
              GOTO 100
  130       CONTINUE
          ELSEIF (IERR .LT. 0) THEN
            CALL SHERR('W',88,KHPOS,KHAR)
            INITZ = 1
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

C                   Check if next char is a valid qualifier, set KWAL

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          KWAL = ' '
          IF ( (KHAR.GE.'A' .AND. KHAR.LE.'Z') .AND. KHPOS.GT.3 ) THEN
            II = 1
  160       IF (II .GT. 26) GOTO 170
              IF (KWALLS(II:II) .EQ. KHAR) THEN
                KWAL = KHAR
                II = 27
               ELSEIF (KWALLS(II:II) .EQ. 'Z') THEN
                II = 27
               ELSE
                II = II+1
              ENDIF
              GOTO 160
  170       CONTINUE

C                   Ok, have strange character, give warning

            IF (KWAL .EQ. ' ') THEN
              CALL SHERR('A',21,KHPOS,KHAR)
              CALL SH2NXD(KHAR,KHPOS)
              KWAL = '-'
            ENDIF
          ENDIF
        ENDIF

      RETURN
      END
