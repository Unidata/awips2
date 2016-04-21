C  =====================================================================
C  pgm: SHEXCD .. Check for valid extremum code (read "shefparm" file)
C
C  use:     CALL SHEXCD(CMD,KHAR,KHPOS,PARCOD)
C
C   in: CMD ....... command or message to control operations - CHAR*12
C   in:               'INITIALIZE' ..... force read shefparm file
C   in:               'GET_VALUE' ...... get shefparm value(s)
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: PARCOD .... up to 8-char parameter code - CHAR*8
C   in: (file) .... sequential access file called "shefparm" - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFPARM',<number>)
C
C  rqd: SHPABG,SHERR,SHSAVU
C  =====================================================================
      SUBROUTINE SHEXCD(CMD,KHAR,KHPOS,PARCOD)

      EXTERNAL       SHPABG,SHERR,SHSAVU

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*12   CMD
      CHARACTER*1    KHAR,KH1,KH2
      CHARACTER*2    KHFIND
      CHARACTER*8    PARCOD
      INTEGER        KHPOS,INITZ,ICHRA,II,NU1,IERR,NUM,IEXCD(26),LUNP

      SAVE       INITZ,ICHRA,IEXCD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shexcd.f,v $
     . $',                                                             '
     .$Id: shexcd.f,v 1.4 1998/07/22 12:32:07 page Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,IEXCD,KHFIND / 0, 26*-9, '*4' /

C                   If first pass, get IEXCD array from "shefparm" file

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          ICHRA = ICHAR('A')
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)
 
          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(A1,A1,1X,I2)',IOSTAT=IERR) KH1,KH2,NUM
            IF (IERR.NE.0 .OR. KH1.EQ.'*') IEXCD(26) = 1
  130       IF (IERR.NE.0 .OR. KH1.EQ.'*') GOTO 140
              NU1 = ICHAR(KH1) - ICHRA
              IF (NU1.GE.0 .AND. NU1.LE.25) THEN
                II = NU1 + 1
                IEXCD(II) = NUM
              ENDIF
              READ(LUNP,'(A1,A1,1X,I2)',IOSTAT=IERR) KH1,KH2,NUM
              GOTO 130
  140       CONTINUE
          ELSEIF (IERR .LT. 0) THEN
            CALL SHERR('W',75,KHPOS,KHAR)
            IEXCD(26) = 1
            INITZ = 1
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

C                   Check if PARCOD has valid extremum code

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          NUM = -9
          IF (KHPOS .GT. 1) THEN
            KH1 = PARCOD(6:6)
            NU1 = ICHAR(KH1) - ICHRA
            IF (NU1.GE.0 .AND. NU1.LE.25) THEN
              II  = NU1 + 1
              NUM = IEXCD(II)
            ENDIF

            IF (NUM .LT. 0) CALL SHERR('E',61,KHPOS,KHAR)
          ENDIF
        ENDIF

      RETURN
      END
