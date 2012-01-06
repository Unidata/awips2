C  =====================================================================
C  pgm: SHTSCD .. Check if valid time series code (read "shefparm" file)
C
C  use:     CALL SHTSCD(CMD,KHAR,KHPOS,PARCOD)
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
      SUBROUTINE SHTSCD(CMD,KHAR,KHPOS,PARCOD)

      EXTERNAL       SHPABG,SHERR,SHSAVU

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*12   CMD
      CHARACTER*1    KHAR,KH1,KH2
      CHARACTER*2    KHFIND
      CHARACTER*8    PARCOD
      INTEGER        ITSCD(1225)
      INTEGER        KHPOS,INITZ,ICHRA,ICHR1,II,NU1,NU2,IERR,NUM,LUNP

      SAVE       INITZ,ICHRA,ICHR1,ITSCD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shtscd.f,v $
     . $',                                                             '
     .$Id: shtscd.f,v 1.7 2003/10/15 18:38:52 dws Exp $
     . $' /
C    ===================================================================
C

      DATA       INITZ,ITSCD,KHFIND / 0, 1225*-9, '*3' /

C                   If first pass, get PECNVT array from "shefparm" file

        IF (CMD.EQ.'INITIALIZE  ' .OR. INITZ.EQ.0) THEN
          INITZ = 0
          ICHRA = ICHAR('A')
          ICHR1 = ICHAR('1')
          CALL SHSAVU('G_SHEFPARM  ',LUNP)
          CALL SHPABG(LUNP,KHFIND,IERR)

C                   Code "RZ" is in ITSCD(621) and is the default
 
          IF (IERR .EQ. 0) THEN
            INITZ = 1
            READ(LUNP,'(A1,A1,1X,I2)',IOSTAT=IERR) KH1,KH2,NUM
            IF (IERR.NE.0 .OR. KH1.EQ.'*') ITSCD(621) = 1
  130       IF (IERR.NE.0 .OR. KH1.EQ.'*') GOTO 140
              NU1 = ICHAR(KH1) - ICHRA

              IF (NU1.LT.0 .OR. NU1.GT.25) THEN
                II = ICHAR(KH1) - ICHR1
                IF (II.GE.0 .AND. II.LE.8) THEN
                  NU1 = 26 + II
                ELSE
                  NU1 = 999
                ENDIF
              ENDIF

              IF (NU1.GE.0 .AND. NU1.LE.34) THEN
                NU2 = ICHAR(KH2) - ICHRA
                IF (NU2.GE.0 .AND. NU2.LE.25) THEN
                  II = 35*NU1 + NU2 + 1
                  ITSCD(II) = NUM
                ELSE
                  NU2 = NU2 + ICHRA - ICHR1
                  IF (NU2.GE.0 .AND. NU2.LE.8) THEN
                    II = 35*NU1 + NU2 + 27
                    ITSCD(II) = NUM
                  ENDIF
                ENDIF
              ENDIF
              READ(LUNP,'(A1,A1,1X,I2)',IOSTAT=IERR) KH1,KH2,NUM
              GOTO 130
  140       CONTINUE
          ELSEIF (IERR .LT. 0) THEN
            CALL SHERR('W',74,KHPOS,KHAR)
            ITSCD(621) = 1
            INITZ = 1
          ENDIF

          IF (IERR .GT. 0) THEN
            CALL SHERR('E',77,KHPOS,KHAR)
            KHPOS = -1
          ENDIF
        ENDIF

C                   Check if valid time series code exists in PARCOD

        IF (CMD .EQ. 'GET_VALUE   ') THEN
          NUM = -9
          IF (KHPOS .GT. 1) THEN
            KH1 = PARCOD(4:4)
            KH2 = PARCOD(5:5)

CC                  The following stmt will allow CBRFC type codes
CC
CC          IF (KH1.GE.'1' .AND. KH1.LE.'9') KH1 = 'R'

            NU1 = ICHAR(KH1) - ICHRA

            IF (NU1.LT.0 .OR. NU1.GT.25) THEN
              II = ICHAR(KH1) - ICHR1
              IF (II.GE.0 .AND. II.LE.8) THEN
                NU1 = 26 + II
              ELSE
                NU1 = 999
              ENDIF
            ENDIF

            IF (NU1.GE.0 .AND. NU1.LE.34) THEN
              NU2 = ICHAR(KH2) - ICHRA
              IF (NU2.GE.0 .AND. NU2.LE.25) THEN
                II = 35*NU1 + NU2 + 1
                NUM = ITSCD(II)
              ELSE
                NU2 = NU2 + ICHRA - ICHR1
                IF (NU2.GE.0 .AND. NU2.LE.8) THEN
                  II = 35*NU1 + NU2 + 27
                  NUM = ITSCD(II)
                ENDIF
              ENDIF
            ENDIF

            IF (NUM .LT. 0) CALL SHERR('E',34,KHPOS,KHAR)
          ENDIF
        ENDIF

      RETURN
      END
