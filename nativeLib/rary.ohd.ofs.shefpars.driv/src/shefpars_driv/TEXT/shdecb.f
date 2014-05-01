C  =====================================================================
C  pgm: SHDECB .. Decode shef .B format messages
C
C  use:     CALL SHDECB(KHAR,KHPOS,IREV,ICONT,QUO,FORMT)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: IREV ...... revision code (0 = no, 1 = yes) - INT
C   in: ICONT ..... continuation code (0 = no, 1 = yes) - INT
C   in: QUO ....... string for quotes, only carried thru this rtn in
C   in:             order to define its size in one place - CHAR*(*)
C  out: FORMT ..... format used here is 'B' but changed to ' ' if
C  out:             an ".END" statement is found so that the B format
C  out:             is treated as done - this added Feb 2006
C
C  rqd: SH2BEG,SH2ST2,SHDOTB,SHERR,SHGETK,SHPCOD,SHPDEC,SHPOS
C  rqd: SHTYPA,SHSAVQ,SHSAVD,SHSAVI,SHSAVK,SHSAVL,SHSAVM,SHSAVP
C  rqd: SHSAVJ,SHSAVN,SHSAVA,SHSAVS
C
C  cmt: Hardwired for 200 fields (in stmt comparing "MREC .GE. 200").
C  =====================================================================
      SUBROUTINE SHDECB(KHAR,KHPOS,IREV,ICONT,QUO,FORMT)

      EXTERNAL       SH2BEG,SH2ST2,SHDOTB,SHERR,SHGETK,SHSAVA,SHSAVS
      EXTERNAL       SHPCOD,SHPDEC,SHPOS,SHTYPA,SHSAVQ,SHSAVJ,SHSAVN
      EXTERNAL       SHSAVD,SHSAVI,SHSAVK,SHSAVL,SHSAVM,SHSAVP

      CHARACTER*1    KHAR,KKK,FORMT
      CHARACTER*8    KHWRD,PARCOD,JKHID,PPP
      CHARACTER*(*)  QUO
      REAL           CODP
      INTEGER        KHPOS,IREV,ICONT,MREC,NOC,III

      SAVE           JKHID,MREC,CODP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shdecb.f,v $
     . $',                                                             '
     .$Id: shdecb.f,v 1.7 2006/03/07 15:18:53 dws Exp $
     . $' /
C    ===================================================================
C

C                   If first line messg, set defaults, get pos data

      IF (ICONT .EQ. 0) THEN
        CALL SHSAVK('I',III,III,III,III,III,III,III)
        CALL SHSAVM('I',III,III,III,III,III,III,III,III)
        CALL SHSAVQ('I',III,KKK)
        CALL SHSAVD('I',III,III)
        CALL SHSAVI('I',III,III)
        CALL SHSAVP('I',III,PPP)
         MREC   = 0
        CALL SHPOS(KHAR,KHPOS,JKHID)
      ENDIF

C                   Get the date and data type elements, parameter code

   30 IF (KHPOS .LE. 2) GOTO 34

        CALL SHTYPA(KHAR,KHPOS)
        CALL SHPCOD(KHAR,KHPOS)
        CALL SHPDEC(KHAR,KHPOS,CODP)

C                   Save data for next field using "SHSAV" rtns

        CALL SHSAVP('G',III,PARCOD)
        IF (KHPOS.GT.2 .OR. PARCOD.NE.'        ') THEN
            IF (PARCOD .EQ. '        ') THEN
              CALL SHERR('E',50,KHPOS,KHAR)
            ELSEIF (KHPOS .NE. 1) THEN
              IF (MREC .GE. 200) THEN
                CALL SHERR('E',40,KHPOS,KHAR)
              ELSE
                MREC = MREC+1
                CALL SHSAVP('S',MREC,PPP)
                CALL SHSAVL('S',MREC,III,III,III,III,III,III)
                CALL SHSAVK('S',MREC,III,III,III,III,III,III)
                CALL SHSAVM('S',MREC,III,III,III,III,III,III,III)
                CALL SHSAVA('S',MREC,III)
                CALL SHSAVS('S',MREC,III)
                CALL SHSAVN('S',MREC,III)
                CALL SHSAVJ('S',MREC,III)
                CALL SHSAVD('S',MREC,III)
                CALL SHSAVI('S',MREC,III)
                CALL SHSAVQ('S',MREC,KKK)
              ENDIF

              CALL SHSAVP('I',III,PPP)
            ENDIF

            IF (KHAR .EQ. '/') THEN
              CALL SHGETK(KHAR,KHPOS)
            ELSEIF (KHPOS .GT. 2) THEN
              CALL SHERR('E',33,KHPOS,KHAR)
            ENDIF
        ENDIF

        GOTO 30
   34 CONTINUE

C                   Check next line to see if it contains body data

      IF (KHPOS .NE. 1) THEN
        CALL SH2BEG(KHAR,KHPOS)

        IF (KHPOS .EQ. 0) THEN
          CALL SHERR('W',46,KHPOS,KHAR)
        ELSEIF (MREC.GT.0 .AND. KHPOS.GT.0 .AND. KHAR.NE.'.') THEN
          CALL SHDOTB(KHAR,KHPOS,MREC,IREV,JKHID,QUO)

C                   If KHPOS=0 then eof, else have dot line so see
C                   if ".END" otherwise the ".END" line is missing.
C                   This is a fatal error if we have a dot line
C                   with a continuation code, else just a warning.

          IF (KHPOS .EQ. 0) THEN
            CALL SHERR('W',46,KHPOS,KHAR)
          ELSEIF (KHPOS .GT. 1) THEN
            CALL SHGETK(KHAR,KHPOS)
            CALL SH2ST2(KHAR,KHPOS,NOC,KHWRD)
            IF (KHWRD .EQ. 'END     ') THEN
              KHPOS = 2
              KHAR  = ':'
              FORMT = ' '
            ELSEIF (KHWRD(1:1).EQ.'B' .AND. KHWRD(2:2).NE.' ') THEN
              CALL SHERR('E',46,KHPOS,KHAR)
            ELSE
              CALL SHERR('W',46,KHPOS,KHAR)
              KHPOS = 3
              CALL SHGETK(KHAR,KHPOS)
            ENDIF
          ENDIF

        ELSEIF (MREC.EQ.0 .AND. KHAR.NE.'.') THEN
            CALL SHERR('W',82,KHPOS,KHAR)

        ENDIF
      ENDIF

      RETURN
      END
