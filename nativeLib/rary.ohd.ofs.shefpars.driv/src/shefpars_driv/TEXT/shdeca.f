C  =====================================================================
C  pgm: SHDECA .. Decode shef .A format messages
C
C  use:     CALL SHDECA(KHAR,KHPOS,IREV,ICONT,QUO)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: IREV ...... revision code (0 = no, 1 = yes) - INT
C   in: ICONT ..... continuation code (0 = no, 1 = yes) - INT
C   in: QUO ....... string for quotes, only carried thru this rtn in
C   in:             order to define its size in one place - CHAR*(*)
C
C  rqd: SH2BLA,SH2SKP,SH3ADD,SH3ADK,SH3ADT,SH3DEC,SH3DT4,SHDBLE,SHERR
C  rqd: SHSAVM,SHOZA1,SHPCOD,SHPDEC,SHPOS,SHTYPA,SHSAVK,SHSAVQ,SHSAVD
C  rqd: SHSAVI,SHOUT,SHSAVP,SHHRFX,SHQUOT,SHGETK,SH2FND
C  =====================================================================
      SUBROUTINE SHDECA(KHAR,KHPOS,IREV,ICONT,QUO)

      EXTERNAL       SH2BLA,SH2SKP,SH3ADD,SH3ADK,SH3ADT,SHOUT,SHSAVP
      EXTERNAL       SH3DEC,SH3DT4,SHDBLE,SHERR,SHSAVM,SHSAVK,SHSAVI
      EXTERNAL       SHOZA1,SHPCOD,SHPDEC,SHPOS,SHTYPA,SHSAVQ,SHSAVD
      EXTERNAL       SHHRFX,SHQUOT,SHGETK,SH2FND

      CHARACTER*1    KHAR,KWAL,LWAL,KKK
      CHARACTER*8    PARCOD,JKHID,KHID,PPP
      CHARACTER*(*)  QUO
      INTEGER        KHPOS,IREV,ICONT,IDOTE,NDIG,NDEC,IDUR,NOC
      INTEGER        K1,K2,K3,K4,K5,K6,N1,N2,N3,N4,N5,N6,III,IERR
      DOUBLE PRECISION   VALUE
      REAL               CODP

      SAVE    KHID,CODP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shdeca.f,v $
     . $',                                                             '
     .$Id: shdeca.f,v 1.6 2001/06/13 14:12:07 dws Exp $
     . $' /
C    ===================================================================
C

      DATA    IDOTE,JKHID / 0, '        ' /

C                   If first line messg, set defaults, get pos data

      IF (ICONT .EQ. 0) THEN
        CALL SHSAVK('I',III,III,III,III,III,III,III)
        CALL SHSAVM('I',III,III,III,III,III,III,III,III)
        CALL SHSAVQ('I',III,KKK)
        CALL SHSAVD('I',III,III)
        CALL SHSAVI('I',III,III)
        CALL SHSAVP('I',III,PPP)
        CALL SHPOS(KHAR,KHPOS,KHID)
      ENDIF

C                   Loop through data elements

   30 IF (KHPOS .LE. 2) GOTO 34

C                   Get date, data type elements - SHTYPA
C                   Initialize parameter code    - SHSAVP
C                   Get the parameter code       - SHPCOD
C                   Edit parameter codes         - SHPDEC
C                   Get PARCOD, check if good    - SHSAVP
C                   Get the real value           - SHDBLE
C                   Check for trailing qualifier - SH3DT4
C                   Check if decimal required    - SH3DEC
C                   Adjust forecast date to zulu - SH3ADK
C                   Check for correct duration   - SH3ADD
C                   Adjust for trace value       - SH3ADT
C                   Look for quote               - SHQUOT
C                   Adjust output date to zulu   - SHOZA1
C                   Get value qualifier code     - SHSAVQ
C                   Output data to shefout file  - SHOUT
C                   Skip any blanks in msg line  - SH2BLA
C                   Skip over slashes            - SH2SKP

        CALL SHTYPA(KHAR,KHPOS)

        IF (KHPOS .GT. 2) THEN
         CALL SHSAVP('I',III,PPP)
         CALL SHPCOD(KHAR,KHPOS)
         CALL SHPDEC(KHAR,KHPOS,CODP)
         IF (KHPOS .GT. 0) THEN
          CALL SHSAVP('G',III,PARCOD)
          IF (PARCOD .EQ. '        ') CALL SHERR('E',3,KHPOS,KHAR)

          NDIG = 0
          IF (KHPOS.LE.2) THEN
            IF(KHPOS.NE.1) CALL SHERR('A',37,KHPOS,KHAR)
          ELSEIF (KHAR.EQ.'/') THEN
            CALL SH2SKP(KHAR,KHPOS,'/')
            CALL SH2BLA(KHAR,KHPOS,NOC)
            CALL SHDBLE(KHAR,KHPOS,VALUE,NDIG,NDEC)
            IF (NDIG .EQ. 0) THEN
              IF(KHPOS.NE.1) CALL SHERR('A',37,KHPOS,KHAR)
            ELSE
              IF(KHPOS.NE.1) CALL SHERR('W',87,KHPOS,KHAR)
            ENDIF
          ELSE
            CALL SHDBLE(KHAR,KHPOS,VALUE,NDIG,NDEC)
            IF (NDIG.EQ.0 .AND. KHPOS.NE.1) THEN
              CALL SHERR('A',37,KHPOS,KHAR)
            ENDIF
            IF (NDIG .LE. 0) THEN
              CALL SH2FND(KHAR,KHPOS,'/')
            ENDIF
          ENDIF

          IF (NDIG .GT. 0) THEN
            CALL SH3DT4(KHAR,KHPOS,LWAL)
            IF (LWAL .EQ. '-') THEN
              CALL SH2FND(KHAR,KHPOS,'/')

            ELSE
              CALL SH2BLA(KHAR,KHPOS,NOC)
              IF (KHAR .EQ. ',') THEN
                CALL SHERR('A',65,KHPOS,KHAR)
                CALL SHGETK(KHAR,KHPOS)
                CALL SH2FND(KHAR,KHPOS,'/')

              ELSE
                CALL SH3DEC(KHAR,KHPOS,VALUE,NDEC)
                CALL SH3ADK(KHAR,KHPOS,K1,K2,K3,K4,K5,K6)
                CALL SH3ADD(KHAR,KHPOS,IDUR)
                CALL SH3ADT(KHAR,KHPOS,VALUE)
                CALL SHQUOT(KHAR,KHPOS,QUO)

                CALL SHOZA1(KHAR,KHPOS,N1,N2,N3,N4,N5,N6)
                CALL SHSAVQ('G',III,KWAL)
                IF (KHPOS .NE. 1) THEN
 
C        Constrain hour to 0-23:  Since the original shefpars allows an
C          hour of 0-24 in some places, a filter is placed here to move
C          a date that has an hour of 24 to the next day with an hour
C          of 1 (done 18 Sep 1997)
 
                  CALL SHHRFX(N1,N2,N3,N4)
                  CALL SHHRFX(K1,K2,K3,K4)

C        Output the decoded shef message

                  CALL SHOUT(KHID,N1,N2,N3,N4,N5,N6,K1,K2,K3,K4,K5,K6,
     $                       PARCOD,IDUR,CODP,VALUE,KWAL,IREV,JKHID,
     $                       IDOTE,QUO,IERR)
                  IF (IERR .NE. 0) CALL SHERR('E',70,KHPOS,KHAR)
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          CALL SH2BLA(KHAR,KHPOS,NOC)
          CALL SH2SKP(KHAR,KHPOS,'/')
         ENDIF
        ENDIF

        GOTO 30
   34 CONTINUE

      RETURN
      END
