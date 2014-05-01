C  =====================================================================
C  pgm: SHDECE .. Decode shef .E format messages
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
C  rqd: SH3ADD,SH3ADK,SH3ADT,SH3DEC,SH3DT4,SHDBLE,SHERR,SHSAVM
C  rqd: SHOZE1,SHOZE2,SHPCOD,SHPDEC,SHPOS,SHTYPE,SHSAVK,SHSAVQ,SHSAVD
C  rqd: SHSAVI,SHOUT,SHSAVN,SHSAVP,SHHRFX,SHQUOT,SH2NXD,SHGETK
C  =====================================================================
      SUBROUTINE SHDECE(KHAR,KHPOS,IREV,ICONT,QUO)

      EXTERNAL       SH3ADD,SH3ADK,SH3ADT,SH3DEC,SH3DT4,SHDBLE,SHERR
      EXTERNAL       SHSAVM,SHOZE1,SHOZE2,SHPCOD,SHPDEC,SHPOS,SHTYPE
      EXTERNAL       SHSAVK,SHSAVQ,SHSAVD,SHSAVI,SHOUT,SHSAVN,SHSAVP
      EXTERNAL       SHHRFX,SHQUOT,SH2NXD,SHGETK

      CHARACTER*1    KHAR,KWAL,LWAL,LCHAR,KKK
      CHARACTER*8    PARCOD,KHID,JKHID,PPP
      CHARACTER*(*)  QUO
      INTEGER        KHPOS,IREV,ICONT,IDUR,NDIG,NDEC,IUNIT,INTVAL,III
      INTEGER        K1,K2,K3,K4,K5,K6,N1,N2,N3,N4,N5,N6,NFLAG,LOOP2
      INTEGER        NY,NM,ND,NH,NN,NS,LNULL,ITMSER,IERR,NOC
      DOUBLE PRECISION   VALUE
      REAL               CODP

      SAVE     NY,NM,ND,NH,NN,NS,IUNIT,INTVAL,LNULL,LCHAR,ITMSER
      SAVE     CODP,KHID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shdece.f,v $
     . $',                                                             '
     .$Id: shdece.f,v 1.5 2001/06/13 14:12:22 dws Exp $
     . $' /
C    ===================================================================
C

      DATA     JKHID / '        ' /
 
C                   If first line messg, set defaults, get pos data

      IF (ICONT .EQ. 0) THEN
        CALL SHSAVK('I',III,III,III,III,III,III,III)
        CALL SHSAVM('I',III,III,III,III,III,III,III,III)
        CALL SHSAVQ('I',III,KKK)
        CALL SHSAVD('I',III,III)
        CALL SHSAVI('I',III,III)
        CALL SHSAVP('I',III,PPP)
         IUNIT  = -9999
         ITMSER = 0
         LCHAR  = ' '
         LNULL  = 0
        CALL SHPOS(KHAR,KHPOS,KHID)
      ENDIF

C                   Loop (30-60) thru messg line, loop (40-50) data desc

   30 IF (KHPOS .LE. 2) GOTO 60

        LOOP2 = 0
   40   IF (KHPOS.LE.2 .OR. LOOP2.EQ.1) GOTO 50
          CALL SHTYPE(KHAR,KHPOS,IUNIT,INTVAL,LCHAR,LNULL,ITMSER)

          CALL SHSAVP('G',III,PARCOD)
          IF (PARCOD .EQ. '        ') THEN
            CALL SHPCOD(KHAR,KHPOS)
            CALL SHPDEC(KHAR,KHPOS,CODP)
            LCHAR = ' '
          ENDIF

          IF (KHAR.NE.'/' .OR. LNULL.EQ.1) LOOP2 = 1
          GOTO 40
   50   CONTINUE

C                   Need parameter code, duration code, no 7am code

        IF (KHPOS.GT.2 .OR. LNULL.EQ.1) THEN
          CALL SHSAVP('G',III,PARCOD)
          IF (PARCOD .EQ. '        ')  CALL SHERR('E',50,KHPOS,KHAR)
          IF (IUNIT .EQ. -9999) CALL SHERR('E',45,KHPOS,KHAR)
          CALL SHSAVN('G',III,NFLAG)
          IF (NFLAG .EQ. 1)     CALL SHERR('E',35,KHPOS,KHAR)

C                   Adjust forecast date to zulu - SH3ADK
C                   Test for correct duration    - SH3ADD
C                   Save copy of cur qual code   - SHSAVQ
C                   Get the real value           - SHDBLE
C                   Check for trailing qualifier - SH3DT4
C                   Check if decimal required    - SH3DEC
C                   Adjust for trace value       - SH3ADT
C                   Look for quote               - SHQUOT
C                   Relative date increment calc - SHOZE1
C                   Adjust output date to zulu   - SHOZE2
C                   Get overall qualifier        - SHSAVQ
C                   Restore temp copy of qualfr  - SHSAVQ
C                   Output data to shefout file  - SHOUT

          CALL SH3ADK(KHAR,KHPOS,K1,K2,K3,K4,K5,K6)
          CALL SH3ADD(KHAR,KHPOS,IDUR)

          NDIG = 0
          IF (KHPOS.GT.2 .AND. LNULL.EQ.0) THEN
            CALL SHSAVQ('S',1,KKK)
            CALL SHDBLE(KHAR,KHPOS,VALUE,NDIG,NDEC)

            IF (NDIG .LE. 0) THEN
              IF (KHPOS .GT. 2) THEN
                IF (NDIG .EQ. 0) THEN
                  CALL SHERR('E',37,KHPOS,KHAR)
                ELSE
                  CALL SHQUOT(KHAR,KHPOS,QUO)
                ENDIF
              ENDIF

            ELSE
              CALL SH3DT4(KHAR,KHPOS,LWAL)
              IF (LWAL .EQ. '-') THEN
                NDIG = 0

              ELSE
                CALL SH2BLA(KHAR,KHPOS,NOC)
                IF (KHAR .EQ. ',') THEN
                  CALL SHERR('A',65,KHPOS,KHAR)
                  CALL SHGETK(KHAR,KHPOS)
                  CALL SH2NXD(KHAR,KHPOS)
                  NDIG = 0

                ELSE
                  CALL SH3DEC(KHAR,KHPOS,VALUE,NDEC)
                  CALL SH3ADT(KHAR,KHPOS,VALUE)
                ENDIF
              ENDIF
            ENDIF
            CALL SHQUOT(KHAR,KHPOS,QUO)
          ENDIF

          CALL SHOZE1(KHAR,KHPOS,IUNIT,INTVAL,ITMSER,NY,NM,ND,NH,NN,NS)

          IF (NDIG .GT. 0) THEN
            CALL SHOZE2(KHAR,KHPOS,IUNIT,NY,NM,ND,NH,NN,NS,
     $                  N1,N2,N3,N4,N5,N6)
            CALL SHSAVQ('G',III,KWAL)
            CALL SHSAVQ('R',1,KKK)
            IF (KHPOS .NE. 1) THEN

C        Constrain hour to 0-23:  Since the original shefpars allows an
C          hour of 0-24 in some places, a filter is placed here to move
C          a date that has an hour of 24 to the next day with an hour
C          of 1 (done 18 Sep 1997)

              CALL SHHRFX(N1,N2,N3,N4)
              CALL SHHRFX(K1,K2,K3,K4)

C        Output the decoded shef message

              CALL SHOUT(KHID,N1,N2,N3,N4,N5,N6,K1,K2,K3,K4,K5,K6,
     $                   PARCOD,IDUR,CODP,VALUE,KWAL,IREV,JKHID,ITMSER,
     $                   QUO,IERR)
              IF (IERR .NE. 0) CALL SHERR('E',70,KHPOS,KHAR)
            ENDIF
          ENDIF

        ENDIF
        GOTO 30
   60 CONTINUE

      RETURN
      END
