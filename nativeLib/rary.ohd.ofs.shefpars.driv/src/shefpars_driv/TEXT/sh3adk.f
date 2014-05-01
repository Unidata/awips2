C  =====================================================================
C  pgm: SH3ADK .. Adjust the forecast date to zulu
C
C  use:      CALL SH3ADK(KHAR,KHPOS,K1,K2,K3,K4,K5,K6)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: K1 ........ adjusted zulu year number (1753-2199) - INT
C  out: K2 ........ adjusted zulu month number (1-12) - INT
C  out: K3 ........ adjusted zulu day number (1-31) - INT
C  out: K4 ........ adjusted zulu hour number (0-24) - INT
C  out: K5 ........ adjusted zulu minute number (0-59) - INT
C  out: K6 ........ adjusted zulu second number (0-59) - INT
C
C  rqd: SH2LOC,SH2ADJ,SHERR,SHSAVK,SHSAVP
C  =====================================================================
      SUBROUTINE SH3ADK(KHAR,KHPOS,K1,K2,K3,K4,K5,K6)

      EXTERNAL       SH2LOC,SH2ADJ,SHERR,SHSAVK,SHSAVP

      CHARACTER*1    KHAR
      CHARACTER*8    PARCOD
      INTEGER        KHPOS,III,K1,K2,K3,K4,K5,K6,NEWADJ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3adk.f,v $
     . $',                                                             '
     .$Id: sh3adk.f,v 1.3 1998/04/07 19:09:48 page Exp $
     . $' /
C    ===================================================================
C

        CALL SHSAVK('G',III,K1,K2,K3,K4,K5,K6)

        IF (KHPOS .NE. 1) THEN

          CALL SHSAVP('G',III,PARCOD)

          IF (PARCOD(4:4).EQ.'F' .AND. K2.LE.0) THEN
            CALL SHERR('W',36,KHPOS,KHAR)
          ENDIF

          IF (K2 .GT. 0) THEN
            CALL SH2LOC(KHAR,KHPOS,K1,K2,K3,K4,K5,K6,NEWADJ,0)
            CALL SH2ADJ(KHAR,KHPOS,K1,K2,K3,K4,K5,K6,NEWADJ,1)
            IF (KHPOS .EQ. 1) CALL SHERR('E',43,KHPOS,KHAR)
          ENDIF

        ENDIF

      RETURN
      END
