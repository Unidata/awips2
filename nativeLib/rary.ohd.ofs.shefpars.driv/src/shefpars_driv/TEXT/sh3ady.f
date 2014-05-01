C  =====================================================================
C  pgm: SH3ADY .. Adjust date for relative year-month-day
C
C  use:      CALL SH3ADY(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: NYR ....... year number (1753-2199) - INT
C  i/o: NMO ....... month number (1-12) - INT
C  i/o: NDA ....... day number (1-31) - INT
C  out: NHR ....... hour number (0-24) - INT
C  out: NMN ....... minute number (0-59) - INT
C  out: NSE ....... second number (0-59) - INT
C
C  rqd: SH2ADJ,SHCDAT,SHERR,SHSAVM
C  =====================================================================
      SUBROUTINE SH3ADY(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE)

      EXTERNAL       SH2ADJ,SHCDAT,SHERR,SHSAVM

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NYR,NMO,NDA,NHR,NMN,NSE
      INTEGER        MYR,MMO,MDA,MHR,MMN,MSE,MEN,III,II
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3ady.f,v $
     . $',                                                             '
     .$Id: sh3ady.f,v 1.3 1998/04/07 19:10:01 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .NE. 1) THEN

          CALL SHSAVM('G',III,MYR,MMO,MDA,MHR,MMN,MSE,MEN)

          IF (MYR.NE.0 .OR. MMO.NE.0 .OR. MDA.NE.0 .OR. MEN.NE.0) THEN

            CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MDA,3)
            CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MMO,4)
            CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MYR,5)
            CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MEN,6)

            IF (KHPOS .NE. 1) THEN
              CALL SHCDAT(NYR,NMO,NDA,II)
              IF (II .GT. 0) CALL SHERR('E',16,KHPOS,KHAR)
            ENDIF

          ENDIF

        ENDIF

      RETURN
      END
