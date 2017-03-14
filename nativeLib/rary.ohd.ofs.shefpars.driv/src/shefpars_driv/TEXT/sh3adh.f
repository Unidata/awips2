C  =====================================================================
C  pgm: SH3ADH .. Adjust date for relative hour-minute-second
C
C  use:      CALL SH3ADH(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,
C  use:                  MHR,MMN,MSE)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: NYR ....... year number (1753-2199) - INT
C  i/o: NMO ....... month number (1-12) - INT
C  i/o: NDA ....... day number (1-31) - INT
C  i/o: NHR ....... hour number (0-24) - INT
C  i/o: NMN ....... minute number (0-59) - INT
C  i/o: NSE ....... second number (0-59) - INT
C   in: MHR ....... hour number increment - INT
C   in: MMN ....... minute number increment - INT
C   in: MSE ....... second number increment - INT
C
C  rqd: SH2ADJ,SHCDAT,SHCTIM,SHERR
C  =====================================================================
      SUBROUTINE SH3ADH(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MHR,MMN,MSE)

      EXTERNAL       SH2ADJ,SHCDAT,SHCTIM,SHERR

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MHR,MMN,MSE,J1,J2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3adh.f,v $
     . $',                                                             '
     .$Id: sh3adh.f,v 1.3 1998/04/07 19:09:36 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .NE. 1) THEN
        IF (MHR.NE.0 .OR. MMN.NE.0 .OR. MSE.NE.0) THEN

          CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MSE,7)
          CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MMN,1)
          CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MHR,2)

          IF (KHPOS .NE. 1) THEN
            CALL SHCDAT(NYR,NMO,NDA,J1)
            CALL SHCTIM(NHR,NMN,NSE,J2)
            IF (J1 .NE. 0) THEN
              CALL SHERR('E',16,KHPOS,KHAR)
            ELSEIF (J2 .NE. 0) THEN
              CALL SHERR('E',17,KHPOS,KHAR)
            ENDIF
          ENDIF

        ENDIF
        ENDIF

      RETURN
      END
