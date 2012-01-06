C  =====================================================================
C  pgm: SH3AD7 .. Adjust observ time to prev 7am, check for no rel time
C
C  use:      CALL SH3AD7(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE)
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
C  rqd: SH2ADJ,SHERR,SHSAVM,SHSAVA
C  =====================================================================
      SUBROUTINE SH3AD7(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE)

      EXTERNAL       SH2ADJ,SHERR,SHSAVM,SHSAVA

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NYR,NMO,NDA,NHR,NMN,NSE
      INTEGER        MYR,MMO,MDA,MHR,MMN,MSE,MEN,III,LADJ,II
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3ad7.f,v $
     . $',                                                             '
     .$Id: sh3ad7.f,v 1.3 1998/04/07 19:09:17 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .NE. 1) THEN

          CALL SHSAVM('G',III,MYR,MMO,MDA,MHR,MMN,MSE,MEN)
          CALL SHSAVA('G',III,LADJ)

          IF (LADJ .EQ. 0) THEN
              CALL SHERR('E',35,KHPOS,KHAR)
            ELSEIF (MYR.NE.0 .OR. MMO.NE.0 .OR. MDA.NE.0) THEN
              CALL SHERR('E',35,KHPOS,KHAR)
            ELSEIF (MHR.NE.0 .OR. MMN.NE.0 .OR. MSE.NE.0) THEN
              CALL SHERR('E',35,KHPOS,KHAR)
            ELSE
              IF (NHR .LT. 7) THEN
                II = -1
                CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,II,3)
              ENDIF
              NHR = 7
              NMN = 0
              NSE = 0
          ENDIF

        ENDIF

      RETURN
      END
