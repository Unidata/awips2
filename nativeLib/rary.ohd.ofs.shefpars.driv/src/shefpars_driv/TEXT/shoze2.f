C  =====================================================================
C  pgm: SHOZE2 .. Adjust date to output zulu time
C
C  use:     CALL SHOZE2(KHAR,KHPOS,IUNIT,NYR,NMO,NDA,NHR,NMN,NSE,
C  use:                 N1,N2,N3,N4,N5,N6)
C
C   in: (vars) .... output variables described elsewhere
C
C  rqd: SHCDAT,SHERR,SHSAVS,SH2LOC,SH2ADJ
C  =====================================================================
      SUBROUTINE SHOZE2(KHAR,KHPOS,IUNIT,NYR,NMO,NDA,NHR,NMN,NSE,
     $                  N1,N2,N3,N4,N5,N6)

      EXTERNAL       SHCDAT,SHERR,SHSAVS,SH2LOC,SH2ADJ

      CHARACTER*1    KHAR
      INTEGER        KHPOS,IUNIT,NYR,NMO,NDA,NHR,NMN,NSE,III,NEWADJ
      INTEGER        LSVG,N1,N2,N3,N4,N5,N6,JSTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shoze2.f,v $
     . $',                                                             '
     .$Id: shoze2.f,v 1.1 1995/11/29 22:21:51 dws Exp $
     . $' /
C    ===================================================================
C

C                   If no error ...
C                   Initialize output obs date
C                   Test local time, adj to zulu - SH2LOC,SH2ADJ

        IF (KHPOS .NE. 1) THEN

            CALL SHSAVS('G',III,LSVG)

            N1 = NYR
            N2 = NMO
            N3 = NDA
            N4 = NHR
            N5 = NMN
            N6 = NSE

            IF ((IUNIT.GE.3 .AND. IUNIT.NE.7) .OR. LSVG.EQ.0) THEN
              CALL SH2LOC(KHAR,KHPOS,N1,N2,N3,N4,N5,N6,NEWADJ,0)
              CALL SH2ADJ(KHAR,KHPOS,N1,N2,N3,N4,N5,N6,NEWADJ,1)
            ENDIF

            IF (KHPOS .NE. 1) THEN
              CALL SHCDAT(N1,N2,N3,JSTAT)
              IF (JSTAT .NE. 0) CALL SHERR('E',66,KHPOS,KHAR)
            ENDIF

        ENDIF

      RETURN
      END
