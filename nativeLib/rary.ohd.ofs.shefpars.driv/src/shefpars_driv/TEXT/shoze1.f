C  =====================================================================
C  pgm: SHOZE1 .. Adjust date to output zulu time, output to binary file
C
C  use:     CALL SHOZE1(KHAR,KHPOS,IUNIT,INTVAL,ITMSER,
C  use:                 NYR,NMO,NDA,NHR,NMN,NSE)
C
C   in: (vars) .... output variables described elsewhere
C
C  rqd:  SH3ADY,SH3ADH,SH2LOC,SH2ADJ,SHSAVM,SHSAVL,SHSAVS
C  =====================================================================
      SUBROUTINE SHOZE1(KHAR,KHPOS,IUNIT,INTVAL,ITMSER,
     $                  NYR,NMO,NDA,NHR,NMN,NSE)

      EXTERNAL       SH3ADY,SH3ADH,SH2LOC,SH2ADJ,SHSAVM,SHSAVL,SHSAVS

      INTEGER        KHPOS,IUNIT,INTVAL,ITMSER,LSVG,NEWADJ
      INTEGER        MYR,MMO,MDA,MHR,MMN,MSE,MEN,III
      INTEGER        NYR,NMO,NDA,NHR,NMN,NSE
      CHARACTER*1    KHAR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shoze1.f,v $
     . $',                                                             '
     .$Id: shoze1.f,v 1.1 1995/11/29 22:21:50 dws Exp $
     . $' /
C    ===================================================================
C

C                   Date relative and time increment calculations
C                   'ITMSER' = 1 for first time series pass, then = 2

        IF (KHPOS.NE.1 .AND. ITMSER.EQ.0) THEN
            ITMSER = 1
            CALL SHSAVL('G',III,NYR,NMO,NDA,NHR,NMN,NSE)
            CALL SHSAVM('G',III,MYR,MMO,MDA,MHR,MMN,MSE,MEN)
            CALL SHSAVS('G',III,LSVG)

C                   Do relative adjustment for yr-mon-day, local time
C                   Convert to zulu if necessary (DIHxx, DINxx, DISxx)

            IF (MYR.NE.0 .OR. MMO.NE.0 .OR. MDA.NE.0) THEN
             CALL SH3ADY(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE)
             IF ((IUNIT.LT.3 .OR. IUNIT.EQ.7) .AND. LSVG.EQ.1) THEN
              CALL SH2LOC(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,0)
              CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,1)
             ENDIF

C                   Do relative adjustment for hr-min, zulu if necessary
C                   Convert back to local if (DIYx, DIMx, DIDx, DIEx)

            ELSEIF (MHR.NE.0 .OR. MMN.NE.0 .OR. MSE.NE.0) THEN
             CALL SH2LOC(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,0)
             CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,1)
             CALL SH3ADH(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,MHR,MMN,MSE)
             IF ((IUNIT.GE.3 .AND. IUNIT.NE.7) .OR. LSVG.EQ.0) THEN
              CALL SH2LOC(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,1)
              CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,-NEWADJ,1)
             ENDIF

C                   No date relatives, make sure time zone is correct

            ELSEIF ((IUNIT.LT.3 .OR. IUNIT.EQ.7) .AND. LSVG.EQ.1) THEN
             CALL SH2LOC(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,0)
             CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,NEWADJ,1)
            ENDIF

          ELSE
            ITMSER = 2

C                   Now do the time increment, should be in correct
C                    time zone at this stage

            CALL SH2ADJ(KHAR,KHPOS,NYR,NMO,NDA,NHR,NMN,NSE,INTVAL,IUNIT)
        ENDIF

      RETURN
      END
