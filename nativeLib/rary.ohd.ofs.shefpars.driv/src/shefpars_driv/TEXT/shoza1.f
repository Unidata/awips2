C  =====================================================================
C  pgm: SHOZA1 .. Adjust date to output zulu time
C
C  use:     CALL SHOZA1(KHAR,KHPOS,N1,N2,N3,N4,N5,N6)
C
C   in: (vars) .... output variables described elsewhere
C
C  rqd: SH3AD7,SH3ADY,SH3ADH,SH2LOC,SH2ADJ,SHSAVM,SHSAVL,SHSAVN
C  =====================================================================
      SUBROUTINE SHOZA1(KHAR,KHPOS,N1,N2,N3,N4,N5,N6)

      EXTERNAL       SH3AD7,SH3ADY,SH3ADH,SH2LOC,SH2ADJ,SHSAVM,SHSAVL
      EXTERNAL       SHSAVN

      CHARACTER*1    KHAR
      INTEGER        N1,N2,N3,N4,N5,N6
      INTEGER        MYR,MMO,MDA,MHR,MMN,MSE,MEN
      INTEGER        KHPOS,NFLAG,III,NEWADJ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shoza1.f,v $
     . $',                                                             '
     .$Id: shoza1.f,v 1.2 1997/12/31 20:34:23 page Exp $
     . $' /
C    ===================================================================
C

C                   If no error ...
C                   Get local obs time in N1-N6  - SHSAVL
C                   Get adjustment times         - SHSAVM
C                   Get 7-am data flag           - SHSAVN
C                   If NFLAG, adjust to prev 7am - SH3AD7
C                    else adj relative ccyymmdd  - SH3ADY
C                   Test local time, adj to zulu - SH2LOC,SH2ADJ
C                   Adjust for relative hhnnss   - SH3ADH

        IF (KHPOS .NE. 1) THEN

          CALL SHSAVL('G',III,N1,N2,N3,N4,N5,N6)
          CALL SHSAVM('G',III,MYR,MMO,MDA,MHR,MMN,MSE,MEN)
          CALL SHSAVN('G',III,NFLAG)

          IF (NFLAG .EQ. 1) THEN
              CALL SH3AD7(KHAR,KHPOS,N1,N2,N3,N4,N5,N6)
            ELSE
              CALL SH3ADY(KHAR,KHPOS,N1,N2,N3,N4,N5,N6)
          ENDIF

          CALL SH2LOC(KHAR,KHPOS,N1,N2,N3,N4,N5,N6,NEWADJ,0)
          CALL SH2ADJ(KHAR,KHPOS,N1,N2,N3,N4,N5,N6,NEWADJ,1)
          CALL SH3ADH(KHAR,KHPOS,N1,N2,N3,N4,N5,N6,MHR,MMN,MSE)

        ENDIF

      RETURN
      END
