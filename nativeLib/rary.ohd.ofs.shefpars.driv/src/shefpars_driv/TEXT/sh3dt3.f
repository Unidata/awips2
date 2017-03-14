C  =====================================================================
C  pgm: SH3DT3 .. Get creation date from next set of digits
C
C  use:     CALL SH3DT3(KHAR,KHPOS,KODE,LY,LM,LD)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: KODE ...... set to 0 if no number found, else unchanged - INT
C   in: LY,LM,LD .. default 4-digit year, month, day - INT
C
C  rqd: SHGETK,SH2NU2,SHERR,SHCDAT,SHCTIM,SHSAVK,SHSAVA,SH4DT0
C  =====================================================================
      SUBROUTINE SH3DT3(KHAR,KHPOS,KODE,LY,LM,LD)

      EXTERNAL       SH2NU2,SHERR,SHCDAT,SHCTIM,SHGETK,SHSAVK,SHSAVA
      EXTERNAL       SH4DT0

      CHARACTER*1    KHAR
      INTEGER        KHPOS,KODE,III,KC,KY,KM,KD,KH,KN,KS,LY,LM,LD,LADJ
      INTEGER        KXX,KYY,KOD,II,JJ,LCT,LYT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt3.f,v $
     . $',                                                             '
     .$Id: sh3dt3.f,v 1.2 1997/12/31 20:15:26 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2) THEN
          KC  = -1
          KY  = -1
          KM  = -1
          KD  = -1
          KH  = -1
          KN  = -1
          KXX = -1
          KYY = -1
          KOD = KODE
          CALL SHGETK(KHAR,KHPOS)
          IF (KOD .EQ. 8) CALL SH2NU2(KHAR,KHPOS,KOD,KM)
          IF (KOD .EQ. 8) CALL SH2NU2(KHAR,KHPOS,KOD,KD)
          IF (KOD .EQ. 8) CALL SH2NU2(KHAR,KHPOS,KOD,KH)
          IF (KOD .EQ. 8) CALL SH2NU2(KHAR,KHPOS,KOD,KN)
          IF (KOD .EQ. 8) CALL SH2NU2(KHAR,KHPOS,KOD,KXX)
          IF (KOD .EQ. 8) CALL SH2NU2(KHAR,KHPOS,KOD,KYY)

          IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
            CALL SHERR('E',80,KHPOS,KHAR)
          ENDIF

          IF (KHPOS .NE. 1) THEN
            IF (KM.LT.0 .OR. KD.LT.0) THEN
              CALL SHERR('E',19,KHPOS,KHAR)
            ELSEIF (KH .LT. 0) THEN
              KH = 12
              KN = 0
              CALL SHSAVA('G',III,LADJ)
              IF (LADJ .NE. 0) KH = 24
            ELSEIF (KN .LT. 0) THEN
              KN = 0
            ELSEIF (KXX.GE.0 .AND. KYY.LT.0) THEN
              KY = KM
              KM = KD
              KD = KH
              KH = KN
              KN = KXX
            ELSEIF (KXX.GE.0 .AND. KYY.GE.0) THEN
              KC = KM
              KY = KD
              KM = KH
              KD = KN
              KH = KXX
              KN = KYY
            ENDIF

            LCT = LY/100
            LYT = LY - 100*LCT
            CALL SH4DT0(LCT,LYT,LM,LD,KC,KY,KM,KD)

            KS = 0
            CALL SHCDAT(KY,KM,KD,II)
            CALL SHCTIM(KH,KN,KS,JJ)
            IF (II .NE. 0) THEN
              CALL SHERR('E',16,KHPOS,KHAR)
            ELSEIF (JJ .NE. 0) THEN
              CALL SHERR('E',17,KHPOS,KHAR)
            ENDIF
          ENDIF

          CALL SHSAVK('P',III,KY,KM,KD,KH,KN,KS)
        ENDIF

      RETURN
      END
