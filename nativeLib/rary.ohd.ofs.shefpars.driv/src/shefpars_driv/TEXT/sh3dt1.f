C  =====================================================================
C  pgm: SH3DT1 .. Convert next 2 to 12 digits to calendar date and time
C
C  use:     CALL SH3DT1(KHAR,KHPOS,KODE,LYR,LMO,LDA,LHR,LMN,LSE)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: KODE ...... code for type of date-time expected (1-7) - INT
C  i/o: LYR ....... year number (1753-2199) and may be altered - INT
C  out: LMO ....... month number (1-12), else unchanged - INT
C  out: LDA ....... day number (1-31), else unchanged - INT
C  out: LHR ....... hour number (0-24), else unchanged - INT
C  out: LMN ....... minute number (0-59), else unchanged - INT
C  out: LSE ....... second number (0-59), else unchanged - INT
C
C  rqd: SHGETK,SH2NU2,SHCDAT,SHCTIM,SHERR
C  =====================================================================
      SUBROUTINE SH3DT1(KHAR,KHPOS,KODE,LYR,LMO,LDA,LHR,LMN,LSE)

      EXTERNAL       SHGETK,SH2NU2,SHCDAT,SHCTIM,SHERR

      CHARACTER*1    KHAR
      INTEGER        KHPOS,KODE,LYR,LMO,LDA,LHR,LMN,LSE,KOD,II,JJ
      INTEGER        LCN,LCNT,LYRT,LMOT,LDAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt1.f,v $
     . $',                                                             '
     .$Id: sh3dt1.f,v 1.6 1998/07/22 12:30:08 page Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .NE. 1) THEN

          KOD = KODE
          CALL SHGETK(KHAR,KHPOS)

          IF (KHAR.LT.'0' .OR. KHAR.GT.'9') THEN
              CALL SHERR('E',2,KHPOS,KHAR)

C                   Look for digit pairs to override date-time
C                    (If the hour is reached, set default min/sec to 0)

          ELSE
 
              LCN  = -1
              LCNT = LYR/100
              LYRT = LYR - 100*LCNT
              LMOT = LMO
              LDAT = LDA
 
                IF (KOD .EQ. 7) LYR = -1
              IF (KOD .GE. 7) CALL SH2NU2(KHAR,KHPOS,KOD,LCN)
              IF (KOD .GE. 6) CALL SH2NU2(KHAR,KHPOS,KOD,LYR)
              IF (KOD .GE. 5) CALL SH2NU2(KHAR,KHPOS,KOD,LMO)
              IF (KOD .GE. 4) CALL SH2NU2(KHAR,KHPOS,KOD,LDA)
              IF (KOD .GE. 3) CALL SH2NU2(KHAR,KHPOS,KOD,LHR)
                IF (KOD .GE. 3) LMN = 0
                IF (KOD .GE. 2) LSE = 0
              IF (KOD .GE. 2) CALL SH2NU2(KHAR,KHPOS,KOD,LMN)
              IF (KOD .GE. 1) CALL SH2NU2(KHAR,KHPOS,KOD,LSE)

              IF (KHPOS .NE. 1) THEN
                IF (KODE.LE.7 .AND. KODE.GE.5) THEN
                  CALL SH4DT0(LCNT,LYRT,LMOT,LDAT,LCN,LYR,LMO,LDA)
                ENDIF
                CALL SHCDAT(LYR,LMO,LDA,II)
                CALL SHCTIM(LHR,LMN,LSE,JJ)
                IF (II .GT. 0) THEN
                  CALL SHERR('E',16,KHPOS,KHAR)
                ELSEIF (JJ .GT. 0) THEN
                  CALL SHERR('E',17,KHPOS,KHAR)
                ENDIF
              ENDIF
          ENDIF

        ENDIF

      RETURN
      END
