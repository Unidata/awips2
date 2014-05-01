C  =====================================================================
C  pgm: SH3DT2 .. Convert next 2-5 digits to cal date (using julian day)
C
C  use:     CALL SH3DT2(KHAR,KHPOS,LYR,LMO,LDA)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  i/o: LYR ....... year number (1753-2199) and may be altered - INT
C  out: LMO ....... month number (1-12), else unchanged - INT
C  out: LDA ....... day number (1-31), else unchanged - INT
C
C  rqd: SHGETK,SH2DT2,SHGCAL,SHERR,SH4DT2
C  =====================================================================
      SUBROUTINE SH3DT2(KHAR,KHPOS,LYR,LMO,LDA)

      EXTERNAL      SHGETK,SH2DT2,SHGCAL,SHERR,SH4DT2

      INTRINSIC     ICHAR
      INTEGER       ICHAR

      CHARACTER*1   KHAR,KH1,KH2,KH3,KH4,KH5,KH6,KH7
      INTEGER       KHPOS,LYR,LMO,LDA,NOFD,JUL,IYR,IZ
      INTEGER       LCN,LCNT,LYRT,LMOT,LDAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt2.f,v $
     . $',                                                             '
     .$Id: sh3dt2.f,v 1.4 2000/03/17 20:03:00 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2) THEN

C                   Save original date using separate century/year nums

          LCN  = -1
          LCNT = LYR/100
          LYRT = LYR - 100*LCNT
          LMOT = LMO
          LDAT = LDA

C                   Get up to 7 digits (NOFD is number of digits found)

          NOFD = 0
          CALL SHGETK(KHAR,KHPOS)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH1)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH2)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH3)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH4)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH5)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH6)
          CALL SH2DT2(KHAR,KHPOS,NOFD,KH7)

C                   If 1-3 digits, have julian date only
C                   If   5 digits, have 2-digit year and julian date
C                   If   7 digits, have century and yr and julian
C                   Else have number error so set JUL to -1

          IF (KHPOS .NE. 1) THEN
            IZ = ICHAR('0')
            IYR = -1
            IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
              JUL = -1
            ELSEIF (NOFD .EQ. 1) THEN
              JUL = ICHAR(KH1)-IZ
              CALL SHERR('W',79,KHPOS,KHAR)
            ELSEIF (NOFD .EQ. 2) THEN
              JUL = 10*ICHAR(KH1)+ICHAR(KH2)-11*IZ
              CALL SHERR('W',79,KHPOS,KHAR)
            ELSEIF (NOFD .EQ. 3) THEN
              JUL = 100*ICHAR(KH1)+10*ICHAR(KH2)+ICHAR(KH3)-111*IZ
            ELSEIF (NOFD .EQ. 5) THEN
              IYR = 10*ICHAR(KH1)+ICHAR(KH2)-11*IZ
              JUL = 100*ICHAR(KH3)+10*ICHAR(KH4)+ICHAR(KH5)-111*IZ
            ELSEIF (NOFD .EQ. 7) THEN
              LCN = 10*ICHAR(KH1)+ICHAR(KH2)-11*IZ
              IYR = 10*ICHAR(KH3)+ICHAR(KH4)-11*IZ
              JUL = 100*ICHAR(KH5)+10*ICHAR(KH6)+ICHAR(KH7)-111*IZ
            ELSE
              JUL = -1
            ENDIF

C                   Check for reasonable date, if so set to LYR,LMO,LDA
C                   Adjust for century or 183 day rules, get cal date

            IF (JUL.GE.1 .AND. JUL.LE.366) THEN
              CALL SH4DT2(LCNT,LYRT,LMOT,LDAT,LCN,IYR,JUL)
               LYR = IYR
              CALL SHGCAL(JUL,LYR,LMO,LDA)
              IF (LMO .GT. 12) JUL = -1
            ELSE
              JUL = -1
            ENDIF

C                   If JUL = -1 then have error in date numbers

            IF (JUL .EQ. -1) CALL SHERR('E',16,KHPOS,KHAR)
          ENDIF

        ENDIF

      RETURN
      END
