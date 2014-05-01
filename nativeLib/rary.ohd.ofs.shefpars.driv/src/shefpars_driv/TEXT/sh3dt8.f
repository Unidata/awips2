C  =====================================================================
C  pgm: SH3DT8 .. Get data date relative increment for "DR" keyword
C
C  use:     CALL SH3DT8(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK,SHERR,SH2NUM,SHSAVM
C  =====================================================================
      SUBROUTINE SH3DT8(KHAR,KHPOS)

      EXTERNAL       SHGETK,SHERR,SH2NUM,SHSAVM

      CHARACTER*1    KHAR,KGOOD
      INTEGER        KHPOS,MYR,MMO,MDA,MHR,MMN,MSE,MEN
      INTEGER        NOD,ISIGN,NUM,III
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh3dt8.f,v $
     . $',                                                             '
     .$Id: sh3dt8.f,v 1.4 2000/03/14 19:13:04 dws Exp $
     . $' /
C    ===================================================================
C

        IF (KHPOS .GT. 2 ) THEN
          CALL SHGETK(KHAR,KHPOS)

          IF     (KHAR.EQ.'Y' .OR. KHAR.EQ.'M' .OR. KHAR.EQ.'D') THEN
            KGOOD = KHAR
          ELSEIF (KHAR.EQ.'H' .OR. KHAR.EQ.'N' .OR. KHAR.EQ.'S') THEN
            KGOOD = KHAR
          ELSEIF (KHAR.EQ.'E' .OR. KHAR.EQ.'C') THEN
            KGOOD = KHAR
          ELSE
            KGOOD = ' '
            CALL SHERR('E',27,KHPOS,KHAR)
          ENDIF

          IF (KGOOD .NE. ' ') THEN
            CALL SHGETK(KHAR,KHPOS)
            ISIGN = 1
            IF (KHAR.EQ.'-')                  ISIGN = -1
            IF (KHAR.EQ.'-' .OR. KHAR.EQ.'+') CALL SHGETK(KHAR,KHPOS)
            CALL SH2NUM(KHAR,KHPOS,4,NOD,NUM)

            IF (NOD.GT.0 .AND. NOD.LE.2) THEN
              CALL SHSAVM('G',III,MYR,MMO,MDA,MHR,MMN,MSE,MEN)

              NUM = ISIGN*NUM
              IF     (KGOOD .EQ. 'Y') THEN
                MYR = NUM
              ELSEIF (KGOOD .EQ. 'M') THEN
                MMO = NUM
              ELSEIF (KGOOD .EQ. 'D') THEN
                MDA = NUM
              ELSEIF (KGOOD .EQ. 'H') THEN
                MHR = NUM
              ELSEIF (KGOOD .EQ. 'N') THEN
                MMN = NUM
              ELSEIF (KGOOD .EQ. 'S') THEN
                MSE = NUM
              ELSEIF (KGOOD .EQ. 'E') THEN
                MEN = NUM
              ELSEIF (KGOOD .EQ. 'C') THEN
                MYR = 100*NUM
              ENDIF

              CALL SHSAVM('P',III,MYR,MMO,MDA,MHR,MMN,MSE,MEN)
            ELSE
              CALL SHERR('E',28,KHPOS,KHAR)
            ENDIF

          ENDIF
        ENDIF

      RETURN
      END
