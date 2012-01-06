C  =====================================================================
C  pgm: SH2NU2 .. Convert cur and next digit to positive number for date
C
C  use:     CALL SH2NU2(KHAR,KHPOS,KOD,NUMDAT)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: KOD ....... set to 0 if no number found, else unchanged - INT
C  out: NUMDAT .... integer value found, else unchanged - INT
C
C  rqd: SHGETK,SHERR
C  =====================================================================
      SUBROUTINE SH2NU2(KHAR,KHPOS,KOD,NUMDAT)

      EXTERNAL       SHGETK,SHERR

      INTRINSIC      ICHAR
      INTEGER        ICHAR

      CHARACTER*1    KHAR,KHARSV
      INTEGER        KHPOS,KOD,NUMDAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2nu2.f,v $
     . $',                                                             '
     .$Id: sh2nu2.f,v 1.1 1995/11/29 22:20:46 dws Exp $
     . $' /
C    ===================================================================
C

C                   If cur digit, save and get 2nd char ... if 2nd char
C                    is digit, create integer date and get next char ...
C                    else set KOD to 0

          IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN

            KHARSV = KHAR
            CALL SHGETK(KHAR,KHPOS)
            IF (KHAR.GE.'0' .AND. KHAR.LE.'9') THEN
              NUMDAT = 10*ICHAR(KHARSV) + ICHAR(KHAR) - 11*ICHAR('0')
              CALL SHGETK(KHAR,KHPOS)
            ELSE
              CALL SHERR('E',2,KHPOS,KHAR)
              KOD = 0
            ENDIF

          ELSE

            KOD = 0

          ENDIF

      RETURN
      END
