C  =====================================================================
C  pgm: SH2SKP .. Get next char that is not the given char (skp gvn chr)
C
C  use:     CALL SH2SKP(KHAR,KHPOS,CHR)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C   in: CHR ....... given character to be skipped - CHAR*1
C
C  rqd: SHGETK
C  =====================================================================
      SUBROUTINE SH2SKP(KHAR,KHPOS,CHR)

      EXTERNAL       SHGETK

      CHARACTER*1    KHAR,CHR
      INTEGER        KHPOS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2skp.f,v $
     . $',                                                             '
     .$Id: sh2skp.f,v 1.1 1995/11/29 22:20:48 dws Exp $
     . $' /
C    ===================================================================
C

C                   Loop until next char is not the given char (or eol)

   10   IF (KHAR.NE.CHR .OR. KHPOS.LE.2) GOTO 20
          CALL SHGETK(KHAR,KHPOS)
          GOTO 10
   20   CONTINUE

      RETURN
      END
