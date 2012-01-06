C  =====================================================================
C  pgm: SH2BLA .. Get next non-blank char, return number of blanks found
C
C  use:     CALL SH2BLA(KHAR,KHPOS,NOFBLA)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C  out: NOFBLA .... number of blanks before non-blank - INT
C
C  rqd: SHGETK
C
C  cmt: Routine will NOT process if current char is "end-of-line".
C  cmt: "KHAR" must be guaranteed not blank if "KHPOS" is less than 2.
C  =====================================================================
      SUBROUTINE SH2BLA(KHAR,KHPOS,NOFBLA)

      EXTERNAL       SHGETK

      CHARACTER*1    KHAR
      INTEGER        KHPOS,NOFBLA,NCHAR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2bla.f,v $
     . $',                                                             '
     .$Id: sh2bla.f,v 1.1 1995/11/29 22:20:42 dws Exp $
     . $' /
C    ===================================================================
C

        NCHAR = 0

C                   Loop until next char is not blank (or eol)

   10   IF (KHAR .NE. ' ') GOTO 20
          NCHAR = NCHAR+1
          CALL SHGETK(KHAR,KHPOS)
          GOTO 10
   20   CONTINUE

C                   Output number of blanks found

        NOFBLA = NCHAR

      RETURN
      END
