C  =====================================================================
C  pgm: SH2BEG .. Get next non-blank character on a non-blank line
C
C  use:     CALL SH2BEG(KHAR,KHPOS)
C
C  i/o: KHAR ...... last buffer char obtained - CHAR*1
C  i/o: KHPOS ..... last char loc: 2=eol,1=err-eol,0=eof,neg=err - INT
C
C  rqd: SHGETK
C
C  cmt: "KHAR" must be guaranteed not blank if "KHPOS" is less than 3.
C  =====================================================================
      SUBROUTINE SH2BEG(KHAR,KHPOS)

      EXTERNAL       SHGETK

      CHARACTER*1    KHAR
      INTEGER        KHPOS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/sh2beg.f,v $
     . $',                                                             '
     .$Id: sh2beg.f,v 1.2 1996/03/18 22:54:13 page Exp $
     . $' /
C    ===================================================================
C

C                   Loop until char is not blank nor an end-of-line
C                   (i.e. read thru trailing blanks, eol, any blank
C                    lines, and up to the next non-blank char)

   10   IF (KHAR.NE.' ' .AND. (KHPOS.NE.1 .AND. KHPOS.NE.2)) GOTO 20
          CALL SHGETK(KHAR,KHPOS)
          GOTO 10
   20   CONTINUE

      RETURN
      END
